#' Add a characteristic to an existing population
#'
#' @param synth_pop_path path to folder where the synth pop(s) is/are
#' @param args list of additional arguments
#' @param pop_type either "household" or "people"  population or "both" for household characteristics joined to people
#' @param method "demo_matching"
#' @param doPar logical, default is FALSE
#' @return a written synth pop with added characteristics
add_characteristic <- function(synth_pop_path, args, pop_type = "people",
                               method="demo_matching", doPar = FALSE){
    char_fun <-switch(method,
                      demo_matching = add_char_demo
                      )
    type <- pop_type
    if (type == "both") type <- "people"
    file_names <-  grep(paste0(type, ".+\\.csv"), list.files(synth_pop_path), value = TRUE)
    # if not in parallel
    if(!doPar){
        out <- lapply(file.path(synth_pop_path, file_names), char_fun,
                      synth_pop_path, pop_type, args)
    } else{
    }
    return(out)
}

#' Add characteristic by matching on demographics
#'
#' @param synth_pop_fn full name of the synth pop
#' @param output_path the path to the folder of where the new output is to go
#' @param pop_type "b" for both; "household", or "people" populations
#' @param args list of additional arguments including the suffix
#' @return logical, writes out new synth pop)
add_char_demo <- function(synth_pop_fn, output_path, pop_type, args){
    ## Read in the population
    if (pop_type == "both"){
        synth_pop_p<- read.csv(synth_pop_fn)
        synth_pop_h <- read.csv(gsub("people", "household", synth_pop_fn))
        synth_pop <- join(synth_pop_p,
                          synth_pop_h, by=c("SERIALNO", "SYNTHETIC_HID",
                                            "place_id", "puma_id", "longitude", "latitude")) # WARNING MAKE MORE GENERIC
    } else {
        synth_pop <- read.csv(synth_pop_fn)
    }
    ## Align the variables we want to match on
    marginals <- args$marginals
    stopifnot(!is.null(marginals))
    var_names <- names(marginals)
    aligned_pop <- align_pums(synth_pop, marginals)
    char_pums <- args$char_pums
    stopifnot(!is.null(char_pums))
    ## Subset the synth pop and the char pums and add them to the demographics
    synth_pop_sp <- dlply(aligned_pop, .variables = paste0(var_names, "_marg"),
                          .fun = identity)
    new_df <- ldply(synth_pop_sp, .fun = demo_sample, char_pums, var_names, args)
    stopifnot(nrow(synth_pop) == nrow(new_df))
    ## write out the new pop
    suffix <- args$suffix
    if(is.null(suffix)) suffix <- "_v2"
    output_path <- paste0(args$output_path, suffix)
    if(!dir.exists(output_path)) dir.create(output_path)
    write.csv(new_df, file.path(output_path, basename(synth_pop_fn)), row.names = FALSE)
    return(TRUE)
}

#' Sample extra characteristics from char pums and add them to the pop df
#'
#' @param pop_df a df that has been subsetted
#' @param char_pums data frame of PUMS characteristics to add
#' @param var_names variables we want to match on
#' @param args list of additional arguments
demo_sample <- function(pop_df, char_pums, var_names, args = NULL){
    criteria <- as.character(pop_df[1, paste0(var_names, "_marg")])
                                        # subsetting the char_pums to match the demographics
    char_pums2 <- subset(char_pums, select = var_names)
    inds <- apply(char_pums2, 1, function(row) identical(as.character(row), criteria))
    if (sum(inds) == 0 ){
        char_sub <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(char_pums)))
        colnames(char_sub) <- colnames(char_pums)
    }else {
        char_sub <- char_pums[inds, ,  drop = FALSE]
     }
    sampled_inds <- sample(1:nrow(char_sub), nrow(pop_df), replace = TRUE)
    char_df_full <- char_sub[sampled_inds,]
    ## join together
    new_df <- cbind(pop_df, char_df_full[, -c(colnames(char_df_full) %in% var_names)])
    return(new_df)
}



#' Set up for creating a set of marginal information for IPF sampling
#'
#' @param var_name name of the variable matching the PUMS/microdata
#' @param type "ord" for ordinal or  "cat" for categorical, the type of variable to be converted to
#' @param bounds dataframe of upper and lower bounds (inclusive) for variables (numeric)
#' @param category_names short name of the category, will be visible to person.  Either length one and the bounds will be pasted to it or length of the number of rows of the bounds with names of your choice.
#' @param output_file if not NULL then we save the file as a RDS object to output_file
#' @param df data frame with place_id and category counts
#' @return a list in the format for use in IPF
#' @export
make_ipf_obj<- function(var_name, type="ord", bounds, category_name, output_file = NULL, df = NULL){
    stopifnot(all(is.numeric(c(bounds[, 1], bounds[, 2]))))
    stopifnot(sum(colnames(bounds) %in% c("upper", "lower")) == 2)
    stopifnot(all(bounds[,1] <= bounds[,2]))
    stopifnot(all(bounds[1:(nrow(bounds) -1), 1] < bounds[2:nrow(bounds), 1]))
    stopifnot(type %in% c("ord", "cat"))
    marg_names <- category_name
    if(length(category_name) != nrow(bounds)){
        stopifnot(length(category_name) == 1)
        marg_names <-  apply(bounds, 1,
                             function(row) paste(category_name, row[1], row[2], sep = "-"))
    }
    if(!is.null(df)){
        stopifnot(colnames(df)[1] == "place_id")
        colnames(df) <- c("place_id", marg_names)  
    }
    ll <- list(df = df, type=type,
               lookup=data.frame(marg_names = marg_names, bounds, stringsAsFactors = FALSE))
    new_ll <- list(ll)
    names(new_ll) <- var_name
    new_ll <- c(new_ll)
    if (!is.null(output_file)) saveRDS( new_ll, output_file)
   
    return(new_ll)
}


