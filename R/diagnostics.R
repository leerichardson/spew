#' Append populations within a directory into two separate files:  household and people populations.
#'
#' @param input_dir path to the directory (bottom level) of populations to append
#' @param output_dir path to output directory
#' @param remove_pops TRUE if we should we remove the smaller populations
#' @return TRUE
append_pops <- function(input_dir, output_dir=input_dir, remove_pops=FALSE){
    filenames <- list.files(input_dir)
    filenames_hh <- filenames[ grep("household", filenames) ]
    filenames_p <- filenames[ grep("people", filenames) ]
    #read in the files
    header_hh <- names(read.csv(paste0(input_dir, filenames_hh[1])))
    header_p <- names(read.csv(paste0(input_dir, filenames_p[1])))
    #warning!!! potentially veryyy slow
    # we do not read the header of every file
    hh_list <- lapply(paste0(input_dir, filenames_hh), read.table, sep=",", skip=1)
    p_list <- lapply(paste0(input_dir, filenames_p), read.table, sep=",", skip=1)
    # rowbinding the list to make one large file
    hh_comb <- do.call('rbind', hh_list)
    rm(hh_list)
    p_comb <- do.call('rbind', p_list)
    rm(p_list)
    colnames(hh_comb) <- header_hh
    colnames(p_comb) <- header_p
    puma_id <- hh_comb$puma_id[1]
    new_filename_hh <- paste0("household_", puma_id, ".csv")
    new_filename_p <- paste0("people_", puma_id, ".csv")
    #remove old files if asked
    if(remove_pops){
        file.remove(paste0(input_dir, filenames_hh))
        file.remove(paste0(input_dir, filenames_p))
    }
    #write out the files
    write.csv(hh_comb, paste0(output_dir, new_filename_hh), row.names=FALSE)
    write.csv(p_comb, paste0(output_dir, new_filename_p), row.names=FALSE)
    return(TRUE) 
}




#' Run basic 'sanity check' diagnostics on a folder of SPEW generated populations.  There should only be one household and one people file in each folder.  If not, look into append_pops().
#' 
#' @param input_dir path to the directory (bottom level) of populations to summarize
#' @param output_dir path to output directory
#' @param save_plots boolean indicating whether the plots should be saved
#' @param pretty if set to TRUE, should generate a summary pdf including plots (currently non-functional)
#' @return  a print out of the files generated

run_diags <- function(input_dir="./", output_dir=input_dir, save_plots=TRUE, pretty=FALSE){
    #read in filenames
    filenames <- list.files(input_dir)
    filename_hh <- filenames[ grep("household", filenames) ]
    filename_p <- filenames[ grep("people", filenames) ]
    stopifnot(length(filename_hh) * length(filename_p) == 1)
    hh_df <- data.table::fread(paste0(input_dir, filename_hh))
    # write log file
    
    puma_id <- unique(hh_df$puma_id)
    log_filename <- paste0("log_", puma_id, ".txt")
    if(!prettY){
      sink(paste0(output_dir, log_filename))
    }
    print(paste("PUMA ID:", puma_id))
    print("\n")
    print("HOUSEHOLDS")
    print("\n")
    print(paste("Number of households:", nrow(hh_df)))
    print("\n")
    print("Household column names:")
    print(colnames(hh_df))
    print("\n")
    print(paste("Average Longitude:", round(mean(hh_df$long),3)))
    print(paste("Average Latitude:", round(mean(hh_df$lati),3)))
    print("\n")
    print(paste("Number of unique households sampled:", length(unique(hh_df$SERIALNO))))
    print("\n")
    if(!pretty){
      sink()
    }
    
    rm(hh_df)

    #the people information
    p_df <- data.table::fread(paste0(input_dir, filename_p))
  
    if(!pretty){
      sink(paste0(output_dir, log_filename), append=TRUE)
    }
    print("PEOPLE")
    print("\n")
    print(paste("Number of individuals:", nrow(p_df)))
    print("\n")
    print("Column names:")
    print(colnames(p_df))
    mfr<-sum(p_df$SEX == 1)/sum(p_df$SEX == 2)
    print(paste("Male to Female Ratio:", round(mfr, 2)))
    if(!pretty){
    sink()
    }
    return(TRUE)

}
