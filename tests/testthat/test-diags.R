context("Diagnostic functions")

test_that("individual functions for diagnostics work",{
    ## check path
    data_path <- system.file("extdata/test_data/42", package = "spew")
    files <- list.files(data_path)
    expect_true( "output" %in% files)

    ## get_header
    header_p <- get_header(data_path, type = "people")
    expect_true(all( c("place_id", "SEX", "RAC1P", "AGEP", "longitude", "latitude") %in% header_p))
    header_h <- get_header(data_path, type = "household")
    expect_true(all(c("place_id",  "NP", "HINCP") %in% header_h))

    ## get_filenames
    ## For US
    ## County level
    file_list <- get_filenames(data_path, summary_level = 2,
                               agent_type = "household",
                               pop_type = "US")
    expect_equal(length(file_list), 2)
    expect_equal(length(file_list[[1]]), 2)
    expect_equal(length(file_list[[1]]$files), 2)
    expect_equal(length(file_list[[2]]$files), 1)
    expect_equal(file_list[[1]]$id, "42003")
    expect_equal(file_list[[2]]$id, "42005")

    ## Summarize state level
    file_list <- get_filenames(data_path, summary_level = 1,
                               agent_type = "household",
                               pop_type = "US")
    expect_equal(length(file_list), 1)
    expect_equal(length(file_list[[1]]), 2)
    expect_equal(length(file_list[[1]]$files), 3)
    expect_equal(file_list[[1]]$id,  "42")

     ## Summarize state level
    file_list <- get_filenames(data_path, summary_level = 1,
                               agent_type = "household",
                               pop_type = "US")
    expect_equal(length(file_list), 1)
    expect_equal(length(file_list[[1]]), 2)
    expect_equal(length(file_list[[1]]$files), 3)
    expect_equal(file_list[[1]]$id,  "42")

    ## Summarize tract level
    file_list <- get_filenames(data_path, summary_level = 3,
                               agent_type = "household",
                               pop_type = "US")
    expect_equal(length(file_list), 3)
    expect_equal(length(file_list[[1]]), 2)
    expect_equal(length(file_list[[1]]$files), 1)
    expect_equal(file_list[[1]]$id,  "42003000001")

    ## Make sure we have household file
    file_list <- get_filenames(data_path, summary_level = 3,
                               agent_type = "household",
                               pop_type = "US")
    expect_true(grepl("household", file_list[[1]]$files[1]))
    ## Make sure we have person file
    file_list <- get_filenames(data_path, summary_level = 3,
                               agent_type = "people",
                               pop_type = "US")
    expect_true(grepl("people", file_list[[1]]$files[1]))

    ## check_var_names
    vars_to_sum_h <- c("HINCP", "NP")
    vars_to_sum_p <- c("RAC1P", "AGEP", "SEX")
    expect_true(check_var_names(header_h, header_p,
                                vars_to_sum_h, vars_to_sum_p))
    vars_to_sum_h <- c("HINCP", "NP")
    vars_to_sum_p <- c("RAC1P", "AGEP", "dog")
    expect_true(!check_var_names(header_h, header_p,
                                 vars_to_sum_h, vars_to_sum_p))
    vars_to_sum_env <- c("school_id")
    expect_true(!check_var_names(header_h, header_p,
                                 vars_to_sum_h, vars_to_sum_p, vars_to_sum_env))
        
    

    ## summarize_spew
    filenames_h <-  get_filenames(data_path, summary_level = 2,
                               agent_type = "household",
                               pop_type = "US")
    sub_files <- filenames_h[[1]]
    vars_to_sum <- c("HINCP", "NP")
    
    marginals <- readRDS(file.path(data_path, "marginals/marg_us.RDS"))
    read_vars <- vars_to_sum
    df <- as.data.frame(do.call('rbind',
                                lapply(sub_files$files, data.table::fread, select = read_vars)))

    ## Align pums
    df_aligned <- align_pums(df[, vars_to_sum], marginals[vars_to_sum])

    ## summarize_features
    features_tab <- summarize_features(df_aligned, marginals, vars_to_sum)
    expect_equal(length(features_tab), length(vars_to_sum))
    expect_equal(length(features_tab[[1]]), 3)
    expect_equal(sum(features_tab[[1]]), nrow(df_aligned))

    ## making it still works when not all variables are marginal versions
    marg2 <- marginals[-1] ## taking out NP
    vars_to_sum <- c("HINCP", "NP")
    marg_vars <- which(names(marg2) %in% vars_to_sum)
    df_aligned <- align_pums(df[, vars_to_sum], marg2[marg_vars])
    features_tab <- summarize_features(df_aligned, marg2, vars_to_sum)
    expect_true(all(as.vector(features_tab[[1]]) == c(7, 8, 10)))

    ## summarize_env
    filenames_p <-  get_filenames(data_path, summary_level = 2,
                               agent_type = "people",
                               pop_type = "US")
    sub_files <- filenames_p[[2]]
    vars_to_sum <- c("RAC1P", "AGEP")
    env_vars <- c("school_id", "workplace_id")
    marginals <- readRDS(file.path(data_path, "marginals/marg_us.RDS"))
    read_vars <- c(vars_to_sum, env_vars, "longitude", "latitude")
    df <- as.data.frame(do.call('rbind',
                                lapply(sub_files$files, data.table::fread, select = read_vars)))
    env_sum <- summarize_environment(df, env_vars)
    expect_true(sum(!is.na(env_sum[[2]])) == 3)
    

    ## summarize_spew
    vars_to_sum <- c("NP", "HINCP")
    sub_files <- filenames_h[[1]]
    households_summary <- summarize_spew(sub_files, marginals, vars_to_sum,
                                         coords = FALSE, read = TRUE)
    expect_equal(length(households_summary), 5)
    #
    sub_files <-  filenames_h[[2]]
    households_summary <- summarize_spew(sub_files, marginals, vars_to_sum,
                                         coords = FALSE, read = TRUE)
    expect_equal(length(households_summary), 5)
    expect_true(all(unlist(households_summary$NP) == c(4, 5, 3)))

    ## Look at all regions
    vars_to_sum <- c("HINCP", "NP")
    filenames_h <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "household",
                                  pop_type = "US")
    summary_h <- lapply(filenames_h, summarize_spew,
                        marginals, vars_to_sum, coords = FALSE, read = TRUE)
    ## People
    vars_to_sum <- c("RAC1P", "AGEP")
    filenames_p <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "people",
                                  pop_type = "US")
    summary_p <- lapply(filenames_p, summarize_spew,
                        marginals, vars_to_sum, coords = TRUE, read = TRUE)
    expect_true(!is.null(summary_p[[1]]$coords_df))

     ## People with environment vars
    vars_to_sum <- c("RAC1P", "AGEP")
    env_vars <- c("school_id", "workplace_id")
    filenames_p <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "people",
                                  pop_type = "US")
    summary_p <- lapply(filenames_p, summarize_spew,
                        marginals, vars_to_sum, env_vars,
                        coords = FALSE, read = TRUE)
    expect_true("school_id" %in% names(summary_p[[1]]))

    ## Get_dfs without marginals
    vars_to_sum <- c("RAC1P", "AGEP")
    env_vars <- c("school_id", "workplace_id")
    filenames_p <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "people",
                                  pop_type = "US")
    summary_p <- lapply(filenames_p, summarize_spew,
                        marginals = NULL, vars_to_sum, env_vars,
                        coords = FALSE, read = TRUE)

    dfs <- get_dfs(summary_p, vars_to_sum)

    ## Organize_summaries
    vars_to_sum <- c("HINCP", "NP")
    filenames_h <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "household",
                                  pop_type = "US")
    summary_h <- lapply(filenames_h, summarize_spew,
                        marginals, vars_to_sum, coords = FALSE, read = TRUE)
    ## People
    vars_to_sum <- c("RAC1P", "AGEP")
    env_vars <- c("school_id", "workplace_id")
    filenames_p <-  get_filenames(data_path, summary_level = 2,
                                  agent_type = "people",
                                  pop_type = "US")
    summary_p <- lapply(filenames_p, summarize_spew,
                        marginals, vars_to_sum, env_vars, coords = TRUE, read = TRUE)
    vars_to_sum_h <- c("HINCP", "NP")
    vars_to_sum_p <- c("RAC1P", "AGEP")
    top_region_id <-  42
    out_list <- organize_summaries(summary_h, summary_p,
                                   header_h, header_p,
                                   vars_to_sum_h, vars_to_sum_p, env_vars,
                                   samp_size=100, top_region_id, coords=TRUE)
    expect_equal(length(out_list), 13)
    out_list <- organize_summaries(summary_h, summary_p,
                                   header_h, header_p,
                                   vars_to_sum_h, vars_to_sum_p, env_vars=NULL,
                                   samp_size=100, top_region_id, coords=FALSE)
    expect_equal(length(out_list), 11)

})

test_that("US diags work", {
    data_path <- system.file("extdata/test_data/42", package = "spew")  
    marginals <- readRDS(file.path(data_path, "marginals/marg_us.RDS"))
    summaries <- summarize_top_region(output_dir = data_path,
                                      type="US",
                                      vars_to_sum_h = c("NP", "HINCP"), 
                                      vars_to_sum_p = c("RAC1P", "AGEP"),
                                      vars_to_sum_env = c("school_id", "workplace_id"),
                                      samp_size=100,
                                      summary_level=2,
                                      marginals = marginals)
    expect_true(length(summaries) == 13)
})


test_that("IPUMS diags work", {
    data_path <- system.file("extdata/test_data/vatican", package = "spew")
    summaries <- summarize_top_region(output_dir = data_path,
                                      type="IPUMS",
                                      vars_to_sum_h = c("NP", "HINCP"), 
                                      vars_to_sum_p = c("RAC1P", "AGEP", "SEX"),
                                      vars_to_sum_env = NULL,
                                      samp_size=100,
                                      summary_level=2,
                                      marginals = NULL)
    expect_true(summaries$n_house == 301)
})


test_that("Custom diags", {
})


