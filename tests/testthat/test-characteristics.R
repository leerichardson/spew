context("Additional characteristic assignments to existing pops")

test_that("Characteristic Assignment Functions", {

    ## demo_sample
    df <- data.frame(Race = c(1, 1, 2, 1), Age = c(25,30, 25, 25), ID = c("A","B","C","D"))    
    char_pums <- df
    criteria <- data.frame(Race=1, Age=25)
    var_names <- c("Race", "Age")
    n <- 50
    pop_df <- data.frame(Race_marg= rep(1, n), Age_marg= rep(25, n), dogs = rep(1:2, n/2))
    out <- demo_sample(pop_df, char_pums, var_names)
    expect_equal(nrow(out), nrow(pop_df))
    expect_equal( sum(paste0(var_names, "_marg") %in% colnames(out)), length(var_names))

    
    ## the case when there are no pums that match the pop df
    df <- data.frame(Race = c(3, 3, 2, 3), Age = c(25,30, 25, 25), ID = c("A","B","C","D"))    
    char_pums <- df
    out <- demo_sample(pop_df, char_pums, var_names)
    expect_equal(nrow(na.omit(out)), 0)
})


