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

    ## Testing make_cat_var_obj
    ll <- vector(mode = "list", length = 2)
    var_name <- "AGEGRP"
    type <- "ord"
    bounds <- data.frame(lower = c(0, 9), upper=c(8, Inf))
        ## The old grouping
        ##    bounds <- data.frame(lower=c(0, 5, 7, 10, 12, 15, 18, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85),
        ##                       upper =  c(4, 6, 9, 11, 14, 17, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf))
    category_name <- "AGEGRP"
    test <- make_cat_var_obj(var_name, type, bounds, category_name)

})


## library(devtools)
## load_all()
## path <- "~/Dropbox/Montreal"
## syneco <- read.csv(file.path(path, "spew_montreal/people_4620700.01.csv"))
## pums_h <- read.csv(file.path(path, "olympus_montreal_pums/pums_h.csv"))
## pums_h <- subset(pums_h, puma_id ==462)
## pums_p <- read.csv(file.path(path, "olympus_montreal_pums/pums_p.csv"))
## pums_p <- subset(pums_p, puma_id ==462)
## colnames(pums_p)
## colnames(pums_h)

