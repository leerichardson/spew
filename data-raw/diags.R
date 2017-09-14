## SKG
## August 8, 2017
## Purpose: Create raw data to test the diagnostics functions

## Make fake US data
id <-  "42003000001"
nchar(id)
N <- 10
us_df_h1<- data.frame(place_id  = id,
                    NP = sample(1:3, N, replace = TRUE),
                    HINCP = sample(runif(N, 0, 500000), N, replace = TRUE))
write.csv(us_df_h1,
          paste0("../tests/test_data/42/output/output_101/eco/household_",
                 id, ".csv"),
          row.names = FALSE)
M <- sum(us_df_h1$NP)
us_df_p1<- data.frame(place_id = id, 
                      SEX = sample(1:2, M, replace = TRUE),
                      RAC1P = sample(1:2, M, replace = TRUE),
                      AGEP = sample(1:20, M, replace = TRUE),
                    longitude = runif(M, 0, .5),
                    latitude = runif(M, 0, .5),
                    school_id = sample(c(NA, NA, NA, 1:3), M, replace = TRUE),
                    workplace_id = sample(c(NA, NA, NA, 1:3), M, replace = TRUE))
write.csv(us_df_p1,
          paste0("../tests/test_data/42/output/output_101/eco/people_",
                 id, ".csv"),
          row.names = FALSE)
                      
## Another tract same county
id <- "42003000002"
nchar(id)
N <- 15
us_df_h2 <- data.frame(place_id = id,
                    NP = sample(1:3, N, replace = TRUE),
                    HINCP = sample(runif(N, 0, 500000), N, replace = TRUE))
write.csv(us_df_h2,
          paste0("../tests/test_data/42/output/output_101/eco/household_",
                 id, ".csv"),
          row.names = FALSE)

M <- sum(us_df_h2$NP)
us_df_p2 <- data.frame(place_id = id,
                      SEX = sample(1:2, M, replace = TRUE),
                      RAC1P = sample(1:2, M, replace = TRUE),
                      AGEP = sample(1:20, M, replace = TRUE),
                      longitude = runif(M, .5, 1),
                      latitude = runif(M, 0, .5),
                      school_id = sample(c(NA, NA, NA, 3:6), M, replace = TRUE),
                      workplace_id = sample(c(NA, NA, NA, 3:5), M, replace = TRUE))
write.csv(us_df_p2,
          paste0("../tests/test_data/42/output/output_101/eco/people_",
                 id, ".csv"),
          row.names = FALSE)

## A different county
id <- "42005000001"
nchar(id)
N <- 12
us_df_h1<- data.frame(place_id = id,
                    NP = sample(1:3, N, replace = TRUE),
                    HINCP = sample(runif(N, 0, 500000), N, replace = TRUE))
write.csv(us_df_h1,
          paste0("../tests/test_data/42/output/output_101/eco/household_",
                 id, ".csv"),
          row.names = FALSE)
M <- sum(us_df_h1$NP)
us_df_p1<- data.frame(place_id = id,
                      SEX = sample(1:2, M, replace = TRUE),
                      RAC1P = sample(1:2, M, replace = TRUE),
                      AGEP = sample(1:20, M, replace = TRUE),
                      longitude = runif(M),
                      latitude = runif(M, .5, 1),
                      school_id = sample(c(NA, NA, NA, 7:9), M, replace = TRUE),
                      workplace_id = sample(c(NA, NA, NA, 8:10), M, replace = TRUE))
write.csv(us_df_p1,
          paste0("../tests/test_data/42/output/output_101/eco/people_",
                 id, ".csv"),
          row.names = FALSE)

### Make marginal information for fake US data
## HINCP
var_name <- "HINCP"
ipf_df <- NULL
bounds <- data.frame(lower = c(0, 50000, 100000),
                     upper = c(50001, 100001, Inf))
category_name <- c("HHINC_0-50", "HINC_50-100", "HINC_500-Inf")
type <- "ord"
marg_HINCP <- list(HINCP = list(
                       df = ipf_df,
                       type = type,
                       lookup = data.frame(marg_names = category_name,
                                           bounds, stringsAsFactors = FALSE)
                       ))
## NP
var_name <- "NP"
ipf_df <- NULL
bounds <- data.frame(lower = c(1, 2, 3),
                     upper = c(1, 2, 3))
category_name <- c("NP_1", "NP_2", "NP_3")
type <- "ord"
marg_NP <- list(NP = list(
                       df = ipf_df,
                       type = type,
                       lookup = data.frame(marg_names = category_name,
                                           bounds, stringsAsFactors = FALSE)
                ))
## RAC1P
var_name <- "RAC1P"
ipf_df <- NULL
bounds <- data.frame(lower = c(1, 2),
                     upper = c(1,2))
category_name <- c("White", "PoC")
type <- "cat"
marg_RAC1P <- list(RAC1P = list(
                       df = ipf_df,
                       type = type,
                       lookup = data.frame(marg_names = category_name,
                                           bounds,  stringsAsFactors = FALSE)
                   ))
## AGE
var_name <- "AGEP"
ipf_df <- NULL
bounds <- data.frame(lower = c(1, 10),
                     upper = c(9, 20))
category_name <- c("Young", "Less Young")
type <- "cat"
marg_AGEP <- list(AGEP = list(
                       df = ipf_df,
                       type = type,
                       lookup = data.frame(marg_names = category_name,
                                           bounds, stringsAsFactors = FALSE)
                 ))
## SEX
var_name <- "SEX"
ipf_df <- NULL
bounds <- data.frame(lower = c(1, 2),
                     upper = c(1,2))
category_name <- c("Male", "Female")
type <- "cat"
marg_SEX <- list(SEX = list(
                       df = ipf_df,
                       type = type,
                       lookup = data.frame(marg_names = category_name,
                                           bounds , stringsAsFactors = FALSE)
                 ))

marginals_obj <- list(NP = marg_NP[[1]], HINCP = marg_HINCP[[1]],
                      RAC1P = marg_RAC1P[[1]], SEX = marg_SEX[[1]],
                      AGEP = marg_AGEP[[1]])
saveRDS(marginals_obj, "../tests/test_data/42/marginals/marg_us.RDS")


## Fake IPUMS data

id <-  "chair"
N <- 1
df_h1<- data.frame(place_id  = id,
                    NP = 1,
                    HINCP = 40)
write.csv(df_h1,
          paste0("../tests/test_data/vatican/output/output_1/eco/household_",
                 id, ".csv"),
          row.names = FALSE)
M <- sum(us_df_h1$NP)
df_p1<- data.frame(place_id = id, 
                      SEX = 1,
                      RAC1P = 1,
                      AGEP = 72,
                    longitude = .5,
                    latitude = .5)
write.csv(df_p1,
          paste0("../tests/test_data/vatican/output/output_1/eco/people_",
                 id, ".csv"),
          row.names = FALSE)

## The conclave
       
id <- "conclave"
N <- 300
df_h2 <- data.frame(place_id = id,
                    NP = sample(1:3, N, replace = TRUE),
                    HINCP = sample(1:100, N, replace = TRUE))
write.csv(df_h2,
          paste0("../tests/test_data/vatican/output/output_2/eco/household_",
                 id, ".csv"),
          row.names = FALSE)

M <- sum(df_h2$NP)
df_p2 <- data.frame(place_id = id,
                      SEX = sample(1, M, replace = TRUE),
                      RAC1P = sample(1:2, M, replace = TRUE),
                      AGEP = sample(50:100, M, replace = TRUE),
                      longitude = runif(M, 0, 1),
                      latitude = runif(M, 0, 1))
write.csv(df_p2,
          paste0("../tests/test_data/vatican/output/output_2/eco/people_",
                 id, ".csv"),
          row.names = FALSE)
