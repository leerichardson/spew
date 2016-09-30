#!/usr/bin/Rscript

## SKG
## 9/30/2016
## Automating chi square tests for states

args <- commandArgs(TRUE) 
base_folder <- args[1] #folder to state (ending in FIPS code)
summary_folder <- args[2] # where we want the written output to go
alpha <- args[3] # alpha value (between 0 and 1)


library(devtools)
library(plyr)
## library(spew)
load_all("~/spew")


## Test tract population characteristics against marginal distributions
##
############## MARGINALS and IPF ######################################
output_folder <- base_folder
##output_folder <- "~/Desktop/10"
##marginal_folder <- "~/Desktop/10/input/marginals"
householder_vars = c("NP", "HINCP", "RAC1P", "AGEP")
output_folder <- file.path(output_folder, "output")
marginal_folder <- file.path(base_folder,  "input", "marginals",  "natstat", "2014", "tract")
##
t <- proc.time()[3]
features_list_m<- spew:::test_features_marg(output_folder, marginal_folder,
                                     householder_vars = householder_vars)
proc.time()[3] - t
##
my_df_2 <- spew:::makeStatDF(features_list_m)
my_df_2$pval <- ifelse(is.nan(my_df_2$pval),  1, my_df_2$pval)
puma_count <- table(my_df_2$puma_id)
alpha <- as.numeric(alpha)
alpha_mc<- alpha / (min(puma_count))
alpha_df <- data.frame(alpha = alpha_mc, lt = "alpha value")

bad_inds <- which(my_df_2$pval < alpha_mc)
rej_percent <- length(bad_inds) / nrow(my_df_2)
print(paste(round(rej_percent,3), "% of tracts are rejected"))
sub_df <- my_df_2[bad_inds,]
sub_df$alpha <- alpha
sub_df$alpha_mc <- alpha_mc

state <- basename(base_folder)

rej_df <- data.frame(state = state, rej_p = rej_percent, n = nrow(my_df_2))


## Write out summaries
write.csv(rej_df, file.path(summary_folder, paste0(state, "_rejection_df.csv")),
                            row.names = FALSE)
write.csv(sub_df, file.path(summary_folder, paste0(state, "_flagged_tracts.csv")),
          row.names = FALSE)
