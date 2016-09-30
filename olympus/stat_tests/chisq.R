## SKG
## 8/19/2016
## Making figure for chi square
## ## Testing whether what we see is normal
## SKG
## 8/19/2016
## Making boxplots for chi square

setwd("~/spew")
library(devtools)
library(plyr)
library(ggplot2)
load_all()

## SRS vs PUMS generation
output_folder <- "~/Desktop/46/"
PUMS_folder <- "~/Desktop/46/input/pums/2013/"
t <- proc.time()[3]
features_list <- test_features(output_folder, PUMS_folder,
                          household_vars = NULL, # c("NP", "HINCP"),
                          people_vars = NULL, #c("SEX", "AGEP"),
                          householder_vars = c("NP", "HINCP", "RAC1P", "AGEP", "SEX" ))
proc.time()[3] - t
my_df_1<- makeStatDF(features_list)

############## MARGINALS and IPF ######################################

output_folder <- "~/Dropbox/ipf_midas/synthpop_ipf"
marginal_folder <- "~/Desktop/46/input/marginals"
householder_vars = c("NP", "HINCP", "RAC1P", "AGEP")
##
t <- proc.time()[3]
features_list_m<- test_features_marg(output_folder, marginal_folder,
                                     householder_vars = householder_vars)
proc.time()[3] - t
##
my_df_2 <- makeStatDF(features_list_m)
my_df_2$pval <- ifelse(is.nan(my_df_2$pval),  1, my_df_2$pval)
alpha <- .05 / (nrow(my_df))
alpha_df <- data.frame(alpha = alpha, lt = "alpha value")
##my_df$vars <- factor(my_df$vars, levels = c("NP", "HINCP", "AGEP", "RAC1P"), labels = c("Household Size",
    #                                                                              "Household Income", "Head of Household Age",
      #                                                                            "Head of Household Race"))
bad_inds <- which(my_df_2$pval < alpha)
length(bad_inds) / nrow(my_df_2)
sub_df <- my_df[bad_inds,]
length(unique(as.character(sub_df$regionID)))
tab <- table(as.character(sub_df$regionID))
length(which(tab > 1))
mean(sub_df$nObs)
sub_df2 <- my_df[-bad_inds,]
mean(sub_df2$nObs)

###########################################3
## Joint distribution
##########################################3

output_folder <- "~/Dropbox/ipf_midas/synthpop_ipf"
joint_folder <- "~/Dropbox/ipf_midas/synthpop_ipf/tables"
marginal_folder <- "~/Desktop/46/input/marginals"
householder_vars = c("NP", "HINCP", "RAC1P", "AGEP")
output_path <- output_paths[1]
##
t <- proc.time()[3]
features_list_j<- test_features_joint(output_folder, joint_folder,
                                      householder_vars = householder_vars,
                                      marginal_folder = marginal_folder)
proc.time()[3] - t
##
my_df_3 <- makeStatDF(features_list_j)
my_df_3$pval <- ifelse(is.nan(my_df$pval),  1,  my_df$pval)
alpha <- .05 / (nrow(my_df))
alpha_df <- data.frame(alpha = alpha, lt = "alpha value")
bad_inds <- which(my_df_3$pval < alpha)
length(bad_inds) / nrow(my_df_3)
sub_df <- my_df_3[bad_inds,]
length(unique(as.character(sub_df$regionID)))
tab <- table(as.character(sub_df$regionID))
length(which(tab > 1))
mean(sub_df$nObs)
sub_df2 <- my_df_3[-bad_inds,]
mean(sub_df2$nObs)


############ Looking at srs
############## MARGINALS and SRS ######################################
output_folder <- "~/Desktop/46/"
marginal_folder <- "~/Desktop/46/input/marginals"
householder_vars = c("NP", "HINCP", "RAC1P", "AGEP")
##
t <- proc.time()[3]
features_list_ms<- test_features_marg(output_folder, marginal_folder,
                                     householder_vars = householder_vars)
proc.time()[3] - t
##
my_df_4 <- makeStatDF(features_list_ms)
bad_inds <- which(my_df_4$pval < alpha)
length(bad_inds) / nrow(my_df_4)


############ Looking at srs
############## JOINT and SRS ######################################


output_folder <- "~/Desktop/46/"
marginal_folder <- "~/Desktop/46/input/marginals"
householder_vars = c("NP", "HINCP", "RAC1P", "AGEP")
##
t <- proc.time()[3]
features_list_js<- test_features_joint(output_folder, joint_folder,
                                     householder_vars = householder_vars, marginal_folder)
proc.time()[3] - t
##
my_df_5<- makeStatDF(features_list_js)


write.csv(my_df_1, "srs_pums.csv", row.names = FALSE)
write.csv(my_df_2, "ipf_marg.csv", row.names = FALSE)
write.csv(my_df_3, "ipf_joint.csv", row.names = FALSE)
write.csv(my_df_4, "srs_marg.csv", row.names = FALSE)
write.csv(my_df_1, "srs_joint.csv", row.names = FALSE)
