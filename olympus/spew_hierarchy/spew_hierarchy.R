# Lee Richardson, 5/27/2016 
# Purpose: Generate the .csv file to use for building 
# the new SPEW hierarchy structure on Olympus 

# Read in the downloaded data from:
# https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes
un_hierarchy <- read.csv("/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/un_download/all/all.csv",
                         stringsAsFactors = FALSE)
un_hierarchy <- un_hierarchy[, -5]
names(un_hierarchy) <- c("country_name", "iso2", "iso3", "numerical_id", "region", "sub_region", "region_id", "sub_region_id")
un_lower <- do.call(cbind, lapply(un_hierarchy, tolower))
un_df <- as.data.frame(un_lower, stringsAsFactors = FALSE)

# Fix various bugs in the hierarchy by making sure 
# that every columnis filled in  ------------------
un_df[which(un_df[, 1] == "antarctica"), 5:8] <- c("antarctica", "antarctica", "10", "10")
un_df[which(un_df[, 1] == "bouvet island"), 5:8] <- c("europe", "northern europe", "150", "154")
un_df[which(un_df[, 1] == "british indian ocean territory"), 5:8] <- c("asia", "south-eastern asia", "142", "35")
un_df[which(un_df[, 1] == "christmas island"), 5:8] <- c("oceania", "australia and new zealand", "9", "53")
un_df[which(un_df[, 1] == "cocos (keeling) islands"), 5:8] <- c("oceania", "australia and new zealand", "9", "53")
un_df[which(un_df[, 1] == "french southern territories"), 5:8] <- c("africa", "eastern africa", "2","14")
un_df[which(un_df[, 1] == "heard island and mcdonald islands"), 5:8] <- c("oceania", "australia and new zealand", "9", "53")
un_df[which(un_df[, 1] == "south georgia and the south sandwich islands"), 5:8] <- c("americas", "south america", "19", "5")
un_df[which(un_df[, 1] == "united states minor outlying islands"), 5:8] <- c("americas", "northern america", "19", "21")

# Remove the white-space from the sub regions 
un_df$sub_region <- gsub(pattern = " ", "_", un_df$sub_region)
un_df$sub_region <- gsub(pattern = "-", "_", un_df$sub_region)

# Verify the entire data-frame is filled out 
stopifnot(all(!is.na(un_df$iso3)))
stopifnot(all(!is.na(un_df$country_name)))
stopifnot(all(!is.na(un_df$region)))
stopifnot(all(!is.na(un_df$sub_region)))

# Write the new data frame out as a .csv 
write.table(un_df, "/mnt/beegfs1/data/shared_group_data/syneco/spew_olympus/spew_hierarchy/spew_hierarchy.csv", 
            sep = ",", row.names = FALSE, qmethod = "double")
