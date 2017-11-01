# Lee Richardson 
# October 25, 2017 
# Purpose: Generate Timings for each method in South Dakota, 

# Load in results generated previously ---
results <- readRDS(file = "plots/run_results.rds")
results$sampling <- as.character(results$sampling)
results$places <- as.character(results$places)

# Change the X-lables to Uppercase ----
srs_unif_inds <- which(results$sampling == "srs_unif")
results$sampling[srs_unif_inds] <- "SRS, Uniform"

srs_roads_inds <- which(results$sampling == "srs_roads")
results$sampling[srs_roads_inds] <- "SRS, Roads"

ipf_unif_inds <- which(results$sampling == "ipf_unif")
results$sampling[ipf_unif_inds] <- "IPF, Uniform"

ipf_roads_inds <- which(results$sampling == "ipf_roads")
results$sampling[ipf_roads_inds] <- "IPF, Roads"

# Make the ggplot boxplot ---
runtime_plot <- ggplot(data = results) + geom_boxplot(aes(x = sampling, y = time)) + 
  labs(x = "", y = "Seconds") + ggtitle("Runtime by Sampling Method") +
  theme_light() + scale_color_grey() +
  theme(
    axis.text.x = element_text(size = axis_text, family = "Palatino"),
    axis.text.y= element_text(size = axis_text, family = "Palatino"),
    axis.title.x= element_text(size = axis_title, family = "Palatino"),
    axis.title.y= element_text(size = axis_title, family = "Palatino"),
    plot.title = element_text(size = title_size, family = "Palatino", hjust = 0.5), 
    legend.text = element_text(size = legend_size), 
    legend.title=element_text(size = legend_size)
  ) 


