# Load required libraries
library(sp)
library(raster)
library(geodata)
library(terra)

# Set the path to the folder with climate data and R script
setwd("~/Downloads/get_data_climate_variables")

# Load sample info "coordinates.txt"
samples <- read.table("coordinates.txt", header = TRUE)
lon <- samples$lon
lat <- samples$lat

# Extract coordinate data
xy <- samples[, c("lon", "lat")]

# Load BioClim data
biodata <- worldclim_global(var = "bio", res = 10, "~/Downloads/get_data_climate_variables")

# Extract Biolclimatic variables using xy coordinates dataframe
biodata_values <- lapply(biodata[[1:19]], function(layer) raster::extract(layer, xy))

# Convert the extracted values to a data frame
biodata_extract <- extract(biodata[[1:19]], xy)

# Attach it to the original df
samples_bio <- cbind(samples, biodata_extract_df)

# Write all bio data as a text file
write.table(samples_bio[, c("lon", "lat", paste0("wc2.1_10m_bio_", 1:19))],
            file = "samples_bio_new.txt", row.names = FALSE, col.names = TRUE)


# Load required libraries
library(heatmaply)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)

# Load all bio variables
bio_variables <- read.table(
  "samples_bio_new.txt", 
  header = FALSE, 
  col.names = c(
    "lon", 
    "lat", 
    paste0("wc2.1_10m_bio_", 1:19)  # Use all 19 variables
  )
)

# Combine all bio data into a single data frame
bio_data <- merge(samples[, c("lon", "lat")], bio_variables, by = c("lon", "lat"))

# Select numeric columns
numeric_data <- bio_data[sapply(bio_data, is.numeric)]


# Calculate correlation matrix for all variables
correlation_matrix <- cor(biodata_extract)


# Mapping of abbreviated names to full names
name_mapping <- c(
  "wc2.1_10m_bio_1" = "Annual Mean Temperature",
  "wc2.1_10m_bio_2" = "Mean Diurnal Range",
  "wc2.1_10m_bio_3" = "Isothermality",
  "wc2.1_10m_bio_4" = "Temperature Seasonality",
  "wc2.1_10m_bio_5" = "Max Temperature of Warmest Month",
  "wc2.1_10m_bio_6" = "Min Temperature of Coldest Month",
  "wc2.1_10m_bio_7" = "Temperature Annual Range",
  "wc2.1_10m_bio_8" = "Mean Temperature of Wettest Quarter",
  "wc2.1_10m_bio_9" = "Mean Temperature of Driest Quarter",
  "wc2.1_10m_bio_10" = "Mean Temperature of Warmest Quarter",
  "wc2.1_10m_bio_11" = "Mean Temperature of Coldest Quarter",
  "wc2.1_10m_bio_12" = "Annual Precipitation",
  "wc2.1_10m_bio_13" = "Precipitation of Wettest Month",
  "wc2.1_10m_bio_14" = "Precipitation of Driest Month",
  "wc2.1_10m_bio_15" = "Precipitation Seasonality",
  "wc2.1_10m_bio_16" = "Precipitation of Wettest Quarter",
  "wc2.1_10m_bio_17" = "Precipitation of Driest Quarter",
  "wc2.1_10m_bio_18" = "Precipitation of Warmest Quarter",
  "wc2.1_10m_bio_19" = "Precipitation of Coldest Quarter",
  "lat" = "Latitude",
  "lon" = "Longitude"
)

# Update column names with unique identifiers
colnames(correlation_matrix) <- make.unique(name_mapping[1:ncol(correlation_matrix)])

# Check for NA or Inf in correlation matrix
if (any(is.na(correlation_matrix)) | any(is.infinite(correlation_matrix))) {
  # Replace NA and Inf with 0
  correlation_matrix[is.na(correlation_matrix) | is.infinite(correlation_matrix)] <- 0
}

# Update row and column names

rownames(correlation_matrix) <- colnames(correlation_matrix) <- name_mapping


# Plot heatmap with hierarchical clustering

heatmaply(
  correlation_matrix, 
  symm = TRUE,
  clustering_distance_rows = "correlation",
  clustering_distance_cols = "correlation",
  show_dendrogram = c(TRUE, TRUE),
  col = colorRampPalette(brewer.pal(11, "RdBu"))(100),
  main = "Heatmap of Co-association among Climate Variables for Arabidopsis Thaliana"
)




