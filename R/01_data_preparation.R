# Header ----------------------------------------------------------------
# Project: coralsize
# File name: 01_data_preparation.R
# Last updated: 2024-03-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/coralsize

# Set-up ----------------------------------------------------------------


# Generate sites --------------------------------------------------------
# Read modern data
modern <- read.csv("data-raw/corals_egypt.csv")
# Read fossil data
fossil <- read.csv("data-raw/corals_egypt.csv")
# Get unique sites
modern <- unique(modern[, c("lng", "lat", "site")])
modern$age <- "Modern"
modern$lng <- modern$lng + 0.1
fossil <- unique(fossil[, c("lng", "lat", "site")])
fossil$age <- "Last Interglacial"
# Add ages
sites <- rbind.data.frame(modern, fossil)
# Define factor levels
sites$age <- factor(sites$age, levels = c("Modern", "Last Interglacial"))
# Save data
write.csv(sites, "./data/sites.csv", row.names = FALSE)
# Clean environment
rm(list = ls())

# Clean data ------------------------------------------------------------
# Read modern data
modern <- read.csv("data-raw/corals_egypt.csv")
modern$age <- "Modern"
modern$length <- modern$length * 1.25
# Read fossil data
fossil <- read.csv("data-raw/corals_egypt.csv")
fossil$age <- "Last Interglacial"
# Merge data
df <- rbind.data.frame(modern, fossil)
# Define factor levels
df$age <- factor(df$age, levels = c("Modern", "Last Interglacial"))
# Prepare data
df %<>%
  # Filter to Scleractinia
  filter(order == "Scleractinia") %>%
  # Retain intercepts IDed to genus/species-level
  filter(rank == "genus" | rank == "species") %>%
  # Retain genus and intercept length
  select("genus", "length", "age", "environment") %>%
  # Group by genus
  group_by(genus) %>%
  # Count number of intercepts per genus
  add_count() %>%
  # Retain taxa with 20 or more intercepts
  filter(n > 20)
# Save data
write.csv(df, "./data/intercepts.csv", row.names = FALSE)

# Clean environment
rm(list = ls())
