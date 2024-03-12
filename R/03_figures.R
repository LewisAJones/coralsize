# Header ----------------------------------------------------------------
# Project: coralsize
# File name: 03_figures.R
# Last updated: 2024-03-11
# Author: Lewis A. Jones
# Email: LewisA.Jones@outlook.com
# Repository: https://github.com/LewisAJones/coralsize

# Set-up ----------------------------------------------------------------
# Package for data handling/manipulation
library(tidyverse)
# Package for colour palettes
library(MetBrewer)
# Package for generic plotting
library(ggplot2)
# Package for adding scale bar and north arrow
library(ggspatial)
# Package for text labels
library(ggrepel)
# Package for label positions in ggplot
library(ggpp)
# Package for combining plots
library(cowplot)
library(ggpubr)
# Packages for getting map data
library(rnaturalearth)
library(rnaturalearthdata)

# Study site ------------------------------------------------------------
# Read data
sites <- read.csv("./data/sites.csv")
# Africa sf
af <- ne_countries(scale = "large", continent = "Africa", returnclass = "sf")
# Define countries for plotting
countries <- c("egypt", "saudi arabia", "jordan", "isreal", "syria",
               "lebanon", "iraq", "iran", "kuwait")
# Get country sfs
countries <- ne_countries(scale = "large", country = countries,
                          returnclass = "sf")

# Define colours for plotting
land <- "darkgrey"
sea <- "aliceblue"

# Make map of Africa
africa <- ggplot(data = af) +
  geom_sf(colour = land, fill = land) +
  geom_sf(data = countries, colour = land, fill = land) +
  geom_rect(aes(xmin = 33, xmax = 38, ymin = 24, ymax = 28),
            fill = NA, colour = "black", linewidth = 0.3) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_sf(xlim = c(-19, 50), ylim = c(-38, 38)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Make locality map
localities <- ggplot(data = countries) +
  geom_sf(colour = "black", fill = land) +
  geom_point(data = sites, aes(x = lng, y = lat, fill = age),
             shape = 21, size = 4,
             colour = "black", alpha = 0.75) +
  geom_text_repel(data = sites, aes(x = lng, y = lat, label = site),
                  colour = "black", size = 3,
                  box.padding = 1, min.segment.length = unit(0, 'cm'),
                  nudge_x = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  # Add colour scale
  scale_colour_met_d("Hiroshige") +
  # Add fill scale
  scale_fill_met_d("Hiroshige") +
  xlab("Longitude (º)") +
  ylab("Latitude (º)") +
  annotation_scale(text_cex = 1) +
  annotation_north_arrow(location = "bl",
                         pad_y = unit(1, "cm"),
                         height = unit(2, "cm"), width = unit(2, "cm")) +
  coord_sf(xlim = c(33, 38), ylim = c(24, 28)) +
  theme_bw() +
  theme(panel.background = element_rect(fill = sea),
        plot.background = element_blank(),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.title = element_blank())

# Create inset
map <- ggdraw(localities) +
  draw_plot(
    {
      africa
    },
    x = 0.735, y = 0.645,
    width = 0.3, height = 0.25
  )

# Save map
ggsave(filename = "./article/figures/map.png", plot = map,
       width = 200, height = 200, units = "mm", dpi = 300,
       bg = "white")

# Clean environment
rm(list = ls())

# Size-frequency distributions ------------------------------------------
# Load data
df <- read.csv("./data/intercepts.csv")
# Generate taxa plot
p <- ggplot(data = df, aes(x = length, y = after_stat(count))) +
  # Plot density
  #geom_histogram(aes(fill = age, colour = age), position="identity", alpha = 0.6) +
  geom_density(stat = "density", aes(fill = age, colour = age), alpha = 0.6) +
  # Add vertical line of median value
  #geom_vline(aes(xintercept = average, colour = age), linetype = 2) +
  # Add points of the median value
  #geom_point(aes(x = average, y = n / 2, fill = age), colour = "black", shape = 21) +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10", limits = c(1, 650)) +
  # Add colour scale
  scale_colour_met_d("Hiroshige") +
  # Add fill scale
  scale_fill_met_d("Hiroshige") +
  # Y-axis lavel
  ylab(lab = "number of intercepts") +
  # X-axis label
  xlab(lab = "colony size (cm)") +
  # Create facets across taxa with free scales
  facet_wrap(~genus, ncol = 4, scales = "free") +
  # Set themes
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "italic")
  )

# Save plot
ggsave(filename = "./article/figures/size-distribution.png", plot = p,
       width = 200, height = 200, units = "mm", dpi = 300,
       bg = "white")

# Generate environment plot
edge <- df[which(df$environment == "Reef edge"), ]
slope <- df[which(df$environment == "Reef slope"), ]

p1 <- ggplot(data = edge, aes(x = length, y = after_stat(count))) +
  # Plot density
  #geom_histogram(aes(fill = age, colour = age), position="identity", alpha = 0.6) +
  geom_density(stat = "density", aes(fill = age, colour = age), alpha = 0.6) +
  # Add vertical line of median value
  #geom_vline(aes(xintercept = average, colour = age), linetype = 2) +
  # Add points of the median value
  #geom_point(aes(x = average, y = n / 2, fill = age), colour = "black", shape = 21) +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10", limits = c(1, 650)) +
  # Add colour scale
  scale_colour_met_d("Hiroshige") +
  # Add fill scale
  scale_fill_met_d("Hiroshige") +
  # Y-axis lavel
  ylab(lab = "number of intercepts") +
  # X-axis label
  xlab(lab = "colony size (cm)") +
  # Add title
  ggtitle(label = "(a) Reef edge") +
  # Create facets across taxa with free scales
  facet_wrap(~genus, ncol = 2, scales = "free") +
  # Set themes
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "italic"),
    aspect.ratio = 0.65
  )

p2 <- ggplot(data = slope, aes(x = length, y = after_stat(count))) +
  # Plot density
  #geom_histogram(aes(fill = age, colour = age), position="identity", alpha = 0.6) +
  geom_density(stat = "density", aes(fill = age, colour = age), alpha = 0.6) +
  # Add vertical line of median value
  #geom_vline(aes(xintercept = average, colour = age), linetype = 2) +
  # Add points of the median value
  #geom_point(aes(x = average, y = n / 2, fill = age), colour = "black", shape = 21) +
  # Transform x-axis to log10
  scale_x_continuous(trans = "log10", limits = c(1, 650)) +
  # Add colour scale
  scale_colour_met_d("Hiroshige") +
  # Add fill scale
  scale_fill_met_d("Hiroshige") +
  # Y-axis lavel
  ylab(lab = "number of intercepts") +
  # X-axis label
  xlab(lab = "colony size (cm)") +
  # Add title
  ggtitle(label = "(b) Reef slope") +
  # Create facets across taxa with free scales
  facet_wrap(~genus, ncol = 2, scales = "free") +
  # Set themes
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = "top",
    strip.text = element_text(face = "italic"),
    aspect.ratio = 0.65
  )

p <- ggarrange(p1, p2, ncol = 2, common.legend = TRUE)
# Save plot
ggsave(filename = "./article/figures/size-distribution-environment.png", plot = p,
       width = 200, height = 300, units = "mm", dpi = 300,
       bg = "white")

# Clean environment
rm(list = ls())

