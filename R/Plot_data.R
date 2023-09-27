## ---------------------------
## Script name: Clean_raw_data.R
##
## Purpose of script: Clean and standardize field time series measurements
##
## Notes:
##
## ---------------------------

# Load packages ----
require(dplyr)
require(ggplot2)
require(sf)
require(leaflet)

# Import data ----
# pre-calibrated data (contains data of sensors prior to actual measurements)
df_precal <- data.table::fread("output/compiled/HOBO_compiled_nonfiltered.csv")

# calibrated data (contains data of sensors prior to actual measurements)
df_cal <- data.table::fread("output/compiled/HOBO_calibrated.csv")

# metadata incl coordinates
df_meta <- data.table::fread("output/meta/meta_clean.csv")

# Set ggplot theme ----

# Map of data points ----
# prepare data
df_meta |>
  st_as_sf(coords = c("Latitude", "Longitude"), crs = st_crs(4326)) |>
  leaflet() |>
  addTiles() |>
  # addProviderTiles("Stamen.Watercolor") |>
  addCircleMarkers()


# plot


# Data overview ----

# Raw vs calibrated (calibration time series) ----

# Raw vs calibrated (full time series excl calibration) ----


