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

# all data incl weather station
load("output/compiled/hobo_ws_canopy_joined.RData")
df_hobo_ws_can

# Set ggplot theme ----

# Map of data points ----
# prepare data
df_meta |>
  st_as_sf(coords = c("Latitude", "Longitude"), crs = st_crs(4326)) |>
  leaflet() |>
  addProviderTiles("Esri.WorldTopoMap") |>
  addTiles(options = providerTileOptions(opacity = 0.4)) |>
  # addProviderTiles("Esri.WorldImagery") |>
  # addProviderTiles("Esri.WorldTerrain") |>
  addCircleMarkers()


# Data overview (incl. weather stations) ----
df_hobo_ws_can |>
  mutate(
    unique_id = paste0(plot_id, metric, Depth)
  ) |>
  ggplot(aes(x = DateTime)) +
  geom_line(
    aes(y = Temp_HOBO_cal, col = metric, group = unique_id)
  ) +
  geom_line(
    aes(y = Temp_WS), col = "black", linewidth = 1, linetype = "11"
  ) +
  theme_bw() +
  scale_color_manual(
    values = c(
      "T_Air" = "#FF2E00",
      "T_Ground" = "#E6C229",
      "T_ShelterGround" = "#4D9078",
      "T_Water" = "#2D7DD2"
    )
  ) +
  facet_wrap(~metric) +
  theme(legend.position = "none") +
  labs(y = "Temperature (°C)", x = "")

ggsave(filename = "graphs/timeseries_overview.svg",
       width = 8, height = 6)

# Raw vs calibrated (calibration time series) ----
df_cal |>
  filter(
    DateTime >= as.POSIXct("2023-09-24 00:00:00", tz = "UTC"),
    DateTime <= as.POSIXct("2023-09-24 05:00:00", tz = "UTC")
  ) |>
  tidyr::pivot_longer(
    cols = c(Temp_HOBO, Temp_HOBO_cal),
    names_to = "Metric",
    values_to = "Temp"
  ) |>
  mutate(
    unique_id = paste0(plot_id, water_depth, metric)
  ) |>
  ggplot() +
  geom_line(
    aes(
      x = DateTime,
      y = Temp,
      col = Calibrated,
      group = unique_id,
      alpha = Calibrated)
    ) +
  facet_wrap(~Metric, ncol = 1) +
  theme_bw() +
  labs(x = "Temperature (°C)", y = "Time (2023-09-24") +
  ggsci::scale_color_npg() +
  scale_alpha_manual(values = c(1, 0.5))


ggsave(filename = "graphs/calibration_ex.svg",
       width = 8, height = 6)



