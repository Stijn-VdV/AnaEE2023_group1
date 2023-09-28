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

# slope results
load("output/slopes/slopes_meta.RData")
slopes_meta

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

ggsave(filename = "graphs/timeseries_overview_new.svg",
       width = 10, height = 6)

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

# Slopes overview ----
slopes_meta |>
  mutate(plot_id = as.numeric(plot_id)) |>
  left_join(df_meta) |>
  left_join(df_precal |> distinct(plot_id, water_depth, metric)) |>
  mutate(
    intercept = equilibrium * (1-slope),
    intercept_log = equilibrium * (1-slope_log)
  ) |>
  ggplot() +
  geom_point(aes(x = c(rep(-3, 81), 47), y = c(rep(-3, 81), 47)), alpha = 0) +
  geom_abline(aes(slope = slope, intercept = intercept, col = Habitat)) +
  geom_abline(slope = 1, intercept = 0, linetype = "11", col = "black") +
  theme_bw() +
  theme(legend.position = "none") +
  facet_wrap(~metric)


slopes_meta |>
  mutate(plot_id = as.numeric(plot_id)) |>
  left_join(df_meta) |>
  left_join(df_precal |> distinct(plot_id, water_depth, metric)) |>
  mutate(
    intercept = equilibrium * (1-slope),
    intercept_log = equilibrium * (1-slope_log),
    Habitat = factor(Habitat,
                     levels = c("Grassland",
                                "Open_forest",
                                "Wet_grassland",
                                "Close_forest",
                                "Fresh_water"))
  ) |>
  ggplot() +
  # geom_boxplot(aes(x = metric, y = slope_log, col = Habitat),
  geom_boxplot(aes(x = metric, y = slope, col = Habitat),
               position = position_dodge2(preserve = "single")) +
  geom_hline(yintercept = 1, linetype = "11") +
  labs(x = "") +
  theme_bw() +
  scale_color_manual(
    values = c(
      "Close_forest" = "#FF2E00",
      "Open_forest" = "#E6C229",
      "Grassland" = "#4D9078",
      "Fresh_water" = "#2D7DD2",
      "Wet_grassland" = "#783C27"
    )
  )# +
  # coord_flip()

ggsave(filename = "graphs/slopes_habitat_nonlog.svg",
       width = 5, height = 6)
