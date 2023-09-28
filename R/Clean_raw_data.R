## ---------------------------
## Script name: Clean_raw_data.R
##
## Purpose of script: Clean and standardize field time series measurements
##
## Notes:
##
##    Ground Spot_5 -> renamed to Spot_3
## ---------------------------

# Load packages ----
require(dplyr)

# List files ----
#' Given differing number of columns, df_list_* are loaded in separately to
#' prevent column naming errors and joining/binding errors.

# non-sheltered ground + air temperatures
df_list_nw <- list.files("data/Summerschool_csv", full.names = TRUE, pattern = "*.csv")

# water temperatures
df_list_w <- list.files("data/Summerschool_csv/Water", full.names = TRUE, pattern = "*.csv")

# sheltered ground temperatures
df_list_g <- list.files("data/Summerschool_csv/Ground", full.names = TRUE, pattern = "*.csv")

# Clean HOBO non-water ----
# plot number 16 was a bit crazy, so manual edits
# data.table::fread(df_list_nw[8])[,c(1, 2, 4, 3)] |> data.table::fwrite(df_list_nw[8])

for (i in 1:length(df_list_nw)) {
  # load file
  df <- data.table::fread(df_list_nw[i])

  # extract filename
  filename <- substr(df_list_nw[i], 23, stringr::str_length(df_list_nw[i])-4)

  # make filepath
  filepath <- paste0("output/", filename, ".csv")

  # set new column names
  df <- df |>
    transform(id_plot = filename) |>
    setNames(c("row_number", "DateTime", "T_Ground", "T_Air", "id_plot")) |>
    dplyr::relocate(id_plot)

  # export file
  data.table::fwrite(df, filepath)

  print(paste0("I've just exported ", filepath))
}

# Clean HOBO water ----
for (i in 1:length(df_list_w)) {
  # load file
  df <- data.table::fread(df_list_w[i])

  # extract filename
  filename <- substr(df_list_w[i], 29, stringr::str_length(df_list_w[i])-4)

  # make filepath
  filepath <- paste0("output/water/", filename, ".csv")

  # set new column names
  df <- df |>
    transform(id_plot = filename) |>
    setNames(c("row_number", "DateTime", "T_Water", "id_plot")) |>
    dplyr::relocate(id_plot)

  # export file
  data.table::fwrite(df, filepath)

  print(paste0("I've just exported ", filepath))
}

# Clean HOBO ground ----
for (i in 1:length(df_list_g)) {
  # load file
  df <- data.table::fread(df_list_g[i])

  # extract filename
  filename <- substr(df_list_g[i], 30, stringr::str_length(df_list_g[i])-4)

  # make filepath
  filepath <- paste0("output/ground/", filename, ".csv")

  # set new column names
  df <- df |>
    transform(id_plot = filename) |>
    setNames(c("row_number", "DateTime", "T_ShelterGround", "id_plot")) |>
    dplyr::relocate(id_plot)

  # export file
  data.table::fwrite(df, filepath)

  print(paste0("I've just exported ", filepath))
}

# Bind & export HOBO water & non-water ----
# hobo ground-air
df_hobo_ga <- lapply(
  list.files("output", full.names = TRUE, pattern = "*.csv"),
  data.table::fread
) |> data.table::rbindlist() |>
  mutate(
    DateTime = lubridate::parse_date_time(DateTime, "mdy IMS p", tz = "Etc/GMT+2"),
    DateTime = lubridate::force_tz(DateTime, tzone = "UTC"),
    DateTime = DateTime - lubridate::hours(2)
  ) |>
  select(-row_number) |>
  tidyr::pivot_longer(
    cols = -c(id_plot, DateTime),
    names_to = "metric",
    values_to = "temperature"
  )

data.table::fwrite(df_hobo_ga, "output/compiled/hobo_ground_air.csv")

# hobo water
df_hobo_water <- lapply(
  list.files("output/water", full.names = TRUE, pattern = "*.csv"),
  data.table::fread
) |> data.table::rbindlist() |>
  mutate(
    DateTime = lubridate::parse_date_time(DateTime, "mdy IMS p", tz = "Etc/GMT+2"),
    DateTime = lubridate::force_tz(DateTime, tzone = "UTC"),
    DateTime = DateTime - lubridate::hours(2)
  ) |>
  select(-row_number) |>
  rename("temperature" = T_Water) |>
  transform(metric = "T_Water")

data.table::fwrite(df_hobo_water, "output/compiled/hobo_water.csv")

# ShelterGround
df_hobo_sground <- lapply(
  list.files("output/ground", full.names = TRUE, pattern = "*.csv"),
  data.table::fread
) |> data.table::rbindlist() |>
  mutate(
    DateTime = lubridate::parse_date_time(DateTime, "mdy IMS p", tz = "Etc/GMT+2"),
    DateTime = lubridate::force_tz(DateTime, tzone = "UTC"),
    DateTime = DateTime - lubridate::hours(2)
  ) |>
  select(-row_number)  |>
  rename("temperature" = T_ShelterGround) |>
  transform(metric = "T_ShelterGround")

data.table::fwrite(df_hobo_sground, "output/compiled/hobo_shelterground.csv")

# Bind all data together ----
df_all <- bind_rows(
  df_hobo_ga, df_hobo_sground
) |>
  bind_rows(df_hobo_water) |>
  rename("Temp_HOBO" = temperature)

# require(ggplot2)
# ggplot(df_all) +
#   geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric, group = id_plot)) +
#   theme_bw()
# plotly::ggplotly()

df_all_prep <- df_all |>
  # clean plot IDs
  mutate(
    plot_id = stringr::str_remove_all(
      id_plot,
      pattern = "Hobo_Prov2_Spot_|Hobo_Tidbit_ShelterGround_Spot_|Hobo_Prov2_UA_Water_")
  ) |>
  # separate plot ID from "15_deep" to "15 & deep" separately
  tidyr::separate(plot_id,
                  into = c("plot_id", "water_depth"),
                  sep = "_") |>
  select(plot_id, water_depth, DateTime, metric, Temp_HOBO) |>
  tidyr::drop_na(Temp_HOBO)

(ggplot(df_all_prep) +
  geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric, group = plot_id)) +
  theme_bw()) |> plotly::ggplotly()


data.table::fwrite(df_all_prep, "output/compiled/HOBO_compiled_nonfiltered.csv")

# Add metadata ----
# clean metadata
df_meta <- data.table::fread("data/meta/metadata.csv") |>
  rename("Latitude" = Latitute)

# add missing longlat data
df_meta[4]$Longitude <- 45.035944
df_meta[4]$Latitude <- 6.401333

# assign shallow/deep and convert plot_id to character (for joining in next step)
df_meta_clean <- df_meta |>
  rename("plot_id" = ID_plot) |>
  group_by(plot_id) |>
  mutate(
    water_depth = case_when(
      Depth == 1 ~ "shallow",
      Depth == max(Depth, na.rm = TRUE) ~ "deep",
      Depth == min(Depth, na.rm = TRUE) ~ "shallow",
      TRUE ~ NA_character_
    )
  ) |>
  ungroup() |>
  select(-ID) |>
  mutate(plot_id = as.character(plot_id))

data.table::fwrite(df_meta_clean, "output/meta/meta_clean.csv")

# pivot to wider and left join metadata to measurements
df_all_clean <- df_all_prep |>
  tidyr::pivot_wider(
    id_cols = c(plot_id, water_depth, DateTime),
    names_from = metric,
    values_from = Temp_HOBO
  ) |>
  left_join(df_meta_clean) |>
  tidyr::pivot_longer(
    cols = c(T_Ground, T_Air, T_ShelterGround, T_Water),
    names_to = "metric",
    values_to = "Temp_HOBO"
  ) |>
  tidyr::drop_na(Temp_HOBO)

# Calibrate sensor data ----
#' Sensors were initially kept in a box in a (relatively) stable environment.
#' Even so, sensors showed offsets. To correct these offsets, we will calculate
#' the offset of each sensor to the median sensor's measurements.
(df_all_clean |>
  mutate(
    unique_id = paste0(plot_id, water_depth) |> stringr::str_remove("NA")) |>
  ggplot() +
  geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric, group = unique_id))) |>
  plotly::ggplotly()

#' At 24 September 2023 at midnight, plot 11 was visually confirmed to be the
#' "median" = reference sensor for calibration.

# T_ShelterGround is not calibrated: not available at time of calibration
df_calib <- df_all_clean |>
  filter(DateTime >= as.POSIXct("2023-09-24 00:00:00", tz = "UTC"),
         DateTime <= as.POSIXct("2023-09-24 05:00:00", tz = "UTC"))

(df_calib |>
    mutate(
      unique_id = paste0(plot_id, water_depth) |> stringr::str_remove("NA")) |>
    ggplot() +
    geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric, group = unique_id))) |>
  plotly::ggplotly()

data.table::fwrite(df_calib, "output/compiled/hobo_calibration_set.csv")

# subset plot 11 (reference for calibrating other sensors)
df_calib_ref <- df_calib |>
  filter(plot_id == "11", metric == "T_Air") |>
  rename("Temp_HOBO_ref" = Temp_HOBO) |>
  select(DateTime, Temp_HOBO_ref)

# calculate mean offset (Temp_HOBO_ref - Temp_HOBO_ref) per plot & sensor
df_calib_offsets <- df_calib |>
  filter(plot_id != "11") |>
  left_join(df_calib_ref) |>
  mutate(T_offset = Temp_HOBO - Temp_HOBO_ref) |>
  group_by(plot_id, metric) |>
  summarize(T_offset_mean = mean(T_offset, na.rm = TRUE),
            .groups = "drop") |>
  mutate(T_offset_mean = ifelse(is.na(T_offset_mean), 0, T_offset_mean))

# calibrate sensor data: succeeded
(df_all_clean |>
    filter(
      # DateTime >= as.POSIXct("2023-09-25 12:00:00", tz = "UTC"),
      DateTime <= as.POSIXct("2023-09-27 11:00:00", tz = "UTC")) |>
    left_join(df_calib_offsets) |>
    mutate(Temp_HOBO_cal = Temp_HOBO - T_offset_mean) |>
    mutate(unique_id = paste0(plot_id, water_depth) |> stringr::str_remove("NA")) |>
    ggplot() +
    geom_line(aes(x = DateTime, y = Temp_HOBO_cal, col = metric, group = unique_id))) |>
  plotly::ggplotly()

# save and export calibrated time series data
df_clean_calibrated <- df_all_clean |>
  # filter(
  #   DateTime >= as.POSIXct("2023-09-25 12:00:00", tz = "UTC"),
  #   DateTime <= as.POSIXct("2023-09-27 11:00:00", tz = "UTC")) |>
  left_join(df_calib_offsets) |>
  mutate(Temp_HOBO_cal = Temp_HOBO - T_offset_mean)

# edit weird timestamps (XX:X7)
df_clean_calibrated <- df_clean_calibrated |>
  filter(lubridate::minute(DateTime) %in% c(7 + seq(0, 50, 10))) |>
  mutate(DateTime = DateTime + lubridate::minutes(3)) |>
  bind_rows(
    df_clean_calibrated |>
      filter(!lubridate::minute(DateTime) %in% c(7 + seq(0, 50, 10)))
  )

# add new column denoting whether a value was calibrated or not
# + coalesce calibrated data column (so no NA values for non-calibrated measurements)
df_clean_calibrated_final <- df_clean_calibrated |>
  mutate(
    Calibrated = ifelse(!is.na(Temp_HOBO_cal), TRUE, FALSE),
    Temp_HOBO_cal = ifelse(is.na(Temp_HOBO_cal), Temp_HOBO, Temp_HOBO_cal)
  )

data.table::fwrite(df_clean_calibrated_final, "output/compiled/HOBO_calibrated.csv")

# save and export calibrated time series data + filter only relevant time series
df_clean_calibrated_filter <- df_clean_calibrated_final |>
  filter(
    DateTime >= as.POSIXct("2023-09-25 10:00:00", tz = "UTC"), # 12 GMT+2
    DateTime <= as.POSIXct("2023-09-27 09:00:00", tz = "UTC")) # 11 GMT+2

data.table::fwrite(df_clean_calibrated_filter, "output/compiled/HOBO_cal_filtered.csv")

# Join weather data ----
# import calibrated data
# df_clean_calibrated <- data.table::fread("output/compiled/HOBO_cal_filtered.csv")

# import weather data
load("data/weatherstation/Weather_Station.RData")
df_ws <- meteo |>
  mutate(
    across(
      c(Temp_WS, RH, WindS_Max, Slr_Avg), ~as.numeric(.x)
    )
  )

# visualize data for testing
df_clean_calibrated_filter |>
  mutate(
    unique_id = paste0(plot_id, water_depth) |> stringr::str_remove("NA")
    ) |>
  select(DateTime, Temp_HOBO, Temp_HOBO_cal, unique_id, metric) |>
  ggplot() +
  geom_line(aes(x = DateTime, y = Temp_HOBO, col = unique_id)) +
  theme_bw() +
  facet_wrap(~metric)

# join weather data
df_hobo_ws <- df_clean_calibrated_filter |>
  left_join(df_ws) |>
  data.table::as.data.table() |>
  tidyr::drop_na(Temp_WS)

# join canopy cover data
can_cov <- data.table::fread("data/meta/canopy_cover_clean.csv") |>
  mutate(plot_id = as.character(plot_id))

df_hobo_ws_can <- df_hobo_ws |>
  left_join(can_cov)

data.table::fwrite(df_hobo_ws_can, "output/compiled/hobo_ws_canopy_joined.csv")
save(df_hobo_ws_can, file = "output/compiled/hobo_ws_canopy_joined.RData")
