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

df_all_clean <- df_all |>
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

(ggplot(df_all_clean) +
  geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric, group = plot_id)) +
  theme_bw()) |> plotly::ggplotly()


data.table::fwrite(df_all_clean, "output/compiled/HOBO_compiled_nonfiltered.csv")

# add metadata
# add weather station data
# calibrate sensor data
# cut time of intrest
# calculate slopes

# Add metadata ----
# clean metadata
df_meta <- data.table::fread("data/meta/metadata.csv") |>
  rename("Latitude" = Latitute)

# add missing longlat data
df_meta[4]$Longitude <- 45.035944
df_meta[4]$Latitude <- 6.401333

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

# pivot to wider and left join
df_all_clean |>
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
  tidyr::drop_na(Temp_HOBO) |>
  distinct() |> nrow()




df_all_clean |>
  left_join(df_meta_clean) |> nrow()
