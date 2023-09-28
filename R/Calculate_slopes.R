## ---------------------------
## Script name: Calculate_slopes.R
##
## Purpose of script:
##
## Date Created: 2023-09-27
##
## Notes: https://happygitwithr.com/
##
## ---------------------------

# load packages ----
require(dplyr)
require(sf)
require(terra)

# Import data ----
"
Required column titles
id_plot -> ID of plot
T_HOBO -> temperature time series HOBO
T_WS -> temperature time series weather station
"
load("output/compiled/hobo_ws_canopy_joined.RData")
temperatures <- df_hobo_ws_can |>
  rename(
    "id_plot" = plot_id,
    "T_HOBO" = Temp_HOBO_cal,
    "T_WS" = Temp_WS
  ) |>
  mutate(
    water_depth = ifelse(is.na(water_depth), "", water_depth),
    id_plot_unique = paste0(id_plot, metric, water_depth)
  )

rm(df_hobo_ws_can); rm()

# # field metadata
# df_meta <- data.table::fread("data/...")
#
# # HOBO
# df_hobo <- data.table::fread("data/...")
#
# # Weather station (WS)
# df_ws <- data.table::fread("data/...")

# Data prep ----
# # join metadata & HOBO
# df_hobo <- left_join(df_hobo, df_meta, by = c("", ""))
#
# rm(df_meta);gc()

# Check timezone HOBO
attr(temperatures$DateTime, "tz")
# df_hobo[, TimeStamp := lubridate::with_tz(TimeStamp, tzone = "Europe/Brussels")]

# # Check timezone WS
# attr(df_ws$TimeStamp, "tz")
# # df_ws[, TimeStamp := lubridate::with_tz(TimeStamp, tzone = "Europe/Brussels")]
#
# # Left_join HOBO & WS
# temperatures <- left_join(..., ..., by = c("" = ""))

# # Subset between Monday 25-09-2023 13:00:00 and Wednesday 27-09-2023 11:00:00
# temperatures <- temperatures |>
#   filter(TimeStamp >= "2023-09-25 13:00:00", TimeStamp <= "2023-09-27 11:00:00")

# Calculate slopes ----
# Plot levels
# plots_id <- temperatures
#
#
#   na.omit(temperatures) %>%
#   mutate(id_plot = as.factor(id_plot_unique)) %>%
#   droplevels()
#
# plots_id <- levels(plots_id$id_plot)

# Initiation
coef_mod_on <- list()

plot_ids <- unique(temperatures$id_plot_unique)

# Starting the loop!
for (i in 1:length(plot_ids)) {
  # define plot id of iteration i
  plot_id <- plot_ids[i]

  print(paste0("Initiating slope calculation for: ", plot_id))

  # subset data to iteration i
  temperatures_i <- filter(temperatures, id_plot_unique == plot_id)

  # model
  mod <- lm(T_HOBO ~ T_WS,
            data = temperatures_i)#, na.action = na.omit)


  # Then the equilibrium per month AND slope (constant):
  coef_mod_on[[i]] <-
    data.frame(as.list(coef(mod))) %>%    # to get both coefficients
    as_tibble() %>%
    dplyr::rename(intercept = 1,
                  slope = "T_WS") %>%
    mutate(equilibrium=intercept/(1-slope),
           id_loop=as.factor(i),
           id_plot=as.factor(plot_id),
           r_squared=summary(mod)$r.squared)
}

# Into one dataframe joining all the plots:
coef_mod_on <- bind_rows(coef_mod_on) %>%
  distinct()

slopes_lidar <- coef_mod_on %>%
  select(id_plot, slope, equilibrium, r_squared) %>%
  arrange(id_plot)

hist(slopes_lidar$equilibrium)
hist(slopes_lidar$slope)
hist(slopes_lidar$slope)

# Clean up intermediate datasets
rm(i,temperatures_i, coef_mod_on)

# add original unique ids, log slope and export
df_ids <- temperatures |>
  distinct(id_plot, Depth, water_depth, Calibrated, id_plot_unique)

slopes_log <- slopes_lidar |>
  mutate(slope_log = log(slope)) |>
  left_join(df_ids, by = c("id_plot" = "id_plot_unique")) |>
  rename(
    "Unique_id" = id_plot, "plot_id" = id_plot.y, "calibrated" = Calibrated,
    "depth.cm" = Depth
  )

save(slopes_log, file = "output/slopes/slopes.RData")

# add metadata
# field metadata, canopy
df_meta <- temperatures |>
  distinct(id_plot_unique, id_plot, water_depth, Depth, Longitude, Latitude,
           metric, Calibrated, canopy_cover, LAI, GAP, img_id, threshold) |>
  rename(
    "Unique_id" = id_plot, "plot_id" = id_plot, "calibrated" = Calibrated,
    "depth.cm" = Depth
  )

slopes_meta <- left_join(slopes_log, df_meta)

save(slopes_meta, file = "output/slopes/slopes_meta.RData")

# Dataviz ----
temperatures |>
  distinct(DateTime, T_WS) |>
  ggplot() +
  geom_line(aes(x = DateTime, y = T_WS))

temperatures |>
  filter(id_plot_unique == "14T_Ground") |>
  ggplot() +
  geom_line(aes(x = DateTime, y = T_WS)) +
  geom_line(aes(x = DateTime, y = T_HOBO), col = "red")

#
# slopes_lidar |>
#   mutate(slope_log = log(slope)) |> View()
