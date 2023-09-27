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
# field metadata
df_meta <- data.table::fread("data/...")

# HOBO
df_hobo <- data.table::fread("data/...")

# Weather station (WS)
df_ws <- data.table::fread("data/...")

# Data prep ----
# join metadata & HOBO
df_hobo <- left_join(df_hobo, df_meta, by = c("", ""))

rm(df_meta);gc()

# Check timezone HOBO
attr(df_hobo$TimeStamp, "tz")
# df_hobo[, TimeStamp := lubridate::with_tz(TimeStamp, tzone = "Europe/Brussels")]

# Check timezone WS
attr(df_ws$TimeStamp, "tz")
# df_ws[, TimeStamp := lubridate::with_tz(TimeStamp, tzone = "Europe/Brussels")]

# Left_join HOBO & WS
temperatures <- left_join(..., ..., by = c("" = ""))

# Subset between Monday 25-09-2023 13:00:00 and Wednesday 27-09-2023 11:00:00
temperatures <- temperatures |>
  filter(TimeStamp >= "2023-09-25 13:00:00", TimeStamp <= "2023-09-27 11:00:00")

# Calculate slopes ----
# Plot levels
plots_id <- na.omit(temperatures) %>%
  mutate(id_plot = as.factor(id_plot)) %>%
  droplevels()

plots_id <- levels(plots_id$id_plot)

# Initiation
coef_mod_on <- list()

# Starting the loop!
for (i in plots_id) {

  temperatures_i <- na.omit(temperatures) %>%
    filter(id_plot==i)

  mod <- lm(T_HOBO ~ T_WS,
            data=temperatures_i, na.action = na.omit)


  # Then the equilibrium per month AND slope (constant):
  coef_mod_on[[i]] <-
    data.frame(as.list(coef(mod))) %>%    # to get both coefficients
    as_tibble() %>%
    dplyr::rename(intercept = 1,
                  slope = "T_WS") %>%
    mutate(equilibrium=intercept/(1-slope),
           id_plot=as.factor(i),
           r_squared=summary(mod)$r.squared)
}

# Into one dataframe joining all the plots:
coef_mod_on <- bind_rows(coef_mod_on) %>%
  distinct()

slopes_lidar <- coef_mod_on %>%
  select(id_plot, slope, equilibrium, r_squared) %>%
  arrange(id_plot)

# Clean up intermediate datasets
rm(i,temperatures_i, coef_mod_on)
