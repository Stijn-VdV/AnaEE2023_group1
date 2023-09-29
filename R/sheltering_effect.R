## load libraries and set the working directory ----
library(tidyverse)
library(ggplot2)

setwd("D:/WORK_PROJECTS/CORSI/2023_summer-school_microclimate/PRACTICAL_1")
getwd()

load("hobo_ws_canopy_joined.RData")  
df_hobo_ws_can

## clean the data
df_clean <- df_hobo_ws_can %>% 
  select(plot_id, DateTime, Habitat, metric, Temp_HOBO = Temp_HOBO_cal, Calibrated) %>% 
  mutate(metric_cal = paste0(metric, "_", Calibrated),
         plot_id    = as.numeric(plot_id)) %>% 
  filter(metric_cal == "T_ShelterGround_FALSE" | metric_cal == "T_Ground_TRUE") %>%
  filter(plot_id < 5) %>% 
  select(-Calibrated, -metric_cal) %>% 
  mutate(metric = ifelse(metric == "T_ShelterGround", "Sheltered", "Non sheltered"))

plot_ts <- df_clean %>% 
  ggplot() +
  geom_line(aes(x = DateTime, y = Temp_HOBO, col = metric), linewidth = 1.5) +
  scale_color_manual(
    values = c(
      "Non sheltered" = "#E6C229",
      "Sheltered" = "#4D9078"
    ),
    name = "Sheltering"
  ) +
  ylim(-5,50) +
  # facet_grid(rows = "plot_id") +
  facet_wrap(~ plot_id, scales = 'free', nrow = 2) +
  theme_bw() +
  labs(title = "Sheltering effect on ground T",
       x = "Time", y = "Temperature (°C)") +
  theme(plot.title = element_text(size = 27, hjust = 0, color = 'black', face = "bold"), 
        axis.text = element_text(size = 15, color = 'black'), 
        axis.title = element_text(size = 21 , hjust = 0.5, color = 'black'),
        legend.title = element_text(size = 23 , hjust = 0.5, color = 'black', face = "bold"),
        legend.text = element_text(size = 20 , hjust = 0, color = 'black'),
        strip.text = element_text(size = 15, color = "black", face = "bold"),
        strip.background = element_rect(fill = "grey80"),
        legend.position = "bottom") 
plot_ts
ggsave("sheltering_effect.png",
       plot_ts,
       device = "png",
       path = "figure/",
       width = 12, height = 12,
       units = "in", dpi = 300)  
ggsave("sheltering_effect_ppt.png",
       plot_ts,
       device = "png",
       path = "figure/",
       width = 18, height = 12,
       units = "in", dpi = 300)  
