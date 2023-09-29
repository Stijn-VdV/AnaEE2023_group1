## load libraries and set the working directory ----
library(tidyverse)
library(ggplot2)
library(lme4)

setwd("D:/WORK_PROJECTS/CORSI/2023_summer-school_microclimate/PRACTICAL_1")
getwd()


# load("slopes.RData")
load("slopes_meta.RData")
plot(slopes_meta$r_squared ~ as.factor(slopes_meta$metric),
     xlab = "Sensor", ylab = "R2")
slopes_meta_for_ground <- slopes_meta %>% 
  filter(canopy_cover >0 & metric == "T_Ground" & str_detect(.$Unique_id, "Ground"))

slopes_meta_for_air <- slopes_meta %>% 
  filter(canopy_cover >0 & metric == "T_Air" & str_detect(.$Unique_id, "Air"))

# forests linear models
## ground
lm_for_ground <- slopes_meta_for_ground %>% 
  lm(slope ~ canopy_cover, data = .)
summary(lm_for_ground)
lm_for_ground$coefficients
slopes_meta_for_ground <- slopes_meta_for_ground %>% 
  mutate(lm_slope  = lm_for_ground$coefficients[2],
         lm_interc = lm_for_ground$coefficients[1])

## air
lm_for_air <- slopes_meta_for_air %>% 
  lm(slope ~ canopy_cover, data = .)
summary(lm_for_air)
lm_for_air$coefficients
slopes_meta_for_air <- slopes_meta_for_air %>% 
  mutate(lm_slope  = lm_for_air$coefficients[2],
         lm_interc = lm_for_air$coefficients[1])


## let's put together the df again
slopes_meta_for <- rbind(slopes_meta_for_air, slopes_meta_for_ground) %>% 
  mutate(metric = ifelse(metric == "T_Air", "Air T (°C)", "Ground T (°C)"))

plot_air_ground <- ggplot(slopes_meta_for) +
  geom_point(aes(x = canopy_cover, y = slope, col = metric), size = 2.2) +
  facet_grid(rows = "metric") +
  geom_abline(aes(slope = lm_slope, intercept = lm_interc, col = metric), linewidth = 1.3) +
  scale_color_manual(values = c('#5777EB', "#9E754A"),
                     name = "Temperature") +
  theme_bw() +
  labs(title = "Buffering effect of canopy cover",
       x = "Canopy cover", y = "Slope") +
  theme(plot.title = element_text(size = 27, hjust = 0, color = 'black', face = "bold"), 
        axis.text = element_text(size = 18, color = 'black'), 
        axis.title = element_text(size = 21 , hjust = 0.5, color = 'black'),
        legend.title = element_text(size = 23 , hjust = 0.5, color = 'black', face = "bold"),
        legend.text = element_text(size = 20 , hjust = 0, color = 'black'),
        strip.text.y = element_text(size = 15, color = "black", face = "bold"),
        strip.background = element_rect(fill = "grey80"),
        legend.position = "none") 
ggsave("slope_canopy_cover.png",
       plot_air_ground,
       device = "png",
       path = "figure/",
       width = 8, height = 5,
       units = "in", dpi = 300)  

