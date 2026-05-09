
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# ------------------- Water quality measurement cleaning ----------------- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# -------------------------------- Set up --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

library(tidyverse)
library(janitor)
library(here)
library(ggridges)

metadata <- read_csv(here("water-quality", "YSI_Data_Begin_1.csv"))

data <- read_csv(here("water-quality", "NCOS_YSI_Water_Quality_Monitoring_0.csv"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# ------------------------------- Cleaning -------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 


wq <- left_join(
  x = data,
  y = metadata,
  by = c("GlobalID" = "ParentGlobalID")
) |> 
  clean_names() |> 
  mutate(monitoring_date = mdy_hms(monitoring_date),
         date = date(monitoring_date)) |> 
  select(date, site_name, 
         depth_code, do_mg_l, salinity_ppt, temperature_c) |> 
  filter(site_name != "other") |> 
  # filtering outliers
  filter(salinity_ppt < 1410) |> 
  group_by(date, site_name) |> 
  summarize(mean_do_mg_l = mean(do_mg_l, na.rm = TRUE),
            mean_salinity_ppt = mean(salinity_ppt, na.rm = TRUE),
            mean_temp_c = mean(temperature_c, na.rm = TRUE)) |> 
  ungroup()

write_csv(wq,
          here("water-quality", "wq_clean.csv"))



ggplot(data = wq,
       mapping = aes(x = site_name,
                     y = mean_salinity_ppt)) +
  geom_boxplot() 

ggplot(data = wq,
       mapping = aes(x = mean_temp_c)) +
  geom_density() +
  geom_vline(aes(xintercept = mean(mean_temp_c)),
             color = "red") +
  geom_vline(aes(xintercept = median(mean_temp_c)),
             color = "blue") +
  facet_wrap(~ site_name)

ggplot(data = wq,
       mapping = aes(x = site_name,
                     y = mean_temp_c)) +
  geom_violin()


ggplot(data = wq,
       mapping = aes(x = mean_temp_c,
                     y = site_name)) +
  geom_density_ridges(alpha = 0.6) +
  stat_summary(geom = "point",
               fun = "median",
               color = "blue") +
  stat_summary(geom = "point",
               fun = "mean",
               color = "red") +
  theme_ridges()

wq |> 
  group_by(site_name) |> 
  summarize(mean = mean(mean_temp_c),
            sd = sd(mean_salinity_ppt),
            median = median(mean_temp_c)) |> 
  ungroup()

wq |> 
  group_by(site_name) |> 
  summarize(mean = mean(mean_salinity_ppt),
            sd = sd(mean_salinity_ppt),
            median = median(mean_salinity_ppt)) |> 
  ungroup()










#
#
# 
