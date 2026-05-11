
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# ------------------------------ Bird cleaning --------------------------- 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# -------------------------------- Set up --------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 

library(tidyverse)
library(janitor)
library(here)
library(ggridges)
library(measurements)

birds <- read_csv(here("birds", "birds.csv"))

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
  ungroup() |> 
  mutate(site_name = recode_values(
    site_name,
    "venoco_bridge" ~ "Venoco Bridge",
    "phelps_bridge" ~ "Phelps Bridge",
    "east_channel" ~ "East Channel"
  )) |> 
  left_join(weather, by = "date") |> 
  select(!c(station, name, latitude, longitude, elevation, prcp, tmin)) |> 
  mutate(tmax = conv_unit(tmax, from = "F", to = "C"))

write_csv(wq,
          here("water-quality", "wq_clean.csv"))

