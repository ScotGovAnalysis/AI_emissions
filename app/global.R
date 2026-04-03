library(tidyverse)

today_date <- Sys.Date()
date_final <- today_date - 2
date_start <-  date_final - 30
models <- c("GPT-5 (high)", "GPT-5 (medium)", "GPT-5 (low)") 
dat_all <- map(seq(from = date_start, to = date_final, length.out = 10), 
               ~read_csv(file = paste(
                 "https://raw.githubusercontent.com/Nidhal-Jegham/HowHungryisAIDashboard/main/output/artificialanalysis_environmental_",
                 as.character(.x),
                 ".csv", sep = ""
               )))  %>% 
  list_rbind() %>% 
    janitor::clean_names()


dat <- dat_all %>% 
  filter(model %in% models) %>% 
  select(model, length, mean_combined_energy_wh, mean_combined_carbon_g_co2e,
         mean_combined_water_site_source_m_l, 
         energy_consumption_of_1_billion_prompts_m_wh, 
         carbon_emissions_of_1_billion_prompts_tons_co2e, 
         water_consumption_of_1_billion_prompts_kiloliter) %>% 
  group_by(model, length) %>% 
  summarise(mean_combined_energy_wh = mean(mean_combined_energy_wh), 
            mean_combined_carbon_g_co2e = mean(mean_combined_carbon_g_co2e), 
            mean_combined_water_site_source_m_l = 
              mean(mean_combined_water_site_source_m_l), 
            energy_consumption_of_1_billion_prompts_m_wh =
              mean(energy_consumption_of_1_billion_prompts_m_wh), 
            carbon_emissions_of_1_billion_prompts_tons_co2e = 
              mean(carbon_emissions_of_1_billion_prompts_tons_co2e), 
            water_consumption_of_1_billion_prompts_kiloliter = 
              mean(water_consumption_of_1_billion_prompts_kiloliter)) %>% 
  ungroup()
              

