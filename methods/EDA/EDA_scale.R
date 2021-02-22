# Code to scale EDA for files #

## ---- Please place this file in a folder named "methods" 
##      under the directory where the scaled EDA data is stored. ----

## Recent update: Feb 22, 2021, by Xinrui Wu

#### For each user:
####     EDA_scaled = (EDA - mean_by_date) / std_by_date

library(tidyverse)
library(lubridate)

setwd('..')

## ------------------ Scale EDA for all observations ------------------ ##

n_obs = 91  # number of observations (files)
for (i in 2:n_obs){
  id = 1000 + i 
  input_name = paste0("../R21_Study - EDA/", id, "_EDA.rds")
  output_name = paste0("./", id, "_EDA_scaled.rds")
  
  if (file.exists(input_name)){
    eda = readRDS(input_name)
    eda$time = as_datetime(eda$ts/1000)
    eda$date = as_date(eda$time)
    eda_scaled = transmute(eda, date, time, Device=E4_serial, EDA=EDA_HighLowPass) %>%
      drop_na() %>%
      group_by(date, Device) %>%
      mutate(EDA_scaled = scale(EDA)) %>%
      ungroup() %>%
      arrange(date, Device)
      transmute(timestamp=time, Device, EDA_scaled)
    saveRDS(eda_scaled, file = output_name)
  }
}
