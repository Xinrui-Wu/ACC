# Code to compute AI for files #
## ---- Please place this file in a folder named "methods" 
##      under the directory where the AI data is stored. ----

## Recent update: Feb 22, 2021, by Xinrui Wu

library(tidyverse)
library(lubridate)
source('./acc_measure_single.R')


## ------------------ Compute AI for all observations ------------------ ##
setwd("..")

n_obs = 91  # number of observations (files)
for (i in 1:n_obs){
  id = 1000 + i 
  input_name = paste0("../R21_Study - ACC/", id, "_acc.rds")
  output_name = paste0("./", id, "_AI.rds")
  
  if (file.exists(input_name)){
    acc = readRDS(input_name)
    acc$time = as_datetime(acc$ts/1000)
    rate = round(1000 / (acc$ts[2] - acc$ts[1]))
    acc_ai = acc_AI(acc, rate, 10)
    saveRDS(acc_ai, file = output_name)
  }
}






