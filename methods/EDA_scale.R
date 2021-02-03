## methods for EDA scaling

library(lubridate)
library(tidyverse)



eda = readRDS('./data/1006_EDA.rds')
eda$time = as_datetime(eda$ts/1000)
eda$date = as_date(eda$time)

#### scaled by user-day ####
eda_scaled_old = transmute(eda, date, time, EDA=EDA_HighLowPass) %>%
  drop_na() %>%
  group_by(date) %>%
  mutate(EDA_scaled = scale(EDA)) %>%
  ungroup() %>%
  transmute(timestamp=time, EDA_scaled)
eda_scaled_old$Date = as_date(eda_scaled_old$timestamp)
date = unique(eda_scaled_old$Date)

  ## plots for 1006, 'Standardized EDA - Each user-day'
par(mfrow=c(2, 3))
for (i in 1:length(date)){
  sub = eda_scaled_old %>%
    filter(Date == date[i])
  plot(x=sub$timestamp, 
       y=sub$EDA_scaled, 
       type='l', frame.plot = F, 
       xlab=date[i], ylab='')
}
dev.off()

#### scaled by user-day per device ####
eda_scaled = transmute(eda, date, time, Device=E4_serial, EDA=EDA_HighLowPass) %>%
  drop_na() %>%
  group_by(Device, date) %>%
  mutate(EDA_scaled = scale(EDA)) %>%
  ungroup() %>%
  arrange(date, Device) %>%
  transmute(timestamp = time, Device, EDA_scaled)

eda_scaled$Date = as_date(eda_scaled$timestamp)
date = unique(eda_scaled$Date)
device = unique(eda_scaled$Device)

  ## plots for 1006, 'Standardized EDA - Each user-day per device'
par(mfrow=c(3,3))
for (i in 1:length(date)){
  for (d in device){
    sub = eda_scaled %>%
      filter(Device == d, Date == date[i])
    if (dim(sub)[1] != 0){
      plot(x=sub$timestamp, 
           y=sub$EDA_scaled, 
           type='l', frame.plot = F, 
           xlab=paste(date[i], d), ylab='')
    }
  }
}
dev.off()