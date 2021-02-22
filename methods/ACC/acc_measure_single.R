# Function to compute AI #
## ---- Please place this file in a folder named "methods" 
##      under the directory where the AI data is stored. ----

## Recent update: Feb 22, 2021, by Xinrui Wu

## acc_AI: compute AI for a single obs.

acc_AI = function(data, rate, epch){
  # Input:
  #   data: accelerometry data for one person with variables 
  #         time: record time in timestamp data type ("POSIXct", "POSIXt")
  #         acc_x, acc_y, acc_z: position variables
  #         device: device name
  #   rate: the rate of the accelerometry data
  #   epch: integer time window lengths (in seconds)
  # Output:
  #   measure: dataframe for AI, including Priod (which continuous period the record is in),
  #                                        timestamp (position of the record in that period) and AI
  require(lubridate) # arrange timestamp
  require("ActivityIndex")
  require(tidyverse)
  
  acc_all = data.frame(transmute(data, time, Device=E4_serial, acc_x, acc_y, acc_z))
  devices = unique(acc_all$Device)
  measure = data.frame(Period=NULL, Device=NULL, timestamp=NULL, AI=NULL) 
  for (d in 1:length(devices)){
    acc = filter(acc_all, Device == devices[d]) %>%
      arrange(time)
    # view loss of record larger than 1/32 (about 0.032) second as break of time
    cutpoint = c(0, which(diff(acc$time) > 0.032), dim(acc)[1])
    
    for (i in 1:(length(cutpoint) - 1)){
      nrow = (cutpoint[i+1] - cutpoint[i]) %/% (rate*epch) * (rate*epch)
      time_ind = seq(from = cutpoint[i]+1, to = cutpoint[i]+nrow, by = rate*epch)
      sub_time = acc[time_ind, 'time']
      sub = data.frame(cbind(1:(cutpoint[i+1] - cutpoint[i]), 
                             acc$acc_x[(cutpoint[i]+1):cutpoint[i+1]], 
                             acc$acc_y[(cutpoint[i]+1):cutpoint[i+1]], 
                             acc$acc_z[(cutpoint[i]+1):cutpoint[i+1]])) [1:nrow, ]
      colnames(sub) = c("Index", "X", "Y", "Z")
      sub_ai = computeActivityIndex(sub, sigma0 = 1e-07, epoch = epch, hertz = rate)
      temp = data.frame(Period = rep(i, dim(sub_ai)[1]), 
                        Device = rep(devices[d], dim(sub_ai)[1]), 
                        timestamp = sub_time, AI = sub_ai$AI / 1e+07)
      measure = rbind(measure, temp)
    }
  }
  return(measure)
}
