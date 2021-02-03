# Function to compute AI and ENMO
## acc_measure: compute AI and ENMO for a single obs.
## plot_acc: plots for acc_measure
## acc_AI: compute AI and ENMO for a single obs.

acc_measure = function(data, rate, epch){
  # Input:
  #   data: accelerometry data for one person with variables 
  #         time: record time in timestamp data type ("POSIXct", "POSIXt")
  #         acc_x, acc_y, acc_z: position variables
  #   rate: the rate of the accelerometry data
  #   epch: integer time window lengths (in seconds)
  # Output:
  #   measure: dataframe for AI and ENMO 
  #   breakTime: timestamps at the begining and end of an incontinuous time period
  require(lubridate) # arrange timestamp
  require("ActivityIndex")
  require(tidyverse)
  
  acc = data.frame(transmute(data, time, acc_x, acc_y, acc_z))
  # view loss of record larger than 1 second as break of time
  cutpoint = c(0, which(diff(acc$time) > 1), dim(acc)[1])
  breakTime = data.frame(before_break = acc$time[which(diff(acc$time) > 1)], 
                         after_break = acc$time[which(diff(acc$time) > 1)+1])
  measure = NULL
  for (i in 1:(length(cutpoint) - 1)){
    nrow = (cutpoint[i+1] - cutpoint[i]) %/% (rate*epch) * (rate*epch)
    sub = data.frame(cbind(1:(cutpoint[i+1] - cutpoint[i]), 
                           acc$acc_x[(cutpoint[i]+1):cutpoint[i+1]], 
                           acc$acc_y[(cutpoint[i]+1):cutpoint[i+1]], 
                           acc$acc_z[(cutpoint[i]+1):cutpoint[i+1]])) [1:nrow, ]
    colnames(sub) = c("Index", "X", "Y", "Z")
    sub_ai = computeActivityIndex(sub, sigma0 = 1e-7, epoch = epch, hertz = rate)
    sub_enmo = sub %>%
      transmute(Index, enmo = (sqrt(X^2+Y^2+Z^2) - 1)) %>%
      transmute(Index, enmo = enmo * (enmo > 0)) %>%
      mutate(Ind = rep(1:dim(sub_ai)[1], each = rate*epch)) %>%
      group_by(Ind) %>%
      summarize(enmo = mean(enmo))
    if (is.null(measure)){
      measure = cbind(1:dim(sub_enmo)[1], rep(i, dim(sub_enmo)[1]), sub_ai$AI, sub_enmo$enmo)
    }
    else {
      temp = cbind(1:dim(sub_enmo)[1], rep(i, dim(sub_enmo)[1]), sub_ai$AI, sub_enmo$enmo)
      measure = rbind(measure, temp)
    }
  }
  measure = data.frame(measure)
  colnames(measure) = c('Index', 'Period', 'AI', 'ENMO')
  measure$AI = measure$AI / 1e7
  return(list(rate = rate, epch = epch, 
              breakTime = breakTime, 
              measure = measure))
}

plot_acc = function(acc_measure){
  # Input:
  #   acc_measure: results from function acc_measure
  # Output:
  #   plots
  
  period = dim(acc_measure$breakTime)[1] + 1
  epch = acc_measure$epch
  measures = acc_measure$measure
  par(mfrow = c(period,2), mar = c(3,3,5,1), mgp = c(2, 1, 0))
  for (i in 1:period){
    sub = measures[which(measures$Period == i),]
    
    if (epch > 60){ xlabel = paste("Time/", epch/60, "mins") }
    else{ xlabel = paste("Time/", epch, "secs")}
    
    plot(sub$AI, type = "l", axes = F, 
         ylab = "AI", xlab = xlabel)
    axis(side = 1)
    axis(side = 2, labels = NA)
    
    plot(sub$ENMO, type = "l", axes = F, 
         ylab = "ENMO", xlab = xlabel)
    axis(side = 1)
    axis(side = 2)
  }
}


acc_AI = function(data, rate, epch){
  # Input:
  #   data: accelerometry data for one person with variables 
  #         time: record time in timestamp data type ("POSIXct", "POSIXt")
  #         acc_x, acc_y, acc_z: position variables
  #   rate: the rate of the accelerometry data
  #   epch: integer time window lengths (in seconds)
  # Output:
  #   measure: dataframe for AI, including Priod (which continuous period the record is in),
  #                                        timestamp (position of the record in that period) and AI
  require(lubridate) # arrange timestamp
  require("ActivityIndex")
  require(tidyverse)
  
  acc = data.frame(transmute(data, time, acc_x, acc_y, acc_z))
  # view loss of record larger than 1 second as break of time
  cutpoint = c(0, which(diff(acc$time) > 1), dim(acc)[1])
  measure = NULL
  for (i in 1:(length(cutpoint) - 1)){
    nrow = (cutpoint[i+1] - cutpoint[i]) %/% (rate*epch) * (rate*epch)
    time_ind = seq(from = cutpoint[i]+1, to = cutpoint[i]+nrow, by = rate*epch)
    sub_time = data[time_ind, 'time']
    sub = data.frame(cbind(1:(cutpoint[i+1] - cutpoint[i]), 
                           acc$acc_x[(cutpoint[i]+1):cutpoint[i+1]], 
                           acc$acc_y[(cutpoint[i]+1):cutpoint[i+1]], 
                           acc$acc_z[(cutpoint[i]+1):cutpoint[i+1]])) [1:nrow, ]
    colnames(sub) = c("Index", "X", "Y", "Z")
    sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
    if (is.null(measure)){
      measure = data.frame(Period = rep(i, dim(sub_ai)[1]), 
                           timestamp = sub_time, 
                           AI = sub_ai$AI)
    }
    else {
      temp = data.frame(Period = rep(i, dim(sub_ai)[1]), 
                        timestamp = sub_time, 
                        AI = sub_ai$AI)
      measure = rbind(measure, temp)
    }
  }
  return(measure)
}


