# Accelerometry Data

# Install the package "ActivityIndex": 
# install.packages("devtools")
# devtools::install_github("javybai/ActivityIndex")

library(lubridate) # arrange timestamp
library("ActivityIndex")
library(tidyverse)


acc = readRDS("./data/1001_acc.rds")
acc$time = as_datetime(acc$ts/1000)
acc$date = as_date(acc$time)
# table(acc$date)  ## 4 days 

rate = 1000 / (acc$ts[2] - acc$ts[1]) # 32 measurements a second

# plot of the raw data (first 30 mins) - with high frequency
maxts = rate*60*30   ## 30mins
par(mfrow = c(1,3))
plot(1:maxts/(32*60), acc$acc_x[1:maxts], type = "l")
plot(1:maxts/(32*60), acc$acc_y[1:maxts], type = "l")
plot(1:maxts/(32*60), acc$acc_z[1:maxts], type = "l")
dev.off()

# attempt to draw Activity Index - first 30 mins
temp = data.frame(cbind(1:maxts, acc$acc_x[1:maxts],acc$acc_y[1:maxts], acc$acc_z[1:maxts]))
colnames(temp) = c("Index", "X", "Y", "Z")
attempt1 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.0000001, epoch = 3, hertz = rate)
attempt2 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.0000001, epoch = 10, hertz = rate)
attempt3 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.0000001, epoch = 30, hertz = rate)
attempt4 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.0000001, epoch = 60, hertz = rate)

par(mfrow = c(2,2), mar = c(3,3,5,1), mgp = c(1.5, 1, 0))
plot(attempt1$AI, type = "l", ylab = "Activity Index", xlab = "", main = "epoch = 3 secs", axes = F)
axis(side = 1, labels = NA)
axis(side = 2, labels = NA)
plot(attempt2$AI, type = "l", ylab = "Activity Index", xlab = "", main = "epoch = 10 secs", axes = F)
axis(side = 1, labels = NA)
axis(side = 2, labels = NA)
plot(attempt3$AI, type = "l", ylab = "Activity Index", xlab = "", main = "epoch = 30 secs", axes = F)
axis(side = 1, labels = NA)
axis(side = 2, labels = NA)
plot(attempt4$AI, type = "l", ylab = "Activity Index", xlab = "", main = "epoch = 60 secs", axes = F)
axis(side = 1, labels = NA)
axis(side = 2, labels = NA)
mtext('Activity Index under Different Epoches in the First 30 Mins', 
      outer = TRUE, cex = 1, side = 1, line = -16, col = 'grey')
dev.off()


attempt1 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.00001, epoch = 10, hertz = rate)
attempt2 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.000001, epoch = 10, hertz = rate)
attempt3 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.0000001, epoch = 10, hertz = rate)
attempt4 = computeActivityIndex(temp, x_sigma0 = NULL, sigma0 = 0.00000001, epoch = 10, hertz = rate)

par(mfrow = c(2,2), mar = c(3,3,5,1), mgp = c(2, 1, 0))
plot(attempt1$AI, type = "l", ylab = "Activity Index", xlab = "", main = "sigma0 = 1e-5", axes = F)
axis(side = 1, labels = NA)
axis(side = 2)
plot(attempt2$AI, type = "l", ylab = "Activity Index", xlab = "", main = "sigma0 = 1e-6", axes = F)
axis(side = 1, labels = NA)
axis(side = 2)
plot(attempt3$AI, type = "l", ylab = "Activity Index", xlab = "", main = "sigma0 = 1e-7", axes = F)
axis(side = 1, labels = NA)
axis(side = 2)
plot(attempt4$AI, type = "l", ylab = "Activity Index", xlab = "", main = "sigma0 = 1e-8", axes = F)
axis(side = 1, labels = NA)
axis(side = 2)
mtext("Activity Index under Different Sigma0's in the First 30 Mins", 
      outer = TRUE, cex = 1, side = 1, line = -16, col = 'grey')
dev.off()


# Activity Index
unique_date = unique(acc$date) # date
cutpoint = c(0, which(diff(acc$ts) / 1000 > 1), dim(acc)[1]) # pause

    ## By Pause
par(mfrow = c(length(cutpoint)-1,5), mar = c(3,3,5,1), mgp = c(2, 1, 0))
for (i in 1:(length(cutpoint) - 1)){
  nrow = (cutpoint[i+1] - cutpoint[i]) %/% rate * rate
  sub = data.frame(cbind(1:(cutpoint[i+1] - cutpoint[i]), 
              acc$acc_x[(cutpoint[i]+1):cutpoint[i+1]], 
              acc$acc_y[(cutpoint[i]+1):cutpoint[i+1]], 
              acc$acc_z[(cutpoint[i]+1):cutpoint[i+1]])) [1:nrow, ]
  colnames(sub) = c("Index", "X", "Y", "Z")
  epch = 10 # 10 sec
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "AI", xlab = paste("Time/", epch, "secs"))
  axis(side = 1)
  axis(side = 2, labels = NA)
  
  epch = 30 # 30 sec
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "AI", xlab = paste("Time/", epch, "secs"))
  axis(side = 1)
  axis(side = 2, labels = NA)
  
  epch = 1 * 60 # 1 min
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "AI", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2, labels = NA)
  
  epch = 5 * 60 # 5 min
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "AI", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2, labels = NA)
  
  epch = 10 * 60 # 10 min
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "AI", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2, labels = NA)
} 
dev.off()

    ## By Date
par(mfrow = c(length(unique_date),2), mar = c(3,3,5,1), mgp = c(2, 1, 0))
for (i in 1:length(unique_date)){
  sub = data.frame(cbind(1:length(which(acc$date == unique_date[i])), 
                          acc$acc_x[which(acc$date == unique_date[i])], 
                          acc$acc_y[which(acc$date == unique_date[i])], 
                          acc$acc_z[which(acc$date == unique_date[i])]))[1:(length(which(acc$date == unique_date[i]))%/%rate*rate),]
  colnames(sub) = c("Index", "X", "Y", "Z")
  epch = 60
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Day ", i), 
       ylab = "Activity Index", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2, labels = NA)
  
  epch = 600
  sub_ai = computeActivityIndex(sub, sigma0 = 0.0000001, epoch = epch, hertz = rate)
  plot(sub_ai$AI, type = "l", axes = F, main = paste("Day ", i), 
       ylab = "Activity Index", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2, labels = NA)
}
dev.off()

    ## all sample
data_all = data.frame(cbind(1:dim(acc)[1], acc$acc_x, 
                       acc$acc_y, acc$acc_z))[1:(dim(acc)[1] %/% rate * rate),]
colnames(data_all) = c("Index", "X", "Y", "Z")
ind_date = c(which(acc$date == unique_date[2])[1], 
             which(acc$date == unique_date[3])[1], 
             which(acc$date == unique_date[4])[1]) / rate
ind_cut = cutpoint[2:4] / rate

par(mfrow = c(2, 1), mar = c(4,4,2,2), mgp = c(2, 1, 0))
epch = 60
sigma = 0.0000001
acc_ai = computeActivityIndex(data_all, sigma0 = sigma, epoch = epch, hertz = rate)
plot(acc_ai$AI, type = "l", axes = F, 
     ylab = "Activity Index", xlab = paste("Time/", epch/60, "mins"))
axis(side = 1)
axis(side = 2, labels = NA)
abline(v = ind_date / epch, col = 'red', lty = 1)
abline(v = ind_cut / epch, col = 'blue', lty = 2)
legend(x = 2200, y = max(acc_ai$AI), legend = c('date', 'pause'), col = c('red', "blue"), 
       lty = c(1, 2), cex = 0.7)

epch = 600
sigma = 0.0000001
acc_ai = computeActivityIndex(data_all, sigma0 = sigma, epoch = epch, hertz = rate)
plot(acc_ai$AI, type = "l", axes = F, 
     ylab = "Activity Index", xlab = paste("Time/", epch/60, "mins"))
axis(side = 1)
axis(side = 2, labels = NA)
abline(v = ind_date / epch, col = 'red', lty = 1)
abline(v = ind_cut / epch, col = 'blue', lty = 2)
legend(x = 220, y = max(acc_ai$AI), legend = c('date', 'pause'), col = c('red', "blue"), 
       lty = c(1, 2), cex = 0.7)
dev.off()



# ENMO
    ## all data
par(mfrow = c(2, 1), mar = c(4,4,2,2), mgp = c(2, 1, 0))
epch = 60
enmo = mutate(acc, Index = 1:dim(acc)[1], x = acc_x, y = acc_y, z = acc_z) %>%
  transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
  transmute(Index, enmo = enmo * (enmo > 0)) %>%
  filter(Index %in% 1:(dim(acc)[1] %/% (rate*epch) * (rate*epch))) %>%
  mutate(Ind = Index %/% (rate*epch) +1) %>%
  group_by(Ind) %>%
  summarize(enmo = mean(enmo))
plot(enmo$enmo, type = "l", axes = F, 
     ylab = "ENMO", xlab = paste("Time/", epch/60, "mins"))
axis(side = 1)
axis(side = 2)
  
epch = 600
enmo = mutate(acc, Index = 1:dim(acc)[1], x = acc_x, y = acc_y, z = acc_z) %>%
  transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
  transmute(Index, enmo = enmo * (enmo > 0)) %>%
  filter(Index %in% 1:(dim(acc)[1] %/% (rate*epch) * (rate*epch))) %>%
  mutate(Ind = Index %/% (rate*epch) +1) %>%
  group_by(Ind) %>%
  summarize(enmo = mean(enmo))
plot(enmo$enmo, type = "l", axes = F, 
     ylab = "ENMO", xlab = paste("Time/", epch/60, "mins"))
axis(side = 1)
axis(side = 2)
dev.off()

    ## AI v.s. ENMO -- epoch = 10 mins
plot(acc_ai$AI[1:min(length(acc_ai$AI), length(enmo$enmo))], 
     enmo$enmo[1:min(length(acc_ai$AI), length(enmo$enmo))], 
     xlab = 'AI', ylab = 'ENMO',  main = 'epch = 600') 


    ## By Pause
par(mfrow = c(length(cutpoint)-1,5), mar = c(3,3,5,1), mgp = c(2, 1, 0))
for (i in 1:(length(cutpoint) - 1)){
  nrow = (cutpoint[i+1] - cutpoint[i]) %/% rate * rate
  sub = data.frame(cbind(1:(cutpoint[i+1] - cutpoint[i]), 
                         acc$acc_x[(cutpoint[i]+1):cutpoint[i+1]], 
                         acc$acc_y[(cutpoint[i]+1):cutpoint[i+1]], 
                         acc$acc_z[(cutpoint[i]+1):cutpoint[i+1]])) [1:nrow, ]
  colnames(sub) = c("Index", "x", "y", "z")
  
  epch = 10 # 10 sec
  sub_enmo = sub %>%
    transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
    transmute(Index, enmo = enmo * (enmo > 0)) %>%
    filter(Index %in% 1:(dim(sub)[1] %/% (rate*epch) * (rate*epch))) %>%
    mutate(Ind = Index %/% (rate*epch) +1) %>%
    group_by(Ind) %>%
    summarize(enmo = mean(enmo))
  plot(sub_enmo, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "ENMO", xlab = paste("Time/", epch, "secs"))
  axis(side = 1)
  axis(side = 2)
  
  epch = 30 # 30 sec
  sub_enmo = sub %>%
    transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
    transmute(Index, enmo = enmo * (enmo > 0)) %>%
    filter(Index %in% 1:(dim(sub)[1] %/% (rate*epch) * (rate*epch))) %>%
    mutate(Ind = Index %/% (rate*epch) +1) %>%
    group_by(Ind) %>%
    summarize(enmo = mean(enmo))
  plot(sub_enmo, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "ENMO", xlab = paste("Time/", epch, "secs"))
  axis(side = 1)
  axis(side = 2)
  
  epch = 1 * 60 # 1 min
  sub_enmo = sub %>%
    transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
    transmute(Index, enmo = enmo * (enmo > 0)) %>%
    filter(Index %in% 1:(dim(sub)[1] %/% (rate*epch) * (rate*epch))) %>%
    mutate(Ind = Index %/% (rate*epch) +1) %>%
    group_by(Ind) %>%
    summarize(enmo = mean(enmo))
  plot(sub_enmo, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "ENMO", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2)
  
  epch = 5 * 60 # 5 min
  sub_enmo = sub %>%
    transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
    transmute(Index, enmo = enmo * (enmo > 0)) %>%
    filter(Index %in% 1:(dim(sub)[1] %/% (rate*epch) * (rate*epch))) %>%
    mutate(Ind = Index %/% (rate*epch) +1) %>%
    group_by(Ind) %>%
    summarize(enmo = mean(enmo))
  plot(sub_enmo, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "ENMO", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2)
  
  epch = 10 * 60 # 10 min
  sub_enmo = sub %>%
    transmute(Index, enmo = (sqrt(x^2+y^2+z^2) - 1)) %>%
    transmute(Index, enmo = enmo * (enmo > 0)) %>%
    filter(Index %in% 1:(dim(sub)[1] %/% (rate*epch) * (rate*epch))) %>%
    mutate(Ind = Index %/% (rate*epch) +1) %>%
    group_by(Ind) %>%
    summarize(enmo = mean(enmo))
  plot(sub_enmo, type = "l", axes = F, main = paste("Continuous Record ", i), 
       ylab = "ENMO", xlab = paste("Time/", epch/60, "mins"))
  axis(side = 1)
  axis(side = 2)
} 
dev.off()









