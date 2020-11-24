# Summary and plots on AI and pause time


library(tidyverse)

## ------------------ Summary for AI and Pause time------------------ ##
n_obs = 91  # number of observations (files)
epch = 10

summary_AI = data.frame(matrix(0, nrow = n_obs, ncol = 12))
colnames(summary_AI) = c('ID', 'AI_mean', 'AI_sd', 'AI_max', 'AI_min', 
                         'AI.25', 'AI.50', 'AI.75', 
                         'Days', 'Continuous Periods', 
                         'Pause_max(hour)', 'Pause_min(hour)')
# pause and period length in seconds
all_pause = data.frame(ID=integer(), pause_num=integer(), pause=double(), period=double()) 


none_ind = 0
for (i in 1:n_obs){
  id = 1000 + i 
  input_name = paste0("./", id, "_AI.rds")
  
  if (file.exists(input_name)){
    acc_ai = readRDS(input_name)
    summary_AI[i, 'ID'] = id
    summary_AI[i, 'AI_mean'] = mean(acc_ai$AI)
    summary_AI[i, 'AI_sd'] = sd(acc_ai$AI)
    summary_AI[i, 'AI_max'] = max(acc_ai$AI)
    summary_AI[i, 'AI_min'] = min(acc_ai$AI)
    summary_AI[i, 'AI_max'] = max(acc_ai$AI)
    summary_AI[i, c('AI.25', 'AI.50', 'AI.75')] = quantile(acc_ai$AI, probs = seq(0.25, 0.75, 0.25))
    summary_AI[i, 'Days'] = length(unique(as.Date(acc_ai$timestamp)))
    summary_AI[i, 'Continuous Periods'] = length(unique(acc_ai$Period))
    
    time_diff = round(diff(as.numeric(acc_ai$timestamp)))
    cum_time = cumsum(time_diff)[(c(which(time_diff > epch), length(time_diff)+1)-1)] - 
      c(0, cumsum(time_diff)[which(time_diff > epch)])
    n_pause = length(which(time_diff > epch))
    all_pause = rbind(all_pause, data.frame(ID=id, pause_num=0, pause=0, period=cum_time[1]))
    if (n_pause > 0){
      sub_pause = time_diff[which(time_diff > epch)]
      all_pause = rbind(all_pause, data.frame(ID=rep(id, n_pause), pause_num=1:n_pause, 
                                              pause=sub_pause, period=cum_time[2:length(cum_time)]))
      summary_AI[i, 'Pause_max(hour)'] = round(max(sub_pause)/3600, 2)
      summary_AI[i, 'Pause_min(hour)'] = round(min(sub_pause)/3600, 2)
    }
    else{
      summary_AI[i, 'Pause_max(hour)'] = 0
      summary_AI[i, 'Pause_min(hour)'] = 0
    }
  }
  
  else{
    none_ind = c(none_ind, i)
  }
}
none_ind = none_ind[-1]
if(length(none_ind) > 0) {summary_AI = summary_AI[-none_ind,]}
saveRDS(summary_AI, file = './methods/summary_AI.rds')

saveRDS(all_pause, file = './methods/pauses.rds')
summary_pause = all_pause %>%
  group_by(ID) %>%
  summarise(num_less_than_1min = sum(pause_num > 0 & pause < 60), 
            num_1min_to_30min = sum(pause_num > 0 & pause >= 60 & pause < 30*60), 
            num_30min_to_1day = sum(pause_num > 0 & pause >= 30*60 & pause < 60*60*24),
            num_more_than_1day = sum(pause_num > 0 & pause >= 60*60*24), 
            num_total = sum(pause_num > 0))
saveRDS(summary_pause, file = './methods/summary_pause.rds')

all_pause = readRDS('./methods/pauses.rds')
time_with_miss_30min = transmute(all_pause, ID, pause_num, period) %>%
  mutate(time_with_miss = period * (period <= 30*60 & pause_num > 0) + 
           rep(30*60, dim(all_pause)[1]) * (period > 30*60 & pause_num > 0))
prob_miss_30min_obs = time_with_miss_30min %>%
  group_by(ID) %>%
  summarize(prob = sum(time_with_miss) / sum(period))
prob_miss_30min_all = sum(time_with_miss_30min$time_with_miss) / sum(time_with_miss_30min$period)
prob_miss_30min_all


## ------------------ Plots ------------------ ##
## summary of AI ##
hist(summary_AI$AI_mean, breaks = 25, axes = F, xlab = '', ylab = '', 
     main = 'Histogram of mean(AI) across Observations')
axis(side = 1)
axis(side = 2)


par(xpd=T, mar=c(5,5,5,7))
ymax = max(summary_AI$AI_max)
ymin = min(summary_AI$AI_min)
plot(x = (summary_AI$ID - 1000), y = summary_AI$AI_mean, type = 'l', axes = F, ylim = c(ymin-50, ymax+50), 
     xlab = 'Observations', ylab = 'AI', main = 'Summary of AI acorss Observations')
axis(side = 1)
axis(side = 2)
points(x = (summary_AI$ID - 1000), y = summary_AI$AI_max, type = 'l', lty = 2, col = 'red')
points(x = (summary_AI$ID - 1000), y = summary_AI$AI_min, type = 'l', lty = 2, col = 'blue')
points(x = (summary_AI$ID - 1000), y = summary_AI$AI.25, type = 'l', lty = 3, col = 'red')
points(x = (summary_AI$ID - 1000), y = summary_AI$AI.50, type = 'l', lty = 3, col = 'blue')
points(x = (summary_AI$ID - 1000), y = summary_AI$AI.75, type = 'l', lty = 3, col = 'red')
text(x = (summary_AI$ID - 1000), y = rep(ymax+20, dim(summary_AI)[1]), 
     labels = summary_AI$Days, col = "dimgrey", cex = .5)
text(x = (summary_AI$ID - 1000), y = rep(ymax+45, dim(summary_AI)[1]), 
     labels = summary_AI$`Continuous Periods`, col = "pink", cex = .5)
legend(95,900, legend=c("AI_max", "AI_min", "AI_mean", 'Days', 'Periods'), 
       col=c('red', 'blue', 'black', 'dimgrey', 'pink'), box.col = "white", 
       horiz=F, lty=c(2,2,1,1,1), cex=0.8)
legend(95,900, legend=c("mean", "maximum", "minimum", 
                        '25% quantile', '50% quantile', '75% quantile'), 
       col=c('black', 'dimgrey', 'dimgrey','red', 'blue','red'), box.col = "white", 
       horiz=F, lty=c(1,2,2,3,3,3), cex=0.8)
legend(91,100, legend=c("mean", 
                        '25% quantile', '50% quantile', '75% quantile'), 
       col=c('black', 'red', 'blue','dimgrey'), box.col = "white", 
       horiz=F, lty=c(1,3,3,3), cex=0.8)



## summary of pause time ##
hist(all_pause$pause)
boxplot((pause/3600)~ID, data = all_pause, ylab='Pause Time / hour')


df = t(as.matrix(summary_pause[,2:5]))
barplot(df, col=c('red', 'blue', 'yellow', 'grey'),
        legend = c('< 1min', '[1min, 30mins)', '[30mins, 1day)', '>= 1day'), 
        xlab = 'ID', ylab = 'Count', main = "Number of Pauses")
par(mfrow = c(2, 2), mgp = c(1,1,0))
barplot(df[1,], col='red', xlab = 'ID', main = 'Pauses < 1 min')
barplot(df[2,], col='blue', xlab = 'ID', main = 'Pauses in [1min, 30mins)')
barplot(df[3,], col='yellow', xlab = 'ID', main = 'Pauses in [30mins, 1day)')
barplot(df[4,], col='grey', xlab = 'ID', main = 'Pauses > 1 day')
dev.off()


## probability of having missing data in the past 30 mins for each observed time ##
barplot(prob_miss_30min_obs$prob, type = 'l', xlab='ID', ylab='Probability', 
        main = 'Prob. of having missing data in the past 30 mins')
hist(prob_miss_30min_obs$prob, xlab='Probability', breaks = seq(from=0, to=0.15, by = 0.01), 
     main = 'Histogram of the Prob. of having missing data in the past 30 mins')
