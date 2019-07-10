####Pulling in Potential HR rankings for all teams####

#Determine number of cores
numCores <- detectCores()
numCores

#Function to pull in HR rank for each ballpark
  bp.hr.rankings = function(i){
  assign(toString(hr_bridge[i,2]), match_score(eval(parse(text = hr_bridge[i,1])),ev = 122), envir = .GlobalEnv)
  }

flag = seq(1,30)

#Run function Multi-thread 
set.seed(99, kind = "L'Ecuyer-CMRG" );
result <- mclapply(flag, bp.hr.rankings, mc.cores = 8)

#Combine into one table
judge = NULL
for(i in 1:30){
  x = result[[i]]
  judge = rbind(judge,x)
}


####Exploratory####
judge %>% 
  #filter(player_name != 'Didi Gregorius') %>%
  arrange(desc(total.gain)) %>%
  print(n = 100)



judge %>%
  arrange(desc(potential.HR.count)) %>%
  print(n = 100)


judge %>% 
  filter(actual.HR.count > 30) %>%
  mutate(gain.percentage = total.gain/actual.HR.count * 100) %>%
  arrange(desc(gain.percentage)) %>%
  print(n = 100)


judge %>% 
  filter(player_name  == 'Didi Gregorius') %>%
  print(n = 30)


judge %>% 
  filter(potential.HR.count != 0 & actual.HR.count != 0) -> dat

judge %>% 
  filter(actual.HR.count >= 30 ) -> dat

judge %>%
  filter(park == 'NYY' & actual.HR.count >= 30) -> dat2


judge %>% 
  filter(potential.HR.count != 0 & actual.HR.count != 0) -> dat3


####Visualization####
#Distribution of Gainers and Losers for each ballpark


mean(dat$total.gain)
median(dat$total.gain)
sd(dat$total.gain)

ggplot(dat, aes(x=park, y=total.gain)) + geom_hline(yintercept = median(dat$total.gain), color='blue', size=1) + geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Boxplot of Gainers/Losers for each Ballpark")


ggplot(dat, aes(x= total.gain)) + geom_histogram(bins = 40) +
  geom_vline(xintercept = mean(dat$total.gain), color = "red") +
  labs(x = "Net Gain/Loss of HRs", y = "Count", title = "Distribution of Gains/Losses")

ggqqplot(dat$total.gain)  + labs(title = "QQ-Plot of the distribution of Gains/Losses")

ggplot(dat, aes(x= potential.HR.count)) + geom_histogram(bins = 45) +
  geom_vline(xintercept = mean(dat$potential.HR.count), color = "red") +
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Potential Home Runs for Each Player for Each Ballpark 2016-2018 (min 30 HRs)")


ggplot(dat2, aes(x= actual.HR.count)) + geom_histogram(bins = 45) +
  geom_vline(xintercept = mean(dat$actual.HR.count), color = "red") +
  labs(x = "Home Runs", y = "Count", title = "Distribution of Actual Home Runs for Each Player 2016-2018 (min 30 HRs)")

ggplot(dat3, aes(x= potential.HR.count)) + geom_histogram(bins = 45) +
  geom_vline(xintercept = mean(dat$potential.HR.count), color = "red") +
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Potential Home Runs for Each Player for Each Ballpark 2016-2018 (min 30 HRs)")

#What does this tell us about the algorithm?



median(judge$total.gain)























