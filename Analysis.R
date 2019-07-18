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
result <- mclapply(flag, bp.hr.rankings, mc.cores = numCores)

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


#For all HRs, for actual HRs
judge %>% 
  filter(potential.HR.count != 0 & actual.HR.count != 0 & park == 'NYY') -> dat

#For All HRs, use for potential HRs
judge %>% 
  filter(potential.HR.count != 0 & actual.HR.count != 0 ) -> dat0

#For HRs Min 30, actual HRs
judge %>%
  filter(park == 'NYY' & actual.HR.count >= 30) -> dat1

#For HRs Min 30, potential HRs 
judge %>% 
  filter(actual.HR.count >= 30 ) -> dat2

judge %>%
  filter(park == 'NYY') %>%
  arrange(desc(total.gain))


judge %>%
  filter(park == 'NYY') %>%
  arrange(total.gain)


####Visualization####
#Distribution of Gainers and Losers for each ballpark

#Total Gains all HRs
mean(dat$total.gain)
median(dat$total.gain)
sd(dat$total.gain)

#Actual total count min 30
mean.pot.hrs =  mean(dat1$potential.HR.count)
median(dat1$potential.HR.count)
sd(dat1$potential.HR.count)

#Potential HR Count min 30
mean(dat2$actual.HR.count)
median(dat2$actual.HR.count)
sd(dat2$actual.HR.count)


#Box Plot of Biggest Winners/Losers for Each HR 
ggplot(dat0, aes(x=park, y=total.gain)) + geom_hline(yintercept = median(dat0$total.gain), color='blue', size=1) +
  geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Boxplot of Gainers/Losers for each Ballpark")


#Box Plot of Biggest Winners/Losers for Each HR (Min 30 HRs)
ggplot(dat0, aes(x=park, y=total.gain)) + geom_hline(yintercept = median(dat0$total.gain), color='blue', size=1) + geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Boxplot of Gainers/Losers for each Ballpark") + coord_flip()


#Distribution of gains/losses for all data
ggplot(dat, aes(x= total.gain)) + geom_histogram(bindwidth = 3) +
  geom_vline(xintercept = mean(dat$total.gain), color = "red") +
  labs(x = "Net Gain/Loss of HRs", y = "Count", title = "Distribution of Gains/Losses")

#QQ plot of all HRs
ggqqplot(dat$total.gain)  + labs(title = "QQ-Plot of the distribution of Gains/Losses")


#Distribution of gains/losses for min 30 HRs
ggplot(dat1, aes(x= total.gain)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean(dat$total.gain), color = "red") +
  labs(x = "Net Gain/Loss of HRs", y = "Count", title = "Distribution of Gains/Losses (Min 30 HRs)")

#QQ plot of Min 30 HRs
ggqqplot(dat1$total.gain)  + labs(title = "QQ-Plot of the distribution of Gains/Losses (Min 30 HRs)")



#Distribution of actual HRs
ggplot(dat, aes(x= actual.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean(dat$actual.HR.count), color = "red") +
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Actual Home Runs for Each Player for Each Ballpark 2016-2018 (All HRs)")

#Distribution of all potential HRs
ggplot(dat, aes(x= potential.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean(dat$potential.HR.count), color = "red") +
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Potential Home Runs for Each Player for Each Ballpark 2016-2018 (All HRs)")



#Distribution of all HRs (Min 30 HRs)
ggplot(dat1, aes(x= actual.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean(dat$actual.HR.count), color = "red") +
  labs(x = "Home Runs", y = "Count", title = "Distribution of Actual Home Runs for Each Player 2016-2018 (min 30 HRs)")

#Distribution of potential HRs (Min 30 HRs)
ggplot(dat2, aes(x= potential.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean(dat2$potential.HR.count), color = "red") + 
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Potential Home Runs for Each Player for Each Ballpark 2016-2018")


#Stacked histogram of HRs at each ballpark

plothist = function(i){ggplot(subset(dat2, park == hr_bridge[i,1]), aes(x= potential.HR.count)) + geom_histogram(binwidth = 3) +
    geom_vline(xintercept = mean.pot.hrs, color = "red") + labs(y = hr_bridge[i,1]) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          axis.title.x=element_blank())}

set.seed(69, kind = "L'Ecuyer-CMRG" );
stackhist <- mclapply(flag, plothist, mc.cores = numCores)

marrangeGrob(stackhist, nrow = 10, ncol = 3)








#What does this tell us about the algorithm?



median(judge$total.gain)























