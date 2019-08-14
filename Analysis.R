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


####MLB Line UP####

#NL
judge %>%
  filter(player_name %in% c('Christian Yelich', 'Javier Baez', 'Freddie Freeman',
                            'Cody Bellinger', 'Nolan Arenado', 'Josh Bell',
                            'Willson Contreras', 'Ketel Marte', 'Ronald Acuna Jr.')) %>%
  filter(park == 'CLE') %>%
  arrange(desc(potential.HR.count))


#AL
judge %>%
  filter(player_name %in% c('George Springer', 'DJ LeMahieu', 'Mike Trout', 'Carlos Santana',
                            'J.D. Martinez', 'Alex Bregman', 'Gary Sanchez', 'Michael Brantley',
                            'Jorge Polanco')) %>%
  filter(park == 'CLE') %>%
  arrange(desc(potential.HR.count))

#Visuals
by_player_2d('Mike Trout',CLE, ev = 125)
by_player_2d('Cody Bellinger',CLE, ev = 125)

judge %>%
  filter(player_name == 'Charlie Blackmon') %>%
  filter(park == 'CLE')



####Visualization####
#Distribution of Gainers and Losers for each ballpark

#Total Gains all HRs
diff = mean(dat$total.gain)
median(dat$total.gain)
sd(dat$total.gain)

#Actual total count min 30
mean.act.hrs =  mean(dat1$actual.HR.count)
median(dat1$actual.HR.count)
sd(dat1$actual.HR.count)

#Potential HR Count min 30
mean.pot.hrs = mean(dat2$potential.HR.count)
median(dat2$potential.HR.count)
sd(dat2$potential.HR.count)
mean.diff = mean(dat2$total.gain)


#Box Plot of Biggest Winners/Losers for Each HR 
ggplot(dat0, aes(x=park, y=total.gain)) + geom_hline(yintercept = diff, color='blue', size=1) +
  geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Boxplot of Gainers/Losers for each Ballpark")


#Box Plot of Biggest Winners/Losers for Each HR (Min 30 HRs)
ggplot(dat2, aes(x=park, y=total.gain)) + geom_hline(yintercept = mean(dat2$total.gain), color='blue', size=1) + geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Boxplot of Gainers/Losers for each Ballpark (Min 30 HRs)") 

#Where is Didi and Freddie
ggplot(dat2, aes(x=park, y=total.gain)) + geom_hline(yintercept = mean(dat2$total.gain), color='blue', size=1) + geom_boxplot() + 
  labs(x = "Ballpark", y = "Net Gain/Loss of HRs", title = "Unique Fit of Didi (min 30 HRs)") + 
  geom_text_repel(data = filter(dat0, player_name == 'Didi Gregorius'), aes(label = paste('Didi'), col = "red")) 


#Distribution of gains/losses for all data
ggplot(dat, aes(x= total.gain)) + geom_histogram(bindwidth = 3) +
  geom_vline(xintercept = diff, color = "red") +
  labs(x = "Net Gain/Loss of HRs", y = "Count", title = "Distribution of Gains/Losses")

#QQ plot of all HRs
ggqqplot(dat$total.gain)  + labs(title = "QQ-Plot of the distribution of Gains/Losses")


#Distribution of gains/losses for min 30 HRs
ggplot(dat2, aes(x= total.gain)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean.diff, color = "red") +
  labs(x = "Net Gain/Loss of HRs", y = "Count", title = "Distribution of Gains/Losses (Min 30 HRs)")

#QQ plot of Min 30 HRs
ggqqplot(dat2$total.gain)  + labs(title = "QQ-Plot of the distribution of Gains/Losses (Min 30 HRs)")


#Distribution of all HRs (Min 30 HRs)
ggplot(dat1, aes(x= actual.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean.act.hrs, color = "red") +
  labs(x = "Home Runs", y = "Count", title = "Distribution of Actual Home Runs for Each Player 2016-2018 (min 30 HRs)")

#Distribution of potential HRs (Min 30 HRs)
ggplot(dat2, aes(x= potential.HR.count)) + geom_histogram(binwidth = 3) +
  geom_vline(xintercept = mean.pot.hrs, color = "red") + 
  labs(x = "Potential HRs", y = "Count", 
       title = "Distribution of Potential Home Runs for Each Player for Each Ballpark 2016-2018")


#Stacked histogram of potential HRs at each ballpark

plothist = function(i){ggplot(subset(dat2, park == hr_bridge[i,1]), aes(x= potential.HR.count)) + geom_histogram(binwidth = 3) +
    geom_vline(xintercept = mean.pot.hrs, color = "red") + labs(y = hr_bridge[i,1]) +
    theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(),
          axis.title.x=element_blank())}

set.seed(69, kind = "L'Ecuyer-CMRG" );
stackhist <- mclapply(flag, plothist, mc.cores = numCores)

marrangeGrob(stackhist, nrow = 10, ncol = 3)

####Actual vs Potential HR Plot####

#All HRs
allhr.lm = lm(formula = potential.HR.count ~ actual.HR.count, data = dat0)
summary(allhr.lm)
par(mfrow = c(2,2))
plot(allhr.lm)
par(mfrow = c(1,1))
dat0$res = allhr.lm$residuals


#Fit a linear model, then combine the results
resultx = result
result1 = NULL
bp.factors = NULL

for(i in 1:30){
  result1[[i]] = resultx[[i]]
  bp.lm = lm(formula = potential.HR.count ~ actual.HR.count, data = result1[[i]])
  result1[[i]]$exp = bp.lm$fitted.values
  result1[[i]]$res = bp.lm$residuals
  bp.factors$int[i] = bp.lm$coefficients[1]
  bp.factors$slope[i] = bp.lm$coefficients[2]
  }

judge %>% 
  group_by(park) %>%
  summarise() -> bp.factors$park



result2 = NULL
for(i in 1:30){
  x = result1[[i]]
  result2 = rbind(result2,x)
  }

park.factors = data.frame(park = bp.factors$park, slope = bp.factors$slope, int = bp.factors$int)


#Find the players with best and worst residuals 
ggplot(dat0, aes(x = actual.HR.count, y = potential.HR.count)) + 
  geom_jitter(data = filter(dat0, abs(res) < 50 )) +
  geom_smooth(method = "lm", aes(x = actual.HR.count, y = potential.HR.count)) + 
  geom_text_repel(data = filter(dat0, abs(res) >= 50),
                  aes(label = paste(player_name,park))) +
  geom_point(data = filter(dat0, res >= 50 ), color = "red") + 
  geom_point(data = filter(dat0, res <= -50 ), color = "blue") +
  labs(x = "Actual Home Run Count", y = "Potential Home Run Count", title = "Best/Worst Match") +
  annotate("text", 20, 150,
           label = "Line: y = 1.148388x + 0.396430")



# Might not need this
# #Min 30 HRs
# min30.lm = lm(formula = potential.HR.count ~ actual.HR.count, data = dat2)
# summary(min30.lm)
# par(mfrow = c(2,2))
# plot(min30.lm)
# par(mfrow = c(1,1))
# dat2$res = min30.lm$residuals
# 
# ggplot(dat2, aes(x = actual.HR.count, y = potential.HR.count)) + 
#   geom_jitter(data = filter(dat2, abs(res) < 50 )) +
#   geom_smooth(method = "lm", aes(x = actual.HR.count, y = potential.HR.count)) + 
#   geom_text_repel(data = filter(dat2, abs(res) >= 50),
#                   aes(label = paste(player_name,park))) +
#   geom_point(data = filter(dat2, res >= 50 ), color = "red") + 
#   geom_point(data = filter(dat2, res <= -50 ), color = "blue")


#What does this tell us about the algorithm?



median(judge$total.gain)























