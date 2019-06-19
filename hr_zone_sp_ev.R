####Create a HR zone for specific velocity####
hr_zone_sp_ev<- function(bp, ev = 100, width = 2.5, inc = 1){

####Filter the HR data to specified EV and ballpark####    
HR_All %>%
  filter(PARK_ID == bp[1,1]) %>%
  filter(launch_speed <= ev) -> foo

#Create a list for HR windows
hr_base <- NULL
hr_base$max <- 0
hr_base$ave <- 0
hr_base$min <- 0
hr_base$angle <- 0



####Zone Building####
#Reset Parameter
p = 1
dir = -45
#Start of loop
while(dir <= 45){
  foo %>% 
    filter(launch_direction <= dir + width & launch_direction >= dir - width) -> range
  hr_base$angle[p] <- dir
  hr_base$max[p] <- ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
  hr_base$ave[p] <- ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
  hr_base$min[p] <- ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
  p = p + 1
  dir = dir + inc
}
return(hr_base)
}












