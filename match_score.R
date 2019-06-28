# #### Potential HR using 2d Plots####
match_score <- function(bp, ev = 100, width = 3.5, inc = 1, rank = "top"){

#To make the function "a little bit" efficient
ev = ifelse(ev >= 122, ev <- 122, ev)

####Filter the HR data to specified EV and ballpark####    
# HR_All %>%
#   filter(PARK_ID == bp[1,1]) %>%
#   filter(launch_speed <= ev) -> foo

#Create a list for HR windows
outfield_base$max <- 0
outfield_base$ave <- 0
outfield_base$min <- 0
outfield_base$angle <- 0

# ####Zone Building####
# #Reset Parameter
# p = 1
# dir = -45
# #Start of loop
# while(dir <= 45){
#   foo %>% 
#     filter(launch_direction <= dir + width & launch_direction >= dir - width) -> range
#   outfield_base$angle[p] <- dir
#   outfield_base$max[p] <- ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
#   outfield_base$ave[p] <- ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
#   outfield_base$min[p] <- ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
#   p = p + 1
#   dir = dir + inc
# }


####Potential Homeruns####

#Reset Parameters
spd <- 90
pot_HRs <- NULL

#For each exit velocity with incremental 1 MPH, create a HR zone
while(spd <= ev){
  
  #Filter out by all potential undetermined HRs with sufficient exit velocity 
  pot_HR_All %>% 
    filter(launch_speed <= 1.025 * spd & launch_speed >= spd) -> potential_HRs
  
  #Reset Parameter
  p = 1
  dir = -45
  
  #Call Zone Building Function
  hr_base <- hr_zone_sp_ev(bp, spd, width, inc)
  
  #Start of loop
  while(dir <= 45){
    if(hr_base$max[p] - hr_base$min[p] == 0){
      p <- p + 1
      dir <- dir + inc}else{
        potential_HRs %>% 
          filter(launch_direction < dir + inc/2 & launch_direction >= dir - inc/2) -> look_range
        look_range %>%
          filter(launch_angle <= hr_base$max[p] & launch_angle >= hr_base$min[p]) -> to_add
        if(count(to_add) != 0){
          to_add$determined = 1}
        pot_HRs <- rbind(pot_HRs, to_add)
        p <- p + 1
        dir <- dir + inc
      }}
  spd = spd + 0.5
  
}
  
  #Select Distinct HR data 
  pot_HRs <- distinct(pot_HRs)
  pot_HRs %>% select('ID', 'determined') -> lj
  
  #Left Join the actual data 
  pot_HR_All %>%
    left_join(lj, by = 'ID') -> ranking
  
  #Set NA to 0
  ranking %>%
    mutate(determined = ifelse(is.na(determined),0,1)) -> ranking
  
  
####Group By Player####

  ranking$player_name = as.character(ranking$player_name)

  
ranking %>% 
  filter(launch_speed <= ev) %>%
  group_by(player_name) %>%
  summarise(ave.exit.velo = sum(launch_speed*determined)/sum(c(determined)),
            ave.launch.angle = sum(launch_angle*determined)/sum(c(determined)),
            ave.distance = sum(hit_distance_sc*determined)/sum(c(determined)),
            potential.HR.count = sum(determined),
            actual.HR.count = sum(is.hr)) %>%
  mutate(total.gain = potential.HR.count - actual.HR.count)-> ranking
ranking %>% arrange(desc(total.gain)) -> ranking
ranking %>% arrange(total.gain) -> ranking2

ifelse(rank == "top", return(ranking),return(ranking2))
}












