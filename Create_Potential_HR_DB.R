####Scraping Table#####

#2016 Season
pot_url_16 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2016%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=h_launch_speed&metric_1_gt=85&metric_1_lt=&metric_2=h_launch_angle&metric_2_gt=10&metric_2_lt=55&metric_3=h_launch_direction&metric_3_gt=-45&metric_3_lt=45&metric_4=api_h_distance_projected&metric_4_gt=285&metric_4_lt=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
pot_rank_16 <- pot_url_16 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
pot_rank_16 <- as.list(pot_rank_16[[1]])
pot_LD16 <- pot_rank_16$Results
pot_Player16 <- pot_rank_16$Player
pot_LD16 <- data.frame(pot_LD16)
pot_to_delete16 <- seq(0, nrow(pot_LD16), 2)
pot_LD16 <- pot_LD16[-pot_to_delete16,]
pot_HR16 = NULL
for(i in 1:nrow(data.frame(pot_LD16))){
  pot_HR16[i] <- substr(as.vector(pot_LD16[i]),1,nchar(as.vector(pot_LD16[i]))-223)
}
pot_HR_data_16$launch_direction <- -1*as.numeric(pot_HR16)


#2017 Season
pot_url_17 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2017%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=h_launch_speed&metric_1_gt=85&metric_1_lt=&metric_2=h_launch_angle&metric_2_gt=10&metric_2_lt=55&metric_3=h_launch_direction&metric_3_gt=-45&metric_3_lt=45&metric_4=api_h_distance_projected&metric_4_gt=285&metric_4_lt=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
pot_rank_17 <- pot_url_17 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
pot_rank_17 <- as.list(pot_rank_17[[1]])
pot_LD17 <- pot_rank_17$Results
pot_Player17 <- pot_rank_17$Player
pot_LD17 <- data.frame(pot_LD17)
pot_to_delete17 <- seq(0, nrow(pot_LD17), 2)
pot_LD17 <- pot_LD17[-pot_to_delete17,]
pot_HR17 = NULL
for(i in 1:nrow(data.frame(pot_LD17))){
  pot_HR17[i] <- substr(as.vector(pot_LD17[i]),1,nchar(as.vector(pot_LD17[i]))-223)
}
pot_HR_data_17$launch_direction <- -1*as.numeric(pot_HR17)



#2018 Season
pot_url_18 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2018%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=h_launch_speed&metric_1_gt=85&metric_1_lt=&metric_2=h_launch_angle&metric_2_gt=10&metric_2_lt=55&metric_3=h_launch_direction&metric_3_gt=-45&metric_3_lt=45&metric_4=api_h_distance_projected&metric_4_gt=285&metric_4_lt=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
pot_rank_18 <- pot_url_18 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
pot_rank_18 <- as.list(pot_rank_18[[1]])
pot_LD18 <- pot_rank_18$Results
pot_Player18 <- pot_rank_18$Player
pot_LD18 <- data.frame(pot_LD18)
pot_to_delete18 <- seq(0, nrow(pot_LD18), 2)
pot_LD18 <- pot_LD18[-pot_to_delete18,]
pot_HR18 = NULL
for(i in 1:nrow(data.frame(pot_LD18))){
  pot_HR18[i] <- substr(as.vector(pot_LD18[i]),1,nchar(as.vector(pot_LD18[i]))-223)
}
pot_HR_data_18$launch_direction <- -1*as.numeric(pot_HR18)



#Creating a GameID
# #2015
# HR_data_15 %>%
#   inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> HR_data_15
# HR_data_15 %>%
#   mutate(date = format(lubridate::mdy(HR_data_15$game_date),"%Y%m%d"),
#          gamecode = paste(teamIDretro,date, sep = "")) -> HR_data_15

#2016
pot_HR_data_16 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> pot_HR_data_16
pot_HR_data_16 %>%
  mutate(date = format(lubridate::mdy(pot_HR_data_16$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> pot_HR_data_16

#2017
pot_HR_data_17 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> pot_HR_data_17
pot_HR_data_17 %>%
  mutate(date = format(lubridate::mdy(pot_HR_data_17$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> pot_HR_data_17

#2018
pot_HR_data_18 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> pot_HR_data_18
pot_HR_data_18 %>%
  mutate(date = format(lubridate::mdy(pot_HR_data_18$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> pot_HR_data_18



#Creating the same GameID for retrosheet
# GL2015 %>%
#   mutate(gamecode = substr(GAME_ID,1,11)) -> GL2015
# GL2015 %>%
#   select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
#          FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog15
# 
# GL2016 %>%
#   mutate(gamecode = substr(GAME_ID,1,11)) -> GL2016
# GL2016 %>%
#   select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
#          FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog16
# 
# GL2017 %>%
#   mutate(gamecode = substr(GAME_ID,1,11)) -> GL2017
# GL2017 %>%
#   select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
#          FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog17 
# 
# GL2018 %>%
#   mutate(gamecode = substr(GAME_ID,1,11)) -> GL2018
# GL2018 %>%
#   select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
#          FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog18
# 

#Joining Game Log and HR data
# HR_data_15 %>%
#   inner_join(GameLog15, by = "gamecode") -> HR_data_15
# HR_data_15 %>%
#   inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> HR_data_15

pot_HR_data_16 %>%
  inner_join(GameLog16, by = "gamecode") -> pot_HR_data_16
pot_HR_data_16 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> pot_HR_data_16

pot_HR_data_17 %>%
  inner_join(GameLog17, by = "gamecode") -> pot_HR_data_17
pot_HR_data_17 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> pot_HR_data_17

pot_HR_data_18 %>%
  inner_join(GameLog18, by = "gamecode") -> pot_HR_data_18
pot_HR_data_18 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> pot_HR_data_18



# #Filtering out inside the park HRs
# dplyr::filter(HR_data_15, !grepl('inside-the-park', des)) -> HR_data_15
# dplyr::filter(HR_data_16, !grepl('inside-the-park', des)) -> HR_data_16
# dplyr::filter(HR_data_17, !grepl('inside-the-park', des)) -> HR_data_17
# dplyr::filter(HR_data_18, !grepl('inside-the-park', des)) -> HR_data_18


#Selecting the necessary Columns
# pot_HR_data_15 %>% 
#   mutate(Year = substr(gamecode,4,7)) %>%
#   select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc") -> Homeruns_15 

pot_HR_data_16 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  dplyr::select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc", "events") -> pot_Homeruns_16 

pot_HR_data_17 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  dplyr::select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc", "events") -> pot_Homeruns_17 

pot_HR_data_18 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  dplyr::select("PARK_ID", "NAME", "player_name", "batter", "Year","launch_speed", "launch_angle", "launch_direction", "hit_distance_sc", "events") -> pot_Homeruns_18 

#Combining the two season's worth of data
#Removing HRs15 until the data is cleaned up
pot_All_Homeruns <- rbind(pot_Homeruns_16, pot_Homeruns_17, pot_Homeruns_18)

#Remove Duplicates

pot_All_Homeruns <- distinct(pot_All_Homeruns)


#Converting Angles into Radians
pot_All_Homeruns %>%
  mutate(hit_distance_sc = as.numeric(as.character(hit_distance_sc)),
         x = launch_speed * cos((pi*launch_direction)/180),
         y = launch_speed * sin((pi*launch_direction)/180),
         z = launch_speed * tan((pi*launch_angle)/180)) -> pot_HR_All


#Create Flag for Actual HRs
pot_HR_All %>% mutate(is.hr = ifelse(events == 'home_run', 1, 0)) -> pot_HR_All

#Create a unique ID for each row
pot_HR_All <- tibble::rowid_to_column(pot_HR_All, "ID")

# #Create a list of ballparks
# HR_All %>%
#   select(PARK_ID) %>%
#   distinct() -> stadiums
# stadiums %>%
#   inner_join(bp_bridge, by = c("PARK_ID" = "Stadium")) -> stadiums
# 

#Converting Angles into Radians using Distance
# pot_All_Homeruns %>%
#   mutate(hit_distance_sc = as.numeric(as.character(hit_distance_sc)),
#          x = hit_distance_sc*cos((pi*launch_direction)/180),
#          y = hit_distance_sc*sin((pi*launch_direction)/180),
#          z = hit_distance_sc*tan((pi*launch_angle)/180)) %>%
#   filter(hit_distance_sc != 0) -> pot_HR_All_Dist


