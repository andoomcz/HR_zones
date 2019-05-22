####Scraping Table#####
#2015 Season
url_15 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=home%5C.%5C.run%7C&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2015%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
rank_15 <- url_15 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
rank_15 <- as.list(rank_15[[1]])
LD15 <- rank_15$Results
Player15 <- rank_15$Player
LD15 <- data.frame(LD15)
to_delete15 <- seq(0, nrow(LD15), 2)
LD15 <- LD15[-to_delete15,]
HR15 = NULL
for(i in 1:nrow(data.frame(LD15))){
  HR15[i] <- substr(as.vector(LD15[i]),1,nchar(as.vector(LD15[i]))-223)
}
HR_data_15$launch_direction <- -1*as.numeric(HR15)


#2016 Season
url_16 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=home%5C.%5C.run%7C&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2016%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
rank_16 <- url_16 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
rank_16 <- as.list(rank_16[[1]])
LD16 <- rank_16$Results
Player16 <- rank_16$Player
LD16 <- data.frame(LD16)
to_delete16 <- seq(0, nrow(LD16), 2)
LD16 <- LD16[-to_delete16,]
HR16 = NULL
for(i in 1:nrow(data.frame(LD16))){
  HR16[i] <- substr(as.vector(LD16[i]),1,nchar(as.vector(LD16[i]))-223)
}
HR_data_16$launch_direction <- -1*as.numeric(HR16)


#2017 Season
url_17 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=home%5C.%5C.run%7C&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2017%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
rank_17 <- url_17 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
rank_17 <- as.list(rank_17[[1]])
LD17 <- rank_17$Results
Player17 <- rank_17$Player
LD17 <- data.frame(LD17)
to_delete17 <- seq(0, nrow(LD17), 2)
LD17 <- LD17[-to_delete17,]
HR17 = NULL
for(i in 1:nrow(data.frame(LD17))){
  HR17[i] <- substr(as.vector(LD17[i]),1,nchar(as.vector(LD17[i]))-223)
}
HR_data_17$launch_direction <- -1*as.numeric(HR17)



#2018 Season
url_18 = read_html("https://baseballsavant.mlb.com/statcast_search?hfPT=&hfAB=home%5C.%5C.run%7C&hfBBT=&hfPR=&hfZ=&stadium=&hfBBL=&hfNewZones=&hfGT=R%7C&hfC=&hfSea=2018%7C&hfSit=&player_type=batter&hfOuts=&opponent=&pitcher_throws=&batter_stands=&hfSA=&game_date_gt=&game_date_lt=&hfInfield=&team=&position=&hfOutfield=&hfRO=&home_road=&hfFlag=&hfPull=&metric_1=&hfInn=&min_pitches=0&min_results=0&group_by=name-event&sort_col=pitches&player_event_sort=h_launch_direction&sort_order=desc&min_pas=0#results")
rank_18 <- url_18 %>% html_nodes( xpath = '//*[@id="search_results"]') %>% html_table()
rank_18 <- as.list(rank_18[[1]])
LD18 <- rank_18$Results
Player18 <- rank_18$Player
LD18 <- data.frame(LD18)
to_delete18 <- seq(0, nrow(LD18), 2)
LD18 <- LD18[-to_delete18,]
HR18 = NULL
for(i in 1:nrow(data.frame(LD18))){
  HR18[i] <- substr(as.vector(LD18[i]),1,nchar(as.vector(LD18[i]))-223)
}
HR_data_18$launch_direction <- -1*as.numeric(HR18)


#Creating a GameID
#2015
HR_data_15 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> HR_data_15
HR_data_15 %>%
  mutate(date = format(lubridate::mdy(HR_data_15$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> HR_data_15

#2016
HR_data_16 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> HR_data_16
HR_data_16 %>%
  mutate(date = format(lubridate::mdy(HR_data_16$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> HR_data_16

#2017
HR_data_17 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> HR_data_17
HR_data_17 %>%
  mutate(date = format(lubridate::mdy(HR_data_17$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> HR_data_17

#2018
HR_data_18 %>%
  inner_join(team_bridge, by = c("home_team" = "MLB_AM")) -> HR_data_18
HR_data_18 %>%
  mutate(date = format(lubridate::mdy(HR_data_18$game_date),"%Y%m%d"),
         gamecode = paste(teamIDretro,date, sep = "")) -> HR_data_18



#Creating the same GameID for retrosheet
GL2015 %>%
  mutate(gamecode = substr(GAME_ID,1,11)) -> GL2015
GL2015 %>%
  select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
         FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog15

GL2016 %>%
  mutate(gamecode = substr(GAME_ID,1,11)) -> GL2016
GL2016 %>%
  select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
         FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog16

GL2017 %>%
  mutate(gamecode = substr(GAME_ID,1,11)) -> GL2017
GL2017 %>%
  select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
         FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog17 

GL2018 %>%
  mutate(gamecode = substr(GAME_ID,1,11)) -> GL2018
GL2018 %>%
  select(gamecode, AWAY_TEAM_ID,HOME_TEAM_ID,PARK_ID,TEMP_PARK_CT,WIND_DIRECTION_PARK_CD,WIND_SPEED_PARK_CT,
         FIELD_PARK_CD, PRECIP_PARK_CD) -> GameLog18


#Joining Game Log and HR data
HR_data_15 %>%
  inner_join(GameLog15, by = "gamecode") -> HR_data_15
HR_data_15 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> HR_data_15

HR_data_16 %>%
  inner_join(GameLog16, by = "gamecode") -> HR_data_16
HR_data_16 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> HR_data_16

HR_data_17 %>%
  inner_join(GameLog17, by = "gamecode") -> HR_data_17
HR_data_17 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> HR_data_17

HR_data_18 %>%
  inner_join(GameLog18, by = "gamecode") -> HR_data_18
HR_data_18 %>%
  inner_join(ballparks, by = c("PARK_ID"="PARKID")) -> HR_data_18



#Filtering out inside the park HRs
dplyr::filter(HR_data_15, !grepl('inside-the-park', des)) -> HR_data_15
dplyr::filter(HR_data_16, !grepl('inside-the-park', des)) -> HR_data_16
dplyr::filter(HR_data_17, !grepl('inside-the-park', des)) -> HR_data_17
dplyr::filter(HR_data_18, !grepl('inside-the-park', des)) -> HR_data_18


#Selecting the necessary Columns
HR_data_15 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc") -> Homeruns_15 

HR_data_16 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc") -> Homeruns_16 

HR_data_17 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  select("PARK_ID", "NAME", "player_name", "batter", "Year", "launch_speed", "launch_angle", "launch_direction", "hit_distance_sc") -> Homeruns_17 

HR_data_18 %>% 
  mutate(Year = substr(gamecode,4,7)) %>%
  select("PARK_ID", "NAME", "player_name", "batter", "Year","launch_speed", "launch_angle", "launch_direction", "hit_distance_sc") -> Homeruns_18 


#Converting Angles into Radians
Homeruns_15 %>%
  mutate(x = launch_speed * cos((pi*launch_direction)/180),
         y = launch_speed * sin((pi*launch_direction)/180),
         z = launch_speed * tan((pi*launch_angle)/180)) -> HRs15

Homeruns_16 %>%
  mutate(x = launch_speed * cos((pi*launch_direction)/180),
         y = launch_speed * sin((pi*launch_direction)/180),
         z = launch_speed * tan((pi*launch_angle)/180)) -> HRs16

Homeruns_17 %>%
  mutate(x = launch_speed * cos((pi*launch_direction)/180),
         y = launch_speed * sin((pi*launch_direction)/180),
         z = launch_speed * tan((pi*launch_angle)/180)) -> HRs17

Homeruns_18 %>%
  mutate(x = launch_speed * cos((pi*launch_direction)/180),
         y = launch_speed * sin((pi*launch_direction)/180),
         z = launch_speed * tan((pi*launch_angle)/180)) -> HRs18

#Combining the two season's worth of data
#Removing HRs15 until the data is cleaned up
HR_All <- rbind(HRs16, HRs17, HRs18)


#Create a list of ballparks
HR_All %>%
  select(PARK_ID) %>%
  distinct() -> stadiums
stadiums %>%
  inner_join(bp_bridge, by = c("PARK_ID" = "Stadium")) -> stadiums

