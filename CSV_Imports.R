####Import CSV Data from Baseball Savant and Match to Players####
fields <- read.csv("http://www-math.bgsu.edu/~albert/baseball/fields.csv")

#Ballpark Name and ID
ballparks <- read.csv("https://www.retrosheet.org/parkcode.txt")
ballparks %>% select(PARKID, NAME) -> ballparks

#Gamelog Header
glfields <- read_csv("~/Desktop/BaseballData/retrosheet_playbyplay/GameLogHeader.csv")

#Player IDs
player_ID <- read_csv("~/Desktop/BaseballData/MatchScore/Data/player_ID_table.csv")


#Game Log Data
GL2015 <- read_csv("~/Desktop/BaseballData/MatchScore/Data/GL2015.csv",
                   col_names = names(glfields))
GL2016 <- read_csv("~/Desktop/BaseballData/MatchScore/Data/GL2016.csv",
                   col_names = names(glfields))
GL2017 <- read_csv("~/Desktop/BaseballData/MatchScore/Data/GL2017.csv",
                   col_names = names(glfields))
GL2018 <- read_csv("~/Desktop/BaseballData/MatchScore/Data/GL2018.csv",
                   col_names = names(glfields))

#HR Data from Baseball Savant
HR_data_15 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/launch_angle_ranking_2015.csv")
HR_data_16 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/launch_angle_ranking_2016.csv")
HR_data_17 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/launch_angle_ranking_2017.csv")
HR_data_18 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/launch_angle_ranking_2018.csv")

#Team Bridge Table
team_bridge <- read.csv("~/Desktop/BaseballData/MatchScore/Data/mlbam_lahman_team_bridge.csv")

#Ballpark Bridge Table
bp_bridge <- read.table("~/Desktop/BaseballData/MatchScore/Data/Ballpark_bridge.txt", header = TRUE, sep = "\t")

#Potential Home Run Data
pot_HR_data_16 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/potential_hr_2016.csv")
pot_HR_data_17 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/potential_hr_2017.csv")
pot_HR_data_18 <- read.csv("~/Desktop/BaseballData/MatchScore/Data/potential_hr_2018.csv")

