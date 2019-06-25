#### Potential HR using 2d Plots####
by_player_2d <- function(pn ,bp, ev = 100, width = 3.5, inc = 1){
  
  #To make the function "a little bit" efficient
  ev = ifelse(ev >= 122, ev <- 122, ev)
  
  ####Filter the HR data to specified EV and ballpark####    
  HR_All %>%
    filter(PARK_ID == bp[1,1]) %>%
    filter(launch_speed <= ev) -> foo
  
  #Setting the layout of all the HRs   
  layout <- list(
    aspectmode = "manual",
    aspectratio = list(
      x = 3,
      y = 3),
    hovermode = "closest", 
    scene = list(
      xaxis = list(
        showline = FALSE, 
        title = "Launch Direction", 
        type = "linear"
      ), 
      yaxis = list(
        title = "Launch Direction", 
        type = "linear"
      ) 
    ), 
    title = paste("Potential", pn, "Home Runs at", foo[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Launch Direction"), 
    yaxis = list(title = "Launch Angle"))
  
  #Create a list for HR windows
  outfield_base$max <- 0
  outfield_base$ave <- 0
  outfield_base$min <- 0
  outfield_base$angle <- 0
  
  ####Zone Building####
  #Reset Parameter
  p = 1
  dir = -45
  #Start of loop
  while(dir <= 45){
    foo %>% 
      filter(launch_direction <= dir + width & launch_direction >= dir - width) -> range
    outfield_base$angle[p] <- dir
    outfield_base$max[p] <- ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
    outfield_base$ave[p] <- ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
    outfield_base$min[p] <- ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
    p = p + 1
    dir = dir + inc
  }
  
  
  linesmax <- list(
    x = -1 * outfield_base$angle,
    y = outfield_base$max,
    type = "scatter",
    mode = "lines",
    line = list(
      color = "rgb(255,0,0)", 
      dash = "solid", 
      width = 3
    ))
  
  
  linesave <- list(
    x = -1 * outfield_base$angle,
    y = outfield_base$ave,
    type = "scatter",
    mode = "lines",
    line = list(
      color = "rgb(128,0,128)", 
      dash = "solid", 
      width = 3
    ))
  
  
  linesmin <-  list(
    x = -1 * outfield_base$angle,
    y = outfield_base$min,
    type = "scatter",
    mode = "lines",
    line = list(
      color = "rgb(0,0,255)", 
      dash = "solid", 
      width = 3
    ))
  
  ####Potential Homeruns####
  
  #Reset Parameters
  spd <- 87
  pot_HRs <- NULL
  
  #Filter by Specific Player
  pot_HR_All %>% 
    filter(player_name == pn) -> player_hard_hit
  
  #Set flag to 0
  player_hard_hit$determined = 0 
  
  #For each exit velocity with incremental 1 MPH, create a HR zone
  while(spd <= ev){
    
    #Filter out by all potential undetermined HRs with sufficient exit velocity 
    player_hard_hit %>% 
      filter(determined == 0) %>%
      filter(launch_speed <= spd & launch_speed >= spd * 0.975) -> potential_HRs
    
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
  
  
  
  ####Plotting####
  #plots for the ballpark  
  f = plot_ly()
  
  #Plot the HR data, along with batter name 
  f <- add_trace(f,x = -1 * foo$launch_direction, y = foo$launch_angle,
                 name = "All Ballpark HRs",
                 type = "scatter",
                 marker = list(size = 6),
                 mode = "markers",
                 hoverinfo = "text",
                 text = ~paste("</br>Launch Direction:",c(-1*foo$launch_direction),
                               "</br> Launch Angle: ",c(foo$launch_angle),
                               "</br> Exit Velocity: ", c(foo$launch_speed),
                               "</br> Player: ", c(paste(foo$player_name))
                 )
  )
  
  
  #Plot the potential homerun data, along with batter name
  f <- add_trace(f,x = -1 * pot_HRs$launch_direction, y = pot_HRs$launch_angle, 
                 name = pn,
                 type = "scatter",
                 marker = list(size = 6),
                 mode = "markers",
                 hoverinfo = "text",
                 text = ~paste("</br>Launch Direction:",c(-1*pot_HRs$launch_direction),
                               "</br> Launch Angle: ",c(pot_HRs$launch_angle),
                               "</br> Exit Velocity: ", c(pot_HRs$launch_speed),
                               "</br> Player: ", c(paste(pot_HRs$player_name))
                 )
  )
  
  
  #Plot the HR Lines generated by the maximum exit velo 
  f <- add_trace(f, x = linesmax$x, y = linesmax$y,  type = linesmax$type, 
                 mode = linesmax$mode, line = linesmax$line,
                 name = "Max Launch Angle",
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Max Angle: ",linesmax$y
                 )
  )
  
  f <- add_trace(f, x = linesave$x, y = linesave$y, type = linesave$type, 
                 mode = linesave$mode, line = linesave$line,
                 name = "Average Launch Angle",
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Average Angle: ",round(linesave$y,1)
                 )
  )
  
  f <- add_trace(f, x = linesmin$x, y = linesmin$y, type = linesmin$type,
                 mode = linesmin$mode, line = linesmin$line,
                 name = "Minimum Launch Angle",
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Min Angle: ",linesmin$y,
                               "</br> Window: ", linesmax$y - linesmin$y
                 )
  )
  
  
  #Set the layout
  f <- layout(f, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)
  
  
  #Show the final plot  
  f

}
 











