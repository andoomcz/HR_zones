
###Combine 3d Plot with stadium dimensions###
by_player_3d <- function(pn, bp, ev = 100, width = 3.5, inc = 1){
  
  #To make the function "a little bit" efficient
  ev = ifelse(ev >= 122, ev <- 122, ev)
  
  #Convert input to string and load JSON file  
  team = toString(bp[1,12])
  filepath = paste("~/Desktop/BaseballData/MatchScore/Data/field/",team,".svg.json", sep = "")
  stadium <- fromJSON(file = filepath)
  
  #Reset Parameters  
  keep = NULL
  s = NULL
  toPlot = list()
  infield = NULL
  
  #Choose "non compass" list of datasets  
  for(s in 1:11){
    if(stadium$shape[[s]]$parentID != "Compass"){
      keep = c(keep,s)}
    if(length(stadium$shape[[s]]$p) == 2){
      infield = s
    }
  }
  
  #Scale and rotate the x,y plots to match actual dimention and the launch direction/angle plots  
  zzz = length(keep)
  for(t in keep){
    if(infield == t){ 
      one = stadium$shape[[t]]$p[[1]]
      two = stadium$shape[[t]]$p[[2]]
      
      toPlot$x[t] = list(2.308*(one[seq(1, length(one), 2)]-125.16))
      toPlot$y[t] = list(2.308*(-1*one[seq(0, length(one), 2)]+203.41))
      toPlot$z[t] = list(rep(0,length(one)/2))
      
      toPlot$x[12] = list(2.308*((two[seq(1, length(two), 2)])-125.16))
      toPlot$y[12] = list(2.308*(-1*two[seq(0, length(two), 2)]+203.41))
      toPlot$z[12] = list(rep(0,length(two)/2))
    } else{
      three = stadium$shape[[t]]$p[[1]]
      toPlot$x[t] = list(2.308*((three[seq(1, length(three), 2)])-125.16))
      toPlot$y[t] = list(2.308*(-1*three[seq(0, length(three), 2)]+203.41))
      toPlot$z[t] = list(rep(0,length(three)/2))
    }
  }
  
  #assign 'tracefield#' name for the list
  for(m in 1:zzz){
    tracename = paste("tracefield",m,sep = "")
    assign(tracename,
           list(x = toPlot$x[keep[m]],
                y = toPlot$y[keep[m]],
                z = toPlot$z[keep[m]],
                type = "scatter3d",
                mode = "lines"))
  }
  
  #Infield lines are split into two, so assign the second set the tracefield list    
  tracefield7 = list(x = toPlot$x[12],
                     y = toPlot$y[12],
                     z = toPlot$z[12],
                     type = "scatter3d",
                     mode = "lines")
  
  
  ####Filter the HR data to specified EV and ballpark####    
  HR_All %>%
    filter(PARK_ID == bp[1,1]) %>%
    filter(launch_speed <= ev) -> foo
  
  #Setting the layout of all the HRs   
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectmode = "manual",
      aspectratio = list(
        x = 3,
        y = 3,
        z = 0.75),
      xaxis = list(
        showline = FALSE, 
        title = "Deviation from Dead Center (in feet)", 
        type = "linear"
      ), 
      yaxis = list(
        title = "y - component", 
        type = "linear"
      ), 
      zaxis = list(
        title = "Launch Angle", 
        type = "linear"
      )
    ), 
    title = paste("Potential", pn, "Home Runs at", bp[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Deviation from Dead Center (in feet)"), 
    yaxis = list(title = "y - component"))
  
  #Filter outside line data to just the field fence data
  # felse(team == "SD"|team == "PHI",
  #        outfield_base <- data.frame(tracefield2$x[[1]], tracefield2$y[[1]],tracefield2$z[[1]]),
  #        outfield_base <- data.frame(tracefield1$x[[1]], tracefield1$y[[1]],tracefield1$z[[1]]))
  # colnames(outfield_base) <- c("x", "y", "z")
  # 
  # outfield_base %>%
  #   mutate(distance = round(sqrt(x^2 + y^2),2),
  #          direction = -1*round(-180 * atan(-x/y)/pi,2)
  #   ) %>%
  #   filter(distance > 250 & direction <= 47 & direction >= -47) %>%
  #   arrange(direction) -> outfield_base
  
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
    x = -3.75 * ev * cos(-1*pi*(outfield_base$angle - 90)/180),
    y = 3.75 * ev * sin(-1*pi*(outfield_base$angle - 90)/180),
    z = outfield_base$max,
    type = "scatter3d",
    mode = "lines",
    line = list(
      color = "rgb(255,0,0)", 
      dash = "solid", 
      width = 3
    ))
  
  
  linesave <- list(
    x = -3.75 * ev * cos(-1*pi*(outfield_base$angle - 90)/180),
    y = 3.75 * ev * sin(-1*pi*(outfield_base$angle - 90)/180),
    z = outfield_base$ave,
    type = "scatter3d",
    mode = "lines",
    line = list(
      color = "rgb(128,0,128)", 
      dash = "solid", 
      width = 3
    ))
  
  
  linesmin <-  list(
    x = -3.75 * ev * cos(-1*pi*(outfield_base$angle - 90)/180),
    y = 3.75 * ev * sin(-1*pi*(outfield_base$angle - 90)/180),
    z = outfield_base$min,
    type = "scatter3d",
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
  
  #Outmost Dimensions
  lineconfig = list(
    #color = "rgb(100,255,0)", 
    dash = "solid", 
    width = 5
  )
  
  ifelse(team == "SD"|team == "PHI",
         f <- add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], z = tracefield2$z[[1]], type = tracefield2$type, mode = tracefield2$mode,
                        hoverinfo = "text",
                        text = ~paste("</br> x: ",tracefield2$x[[1]],
                                      "</br> y: ",tracefield2$y[[1]],
                                      "</br> Distance: ", round(sqrt((tracefield2$x[[1]])^2 + (tracefield2$y[[1]])^2),2),
                                      "</br> Dir: ", round(-180*atan(-tracefield2$x[[1]]/tracefield2$y[[1]])/pi,2)
                        ),
                        line = lineconfig
         ),
         f <- add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], z = tracefield1$z[[1]], type = tracefield1$type, mode = tracefield1$mode,
                        hoverinfo = "text",
                        text = ~paste("</br> x: ",tracefield1$x[[1]],
                                      "</br> y: ",tracefield1$y[[1]],
                                      "</br> Distance: ", round(sqrt((tracefield1$x[[1]])^2 + (tracefield1$y[[1]])^2),2),
                                      "</br> Dir: ", round(-180*atan(-tracefield1$x[[1]]/tracefield1$y[[1]])/pi,2)
                        ),
                        line = lineconfig
         ))
  
  #Rest of the Ballpark
  ifelse(team == "SD"|team == "PHI",
         f <- add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], z = tracefield1$z[[1]], type = tracefield1$type, mode = tracefield1$mode),
         f <- add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], z = tracefield2$z[[1]], type = tracefield2$type, mode = tracefield2$mode))
  f = add_trace(f, x = tracefield3$x[[1]], y = tracefield3$y[[1]], z = tracefield3$z[[1]], type = tracefield3$type, mode = tracefield3$mode)
  f = add_trace(f, x = tracefield4$x[[1]], y = tracefield4$y[[1]], z = tracefield4$z[[1]], type = tracefield4$type, mode = tracefield4$mode)
  f = add_trace(f, x = tracefield5$x[[1]], y = tracefield5$y[[1]], z = tracefield5$z[[1]], type = tracefield5$type, mode = tracefield5$mode)
  f = add_trace(f, x = tracefield6$x[[1]], y = tracefield6$y[[1]], z = tracefield6$z[[1]], type = tracefield6$type, mode = tracefield6$mode)
  f = add_trace(f, x = tracefield7$x[[1]], y = tracefield7$y[[1]], z = tracefield7$z[[1]], type = tracefield7$type, mode = tracefield7$mode)
  
  
  
  #Plot the HR data, along with batter name 
  f <- add_trace(f,x = c(-3.76*foo$y), y = c(3.76*foo$x), z = c(foo$launch_angle), 
                 name = "All Ballpark HRs",
                 type = "scatter3d",
                 marker = list(size = 4),
                 mode = "markers",
                 hoverinfo = "text",
                 text = ~paste("</br>Launch Direction:",c(-1*foo$launch_direction),
                               "</br> Launch Angle: ",c(foo$launch_angle),
                               "</br> Exit Velocity: ", c(foo$launch_speed),
                               "</br> Player: ", c(paste(foo$player_name))
                 )
  )
  
  
  #Plot the potential homerun data, along with batter name
  f <- add_trace(f,x = c(-3.76*pot_HRs$y), y = c(3.76*pot_HRs$x), z = c(pot_HRs$launch_angle), 
                 name = pn,
                 type = "scatter3d",
                 marker = list(size = 4),
                 mode = "markers",
                 hoverinfo = "text",
                 text = ~paste("</br>Launch Direction:",c(-1*pot_HRs$launch_direction),
                               "</br> Launch Angle: ",c(pot_HRs$launch_angle),
                               "</br> Exit Velocity: ", c(pot_HRs$launch_speed),
                               "</br> Player: ", c(paste(pot_HRs$player_name))
                 )
  )
  
  
  #Plot the HR Lines generated by the maximum exit velo 
  f <- add_trace(f, x = linesmax$x, y = linesmax$y, z = linesmax$z, type = linesmax$type, 
                 mode = linesmax$mode, line = linesmax$line,
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Max Angle: ",linesmax$z
                 )
  )
  
  f <- add_trace(f, x = linesave$x, y = linesave$y, z = linesave$z, type = linesave$type, 
                 mode = linesave$mode, line = linesave$line,
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Average Angle: ",round(linesave$z,1)
                 )
  )
  
  f <- add_trace(f, x = linesmin$x, y = linesmin$y, z = linesmin$z, type = linesmin$type,
                 mode = linesmin$mode, line = linesmin$line,
                 hoverinfo = "text",
                 text = ~paste("</br> Direction: ", -1 * outfield_base$angle,
                               "</br> Min Angle: ",linesmin$z,
                               "</br> Window: ", linesmax$z - linesmin$z
                 )
  )
  
  
  #Set the layout
  f <- layout(f, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)
  
  
  #Show the final plot  
  f
}












