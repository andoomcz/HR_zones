#Plots homeruns at a specific ballpark and loess lines to create intial HR zone
hr_2dzone_init <- function(bp,ev = 100,width = 2.5){
  foo <- bp 
  foo <- foo[order(foo[,"launch_direction"], decreasing = T), ] #Order by launch direction
  
  foo %>%
    filter(launch_speed <= ev) -> foo #Filter by desired exit velo
  
  foo$min <- 0 #Add new columns
  foo$ave <- 0
  foo$max <- 0
  
  for(m in 1:nrow(foo)){
    c <- foo[m,8]
    foo %>% 
      filter(launch_direction <= c + width & launch_direction >= c - width) -> range
    foo$min[m] = ifelse(min(range$launch_angle) > 0, min(range$launch_angle), 0)
    foo$ave[m] = ifelse(mean(range$launch_angle) > 0, mean(range$launch_angle),0)
    foo$max[m] = ifelse(max(range$launch_angle) < 90, max(range$launch_angle), 0)
  } #Assign local min/max/ave 
  
  trace1 <- list(
    x = -1*foo$launch_direction, 
    y = foo$launch_angle, 
    type = "scatter",
    marker = list(size = 10),
    mode = "markers",
    hoverinfo = "text",
    text = ~paste("</br>Launch Direction:",foo$launch_direction,
                  "</br> Launch Angle: ",foo$launch_angle,
                  "</br> Exit Velocity: ",foo$launch_speed,
                  "</br> Player: ", paste(foo$player_name))
  )
  
  lines1 <- list(
    x = -1*foo$launch_direction,
    y = foo$min,
    line = list(shape = "loess")
  )
  
  lines2 <- list(
    x = -1*foo$launch_direction,
    y = foo$ave,
    line = list(shape = "loess")
  )
  
  lines3 <- list(
    x = -1*foo$launch_direction,
    y = foo$max,
    line = list(shape = "loess")
  )
  
  layout <- list(
    autosize = TRUE, 
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
    title = paste("Home Runs at", foo[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Launch Direction"), 
    yaxis = list(title = "Launch Angle"))
  
  p <- plot_ly()
  p <- add_trace(p, x = trace1$x, y = trace1$y, type = trace1$type, marker = trace1$marker, mode = trace1$mode, hoverinfo = trace1$hoverinfo, text = trace1$text)
  p <- add_lines(p, x = lines1$x, y = lines1$y, line = lines1$line)
  p <- add_lines(p, x = lines2$x, y = lines2$y, line = lines2$line)
  p <- add_lines(p, x = lines3$x, y = lines3$y, line = lines3$line)
  p <- layout(p, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis)
  p
}
