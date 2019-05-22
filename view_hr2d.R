####View HR 2D with boundaries####
view_hr2d <- function(bp,ev = 100,width = 2.5){
  foo <- bp 
  foo <- foo[order(foo[,"launch_direction"], decreasing = T), ] #Order by launch direction
  
  foo %>%
    filter(launch_speed <= ev) -> foo #Filter by desired exit velo
  
  foo$min <- 0 #Add new columns
  foo$ave <- 0
  foo$max <- 0
  
  for(m in 1:nrow(foo)){
    c <- foo[m,7]
    foo %>% 
      filter(launch_direction <= c + width & launch_direction >= c - width) -> range
    foo$min[m] = min(range$launch_angle)
    foo$ave[m] = mean(range$launch_angle)
    foo$max[m] = max(range$launch_angle)
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
  
  trace2 <- list(
    x = -1*foo$launch_direction,
    y = foo$min,
    type = "scatter",
    mode = "smooth"
  )
  
  trace3 <- list(
    x = -1*foo$launch_direction,
    y = foo$ave,
    type = "scatter",
    mode = "smooth"
  )
  
  trace4 <- list(
    x = -1*foo$launch_direction,
    y = foo$max,
    type = "scatter",
    mode = "smooth"
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
  p <- add_trace(p, x = trace2$x, y = trace2$y, type = trace2$type, mode = trace2$mode)
  p <- add_trace(p, x = trace3$x, y = trace3$y, type = trace3$type, mode = trace3$mode)
  p <- add_trace(p, x = trace4$x, y = trace4$y, type = trace4$type, mode = trace4$mode)
  p <- layout(p, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis)
  p
}

