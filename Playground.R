##### Creating Border Algorithm #####
#For a given dataset arrange by launch direction
foo <- BOS07
foo <- foo[order(foo[,"launch_direction"], decreasing = T), ]

xstart = list()
start = min(foo$launch_direction)
xend = max(foo$launch_direction)

t = 1


while(start <= xend){
  foo %>% 
    filter(launch_direction <= start + 2.5 & launch_direction >= start - 2.5) -> range
  xstart$x[t] = start
  xstart$ymax[t] = ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
  xstart$ymean[t] = ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
  xstart$ymin[t] = ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
  start = start + 1
  t = t + 1
}

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
  x = -1*xstart$x,
  y = xstart$ymin,
  line = list(shape = "loess")
)

lines2 <- list(
  x = -1*xstart$x,
  y = xstart$ymean,
  line = list(shape = "loess")
)


lines3 <- list(
  x = -1*xstart$x,
  y = xstart$ymax,
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
  title = paste("Home Runs at", foo[1,2], "with exit velocity <= ", 100, "mph") ,
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





####New Function
plot2d_hr <- function(bp, ev = 100, width = 2.5, increment = 1){
  foo <- bp 
  foo <- foo[order(foo[,"launch_direction"], decreasing = T), ] #Order by launch direction
  
  xstart = list()
  start = min(foo$launch_direction)
  xend = max(foo$launch_direction)
  t = 1
  
  foo %>%
    filter(launch_speed <= ev) -> foo #Filter by desired exit velo

  while(start <= xend){
    foo %>% 
      filter(launch_direction <= start + width & launch_direction >= start - width) -> range
    xstart$x[t] = start
    xstart$ymax[t] = ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
    xstart$ymean[t] = ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
    xstart$ymin[t] = ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
    start = start + increment
    t = t + 1
  }

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
    x = -1*xstart$x,
    y = xstart$ymin,
    line = list(shape = "spline")
  )
  
  lines2 <- list(
    x = -1*xstart$x,
    y = xstart$ymean,
    line = list(shape = "spline")
  )
  
  
  lines3 <- list(
    x = -1*xstart$x,
    y = xstart$ymax,
    line = list(shape = "spline")
  )
  
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectmode = "manual",
      aspectratio = list(
        x = 10,
        y = 1),
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
  p <- add_trace(p, x = trace1$x, y = trace1$y, type = trace1$type, marker = trace1$marker, 
                 mode = trace1$mode, hoverinfo = trace1$hoverinfo, text = trace1$text, name = 'Home Runs')
  p <- add_lines(p, x = lines1$x, y = lines1$y, line = lines1$line, name = 'Minumum Launch Angle')
  p <- add_lines(p, x = lines2$x, y = lines2$y, line = lines2$line, name = 'Mean Launch Angle')
  p <- add_lines(p, x = lines3$x, y = lines3$y, line = lines3$line, name = 'Maximum Launch Angle')
  p <- layout(p, title = layout$title, scene = layout$scene, xaxis = layout$xaxis, yaxis = layout$yaxis)
  p
}

