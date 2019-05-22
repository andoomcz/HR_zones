###Combine 3d Plot with stadium dimensions###
view_hr <- function(bp,ev = 100){
  team = toString(bp[1,12])
  filepath = paste("~/Desktop/BaseballData/MatchScore/Data/field/",team,".svg.json", sep = "")
  stadium <- fromJSON(file = filepath)
  keep = NULL
  s = NULL
  toPlot = list()
  for(s in 1:11){
    if(stadium$shape[[s]]$parentID != "Compass"){
      keep = c(keep,s)}
    if(is.null(stadium$shape[[s]]$ID) == FALSE){
      infield = s
    }
  }
  
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
  
  for(m in 1:zzz){
    tracename = paste("tracefield",m,sep = "")
    assign(tracename,
           list(x = toPlot$x[keep[m]],
                y = toPlot$y[keep[m]],
                z = toPlot$z[keep[m]],
                type = "scatter3d",
                mode = "lines"))
  }
  
  tracefield7 = list(x = toPlot$x[12],
                     y = toPlot$y[12],
                     z = toPlot$z[12],
                     type = "scatter3d",
                     mode = "lines")
  
  f = plot_ly()
  f = add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], z = tracefield1$z[[1]], type = tracefield1$type, mode = tracefield1$mode,
                hoverinfo = "text",
                text = ~paste("</br> x: ",tracefield1$x[[1]],
                              "</br> y: ",tracefield1$y[[1]],
                              "</br> Distance: ", round(sqrt((tracefield1$x[[1]])^2 + (tracefield1$y[[1]])^2),2)))
  f = add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], z = tracefield2$z[[1]], type = tracefield2$type, mode = tracefield2$mode)
  f = add_trace(f, x = tracefield3$x[[1]], y = tracefield3$y[[1]], z = tracefield3$z[[1]], type = tracefield3$type, mode = tracefield3$mode)
  f = add_trace(f, x = tracefield4$x[[1]], y = tracefield4$y[[1]], z = tracefield4$z[[1]], type = tracefield4$type, mode = tracefield4$mode)
  f = add_trace(f, x = tracefield5$x[[1]], y = tracefield5$y[[1]], z = tracefield5$z[[1]], type = tracefield5$type, mode = tracefield5$mode)
  f = add_trace(f, x = tracefield6$x[[1]], y = tracefield6$y[[1]], z = tracefield6$z[[1]], type = tracefield6$type, mode = tracefield6$mode)
  f = add_trace(f, x = tracefield7$x[[1]], y = tracefield7$y[[1]], z = tracefield7$z[[1]], type = tracefield7$type, mode = tracefield7$mode)
  
  HR_All %>%
    filter(PARK_ID == bp[1,1]) %>%
    filter(launch_speed <= ev) -> foo
  f <- add_trace(f,x = c(-3.76*foo$y), y = c(3.76*foo$x), z = c(foo$launch_angle), 
               type = "scatter3d",
               marker = list(size = 4),
               mode = "markers",
               hoverinfo = "text",
               text = ~paste("</br>Launch Direction:",c(foo$launch_direction),
                             "</br> Launch Angle: ",c(foo$launch_angle),
                             "</br> Exit Velocity: ", c(foo$launch_speed),
                             "</br> Player: ", c(paste(foo$player_name)))
  )
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
    title = paste("Home Runs at", bp[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Deviation from Dead Center (in feet)"), 
    yaxis = list(title = "y - component"))
  f <- layout(f, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)
  f
}
