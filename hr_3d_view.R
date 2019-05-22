###Combine 3d Plot with stadium dimensions###
view_hr_test <- function(bp,ev = 100,width = 2.5){

#Convert input to string and load JSON file  
  team = toString(bp[1,12])
  filepath = paste("~/Desktop/BaseballData/MatchScore/Data/field/",team,".svg.json", sep = "")
  stadium <- fromJSON(file = filepath)

#Reset Parameters  
  keep = NULL
  s = NULL
  toPlot = list()

#Choose "non compass" list of datasets  

  for(s in 1:11){
    if(stadium$shape[[s]]$parentID != "Compass"){
      keep = c(keep,s)}
    if(is.null(stadium$shape[[s]]$ID) == FALSE){
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


#Filter the HR data to specified EV and ballpark    
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
    title = paste("Home Runs at", bp[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Deviation from Dead Center (in feet)"), 
    yaxis = list(title = "y - component"))

#Filter outside line data to just the field fence data
outfield_base = data.frame(tracefield1$x[[1]], tracefield1$y[[1]],tracefield1$z[[1]])
colnames(outfield_base) <- c("x", "y", "z")

outfield_base %>% 
  mutate(distance = round(sqrt(x^2 + y^2),2),
         direction = -1*round(-180 * atan(-x/y)/pi,2)
         ) %>%
  filter(distance > 250 & direction <= 45 & direction >= -45) -> outfield_base

outfield_base$max <- 0
outfield_base$ave <- 0
outfield_base$min <- 0



#Zone Building
for(r in 1:nrow(outfield_base)){
  dir <- outfield_base[r,5]
  foo %>% 
    filter(launch_direction <= dir + width & launch_direction >= dir - width) -> range
  outfield_base$max[r] <- ifelse(max(range$launch_angle) > 0,max(range$launch_angle),0)
  outfield_base$ave[r] <- ifelse(is.na(mean(range$launch_angle)) == TRUE,0,mean(range$launch_angle))
  outfield_base$min[r] <- ifelse(min(range$launch_angle) < 90,min(range$launch_angle),0)
}

linesmax <- list(
  x = outfield_base$x,
  y = outfield_base$y,
  z = outfield_base$max,
  type = "scatter3d",
  mode = "lines",
  line = list(
    color = "rgb(255,0,0)", 
    dash = "solid", 
    width = 3
  ))
  

linesave <- list(
  x = outfield_base$x,
  y = outfield_base$y,
  z = outfield_base$ave,
  type = "scatter3d",
  mode = "lines",
  line = list(
    color = "rgb(128,0,128)", 
    dash = "solid", 
    width = 3
  ))


linesmin <-  list(
  x = outfield_base$x,
  y = outfield_base$y,
  z = outfield_base$min,
  type = "scatter3d",
  mode = "lines",
  line = list(
    color = "rgb(0,0,255)", 
    dash = "solid", 
    width = 3
  ))


#plots for the ballpark  
f = plot_ly()

#Outmost Dimensions
lineconfig = list(
  #color = "rgb(100,255,0)", 
  dash = "solid", 
  width = 5
)

f = add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], z = tracefield1$z[[1]], type = tracefield1$type, mode = tracefield1$mode,
              hoverinfo = "text",
              text = ~paste("</br> x: ",tracefield1$x[[1]],
                            "</br> y: ",tracefield1$y[[1]],
                            "</br> Distance: ", round(sqrt((tracefield1$x[[1]])^2 + (tracefield1$y[[1]])^2),2),
                            "</br> Dir: ", round(-180*atan(-tracefield1$x[[1]]/tracefield1$y[[1]])/pi,2)
                            ),
              line = lineconfig
              )

#Rest of the Ballpark
f = add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], z = tracefield2$z[[1]], type = tracefield2$type, mode = tracefield2$mode)
f = add_trace(f, x = tracefield3$x[[1]], y = tracefield3$y[[1]], z = tracefield3$z[[1]], type = tracefield3$type, mode = tracefield3$mode)
f = add_trace(f, x = tracefield4$x[[1]], y = tracefield4$y[[1]], z = tracefield4$z[[1]], type = tracefield4$type, mode = tracefield4$mode)
f = add_trace(f, x = tracefield5$x[[1]], y = tracefield5$y[[1]], z = tracefield5$z[[1]], type = tracefield5$type, mode = tracefield5$mode)
f = add_trace(f, x = tracefield6$x[[1]], y = tracefield6$y[[1]], z = tracefield6$z[[1]], type = tracefield6$type, mode = tracefield6$mode)
f = add_trace(f, x = tracefield7$x[[1]], y = tracefield7$y[[1]], z = tracefield7$z[[1]], type = tracefield7$type, mode = tracefield7$mode)

#Plot the HR data, along with player name of the hitter  
f <- add_trace(f,x = c(-3.76*foo$y), y = c(3.76*foo$x), z = c(foo$launch_angle), 
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

#Plot the HR Lines
f <- add_trace(f, x = linesmax$x, y = linesmax$y, z = linesmax$z, type = linesmax$type, mode = linesmax$mode, line = linesmax$line)
f <- add_trace(f, x = linesave$x, y = linesave$y, z = linesave$z, type = linesave$type, mode = linesave$mode, line = linesave$line)
f <- add_trace(f, x = linesmin$x, y = linesmin$y, z = linesmin$z, type = linesmin$type, mode = linesmin$mode, line = linesmin$line)


#Set the layout
f <- layout(f, title = layout$title, scene = layout$scene,
            xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)


#Show the final plot  
  f
}
  
  
  
  
  
  
  
  
  
  
  
  
