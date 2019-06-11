# Give the input file name to the function.

dim_ballpark <- function(bp){
  team = toString(bp[1,12])
  filepath = paste("~/Desktop/BaseballData/MatchScore/Data/field/",team,".svg.json", sep = "")
  stadium <- fromJSON(file = filepath)
  keep = NULL
  s = NULL
  toPlot = list()
  
  for(s in 1:11){
    if(stadium$shape[[s]]$parentID != "Compass"){
      keep = c(keep,s)}
    if(length(stadium$shape[[s]]$p) == 2){
      infield = s
    }
  }
  
  zzz = length(keep)
  for(t in keep){
    if(infield == t){ 
      one = stadium$shape[[t]]$p[[1]]
      two = stadium$shape[[t]]$p[[2]]
      
      toPlot$x[t] = list(2.269632*(one[seq(1, length(one), 2)]-125.16))
      toPlot$y[t] = list(2.269632*(-1*one[seq(0, length(one), 2)]+203.41))
    
      toPlot$x[12] = list(2.269632*((two[seq(1, length(two), 2)])-125.16))
      toPlot$y[12] = list(2.269632*(-1*two[seq(0, length(two), 2)]+203.41))
      } else{
        three = stadium$shape[[t]]$p[[1]]
        toPlot$x[t] = list(2.269632*((three[seq(1, length(three), 2)])-125.16))
        toPlot$y[t] = list(2.269632*(-1*three[seq(0, length(three), 2)]+203.41))
      }
    }
  
  for(m in 1:zzz){
    tracename = paste("tracefield",m,sep = "")
    assign(tracename,
           list(x = toPlot$x[keep[m]],
                y = toPlot$y[keep[m]],
                type = "scatter",
                mode = "lines"))
    }
  
  tracefield7 = list(x = toPlot$x[12],
                     y = toPlot$y[12],
                     type = "scatter",
                     mode = "lines")
  
  f = plot_ly(width = 1000, height = 1000)
  
###Create Lines for the Outermost Lines
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      aspectmode = "manual",
      aspectratio = list(
        x = 1,
        y = 1),
      xaxis = list(
        showline = FALSE, 
        title = "", 
        type = "linear"
      ), 
      yaxis = list(
        title = "", 
        type = "linear"
      ) 
    ), 
    title = paste("Dimension of", bp[1,2]) ,
    xaxis = list(title = ""), 
    yaxis = list(title = ""),
    width = 1200,
    height = 1200)
  
  ifelse(team == "SD"|team == "PHI",
         f <- add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], type = tracefield2$type, mode = tracefield2$mode,
                        hoverinfo = "text",
                        text = ~paste("</br> x: ",tracefield2$x[[1]],
                                      "</br> y: ",tracefield2$y[[1]],
                                      "</br> Distance: ", round(sqrt((tracefield2$x[[1]])^2 + (tracefield2$y[[1]])^2),2),
                                      "</br> Dir: ", round(-180*atan(-tracefield2$x[[1]]/tracefield2$y[[1]])/pi,2)
                        ),
                        line = lineconfig
         ),
         f <- add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], type = tracefield1$type, mode = tracefield1$mode,
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
         f <- add_trace(f, x = tracefield1$x[[1]], y = tracefield1$y[[1]], type = tracefield1$type, mode = tracefield1$mode),
         f <- add_trace(f, x = tracefield2$x[[1]], y = tracefield2$y[[1]], type = tracefield2$type, mode = tracefield2$mode))
  
  f = add_trace(f, x = tracefield3$x[[1]], y = tracefield3$y[[1]], type = tracefield3$type, mode = tracefield3$mode)
  f = add_trace(f, x = tracefield4$x[[1]], y = tracefield4$y[[1]], type = tracefield4$type, mode = tracefield4$mode)
  f = add_trace(f, x = tracefield5$x[[1]], y = tracefield5$y[[1]], type = tracefield5$type, mode = tracefield5$mode)
  f = add_trace(f, x = tracefield6$x[[1]], y = tracefield6$y[[1]], type = tracefield6$type, mode = tracefield6$mode)
  f = add_trace(f, x = tracefield7$x[[1]], y = tracefield7$y[[1]], type = tracefield7$type, mode = tracefield7$mode)
  
  #Set the layout
  f <- layout(f, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis)
  
  f
  }
