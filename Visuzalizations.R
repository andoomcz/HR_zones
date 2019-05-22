#Exploring with 3D Data
#Plotting every home run in 2017, 2018 using plot_ly
p <- plot_ly(x = c(0,HR_All$x), y = c(0,HR_All$y), z = c(0,HR_All$launch_angle),
        type = "scatter3d",
        marker = list(size = 4),
        mode = "markers",
        hoverinfo = "text",
        text = ~paste("</br>Launch Direction:",c("Base Point",HR_All$launch_direction),
                      "</br> Launch Angle: ",c("",HR_All$launch_angle),
                      "</br> Exit Velocity: ", c("",HR_All$launch_speed),
                      "</br> Player: ", c("",paste(HR_All$player_name))))
layout <- list(
  autosize = TRUE, 
  hovermode = "closest", 
  scene = list(
    xaxis = list(
      showline = FALSE, 
      title = "y - component", 
      type = "linear"
    ), 
    yaxis = list(
      title = "Deviation from Dead Center", 
      type = "linear"
    ), 
    zaxis = list(
      title = "Launch Angle", 
      type = "linear"
    )
  ), 
  title = "Home Run Visualization", 
  xaxis = list(title = "y - component"), 
  yaxis = list(title = "Deviation from Dead Center"))

p <- layout(p, title = layout$title, scene = layout$scene,
            xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)

p
        
plot_ly(
  x = c(0,HR_All$launch_direction), y = c(0,HR_All$launch_speed), z = c(0,HR_All$launch_angle),
        type = "scatter3d",
        marker = list(size = 4),
        mode = "markers",
        hoverinfo = "text",
        text = ~paste("</br>Launch Direction:",c("Base Point",HR_All$launch_direction),
                      "</br> Launch Angle: ",c("",HR_All$launch_angle),
                      "</br> Exit Velocity: ", c("",HR_All$launch_speed),
                      "</br> Player: ", c("",paste(HR_All$player_name)))) 


#Summary of HRs
HR_All %>%
  select(PARK_ID) %>%
  group_by(PARK_ID) %>%
  summarise(N = n()) %>%
  arrange(desc(N))-> HR_Summary_By_Park

ggplot(HR_All, aes(PARK_ID)) + geom_bar()


#Create a dataframe for each ballpark
for(i in 1:nrow(stadiums)){
  HR_All %>%
    filter(PARK_ID == stadiums[i,1]) -> x
  x %>% inner_join(stadiums, by = c("PARK_ID" = "PARK_ID")) -> x
  assign(toString(stadiums[i,2]),x)
}

###Function to Plot Home Runs for a Specific Ballpark for Specific EV or less###
plot3d_hr <- function(bp,ev = 100){
  HR_All %>%
    filter(PARK_ID == bp[1,1]) %>%
    filter(launch_speed <= ev) -> foo
  p <- plot_ly(x = c(0,foo$x), y = c(0,foo$y), z = c(0,foo$launch_angle), 
          type = "scatter3d",
          marker = list(size = 4),
          mode = "markers",
          hoverinfo = "text",
          text = ~paste("</br>Launch Direction:",c("Base Point",foo$launch_direction),
                        "</br> Launch Angle: ",c("",foo$launch_angle),
                        "</br> Exit Velocity: ", c("",foo$launch_speed),
                        "</br> Player: ", c("",paste(foo$player_name)))
          )
  layout <- list(
    autosize = TRUE, 
    hovermode = "closest", 
    scene = list(
      xaxis = list(
        showline = FALSE, 
        title = "y - component", 
        type = "linear"
      ), 
      yaxis = list(
        title = "Deviation from Dead Center", 
        type = "linear"
      ), 
      zaxis = list(
        title = "Launch Angle", 
        type = "linear"
      )
    ), 
    title = paste("Home Runs at", bp[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "y - component"), 
    yaxis = list(title = "Deviation from Dead Center"))
  p <- layout(p, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis, zaxis = layout$zaxis)
  p
  }

plot2d_hr <- function(bp,ev){
  HR_All %>%
    filter(PARK_ID == bp[1,1]) %>%
    filter(launch_speed <= ev) -> foo
    p <- plot_ly(x = -1*foo$launch_direction, y = foo$launch_angle, 
                 type = "scatter",
                 marker = list(size = 10),
                 mode = "markers",
                 hoverinfo = "text",
                 text = ~paste("</br>Launch Direction:",foo$launch_direction,
                               "</br> Launch Angle: ",foo$launch_angle,
                               "</br> Exit Velocity: ",foo$launch_speed,
                               "</br> Player: ", paste(foo$player_name))
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
    title = paste("Home Runs at", bp[1,2], "with exit velocity <= ", ev, "mph") ,
    xaxis = list(title = "Launch Direction"), 
    yaxis = list(title = "Launch Angle"))
  p <- layout(p, title = layout$title, scene = layout$scene,
              xaxis = layout$xaxis, yaxis = layout$yaxis)
  p
}



###Testing Plots
foo <- BOS07
plot_ly(x = c(0,foo$x), y = c(0,foo$y), z = c(0,foo$launch_angle), 
        type = "scatter3d",
        marker = list(
          size = 4
        ), 
        mode = "markers",
        hoverinfo = "text",
        text = ~paste("</br>Launch Direction:",c("Base Point",foo$launch_direction),
                      "</br> Launch Angle: ",c("",foo$launch_angle),
                      "</br> Exit Velocity: ", c("",foo$launch_speed)))


p <- ggplot(foo, aes(launch_direction,launch_angle)) + geom_point() + stat_smooth()
ggplotly(p)












