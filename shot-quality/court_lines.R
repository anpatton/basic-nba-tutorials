library(tidyverse)

circleFun <- function(center = c(0,0), radius = 23.9, npoints = 10000){
  
  r <- radius 
  
  tt <- seq(0,2*pi,length.out = npoints)
  
  xx <- center[1] + r * cos(tt)
  
  yy <- center[2] + r * sin(tt)
  
  return(data.frame(x = xx, y = yy))
  
}

line_df_small <- data.frame(x = c(-25, -25, 25, -25, 6, -6, 6, 3, 21.99, -21.99), 
                            y = c(-4, 32, 32, -4, -4, -4, 15, 0, -4, -4),
                            xend = c(-25, 25, 25, 25, 6, -6, -6, -3, 21.99, -21.99), 
                            yend = c(32, 32, -4, -4, 15, 15, 15, 0, 9.5, 9.5), 
                            point = c("L sideline", "midcourt", "R sideline", "baseline", 
                                      "R key", "L key", "foul line", "backboard", "R three side", "L three side"))

background_rect_small <- data.frame(xmin = -25, 
                                    xmax = 25, 
                                    ymin = -4, 
                                    ymax = 32)

circ_df_3 <- circleFun() %>% 
  filter(x > -22 & x < 22) %>% 
  filter(y >= -4)

circ_df_key <- circleFun(center = c(0, 15), radius = 6)