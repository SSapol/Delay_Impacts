library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(gridExtra)

attacktracks <- simruns(1000, .25, 1, 1, 1, .25, .5, 1/3, 1/3, 1/3, 0)

attacktracks <- data.frame(attacktracks)

iraq         <- df_iraqattacks
iraq[,2]     <- NULL
names(iraq)  <- c("month", "monthlyattacks")
month        <- 41:132
monthlyattacks <- NA
add          <- data.frame(month, monthlyattacks)
iraq         <- rbind(iraq, add)
iraq$run     <- 51


month          <- attacktracks$month
monthlyattacks <- attacktracks$monthlyattacks
run            <- attacktracks$run
attacks        <- data.frame(month, monthlyattacks, run)
attacks$color  <- "Simulation"

ggplot(subset(attacktracks, attacktracks$missiontype == 1 & attacktracks$month <= 40), aes(x = month, y = monthlyattacks, group = run)) + 
         geom_line(alpha = .05) + 
         geom_line(data = iraq, aes(x =  month, y = monthlyattacks, group = run), color = "red", size = 1) + 
         #geom_hline(yintercept = .084, color = "red", linetype = 2, size = 1) + 
         geom_segment(aes(x=1,xend=40,y=.084,yend=.084), linetype = 2, size = 1, color = "red") +
         xlim(1, 40)
       