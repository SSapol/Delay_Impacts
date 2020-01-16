library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(gridExtra)

#CHANGE OUTPUT OF SIM TO DF

attacktracks <- simruns(500, .25, 1, 1, 1, .25, .5, 1/3, 1/3, 1/3, 0)
attacktracks
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

AttackTrackPlot <- ggplot(subset(attacktracks, attacktracks$missiontype == 1 & attacktracks$month <= 132), aes(x = month, y = monthlyattacks, group = as.factor(run), color = as.factor(run))) + 
         geom_line(alpha = .15) + 
         geom_line(data = iraq, aes(x =  month, y = monthlyattacks, group = run), color = "black", size = 1.5) + 
         xlim(1, 132) + 
         ylim(0, 0.5) +
         ggtitle('Attacks per Vehicle Model (500 runs) vs. Historical Data') + 
         theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) + 
         labs(x = "Month", y = "Attacks/Vehicle", face = "bold") + 
         annotate("text", x = 55, y = .048, label = 'bold("Historical Data")', parse = TRUE) +
         scale_color_manual(values = rep(wes_palette(name="Darjeeling1"), 1309/4)) +
         theme(legend.position="none")
       
AttackTrackPlot

