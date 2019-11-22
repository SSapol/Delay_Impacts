###Scenario Type####

####Create DOE Matrix####
M                       <- rep(10000, 11^3*6)
EE                      <- rep(.25, 11^3*6)
uncert                  <- rep(1, 11^3*6)
flexible                <- rep(c(0, 1, 1, 1, 1, 1), each = 11^3)
designcost              <- rep(1, 11^3*6)
armor2per               <- rep(.25, 11^3*6)
armor3per               <- rep(.5, 11^3*6)
LSCOOption              <- rep(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), each = 121)
CRLCOOption             <- rep(c(0, 0, 0, 0, 0, 0,0,0,0,0,0,
                                 .1,.1,.1,.1,.1,.1,.1,.1,.1,.1,.1,
                                 .2,.2,.2,.2,.2,.2,.2,.2,.2,.2,.2,
                                 .3,.3,.3,.3,.3,.3,.3,.3,.3,.3,.3,
                                 .4,.4,.4,.4,.4,.4,.4,.4,.4,.4,.4,
                                 .5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,
                                 .6,.6,.6,.6,.6,.6,.6,.6,.6,.6,.6,
                                 .7,.7,.7,.7,.7,.7,.7,.7,.7,.7,.7,
                                 .8,.8,.8,.8,.8,.8,.8,.8,.8,.8,.8,
                                 .9,.9,.9,.9,.9,.9,.9,.9,.9,.9,.9,
                                 1,1,1,1,1,1,1,1,1,1,1), 11)
MESCDOption             <- rep(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), 121)
Delay                   <- rep(c(0, 0, 3, 6, 9, 12), each = 11^3)
DOE                     <- data.frame(M, EE, uncert, flexible, designcost, armor2per, armor3per, LSCOOption, CRLCOOption, MESCDOption, Delay)

####Subset DOE to Total Probability = 1####
DOE$TotalProb           <- DOE$LSCOOption + DOE$CRLCOOption + DOE$MESCDOption
DOE                     <- subset(DOE, TotalProb == 1)
DOE$Run                 <- 1:length(DOE$M)

####Run DOE Matrix####
results     <- data.frame()
for(i in 1:length(DOE$M)){
  df        <- simruns(DOE$M[i], DOE$EE[i], DOE$uncert[i], DOE$flexible[i], DOE$designcost[i], DOE$armor2per[i], DOE$armor3[i], DOE$LSCOOption[i], DOE$CRLCOOption[i], DOE$MESCDOption[i], DOE$Delay[i]) 
  df        <- data.frame(df)
  results   <- rbind(results, df)
}

####Build Results Data Frame####
results$Run             <- rep(1:372, each = 4)
results$flexible        <- rep(c(0, 1, 1, 1, 1, 1), each = 372*4/6)
results$Delay           <- rep(c(0, 0, 3, 6, 9, 12), each = 372*4/6)
results$Scenario        <- rep(c("Uncertainty", "Flexibility", "3 Month Delay", "6 Month Delay", "9 Month Delay", "12 Month Delay"), each = 372*4/6)
results$group           <- rep(1:(372*4/6), 3)
results$bin             <- rep(1:62, each = 4)
results                 <- left_join(results, DOE, by = c("Run", "flexible", "Delay"))
ArmorNames              <- data.frame(1:4, c("Standard", "Simple", "Advanced", "Robust")) 
names(ArmorNames)       <- c("armortype", "Armor")
results                 <- left_join(results, ArmorNames, by = c("armortype"))
#results$Label           <- ifelse(results$Delay == 18 & results$LSCOOption == 1 & results$CRLCOOption == 0 & results$MESCD == 0, as.character(results$Armor), "")
results$LSCOOptionText  <- paste("LSCO = ", results$LSCOOption)
results$CRLCOOptionText <- paste("CRLCO = ", results$CRLCOOption)
results$MESCDOptionText <- paste("MESCD = ", results$MESCDOption)
results$TENPC           <- results$ENPC * 1000
results$Scenario        <- factor(results$Scenario, levels = c("Uncertainty", "Flexibility", "3 Month Delay", "6 Month Delay", "9 Month Delay", "12 Month Delay"))


####Plot Results####
ScenarioSA <- ggplot(results, aes(x = Scenario, y = TENPC, group = group, linetype = as.factor(armortype), color = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1 ) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold", size = .5) +
  #geom_text(size = 3, position = position_nudge(x = 0.3)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 8)) +
  scale_color_grey(end = .5) + 
  theme(legend.position="none") + 
  facet_grid(LSCOOptionText ~ CRLCOOptionText + MESCDOptionText ) 

ScenarioSA

short <- results #subset(results, group <= 8)

short2 <-short[order(short$bin, short$Scenario),]
rownames(short2) <- NULL

short2

select <- aggregate(TENPC ~ bin + Scenario, data = short2, min)
select$select <- "min"

short3    <- left_join(short2, select, by = c("bin", "Scenario", "TENPC"))
short3

short3    <- short3[!is.na(short3$select), ]
rownames(short3) <- NULL

short3$tag <- paste(short3$LSCOOptionText, " ", short3$CRLCOOptionText, " ", short3$MESCDOptionText)
short3$Armor <- factor(short3$Armor, levels = c("Standard", "Simple", "Advanced", "Robust"))


ggplot(short3, aes(x = Scenario)) + 
  geom_histogram(stat = "count")+ 
  theme(legend.position="none") + 
  facet_wrap(facets = "Armor", ncol = 4)

ggplot(short3, aes(x = LSCOOption, y = CRLCOOption, color = Armor, shape = Armor, fill = Armor)) + 
  geom_point(size = 4) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9", "#009E73")) +
  facet_wrap(facets = "Scenario", ncol = 6)

library(tidyverse)
change <- results

change <- change[order(change$group),]
rownames(change) <- NULL
change$bin <- rep(1:(744/12), each = 12)
View(change)
write.csv(results, "results2.csv")

library(plotly)

scenarioSA <- read.csv("C:/Users/Steve/Desktop/scenarioSA.csv")
plot_ly(x=short3$LSCOOption, y=short3$CRLCOOption, z=short3$MESCDOption, type="scatter3d", mode="markers", 
        color=short3$Armor)
