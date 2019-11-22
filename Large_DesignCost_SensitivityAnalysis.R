###Scenario Type####

####Create DOE Matrix####
M                       <- rep(10000, 11^2*6)
EE                      <- rep(.25, 11^2*6)
uncert                  <- rep(1, 11^2*6)
flexible                <- rep(c(0, 1, 1, 1, 1, 1), each = 11^2)
designcost              <- rep(1, 11^2*6)
armor2per               <- rep(rep(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), each = 11), 6) 
armor3per               <- rep(rep(c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), 11), 6)
LSCOOption              <- rep(1/3, 11^2*6)
CRLCOOption             <- rep(1/3, 11^2*6)
MESCDOption             <- rep(1/3, 11^2*6)
Delay                   <- rep(c(0, 0, 3, 6, 9, 12), each = 11^2)
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
results$Run             <- rep(1:726, each = 4)
results$flexible        <- rep(c(0, 1, 1, 1, 1, 1), each = 726*4/6)
results$Delay           <- rep(c(0, 0, 3, 6, 9, 12), each = 726*4/6)
results$Scenario        <- rep(c("Uncertainty", "Flexibility", "3 Month", "6 Month", "9 Month", "12 Month"), each = 726*4/6)
results$group           <- rep(1:(726*4/3), 3)
results$bin             <- rep(1:11^2, each = 4)
results                 <- left_join(results, DOE, by = c("Run", "flexible", "Delay"))
ArmorNames              <- data.frame(1:4, c("Standard", "Simple", "Advanced", "Robust")) 
names(ArmorNames)       <- c("armortype", "Armor")
results                 <- left_join(results, ArmorNames, by = c("armortype"))
#results$Label           <- ifelse(results$Delay == 6 & results$LSCOOption == 1 & results$CRLCOOption == 0 & results$MESCD == 0, as.character(results$Armor), "")
results$TENPC           <- results$ENPC * 1000
results$Scenario        <- factor(results$Scenario, levels = c("Uncertainty", "Flexibility", "3 Month", "6 Month", "9 Month", "12 Month"))


####Plot Results####
ScenarioSA <- ggplot(results, aes(x = Scenario, y = TENPC, group = group, linetype = as.factor(armortype), color = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1 ) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold", size = .5) +
  #geom_text(size = 3, position = position_nudge(x = 0.3)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 8)) +
  scale_color_grey(end = .5) + 
  theme(legend.position="none") + 
  facet_grid(armor2per ~ armor3per) 

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

write.csv(short3, "large_DC_SA.csv")

plot <- read.csv("large_DC_SA.csv")
plot$Scenario <- factor(plot$Scenario, levels = c("Uncertainty", "Flexibility", "3 Month", "6 Month", "9 Month", "12 Month"))
plot$Armor <- factor(plot$Armor, levels = c("Standard", "Simple", "Advanced", "Robust"))


ggplot(plot, aes(x = armor2per, y = armor3per, color = Armor, shape = Armor, fill = Armor)) + 
         geom_point(size = 4) + 
         scale_shape_manual(values = c(16, 17, 18)) +
         scale_color_manual(values=c("#E69F00", "#56B4E9", "#009E73")) +
         facet_wrap(facets = "Scenario", ncol = 6)

