###Scenario Type####

####Create DOE Matrix####
Run                     <- 1:81
M                       <- rep(10000, 81)
EE                      <- rep(.25, 81)
uncert                  <- rep(1, 81)
flexible                <- rep(1, 81)
designcost              <- rep(1, 81)
armor2per               <- rep(.25, 81)
armor3per               <- rep(.5, 81)
LSCOOption              <- rep(c(0, .5, 1), each = 9)
CRLCOOption             <- c(c(0, 0, 0, .5, .5, .5, 1, 1, 1), c(0, 0, 0, .5, .5, .5, 1, 1, 1), c(0, 0, 0, .5, .5, .5, 1, 1, 1))
MESCDOption             <- rep(c(0, .5, 1), 9)
Delay                   <- rep(c(0, 6, 12), each = 27)
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
results$Run             <- rep(1:18, each = 4)
results$Delay           <- rep(c(0, 6, 12), each = 24)
results$Scenario        <- rep(c("Flexibility", "6 Month Delay", "12 Month Delay"), each = 24)
results$group           <- rep(1:24, 3)
results                 <- left_join(results, DOE, by = c("Run", "Delay"))
ArmorNames              <- data.frame(1:4, c("Standard", "Simple", "Advanced", "Robust")) 
names(ArmorNames)       <- c("armortype", "Armor")
results                 <- left_join(results, ArmorNames, by = c("armortype"))
results$Label           <- ifelse(results$Delay == 12 & results$LSCOOption == 1 & results$CRLCOOption == 0 & results$MESCD == 0, as.character(results$Armor), "")
results$LSCOOptionText  <- paste("LSCO = ", results$LSCOOption)
results$CRLCOOptionText <- paste("CRLCO = ", results$CRLCOOption)
results$MESCDOptionText <- paste("MESCD = ", results$MESCDOption)
results$TENPC           <- results$ENPC * 1000
results$Scenario        <- factor(results$Scenario, levels = c("Flexibility", "6 Month Delay","12 Month Delay"))


####Plot Results####
ScenarioSA <- ggplot(results, aes(x = Scenario, y = TENPC, group = group, linetype = as.factor(armortype), color = as.factor(armortype), label = Label)) + 
  geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1 ) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold", size = .5) +
  geom_text(size = 3, position = position_nudge(x = 0.3)) + 
  theme(axis.text.x=element_text(angle = -45, hjust = 0, size = 8)) +
  scale_color_grey(end = .5) + 
  theme(legend.position="none") + 
  facet_grid(~ LSCOOptionText + CRLCOOptionText + MESCDOptionText) 

ScenarioSA
