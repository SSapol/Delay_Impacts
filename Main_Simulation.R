####Run Simulation for Initilal 5 Scenarios######

####Create DOE Matrix####
Run         <- 1:5
M           <- rep(10000, 5)
EE          <- rep(.25, 5)
uncert      <- rep(c(0, 1, 1, 1, 1))
flexible    <- rep(c(0, 0, 1, 1, 1))
designcost  <- rep(c(0, 0, 1, 1, 1))
armor2per   <- rep(.25, 5)
armor3per   <- rep(.5, 5)
LSCOOption  <- rep(1/3, 5)
CRLCOOption <- rep(1/3, 5)
MESCDOption <- rep(1/3, 5)
Delay       <- rep(c(0, 0, 0, 6, 12))

DOE         <- data.frame(M, EE, uncert, flexible, designcost, armor2per, armor3per, LSCOOption, CRLCOOption, MESCDOption, Delay)
DOE$Run     <- 1:length(DOE$M)
DOELength   <- length(DOE$Run)

results     <- data.frame()

####Run DOE Matrix####
for(i in 1:DOELength){
  df        <- simruns(DOE$M[i], DOE$EE[i], DOE$uncert[i], DOE$flexible[i], DOE$designcost[i], DOE$armor2per[i], DOE$armor3[i], DOE$LSCOOption[i], DOE$CRLCOOption[i], DOE$MESCDOption[i], DOE$Delay[i]) 
  df        <- data.frame(df)
  results   <- rbind(results, df)
}
results
results$Run      <- rep(1:5, each = 4)
results$scenario <- rep(c("Base", "Uncertainty", "Flexibility", "6 Month Delay", "12 Month Delay"), each = 4)

#Reorder scenarios###
results$scenario                   <- factor(results$scenario, levels = c("Base", "Uncertainty", "Flexibility", "Flex with Design", "6 Month Delay","12 Month Delay"))

#Set total number of vehicles and develop total ENPC
totalvehicles                      <- 1000
results$TENPC                      <- results$ENPC * totalvehicles

####Plot Results####

#Standardize y-axis for graphs
ylim1 <- 5500
ylim2 <- 5900

results

#Plot Base vs. Uncertainty
Base_v_Uncertainty <- 
  ggplot(subset(results, (scenario == "Base" | scenario == "Uncertainty")), aes(x = scenario, y = TENPC, group = as.factor(armortype), color = as.factor(armortype), label = armortype, linetype = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype)), size = 1) + 
  geom_text(data = subset(results, scenario == "Uncertainty"), aes(color = as.factor(armortype), label = c("Standard", "", "Advanced", "Robust")),position = position_nudge(x = 0.2)) +
  geom_text(data = subset(results, scenario == "Base"), aes(color = as.factor(armortype), label = c("", "Simple", "", "")),position = position_nudge(x = -0.2)) +
  ggtitle('Baseline vs. Uncertainty') + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold") +
  scale_color_grey(end = .5) +
  ylim(ylim1, ylim2) + 
  theme(
    panel.background = element_blank(),
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  ) + 
  theme(legend.position="none")  

Base_v_Uncertainty

#Uncertainty vs. Flexibility

Uncertainty_v_Flexibility <-
  ggplot(subset(results, (scenario == "Uncertainty" | scenario == "Flexibility")), aes(x = scenario, y = TENPC, group = as.factor(armortype), color = as.factor(armortype), label = armortype, linetype = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype)), size = 1) + 
  geom_text(data = subset(results, scenario == "Flexibility"), aes(color = as.factor(armortype), label = c("Standard", "Simple", "", "Robust")),position = position_nudge(x = 0.2)) +
  geom_text(data = subset(results, scenario == "Uncertainty"), aes(color = as.factor(armortype), label = c("", "", "Advanced", "")),position = position_nudge(x = -0.2)) +
  ggtitle('Uncertainty vs. Flexibility') + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold") + 
  theme(legend.position="none") + 
  scale_color_grey(end = .5) +
  ylim(ylim1, ylim2) + 
  theme(
    panel.background = element_blank(),
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  )


Uncertainty_v_Flexibility

#Plot the Impact of Delays

ImpactOfDelays <- 
  ggplot(subset(results, (scenario == "Flexibility" | scenario == "6 Month Delay"| scenario == "12 Month Delay" )), aes(x = scenario, y = TENPC, group = as.factor(armortype), color = as.factor(armortype), label = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1) + 
  geom_text(data = subset(results, scenario == "12 Month Delay"), aes(color = as.factor(armortype), label = c("Standard", "Simple", "Advanced", "Robust")),position = position_nudge(x = 0.25)) + 
  ggtitle('Impact of Delays') + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  labs(x = "Scenario", y = "Total Expected Net Present Cost ($M)", face = "bold") + 
  theme(legend.position="none") + 
  scale_color_grey(end = .5) +
  ylim(ylim1, ylim2) +
  theme(
    panel.background = element_blank(),
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black")
  ) + 
  theme(legend.position="none")  

ImpactOfDelays

#Plot all togther 
MainPlot <- grid.arrange(Base_v_Uncertainty, Uncertainty_v_Flexibility, ImpactOfDelays, ncol = 3)
MainPlot

