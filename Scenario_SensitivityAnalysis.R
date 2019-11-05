###Scenario Type####

####Create DOE Matrix####
Run         <- 1:81
M           <- rep(1000, 81)
EE          <- rep(.25, 81)
uncert      <- rep(1, 81)
flexible    <- rep(1, 81)
designcost  <- rep(1, 81)
armor2per   <- rep(.25, 81)
armor3per   <- rep(.5, 81)
LSCOOption  <- rep(c(0, .5, 1), each = 9)
CRLCOOption <- c(c(0, 0, 0, .5, .5, .5, 1, 1, 1), c(0, 0, 0, .5, .5, .5, 1, 1, 1), c(0, 0, 0, .5, .5, .5, 1, 1, 1))
MESCDOption <- rep(c(0, .5, 1), 9)
Delay       <- rep(c(0, 6, 12), each = 27)

DOE         <- data.frame(M, EE, uncert, flexible, designcost, armor2per, armor3per, LSCOOption, CRLCOOption, MESCDOption, Delay)
DOE$TotalProb <- DOE$LSCOOption + DOE$CRLCOOption + DOE$MESCDOption
DOE         <- subset(DOE, TotalProb == 1)

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
results$Run   <- rep(1:18, each = 4)
results$Delay <- rep(c(0, 6, 12), each = 24)
results$group <- rep(1:24, 3)

results     <- left_join(results, DOE, by = c("Run", "Delay"))
results

####Plot Results####
ggplot(results, aes(x = as.factor(Delay), y = ENPC, group = group, linetype = as.factor(armortype), color = as.factor(armortype))) + 
  geom_line() + 
  facet_grid(~ LSCOOption + CRLCOOption + MESCDOption)
