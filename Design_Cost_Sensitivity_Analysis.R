####Design Cost Sensitivity Analysis####

####Create DOE Matrix####
Run         <- 1:15
M           <- rep(10000, 15)
EE          <- rep(.25, 15)
uncert      <- rep(1, 15)
flexible    <- rep(1, 15)
designcost  <- rep(1, 15)
armor2per   <- rep(c(0,.25, .5, .75, 1), 3)
armor3per   <- rep(c(0,.25, .5, .75, 1), 3)
LSCOOption  <- rep(1/3, 15)
CRLCOOption <- rep(1/3, 15)
MESCDOption <- rep(1/3, 15)
Delay       <- rep(c(0, 6, 12), each = 5)

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

results$Run   <- rep(1:15, each = 4)
results$Delay <- rep(c(0, 6, 12), each = 20)
results$group <- rep(1:20, 3)
results       <- left_join(results, DOE, by = c("Run", "Delay"))
results

####Plot Results####
ggplot(results, aes(x = as.factor(Delay), y = ENPC, group = group, linetype = as.factor(armor2per))) + 
  geom_line() + 
  #theme(legend.position="none") + 
  facet_grid(~ armortype)
