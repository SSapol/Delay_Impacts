library(reshape2)
library(dplyr)
library(ggplot2)
library(data.table)
library(tidyverse)
library(gridExtra)

####  Iraq Historical Data ####
iraq_months            <- c(1:40)                                                   # Number of months of data
iraq_dates             <- seq(as.Date("2005/2/15"), by = "month", length.out = 40)  # Dates Data is collected
iraq_attackspervehicle <- c(0.047,0.051,0.052,0.056,0.057,                          # Attacks per month.  Used # of attacks/# of soldiers/ 4 Vehicles per soldier
                            0.064,0.065,0.071,0.066,0.061,
                            0.063,0.075,0.074,0.084,0.082,
                            0.095,0.107,0.108,0.129,0.127,
                            0.132,0.129,0.132,0.140,0.129,
                            0.140,0.131,0.134,0.124,0.106,
                            0.104,0.076,0.056,0.043,0.042,
                            0.043,0.041,0.044,0.043,0.045)

df_iraqattacks         <- data.frame(iraq_months, as.Date(iraq_dates), iraq_attackspervehicle)  #Merge into data frame

####Calculate mean number of attacks####
iraq_meanattacks <- mean(df_iraqattacks$iraq_attackspervehicle)

####Plot Attacks per Vehicle####
ggplot(df_iraqattacks, aes(x = as.Date(iraq_dates), y = iraq_attackspervehicle)) + 
  geom_line() + 
  geom_hline(yintercept = iraq_meanattacks, color = "red", linetype = 2, alpha = .5) + 
  annotate("text", x = as.Date("2007/11/15"), y = .12, label = "Attacks/Vehicle") + 
  annotate("text", x = as.Date("2005/8/15"), y = .09, label = "Mean Attacks/Vehicle = .0842", color = "red") +
  ggtitle('Iraq (2/2005 to 7/2008) - Attacks per Vehicle') + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  labs(x = "Month", y = "Attacks per Vehicle", face = "bold") + 
  theme(legend.position="none") + 
  scale_color_grey(end = .5) 


#Predictors
iraq_Xtminus1 <- c(0.047, 0.051, 0.052,0.056,0.057,0.064,0.065,0.071,0.066,0.061,0.063,0.075,0.074,0.084,0.082,0.095,0.107,0.108,0.129,0.127,0.132,0.129,0.132,0.140,0.129,0.140,0.131,0.134,0.124,0.106,0.104,0.076,0.056,0.043,0.042,0.043,0.041,0.044,0.043)
iraq_Xt       <- c(0.051, 0.052,0.056,0.057,0.064,0.065,0.071,0.066,0.061,0.063,0.075,0.074,0.084,0.082,0.095,0.107,0.108,0.129,0.127,0.132,0.129,0.132,0.140,0.129,0.140,0.131,0.134,0.124,0.106,0.104,0.076,0.056,0.043,0.042,0.043,0.041,0.044,0.043,0.045)

#Regression
iraq_reg               <- data.frame(iraq_Xtminus1, iraq_Xt)
iraq_reg$prediction    <- iraq_reg$iraq_Xtminus1 * 1
iraq_reg$residual      <- iraq_reg$iraq_Xt - iraq_reg$pred
iraq_reg$residsq       <- iraq_reg$residual^2
iraq_reg$squares       <- iraq_Xt^2

#Calculate R^2
iraq_sumresidsq        <- sum(iraq_reg$residsq)
iraq_sumsq             <- sum(iraq_reg$squares)
iraq_reg_rsquared      <- 1 - sum(iraq_sumresidsq)/sum(iraq_sumsq)
iraq_reg_rsquared

iraq_meanresid         <- mean(iraq_reg$residual)
iraq_stdevresid        <- sd(iraq_reg$residual)

###Plot of Regression####
ggplot(iraq_reg, aes(x = iraq_Xtminus1, y = iraq_Xt)) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0, alpha = .5, linetype = 3, color = 'black') +
  labs(x = expression(Attacks[t-1]), y = expression(Attacks[t]), face = "bold") +
  ggtitle('Previous Month as a Predictor of Number of Attacks')  +
  annotate("text", x = .105, y = .09, label = "R-Squared = 98.96%") + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) 




####  Simulation ####
simruns  <- function(M, EE, unc, flex, design, armor2designpercentage, armor3designpercentage, LSCOPercentage, CRLCOPercentage, MESCDPercentage, delay) {
  
  ####  Simulation Initial Conditions  ####  
  set.seed(100)
  totalrvneeded                  <- M*132*4                            #Total number of unique random variables needed (total lines)
  discountrate                   <- .021                                #discount rate used
  totalruns                      <- M*4                                #Total runs M runs per each armor type (ex. M = 100, 400 total runs or 100 runs for each armor type)
  indexnumber                    <- 1:totalrvneeded                    #Used to index each indivdiual time period
  run                            <- rep(1:totalruns, each = 132)       #Indexes by run 
  month                          <- rep(1:132, M*4)                    #Loops 132months for every run
  armortype                      <- rep(1:4, each = M*132)             #Allocates armor type for each run
  
  ####  Armor Type Initial Conditions   ###
  
  #Initial Costs#
  armor1cost                     <- 4                                #Purchase Cost for Standard Armor
  armor2cost                     <- 4.1                                #Purchase Cost for Passive Armor
  armor3cost                     <- 4.5                                #Purchase Cost for Reactive Armor  
  armor4cost                     <- 5                                  #Purchase Cost for Robust Armor
  
  #Design Costs# 
  # armor2designpercentage         <- .25                                                             #Percent Cost of design for Passive Armor  (Hook Cost)
  # armor3designpercentage         <- .5                                                             #Percent Cost of design for Reactive Armor (Hook Cost)
  armor2design                   <- design * (armor2cost - armor1cost) * armor2designpercentage    #Design Cost for Passive Armor
  armor3design                   <- design * (armor3cost - armor1cost) * armor3designpercentage    #Design Cost for Reactive Armor
  
  #Armor Percent Improvement Levels#
  armor1improve                  <- 0                                  #Percent improvement over Standard Armor for Standard Armor (ITS THE SAME!)
  armor2improve                  <- .10                                #Percent improvement over Standard Armor for Passive Armor 
  armor3improve                  <- .40                                #Percent improvement over Standard Armor for Reactive Armor 
  armor4improve                  <- .70                                #Percent improvement over Standard Armor for Robust Armor 
  
  #Mission Type Random Variable##
  missiontypeRV                  <- rep(runif(M*4, min = 0, max = 1), each = 132)   #Creates a random variable to determine mission type for each run
  bogdwellRV                     <- rep(runif(M*4, min = 0, max = 6), each = 132)   #Creates a random variable to bog dwell for each run
  
  #Create Initial Data Frame
  df                             <- data.frame(indexnumber, run, month, armortype, missiontypeRV, bogdwellRV)
  
  #### Mission Type Assignments ####
  df$missiontype                 <- ifelse(df$missiontypeRV <= LSCOPercentage, 1,                   #Assigns Mission Type of LSCO (1) with probability of 1/3 
                                           ifelse(df$missiontypeRV <= LSCOPercentage + CRLCOPercentage, 2,            #Assigns Mission Type of CRLCO (2) with probability of 1/3 
                                                  ifelse(df$missiontypeRV <= 1, 3,4)))   #Assigns Mission Type of MEDCL (3) with probability of 1/3 
  
  #### Bogdwell assignments ####
  df$bogdwell                    <- ifelse(df$missiontype == 1 & df$bogdwellRV <= 2, 1,                  #Assigns bogwell conditions based on the mission type and bogdwellrv
                                           ifelse(df$missiontype == 1 & df$bogdwellRV <= 4, 2,
                                                  ifelse(df$missiontype == 1 & df$bogdwellRV <= 6, 3, 
                                                         ifelse(df$missiontype == 2 & df$bogdwellRV <= 3, 3, 4))))
  
  #### Builds BOGDWELL Scenarios ####
  bdmonth                        <- rep(1:132, 4)
  #br                            <- rep(1:4, each = 132)
  bog                            <- c(1,1,1,1,1,1,1,1,1,1,1,1)                                             #Represents 12 months of deloyment
  dwell                          <- c(0,0,0,0,0,0,0,0,0,0,0,0)                                             #Represents 12 months of deloyment   
  bogdwell1                      <- c(dwell, bog,dwell,bog,dwell,bog,dwell,bog,dwell,bog, dwell)           #Represents 1:1 BOG:Dwell Ratio
  bogdwell2                      <- c(dwell, bog,dwell,dwell,bog,dwell,dwell,bog,dwell,dwell, bog)         #Represents 1:2 BOG:Dwell Ratio  
  bogdwell3                      <- c(dwell, bog,dwell,dwell,dwell,bog,dwell,dwell,dwell,bog, dwell)       #Represents 1:3 BOG:Dwell Ratio
  bogdwell4                      <- c(dwell, bog,dwell,dwell,dwell, dwell, bog,dwell,dwell,dwell,dwell)    #Represents 1:4 BOG:Dwell Ratio
  
  #### Deployment assignements per scenario ####
  df$deployed                    <- ifelse(df$bogdwell == 1, bogdwell1[bdmonth],                          #Lookup deployment in a given month based on the scenario selected above
                                           ifelse(df$bogdwell == 2, bogdwell2[bdmonth],
                                                  ifelse(df$bogdwell == 3, bogdwell3[bdmonth],
                                                         ifelse(df$bogdwell == 4, bogdwell4[bdmonth], NA))))
  
  #### Set in initial attack parameters and Base Attack Parameters  ####
  df$baselineattack              <- ifelse(df$missiontype == 1, iraq_meanattacks,                          #Sets the attacks per month in LSCO based on the mean of Iraq Attacks
                                           ifelse(df$missiontype == 2, iraq_meanattacks/2, 0))             #Sets the attacks per month in CRLCO based on the mean/2 of Iraq Attacks
  
  #### Create Uncertainty Attack parameters  ####
  attackrv                       <- runif(4*M*132, 0, 1)                                                             #Creates the RV Vector for attack start point
  df$initialattackrv             <- ifelse(df$month == 1, attackrv[indexnumber], 0)                                  #Assigns initial attack RV at Month = 1
  df$initialmonthlyattack        <- ifelse(df$missiontype == 1, .5*iraq_meanattacks + iraq_meanattacks*df$initialattackrv,     #Creates LSCO initial attack condition between iraq attacks with potential for a 50% increase or decrease to start
                                           ifelse(df$missiontype == 2, iraq_meanattacks*df$initialattackrv, 0))                     #Creates CRLCO initial attack condtion between 0 and iraq mean
  df$attackstart                 <- ifelse(df$month == 1, df$initialmonthlyattack[indexnumber], 0)         #creates monthly start
  df$attackerror                 <- rnorm(totalrvneeded, iraq_meanresid, iraq_stdevresid)                    #Determines monthly random error from Iraq scenario
  df$attacknew                   <- df$attackstart + df$attackerror                                        #Incorporates error into attack variable 
  df                             <- data.table(df)                                                         #reestablishes DF
  df[, monthlyattacks := cumsum(attacknew), by=list(run)]                                                  #creates cumulative attack rate incorporating monthly error
  
  df$monthlyattacks              <- ifelse(df$monthlyattacks <0, 0, df$monthlyattacks)                     #Monthly Attacks cannot go negative
  df$monthlyattacks              <- ifelse(df$missiontype == 3, 0, df$monthlyattacks)                                #If MESCD, then attacks is 0
  #df$monthlyattacks              <- df$monthlyattacks
  
  #### Set Enemy Effectiveness Percentage ####
  df$enemyeffectiveness          <- rep(EE, totalrvneeded)          #Establishes enemy effectiveness
  
  #### Upgrade criteria ####
  
  df$decisionshift               <- ifelse(df$month <= 1, 0, shift(df$monthlyattacks, 0, type = "lag"))    #Shifts decsion to later date (Currently at 0)
  df$upgradecriteria             <- ifelse(df$month <= 12, 0,                                              #Does not allow an upgrade before deployment
                                           ifelse(df$monthlyattacks > 0.00, 1, 0))                         #Upgrade criteria, currently if > 0.
  df$samerun                     <- ifelse(df$run == 1 & df$month == 1, 0,                                 #Ensures calculations on the same run
                                           ifelse(df$run == shift(df$run, 1L, type = "lag"), 1, 0))
  df$decisiondelay               <- shift(df$upgradecriteria, delay, type = "lag")                         #Delay built into system (Part of function)
  df$upgrade                     <- ifelse(df$month <= 12, 0, df$decisiondelay * df$samerun * flex)        #Decision to upgrade based
  df[, upgraded := cumsum(upgrade), by=list(run)]                                                          #Determines if upgrade has been performed (if>=1)
  df$upgraded                    <- df$upgraded
  df$call                        <- ifelse(df$upgraded == 1 & shift(df$upgraded, 1L, type = "lag") == 1, 0,#Calls option (Only 1 per run) 
                                           ifelse(df$upgraded == 1, 1, 0))
  
  ####  Set Armor Effectiveness Percentage Based on Vehicle ####
  df$armoreffectiveness          <- ifelse(df$armortype == 1, armor1improve,                                   #Defines armor effectiveness level based on scenario
                                           ifelse(flex == 0 & df$armortype == 2, armor2improve, 
                                                  ifelse(flex == 0 & df$armortype == 3, armor3improve, 
                                                         ifelse(df$armortype == 4, armor4improve, 
                                                                ifelse(flex == 1 & df$armortype == 2 & df$upgraded < 1, armor1improve, 
                                                                       ifelse(flex == 1 & df$armortype == 2 & df$upgraded >= 1, armor2improve, 
                                                                              ifelse(flex == 1 & df$armortype == 3 & df$upgraded < 1, armor1improve, 
                                                                                     ifelse(flex == 1 & df$armortype == 3 & df$upgraded >= 1, armor3improve, NA))))))))
  
  ####  Create Armor Effectiveness Multiplier ####
  df$armoreffectivenessmultiplier<- 1-df$armoreffectiveness
  
  ####  Set probability of loss  ####
  df$lossprobability             <- (1 - unc)*df$deployed * df$baselineattack * df$enemyeffectiveness * df$armoreffectivenessmultiplier + unc * df$deployed * df$monthlyattacks * df$enemyeffectiveness * df$armoreffectivenessmultiplier
  
  ####  Determine Loss  ####
  df$randloss                    <- rep(runif(indexnumber, min = 0, max = 1))         #create string of RVs for each index to determine loss         
  df$Loss                        <- ifelse(df$lossprobability>df$randloss, 1, 0)                   #if probability of loss is > randloss, then vehicles is lost
  
  #### Determine Costs ####
  df$purchasecost                <- ifelse(df$armortype == 1 & month == 1, armor1cost, 
                                           ifelse(df$armortype == 2 & month == 1, armor2cost - flex * (armor2cost - armor1cost) + design * armor2design, 
                                                  ifelse(df$armortype == 3 & month == 1, armor3cost - flex * (armor3cost - armor1cost) + design *armor3design, 
                                                         ifelse(df$armortype == 4 & month == 1, armor4cost, 0))))
  df$replacementcost             <- ifelse(df$armortype == 1, armor1cost*df$Loss, 
                                           ifelse(df$armortype == 2, armor2cost*df$Loss, 
                                                  ifelse(df$armortype == 3, armor3cost*df$Loss, 
                                                         ifelse(df$armortype == 4, armor4cost*df$Loss, 0))))
  df$callcost                    <- df$call * ifelse(df$armortype == 2, armor2cost - armor1cost - design * armor2design, 
                                                     ifelse(df$armortype == 3, armor3cost - armor1cost - design * armor3design, 0))
  df$presentcost                 <- df$purchasecost + df$replacementcost/(1+discountrate/12)^month + df$callcost/(1+discountrate/12)^month
  
  #### Outputs ####
  runsummary   <- df %>%
    select(armortype, run, presentcost) %>%
    group_by(armortype, run) %>%
    summarise(NPC = sum(presentcost))
  
  armorsummary <- runsummary %>%
    select(armortype, NPC) %>%
    group_by(armortype)   %>%
    summarise(ENPC = mean(NPC))
  
  out <- armorsummary
  
  return(out)
}

####Set Intial Condtiosn for Simulation####
eff <- .25
M   <- 10000

####Run Simulation for 6 Scenarios######

baseruns                           <- simruns(M, eff, 0, 0, 0, .25, .5, 1/3, 1/3, 1/3, 0)
uncertaintyruns                    <- simruns(M, eff, 1, 0, 0, .25, .5, 1/3, 1/3, 1/3, 0)
flexibleruns                       <- simruns(M, eff, 1, 1, 1, .25, .5, 1/3, 1/3, 1/3, 0)
delay6                             <- simruns(M, eff, 1, 1, 1, .25, .5, 1/3, 1/3, 1/3, 6)
delay12                            <- simruns(M, eff, 1, 1, 1, .25, .5, 1/3, 1/3, 1/3, 12)

#Label with Scenario Name
baseruns$scenario                  <- "Base"
uncertaintyruns$scenario           <- "Uncertainty"
flexibleruns$scenario              <- "Flexibility"
delay6$scenario                    <- "6 Month Delay"
delay12$scenario                   <- "12 Month Delay"

#Merge results into single dataframe
results                            <- as.data.frame(rbind(baseruns, uncertaintyruns, flexibleruns, delay6, delay12))

#Reorder scenarios###
results$scenario                   <- factor(results$scenario, levels = c("Base", "Uncertainty", "Flexibility", "Flex with Design", "6 Month Delay","12 Month Delay"))

#Set total number of vehicles and develop total ENPC
totalvehicles                      <- 1000
results$ENPC                       <- results$ENPC * totalvehicles


  #Standardize y-axis for graphs
  ylim1 <- 5500
  ylim2 <- 5900
  
  #Plot Base vs. Uncertainty
  Base_v_Uncertainty <- 
    ggplot(subset(results, (scenario == "Base" | scenario == "Uncertainty")), aes(x = scenario, y = ENPC, group = as.factor(armortype), color = as.factor(armortype), label = armortype, linetype = as.factor(armortype))) + 
    geom_line(aes(color = as.factor(armortype)), size = 1) + 
    geom_text(data = subset(results, scenario == "Uncertainty"), aes(color = as.factor(armortype), label = c("Standard", "", "Reactive", "Robust")),position = position_nudge(x = 0.15)) +
    geom_text(data = subset(results, scenario == "Base"), aes(color = as.factor(armortype), label = c("", "Passive", "", "")),position = position_nudge(x = -0.15)) +
    ggtitle('Base vs. Uncertainty') + 
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
    labs(x = "Scenario", y = "Expected Net Present Cost ($M)", face = "bold") + 
    theme(legend.position="none") + 
    scale_color_grey(end = .5) +
    ylim(ylim1, ylim2)
  
  Base_v_Uncertainty
  
  #Uncertainty vs. Flexibility
  
  Uncertainty_v_Flexibility <-
    ggplot(subset(results, (scenario == "Uncertainty" | scenario == "Flexibility")), aes(x = scenario, y = ENPC, group = as.factor(armortype), color = as.factor(armortype), label = armortype, linetype = as.factor(armortype))) + 
    geom_line(aes(color = as.factor(armortype)), size = 1) + 
    geom_text(data = subset(results, scenario == "Flexibility"), aes(color = as.factor(armortype), label = c("Standard", "Passive", "Reactive", "Robust")),position = position_nudge(x = 0.15)) +
    geom_text(data = subset(results, scenario == "Uncertainty"), aes(color = as.factor(armortype), label = c("", "", "", "")),position = position_nudge(x = -0.15)) +
    ggtitle('Uncertainty vs. Flexibility') + 
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
    labs(x = "Scenario", y = "Expected Net Present Cost ($M)", face = "bold") + 
    theme(legend.position="none") + 
    scale_color_grey(end = .5) +
    ylim(ylim1, ylim2)
  
  Uncertainty_v_Flexibility
  
  #Plot the Impact of Delays
  
  ImpactOfDelays <- 
    ggplot(subset(results, (scenario == "Flexibility" | scenario == "6 Month Delay"| scenario == "12 Month Delay" )), aes(x = scenario, y = ENPC, group = as.factor(armortype), color = as.factor(armortype), label = as.factor(armortype))) + 
    geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1) + 
    geom_text(data = subset(results, scenario == "12 Month Delay"), aes(color = as.factor(armortype), label = c("Standard", "Passive", "Reactive", "Robust")),position = position_nudge(x = 0.25)) + 
    ggtitle('Impact of Delays') + 
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
    labs(x = "Scenario", y = "Expected Net Present Cost ($M)", face = "bold") + 
    theme(legend.position="none") + 
    scale_color_grey(end = .5) +
    ylim(ylim1, ylim2)
  
  ImpactOfDelays
  
  #Plot all togther 
  grid.arrange(Base_v_Uncertainty, Uncertainty_v_Flexibility, ImpactOfDelays, ncol = 3)
  
  
