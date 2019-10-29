####Enemy Effectiveness Sensitivity Analysis####

flex_0      <- simruns(M, 0, 1,1,1,.25, .5, 0)
flex_25     <- simruns(M, 0.25, 1,1,1,.25, .5,0)
flex_50     <- simruns(M, 0.5, 1,1,1,.25, .5,0)
flex_75     <- simruns(M, 0.75, 1,1,1,.25, .5,0)
flex_100    <- simruns(M, 1, 1,1,1,.25, .5,0)

delay6_0      <- simruns(M, 0, 1,1,1,.25, .5,6)
delay6_25     <- simruns(M, 0.25, 1,1,1,.25, .5,6)
delay6_50     <- simruns(M, 0.5, 1,1,1,.25, .5,6)
delay6_75     <- simruns(M, 0.75, 1,1,1,.25, .5,6)
delay6_100    <- simruns(M, 1, 1,1,1,.25, .5,6)

delay12_0      <- simruns(M, 0, 1,1,1,.25, .5,12)
delay12_25     <- simruns(M, 0.25, 1,1,1,.25, .5,12)
delay12_50     <- simruns(M, 0.5, 1,1,1,.25, .5,12)
delay12_75     <- simruns(M, 0.75, 1,1,1,.25, .5,12)
delay12_100    <- simruns(M, 1, 1,1,1,.25, .5,12)

flex           <- rbind(flex_0, flex_25, flex_50, flex_75, flex_100)
delay6         <- rbind(delay6_0, delay6_25, delay6_50, delay6_75, delay6_100)
delay12        <- rbind(delay12_0, delay12_25, delay12_50, delay12_75, delay12_100)

sens_results   <- rbind(flex, delay6, delay12)
sens_results$EnemyEffectivenss <- rep(c(0, .25, .5, .75, 1), each =4) 
sens_results$scenario <- rep(c("Flexibility", "6 Month Delay", "12 Month Delay"), each = 20)
sens_results$scenario <- factor(sens_results$scenario, levels = c("Flexibility", "Flex with Design", "6 Month Delay","12 Month Delay"))

totalvehicles                      <- 1000
sens_results$ENPC                  <- sens_results$ENPC * totalvehicles
sens_results   <- as.data.frame(sens_results)

sens_results$diff  <- ifelse(sens_results$scenario == "Flexibility", sens_results$ENPC - sens_results$ENPC, 
                             ifelse(sens_results$scenario == "6 Month Delay", sens_results$ENPC - lag(sens_results$ENPC, n = 20L), 
                                    ifelse(sens_results$scenario == "12 Month Delay", sens_results$ENPC - lag(sens_results$ENPC, n = 40L), NA)))
                             
eeplot <- ggplot(subset(sens_results, armortype == 2 | armortype == 3), aes(x = scenario, y = diff, group = as.factor(armortype), color = as.factor(armortype))) + 
  geom_line(aes(color = as.factor(armortype), linetype = as.factor(armortype)), size = 1) + 
  ggtitle('Impact of Delays') + 
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5)) + 
  labs(x = "Scenario", y = "Expected Net Present Cost ($M)", face = "bold") + 
  theme(legend.position="none") + 
  scale_color_grey(end = .5) + 
  facet_grid(~EnemyEffectivenss) +
  ylim(0, 1000) 

eeplot
