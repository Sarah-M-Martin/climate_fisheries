
# Explore species diversity of catches by habitat type

library(vegan)
library(tidyverse)

catch <- read.csv("../dat/catch_habitat.csv")

##################################################
# Caclulate species diversity by habitat type  
##################################################

Reefs_trap <- catch %>%
  filter(gear_code == "FIXS") %>%       #  static trap fisheries
  filter(set_quantile != "NA") %>%      # drop operations with unknown habitat (i.e. dominant species not in UVC dataset)
  group_by(operation_id,species_code) %>%
  summarise(catch_kg = sum(catch_kg)) %>%
  pivot_wider(names_from = species_code,values_from = catch_kg) %>%
  mutate_all(~replace(., is.na(.), 0)) 

H <- data.frame(matrix(NA,ncol=2,nrow=length(Reefs_trap$operation_id)))
colnames(H) <- c("operation_id","div")

# calculate Shannon's H Index of Diversity for each operation

H[,1] <- Reefs_trap$operation_id
H[,2] <- diversity(Reefs_trap[,-1], "shannon")

#######################################################################
# MOdel difference in diversity among habitat types
#######################################################################

# Including potential confounding covariates

traps_cpue_sets <- read.csv("../dat/traps_cpue_sets.csv")  # Cleaned, filtered and summed cpue by set

# link covariates by operation

H$year  <- traps_cpue_sets$year [match(H$operation_id, traps_cpue_sets$operation_id )]
H$month <- traps_cpue_sets$month [match(H$operation_id, traps_cpue_sets$operation_id )]
H$area  <- traps_cpue_sets$area [match(H$operation_id, traps_cpue_sets$operation_id )]
H$vessel_group <- traps_cpue_sets$vessel_group [match(H$operation_id, traps_cpue_sets$operation_id )]
H$location_code <- traps_cpue_sets$location_code [match(H$operation_id, traps_cpue_sets$operation_id )]
H$tar_spp <- traps_cpue_sets$tar_spp [match(H$operation_id, traps_cpue_sets$operation_id )]
H$cpue  <- traps_cpue_sets$cpue [match(H$operation_id, traps_cpue_sets$operation_id )]
H$set_quantile  <- traps_cpue_sets$set_quantile [match(H$operation_id, traps_cpue_sets$operation_id )]

H <- H %>%
  drop_na(year, month, area, location_code,tar_spp, vessel_group, cpue,div,set_quantile) %>% 
  filter(cpue!=0, !is.infinite(cpue)) 

 H[c("set_quantile","year","month","area","vessel_group")] <- lapply(H[c("set_quantile","year","month","area","vessel_group")], factor) 
 
 ####################################################################

 # Static trap diversity
  library(glmmTMB)

  MD1 <- glmmTMB(div ~  set_quantile 
                 + cpue
                 + year
                 + month
                 + vessel_group
                 + tar_spp
                 + (1|location_code/area), family= ziGamma(link="log"), ziformula=~1, data = H)
  
 # AIC(MD0,MD1)
 
  stats::drop1(MD1,test="Chisq") # remove CPUE 

   MD2 <- glmmTMB(div ~  set_quantile 
                 + year
                 + month
                 + vessel_group
                 + tar_spp
                 + (1|location_code/area), family= ziGamma(link="log"), ziformula=~1, data = H)

  summary(MD2)
  stats::drop1(MD2,test="Chisq")

  # assess residuals
  library(DHARMa)
  simulationOutput <- simulateResiduals(fittedModel = MD2, plot = F)
  hist(residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7)))
  plot(simulationOutput)
  
  par(mfrow=c(2,4))
  plotResiduals(simulationOutput, form = H$cpue)
  plotResiduals(simulationOutput, form = H$set_quantile) 
  plotResiduals(simulationOutput, form = H$year)
  plotResiduals(simulationOutput, form = H$month)
  plotResiduals(simulationOutput, form = H$vessel_group)
  plotResiduals(simulationOutput, form = H$tar_spp)
  plotResiduals(simulationOutput, form = H$location_code)
  plotResiduals(simulationOutput, form = H$area)
  
  # plot standardised diversity by habitat type
  
  habitats <- c("Coral", "Rocky",  "Mixed",  "Macroalgal")
  
  ggplot(data = H, aes(x=as.factor(set_quantile), y=fit, colour = as.factor(set_quantile))) + 
    geom_boxplot() +
    ylab("Shannon Diversity (H')") +
    xlab("" )+
    scale_x_discrete(labels = habitats) +
    scale_colour_manual(values = c(  "steelblue4",
                                     "steelblue", 
                                     "seagreen3",  
                                     "seagreen4") ) +
    geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.35)) +
    theme_bw() +
    theme(legend.position = "none") 
