
#######################################################################
# cpue by habitat type
#######################################################################

library(tidyverse)

traps <- read.csv("../dat/traps_catch.csv") # catch by species 

########### Sum CPUE across species to get total CPUE per set #############

traps_sets <- traps %>% 
  group_by(operation_id, quantile) %>%
  mutate(tot_n_spp_set = length(species )) %>%
  mutate(n_spp_set = length(na.omit(habitat_assoc_PC1))) %>% 
  filter(n_spp_set != 0) %>% #  Remove sets with no habitat association information 
  group_by(trip_id, operation_id, year, month,  area, 
           location_code, gear_code, tar_spp, vessel_group, quantile,n_spp_set, tot_n_spp_set) %>%
  dplyr::summarise(set_cpue = sum(cpue), set_hab_assoc = mean(per_association,na.rm=T)) %>%     # set association = average of all species in set
          drop_na(trip_id, operation_id, year, month, area, location_code,gear_code,tar_spp, vessel_group, quantile, set_cpue, set_hab_assoc) %>% 
  filter(set_cpue!=0, !is.infinite(set_cpue))  %>%
  group_by(gear_code, quantile) %>% 
    mutate(set_cpue_95 = ifelse(set_cpue > quantile(set_cpue, 0.95), NA, set_cpue)) %>%   # drop outliers by gear type
     drop_na(set_cpue_95) 
 
  # Save file of set level information

   # traps_sets <- traps_sets %>%
   #   filter(gear_code == "FIXS")

  write.csv(traps_sets,"../dat/traps_sets.csv", row.names = FALSE)

###################################################################
# Weighted CPUE by habitat to get overall MEAN CPUE for each habitat 
####################################################################

# Weight habitat association to sum to 1 across each habitat type

traps_sets_FIXS <- traps_sets %>%
  filter(gear_code == "FIXS") %>%
  group_by (quantile) %>%         # weighting by habitat
  mutate (hab_weighted_assoc = set_hab_assoc / sum(set_hab_assoc)) %>%
  mutate (hab_weighted_cpue = set_cpue_95  * hab_weighted_assoc) 

##########################################

# Find weighted means

traps_sets_FIXS %>%
  group_by(quantile) %>%
  summarise(tot = weighted.mean(set_cpue, set_hab_assoc) )

# Bootstrap to find standard error of the weighted mean 

library(boot)

samplewmean <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  return(weighted.mean(d, w))   
}

# create empty dataframe
res <- data.frame(Habitat = c(1,2,3,4),
                  wmean = c(NA,NA,NA,NA),
                  s_e = c(NA,NA,NA,NA))

# Calculate bootstrapped weighted mean and SE for each quantile
for (i in 1:4){
  dat <- traps_sets_FIXS %>%
    filter(quantile == i)
  
  results <- boot(data = dat[, "set_cpue", drop = FALSE], 
                       statistic = samplewmean, 
                       R=10000, 
                       j = dat[,"set_hab_assoc", drop = FALSE])
  
  res[i,2] <- results$t0
  res[i,3] <- sd(results$t)
  
}


##########################################
# Plot means as dotplot
##########################################

habitats <-c("Coral",      "Rocky"   ,   "Mixed" ,     "Macroalgal")

 cpue_plot_FIXS <- res %>%
  ggplot(aes(x = as.factor(Habitat),
             y = wmean, color = as.factor(Habitat)))+
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = wmean - s_e, ymax = wmean + s_e, width = 0.2)) +
  scale_color_manual(values = c(  "steelblue4",
                                  "steelblue", 
                                  "seagreen3",  
                                  "seagreen4") ) +
  theme(axis.ticks=element_blank(),
  panel.background = element_rect(fill = 'white', color = 'grey'),) +
  ylab(bquote(cpue~(kg ~ trap^-1))) +
  xlab("")+
  scale_x_discrete(labels = habitats) +
  theme(legend.position = "none")

#############################################################
# Plot
#############################################################

# Load total catch
FIXS_catch_prop <- readRDS("../dat/FIXS_catch_prop.rds")

# Load revenue 
wmean_set_rev <- readRDS("../dat/w_mean_setrev.rds")

# Plot together
library(ggpubr)
ggarrange(cpue_plot_FIXS,FIXS_catch_prop,wmean_set_rev,labels = "auto", ncol=3)

