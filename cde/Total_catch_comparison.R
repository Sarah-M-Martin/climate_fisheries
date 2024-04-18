
library(tidyverse)

traps <- read.csv("../dat/traps_catch.csv")  
traps_sets <- read.csv("../dat/traps_sets.csv") 

#########################################################################
# Plot mean annual catches by habitat association
#########################################################################

# Sum catches by species and year 
tot_catch <-  traps %>%
  filter(gear_code == "FIXS") %>%
  filter(operation_id %in% traps_sets$operation_id) %>%   # filter for sets within the cleaned dataset 
  group_by (year, species, per_association, quantile) %>%
  dplyr:: summarise(catch_kg = sum(catch_kg, na.rm = TRUE))  # sum across operations

# Proportion of catch associated with each habitat

rel_catch <- tot_catch  %>%   
  mutate(catch_prop = catch_kg * (per_association/100)) %>%
  group_by(quantile, year) %>%
  summarise(tot = sum(catch_prop, na.rm = T)) %>%  # sum across species
  group_by(quantile) %>% 
  summarise(st_dev = sd(tot), annual_mean = mean(tot)) %>% # average catch across years  
  mutate(prop = annual_mean/sum(annual_mean) * 100, s_e = (st_dev /sqrt(4)), prop_se = s_e/annual_mean*prop)
         
############## Plot with error bars ##############

habitats <- c("Coral","Rocky","Mixed","Macroalgal")

rel_catch  %>%
  ggplot(aes(as.factor(quantile) ,
             prop, 
             fill = as.factor(quantile)  )) +
  geom_col() +
  geom_errorbar(aes(ymax = prop + prop_se, ymin=ifelse(prop - prop_se >= 0, prop - prop_se, 0)), width=0.1) + 
  scale_fill_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) +
  tidytext:: scale_x_reordered() +
  theme(axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'white', color = 'grey'),) +
  theme(legend.position = "none") +
  scale_x_discrete(labels = habitats) +
  xlab("")+
  labs(y = "Mean annual catch (%) 2017-2020")


##########################################################################################
