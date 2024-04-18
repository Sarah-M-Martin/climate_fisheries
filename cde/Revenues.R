

# Revenue per unit effort for static traps

################################################################################
################################################################################

library(tidyverse)
# add prices to catch data

traps <- read.csv("../dat/traps_catch.csv")
spp_price <- read.csv("../dat/spp_price.csv")

# Add mean price per kg to each species in catch dataset
traps$price_kg <- spp_price$mean_price [match(traps$species_label , spp_price$species_label)]

traps_rev <-  traps %>%
  mutate(revenue = price_kg * cpue) %>%
  group_by(operation_id, quantile) %>%
  summarise(set_cpue = sum(cpue), set_rev = sum(revenue))   

# filter for sets not included in traps_sets (already cleaned)
traps_sets <- read.csv("../dat/traps_sets.csv")
traps_sets_rev <- left_join(traps_sets,traps_rev, by = c("operation_id","quantile")) %>%
  drop_na(set_rev)%>%
  mutate(set_cpue = set_cpue.x) %>%
  select(-c(set_cpue.x,set_cpue.y))

################################################################################

# Weighted means
traps_sets_rev %>%
  group_by(quantile) %>%
  summarise(wmean_usd = weighted.mean(set_rev, set_hab_assoc) )

# Bootstrap to find standard error 

library(boot)

samplewmean <- function(d, i, j) {
  d <- d[i, ]
  w <- j[i, ]
  return(weighted.mean(d, w))   
}

# create empty dataframe
res_rev <- data.frame(Habitat = c(1,2,3,4),
                  wmean = c(NA,NA,NA,NA),
                  s_e = c(NA,NA,NA,NA))

# Calculate bootstrapped weighted mean and SE for each quantile
for (i in 1:4){
  samp <- traps_sets_rev %>%
    filter(quantile == i)
  
  results <- boot(data = samp[, "set_rev", drop = FALSE], 
                       statistic = samplewmean, 
                       R=10000, 
                       j = samp[,"set_hab_assoc", drop = FALSE])
  
  res_rev[i,2] <- results$t0
  res_rev[i,3] <- sd(results$t)
  
}

################################################################################
# Plot means with error bars
################################################################################

Habitat <- c("Coral","Rocky","Mixed","Macroalgal")

wmean_set_rev <- res_rev %>%
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
   ylab(bquote(Revenue ~(USD ~ trap^-1))) +
  xlab("")+
  scale_x_discrete(labels = Habitat) +
  theme(legend.position = "none")

ggsave("../res/w_mean_setrev.jpg")
saveRDS(wmean_set_rev, "../dat/w_mean_setrev.rds")

################################################################################
