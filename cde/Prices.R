##################
# Estimate mean species price per kg in USD

pacman::p_load(readxl, tidyr, dplyr, janitor, ggplot2, forcats, nlme, install=T)

####################
# Price data
####################

prices <- read.csv('../Packets.csv', encoding='latin1') %>%
  clean_names()

# => Select only packets containing one species

single_spp <- prices %>%
  filter(n==1) %>%      
  select(-c("avg_weight_price","quantite_indiv_vente")) %>% # remove empty cols
  mutate(avg_packaging_price = coalesce(avg_packaging_price,total_price), 
  no_packets = round(quantite_vente / 3.5,0), # add number of packets (average packet weight is ~ 3.5 kg)
  ppkg =  avg_packaging_price / 3.5) # add price per kg
         
# 75% of trips result in single species packets

Species_overlap <- read.csv('../dat/Species_match.csv', encoding='latin1') # species codes

# Select only species in UVC dataset
reef_species <- Species_overlap[(Species_overlap$UVC_matches > 0),"species_code"]
single_spp$reef <- NA

for (i in 1: length(single_spp$reef)){
  ifelse((single_spp$esp_cod_fao[i] %in% reef_species),
         single_spp$reef[i] <- 1, 
         single_spp$reef[i] <- 0)
}

#################################################################################
# Plot price by species
#################################################################################

# 2017-2020 exchange rate of 0.073 (www.xe.com)

single_spp$USDppkg <- single_spp$ppkg * 0.073
single_spp %>%
  filter(reef==1) %>%
  mutate(species = fct_reorder(species,USDppkg,mean,na.rm=T)) %>%
  ggplot(aes(species,USDppkg, fill = "grey")) +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.35)) +
  geom_boxplot() + 
  theme_bw() +
  coord_flip() +
  ylab(bquote(Price~ (USD ~ kg^-1))) + 
  xlab("") +
  theme(legend.position = "none") 

################################################################################

# Species mean prices per kg
spp_price <- single_spp %>%
  group_by(species_label) %>%
  summarise(mean_price = mean(USDppkg,na.rm=T)) %>%
   arrange(desc(mean_price)) %>%
  as.data.frame()

# add prices to species in the catch dataset
traps <- read.csv("../dat/traps_catch.csv")  

spp_price_latin <- traps %>% 
  select(species, species_label) %>%
  filter(!duplicated(species)) %>%
  left_join(spp_price)

# Give average genus price to those species missing a price
spp_price_latin <- spp_price_latin %>%
  mutate(genus = gsub( " .*$", "", species)) %>%
  mutate(genus = replace(genus, genus == "Lethrinidae" , "Lethrinus")) %>%
  group_by(genus) %>%
  mutate(genus_mean_price = mean(mean_price,na.rm=T)) %>% # add mean genus price
  mutate(mean_price = replace(mean_price, is.na(mean_price),genus_mean_price[is.na(mean_price)])) %>% # replace species missing a price with average genus price
  as.data.frame()

################################################################################

 # Plot price ranges by habitat

 ranges <- data.frame(minimum = NA, maximum = NA)
 
 spp_comp_95 <-  spp_comp %>%
   filter(csum < 96) %>%
   left_join(spp_price_latin) 
 
 for(i in 1:length(habitats)){
   tmp <- subset(spp_comp_95, spp_comp_95$quantile == habitats[i]) 
   ranges[i,"minimum"] <- range(tmp$mean_price)[1]
   ranges[i,"maximum"] <- range(tmp$mean_price)[2]
  }

 spp_comp_95$quantile <- factor(spp_comp_95$quantile, c("Coral", "Rocky", "Mixed" , "Macroalgal" ) )
 
 spp_comp_95 %>%
     ggplot(aes(quantile,mean_price)) + 
     geom_point(aes(size = prop, colour = factor(quantile)),position = position_jitter(w = 0.2, h = 0)) +
     scale_colour_manual(values = c("steelblue4",  "steelblue", "seagreen3", "seagreen4" )) + 
     coord_flip() +
     geom_segment(aes(x = 1, y = min(ranges$minimum [1]), xend = 1, yend = max(ranges$maximum[1])), col = "steelblue4", linewidth = 2) + # plot range lines
     geom_segment(aes(x = 2, y = min(ranges$minimum [2]), xend = 2, yend = max(ranges$maximum[2])), col = "steelblue",  linewidth = 2) + # plot range lines
     geom_segment(aes(x = 3, y = min(ranges$minimum [3]), xend = 3, yend = max(ranges$maximum[3])), col = "seagreen3",  linewidth = 2) + # plot range lines
     geom_segment(aes(x = 4, y = min(ranges$minimum [4]), xend = 4, yend = max(ranges$maximum[4])), col = "seagreen4",  linewidth = 2) + # plot range lines
     theme_bw() +
     ylab(bquote(Price~ (USD ~ kg^-1))) + 
     xlab("") +
     ylim(2,3) +
     theme(legend.position = "none") 

 