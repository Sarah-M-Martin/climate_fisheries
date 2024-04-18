
# Estimate nutrient content of fish associated with each habitat

pacman::p_load(dplyr, tidyr, readxl, janitor, factoextra, gridExtra, reshape2, install=T)


# Load nutrient data for Ca, Fe, Se, Zn and Omega-3 by habitat type
Nutrients <- read.csv('../dat/Nutrients_Robinson2022.csv') %>%
  clean_names() %>%
  mutate(species  = replace(species , species =="Plectorhinchus vittatus", "Plectorhinchus orientalis")) %>% # synonym
  select(species,habitat_dummy,mu,nutrient) %>%
  mutate(
    habitat_dummy = replace(habitat_dummy, habitat_dummy == 1,4),  # adjust habitat labels
    habitat_dummy = replace(habitat_dummy, habitat_dummy  == 0,1), 
  )

Nutrients_wide <- Nutrients %>%
  spread(nutrient,mu)

# add habitats 2 & 3 (no macroalgal enrichment, same as habitat 1)

Nutrients_wide <- Nutrients_wide %>%
  filter(habitat_dummy==1) %>%
  mutate(habitat_dummy = replace(habitat_dummy, habitat_dummy==1,2)) %>%
  bind_rows(Nutrients_wide,.) 

Nutrients_wide <- Nutrients_wide %>%
  filter(habitat_dummy==1) %>%
  mutate(habitat_dummy = replace(habitat_dummy, habitat_dummy==1,3)) %>%
  bind_rows(Nutrients_wide,.)
colnames(Nutrients_wide)[2] <- "quantile"

# 18  species have same nutrient concentration estimates across all habitats - replicate rows for each habitat type
missing_spp <- as.factor(names(which(table(Nutrients_wide$species) == 3)))

Nutrients_wide <- Nutrients_wide %>%
  filter(species %in% missing_spp & quantile == 1) %>%
  mutate(quantile = replace(quantile, quantile==1,4)) %>%
  bind_rows(Nutrients_wide,.)

##################################################################################
##################################################################################

# Load nutrients dataset from FishBase for Vit A and protein

Nutrients_FB <- read.csv('../dat/Nutrients/Nutrients_FB.csv') %>%
  clean_names()%>%
  mutate(scientific_name  = replace(scientific_name, scientific_name  =="Plectorhinchus vittatus", "Plectorhinchus orientalis")) %>%
  rename("species" = "scientific_name", "vitamin_a_ug_100g"="vitamin_a_mg_100g", "selenium_ug_100g" = "selenium_mg_100g" )

# Units
# Ca(mg), Fe(mg), Om3(g), Se(ug), Zn(mg), VitA (ug), protein(g)

# Join datasets
Nutrients_wide <- Nutrients_wide %>% 
  left_join(Nutrients_FB[,c("species","vitamin_a_ug_100g","protein_g_100g")], by = 'species')  %>%
  rename(vit_a = vitamin_a_ug_100g, protein = protein_g_100g) 

Nutrients_wide %>%
  filter(quantile == 1) %>%
  gather(variable, value, -species,-quantile) %>%
  ggplot(aes(x=species,y=value,fill=species)) +
  geom_bar(stat="identity", width = 0.7) +
  facet_wrap(~variable, scales = "free_y") + 
  coord_flip() +
  theme_minimal() +
  theme(legend.position = "none") +
 labs(x = "Nutritional value per 100g",
     y = "")

##################################################################################
##################################################################################
# Link nutrient data to species composition habitat data

spp_comp <- read.csv("../dat/spp_comp.csv") 

spp_comp$quantile <- as.factor(spp_comp$quantile)
levels(spp_comp$quantile) <- c("Coral", "Rocky", "Mixed" , "Macroalgal" ) 
 
Nutrients_wide$quantile <- as.factor(Nutrients_wide$quantile)
levels(Nutrients_wide$quantile) <- c("Coral", "Rocky", "Mixed" , "Macroalgal" )  

spp_comp_nut <- left_join(spp_comp,Nutrients_wide, by = c("species", "quantile"))
# nutrient conc per 100g of fish

################################################################################
# Scale to daily consumption amount 
################################################################################

# https://www.fao.org/fishery/en/facp/syc?lang=en (FAO 2017)
# With 58.9 kg of fish/caput/year (2017), Seychelles is among the highest per-capita fish-consuming countries in the world. 
# 161g per day

daily_cons <- spp_comp_nut %>% 
  mutate(daily_g = (prop/100)*161)  %>% 
  mutate(across(Ca:protein, ~ .x /100* daily_g )) # nutrient values by species per 161g of fish daily consumption quantity

hab_nutrient <- daily_cons %>% 
  select(-csum,-species,-tot,-hab_tot,-prop) %>%
  group_by(quantile) %>% 
  dplyr:: summarise(across(everything(),sum,na.rm=T))  # Total nutrient values by habitat

################################################################################
### Radar chart - relative scale
################################################################################

mx <- hab_nutrient %>%
  dplyr::  summarise(across(Ca:protein, max))
mn <- hab_nutrient %>%
  dplyr::  summarise(across(Ca:protein, min))

hab_nutrient <- hab_nutrient[,-c(1,9)] 
hab_nutrient <- rbind(mx,mn,hab_nutrient)  
hab_nutrient <- as.data.frame(hab_nutrient)
rownames(hab_nutrient) <- c("max", "min", "Coral", "Rocky", "Mixed", "Macroalgal")


library(fmsb)
source('Radar_chart_func.R')
names(hab_nutrient) <- c("calcium", "iron",  "omega 3", "selenium","zinc","vit A","protein")
colours <- c("steelblue4",
             "steelblue", 
             "seagreen3",  
             "seagreen4"  )

titles <- c("Coral", "Rocky", "Mixed", "Macroalgal")

windows()
par(mfrow = c(1,4))
for(i in 1:4){
  create_beautiful_radarchart(
    data = hab_nutrient[c(1, 2, i+2), ], caxislabels = c(),
    color = colours[i], title = titles[i]
  )
}

################################################################
# plot relative to RNI 
################################################################

# Ca(mg), Fe(mg), Om3(g), Se(ug), Zn(mg), VitA (ug), protein(g)
RNI <- c(1000,13.7,1.1,34,7,600,56)          # males 19 - 65 based mostly on WHO 2004 and National academies 2006
hab_nutrient <- rbind(RNI,hab_nutrient)
rownames(hab_nutrient)[1] <- "RNI"

hab_nutrient <- hab_nutrient %>%
  arrange(factor(rownames(hab_nutrient),c("max","min","RNI","Coral", "Rocky", "Mixed", "Macroalgal")))  #rearrange

radar_plots <- hab_nutrient

prop_RNI   <-   hab_nutrient %>% 
  slice(3:7) %>%
  mutate(across("calcium",~.x/hab_nutrient["RNI",1] *100)) %>%
  mutate(across("iron",~.x/hab_nutrient["RNI",2] *100)) %>%
  mutate(across("omega 3" ,~.x/hab_nutrient["RNI",3] *100))%>%
  mutate(across("selenium" ,~.x/hab_nutrient["RNI",4] *100)) %>%
  mutate(across("zinc" ,~.x/hab_nutrient["RNI",5] *100)) %>%
  mutate(across("vit A"  ,~.x/hab_nutrient["RNI",6] *100)) %>%
  mutate(across("protein" ,~.x/hab_nutrient["RNI",7] *100))

########################################################################

prop_RNI <- prop_RNI[-1,]

# Cap nutrients at a maximum of 100 %
prop_RNI$selenium [which(prop_RNI$selenium > 100)] <- 100
prop_RNI$habitat  <- factor(rownames(prop_RNI) , levels=c(  "Macroalgal",  "Mixed" , "Rocky" , "Coral" ))

####################

library(grid)
library(cowplot)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

Final_barplot <- prop_RNI %>%
  melt(id.vars=c("habitat")) %>%
  ggplot(aes(x=habitat,y=value,fill=variable)) +
  geom_bar(stat="identity", width=0.7)+
  coord_flip() +
  theme_minimal() +
  theme(legend.title=element_blank()) +
  labs(x = "",
       y = "% contribution to RNI") +
  scale_fill_manual(values=cbPalette)

Final_barplot_label <- plot_grid(Final_barplot, labels = "b")

#####################################################################################
# plot together

windows()
par(xpd = TRUE, mfrow = c(2, 4), mar = c(0, 1, 1, 1)) 

#coral
create_beautiful_radarchart(
  data = radar_plots[c(1, 2, 4), ], caxislabels = c("","","","",""),
  color = colours[1], title = titles[1]
)
mtext(expression(bold("a")), side=3, adj = 0, cex = 1.2)

#rocky
create_beautiful_radarchart(
  data = radar_plots[c(1, 2, 5), ], caxislabels = c("","","","",""),
  color = colours[2], title = titles[2]
)
#mixed 
create_beautiful_radarchart(
  data = radar_plots[c(1, 2, 6), ], caxislabels = c("","","","",""),
  color = colours[3], title = titles[3]
)
#macroalgal
create_beautiful_radarchart(
  data = radar_plots[c(1, 2, 7), ], caxislabels = c("","","","",""),
  color = colours[4], title = titles[4]
)

vp.BottomRight <- viewport(height=unit(.5, "npc"), width=unit(1, "npc"), 
                           just=c("left","top"), 
                           y=0.5, x=0)

print(Final_barplot_label, vp=vp.BottomRight)
