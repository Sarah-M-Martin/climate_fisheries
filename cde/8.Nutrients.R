
# Compare nutrient content of sets from each habitat type based on nutrient prediction models and catch composition data

library(janitor)
library(fmsb)
library(reshape2)

# Read in nutrient predictions for Ca, Fe, Omega 3, Se, Zn
Nutrients <- read.csv('../dat/Nut_pred_SEZ.csv') %>%
  clean_names() %>%
  mutate(species  = replace(species , species =="Plectorhinchus vittatus", "Plectorhinchus orientalis")) %>% # replace synonym
  dplyr:: select(species,habitat_dummy,mu,nutrient) %>%
  mutate(mu = mu*10) %>% # convert to /kg
  mutate(
    habitat_dummy = replace(habitat_dummy, habitat_dummy == 1,4),   # change habitat labels to match rest of data
    habitat_dummy = replace(habitat_dummy, habitat_dummy  == 0,1), 
      ) %>%
  spread(nutrient,mu)

# Species in habitats 2 & 3 allocated same nutrient content as habitat 1 (no macroalgal enrichment)

Nutrients_wide <- Nutrients %>%
  filter(habitat_dummy==1) %>%
  mutate(habitat_dummy = replace(habitat_dummy, habitat_dummy==1,2)) %>%
  bind_rows(Nutrients,.) 
  
Nutrients_wide <- Nutrients_wide %>%
  filter(habitat_dummy==1) %>%
  mutate(habitat_dummy = replace(habitat_dummy, habitat_dummy==1,3)) %>%
  bind_rows(Nutrients_wide,.)
  
colnames(Nutrients_wide)[2] <- "set_quantile"

##################################################################################

# Read in nutrient predictions for Vit A and protein (FishBase)
Nutrients_FB <- read.csv('../dat/Nut_pred_global.csv') %>%
  clean_names()%>%
  mutate(scientific_name  = replace(scientific_name, scientific_name  =="Plectorhinchus vittatus", "Plectorhinchus orientalis")) %>%
  rename("species" = "scientific_name")

Nutrients_wide <- Nutrients_wide %>% 
  left_join(Nutrients_FB[,c("species","vitamin_a_mg_100g","protein_g_100g")], by = 'species') %>%
  mutate(vit_a = vitamin_a_mg_100g *10) %>%  # convert to / kg
  mutate(protein = protein_g_100g*10) %>%  # convert to / kg
  dplyr::select(-c(vitamin_a_mg_100g, protein_g_100g))

##################################################################################

# Link to CAS data
catch <- read.csv("../dat/catch_habitat.csv")
traps_cpue_sets <- read.csv("../dat/traps_cpue_sets.csv")

# Use consistent filtering 
catch_filtered <- filter(catch, operation_id %in% traps_cpue_sets$operation_id) %>%
                 dplyr:: select(operation_id, species, cpue, set_quantile, gear_code)

cpue_nut <- left_join(catch_filtered,Nutrients_wide, by = c("species", "set_quantile"))
head(cpue_nut)

# Select static traps
cpue_nut_static <- cpue_nut %>%
  filter(gear_code== "FIXS")

nut_set <- cpue_nut_static %>% 
  mutate(across(Ca:protein, ~ .x * cpue))

# sum across species for total per operation
nut_tot_set <- nut_set %>% 
  dplyr:: select(-species, -gear_code) %>%
  group_by(operation_id,set_quantile) %>% 
  dplyr:: summarise(across(everything(),sum,na.rm=T))

#############################################################################
# Daily nutrient intake by habitat type
#############################################################################

# https://www.fao.org/fishery/en/facp/syc?lang=en
# With 58.9 kg of fish/caput/year (2019), Seychelles is among the highest per-capita-fish-consuming countries in the world. 
# 161g per day

nut_habitat_day <-  nut_tot_set %>%   
  mutate(across(cpue:protein, ~ .x /cpue * 0.161)) %>%   # scale all nutrients to daily consumption amount
  group_by(set_quantile) %>% 
  summarise(across(cpue: protein,
                   mean,   # average across all operations
                   na.rm = TRUE)) %>%
                    as.data.frame()

mx <- nut_habitat_day %>%
  summarise(across(Ca:protein, max))
mn <- nut_habitat_day %>%
  summarise(across(Ca:protein, min))

nut_habitat_day <- nut_habitat_day[,-c(1,2)] 
nut_habitat_day <- rbind(mx,mn,nut_habitat_day)
rownames(nut_habitat_day) <- c("max","min",1,2,3,4)

################################################################
# Plot difference in nutrient content among habitats
################################################################

# load plotting function
source('Radar_chart_func.R')

colours <- c("steelblue4",
             "steelblue", 
             "seagreen3",  
             "seagreen4")

titles <- c("Coral", "Rocky", "Mixed", "Macroalgal")

# Radar chart
par(xpd = TRUE, mfrow = c(1, 4), mar = c(0, 1, 1, 1)) 
for(i in 1:4){
  create_beautiful_radarchart(
    data = nut_habitat_day[c(1, 2, i+2), ], caxislabels = c("","","","",""),
    color = colours[i], title = titles[i]
  )
}

################################################################
# Plot as proportion of RNI 
################################################################

# Ca(mg), Fe(mg), Om3(g), Se(ug), Zn(mg), VitA (ug), protein(g)
RNI <- c(1000,13.7,1.1,34,7,600,56)          # males 19 - 65 (WHO, 2004) and (National Academies, 2006)
nut_habitat_day <- rbind(RNI,nut_habitat_day)

rownames(nut_habitat_day) <- c("RNI","max","min", "1", "2","3","4")
colnames(nut_habitat_day) <- c("calcium","iron","omega 3", "selenium", "zinc","vit A","protein")

nut_habitat_day <- nut_habitat_day %>%
  arrange(factor(rownames(nut_habitat_day),c("max","min","RNI", "1", "2","3","4"))) 

prop_RNI   <-   nut_habitat_day %>% 
  slice(3:7) %>%
  mutate(across("calcium",~.x/nut_habitat_day["RNI",1] *100)) %>%
  mutate(across("iron",~.x/nut_habitat_day["RNI",2] *100)) %>%
  mutate(across("omega 3" ,~.x/nut_habitat_day["RNI",3] *100))%>%
  mutate(across("selenium" ,~.x/nut_habitat_day["RNI",4] *100)) %>%
  mutate(across("zinc" ,~.x/nut_habitat_day["RNI",5] *100)) %>%
  mutate(across("vit A"  ,~.x/nut_habitat_day["RNI",6] *100)) %>%
  mutate(across("protein" ,~.x/nut_habitat_day["RNI",7] *100))

prop_RNI$habitat <-  c("RNI","Coral", "Rocky", "Mixed", "Macroalgal")
prop_RNI$selenium[which(prop_RNI$selenium > 100)] <- 100 # Cap selenium at a maximum of 100%

# Cap total nutrients at 100% 
tot_100 <- prop_RNI %>% 
  slice(2:5) %>%
  gather(variable, value, -habitat) %>% 
  mutate(percentage = value/7) %>%  # percentage across all 7 nutrients
dplyr:: select(-value) %>% 
  spread(variable, percentage)

cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# # Stacked barchart of total nutrients 
tot_100 %>%
  melt(id.vars=c("habitat")) %>%
  ggplot(aes(x=habitat,y=value,fill=variable)) +
  geom_bar(stat="identity", width=0.7)+
  coord_flip() +
  theme_minimal() +
  scale_x_discrete(limits = rev(titles)) +
  theme(legend.title=element_blank()) +
  labs(x = "",
       y = "% contribution to RNI") +
  scale_fill_manual(values=cbPalette)

