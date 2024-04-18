
# Estimate species composition of catches associated with each habitat

library(tidytext)

traps <- read.csv("../dat/traps_catch.csv")  

spp_catch <-  traps %>%
  filter(gear_code == "FIXS") %>%
  group_by (year, species,per_association, quantile) %>%
  dplyr:: summarise(catch_kg = sum(catch_kg,na.rm = TRUE)) %>%  # sum across operations
  mutate(catch_prop = catch_kg * (per_association/100)) %>%
  group_by(quantile, species) %>%
  summarise(tot = mean(catch_prop,na.rm=T)) %>% # average across years
  filter (!is.na(tot)) %>%                      # remove species with no habitat association 
  mutate (species=factor(species), quantile = factor(quantile)) %>%
  droplevels()

spp_comp <- spp_catch %>%
  group_by(quantile) %>%
  mutate(hab_tot = sum(tot)) %>%
  ungroup() %>%
  group_by(quantile, species) %>%
  mutate(prop = tot/hab_tot*100) %>%
  arrange(quantile, desc(prop)) %>%
  ungroup() %>%
  group_by(quantile) %>%
  dplyr::mutate(csum = cumsum(prop)) %>%
  as.data.frame()

levels(spp_comp$quantile) <- c("Coral", "Rocky", "Mixed" , "Macroalgal" )  

#########################################################
# Add species prices
spp_price <- read.csv("../dat/spp_price.csv")
spp_comp_price <- spp_comp %>%
  left_join(spp_price) 

# Number of species in 95% of the catch
species_95 <- spp_comp %>%
  filter(csum < 96) %>%
  group_by(quantile) %>%
  summarise(n_spp = length(csum))

###################################################################################
 # Shannon diversity (based on proportion of each species contribution to biomass)

H <- spp_comp %>%
  mutate(div = prop * log(prop)) %>%
  group_by(quantile) %>%
  summarise(H = -sum(div, na.rm=T))


diversity_text <- data.frame(x = 20, y = c(10, 13, 22.5, 30),
                     quantile = as.factor(c("Coral", "Rocky", "Mixed" , "Macroalgal" )),
                      text=c("H' = -207 \nn95 = 16", "H' = -222 \nn95 = 16", "H' = -268 \nn95 = 11","H' = -331 \nn95 = 7"))

# Set genus as initials
spp_comp$species <- sub("[^A-Z].+? ", ". ", spp_comp$species)
plot(1:10, main = expression(('Mapped territories of different '*italic(C.~austriacus))))

spp_comp %>%
  group_by(quantile) %>%
  ungroup() %>%
  mutate(quantile =as.factor(quantile),
         species = reorder_within(species, prop, quantile)) %>%
  ggplot(aes(species,
             prop,fill=quantile)) + 
  geom_bar(stat = "identity")  +
  geom_vline(data = species_95, mapping = aes(xintercept = 40.5 - n_spp), linetype="dotted") +
  geom_text(data = diversity_text, aes(x=x, y=y, label=text),
             hjust=0, size=5, show.legend=FALSE) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~quantile,  scales = "free") +   
  coord_flip() +
  scale_x_reordered() +
  ylab("Species composition by habitat (%)") +
  xlab("Species") +
  theme(legend.position = "none") +
  scale_fill_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) 

