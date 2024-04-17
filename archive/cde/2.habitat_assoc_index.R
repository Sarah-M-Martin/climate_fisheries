################################################################################

# Link UVC benthic and fish datasets to analyse species-habitat associations

library(tidyverse)
library(ggridges)

# Load PC1 by site
load("../dat/Benthic_PC1.Rdata")

# Load UVC species by site
load("../dat/fish_overlap.Rdata")

################################################################################

# Ridgeline plot of species density along PC1 by functional group

fish_overlap %>%
  mutate(FG.fine = fct_reorder(FG.fine, PC1)) %>%
  ggplot(aes(x = PC1, y = FG.fine, fill = FG.fine)) +
  geom_density_ridges() +
  geom_vline(xintercept = -0.80788161, linetype  = "dashed", colour = "grey") +
  geom_vline(xintercept = -0.04131737, linetype  = "dashed", colour = "grey") +
  geom_vline(xintercept = 1.19393367, linetype  = "dashed", colour = "grey") +
  theme_bw() +
  theme(legend.position = "none",   panel.border = element_blank()) +
  scale_fill_viridis_d(option = "viridis") +
  xlab("PC1") +
  ylab("Functional Group")

################################################################################
# add sample size

fish_overlap <- fish_overlap %>% 
                  group_by(Species) %>% 
                    mutate(species_n = paste(Species,"n =",n()))
                      
################################################################################
# Extract PC1 quantiles 
################################################################################

# Set quantiles by distribution of individuals  (same number of individuals in each bin)

fish_overlap$quantile_2 <- ntile(fish_overlap$PC1,4)  # 4 evenly distributed bins based on the number of fish in each PC1 category

# find boundary values

boundaries <- c(
min (fish_overlap[which(fish_overlap$quantile_2 == 1),"PC1"]), max(fish_overlap[which(fish_overlap$quantile_2 == 1),"PC1"]),
max(fish_overlap[which(fish_overlap$quantile_2 == 2),"PC1"]),
max(fish_overlap[which(fish_overlap$quantile_2 == 3),"PC1"]),
max(fish_overlap[which(fish_overlap$quantile_2 == 4),"PC1"]))

################################################################################
# Check % coral cover for each quantile grouping
################################################################################

fish_overlap$coral   <- benth_PCA$hard.coral[match(fish_overlap$site, rownames(benth_PCA))]
fish_overlap$macroalgae  <- benth_PCA$macroalgae[match(fish_overlap$site, rownames(benth_PCA))]
fish_overlap$complexity  <- benth_PCA$complexity [match(fish_overlap$site, rownames(benth_PCA))]

# Plot % coral cover by quantile

# Quantile method 2
 colour <- c("steelblue4","steelblue",   "seagreen3",  "seagreen4")

# Explore density of habitats
# Group sites by PC1
 
site_summary <- fish_overlap %>% 
  group_by(site) %>%
  summarise(coral_cover = median(coral), algal_cover = median(macroalgae), 
            complex = median (complexity), PC1 = median (as.numeric(PC1))) 

site_summary$quant <- NA

for(i in 1:length(site_summary$PC1)){
ifelse(site_summary$PC1[i] <= boundaries[2], site_summary$quant[i]  <- 1,
       ifelse(site_summary$PC1[i]  > boundaries[2] & site_summary$PC1[i]  <= boundaries[3],site_summary$quant[i]  <- 2,
              ifelse(site_summary$PC1[i]  > boundaries[3] & site_summary$PC1[i]  < boundaries[4],site_summary$quant[i]  <- 3,
                     ifelse(site_summary$PC1[i]  >= boundaries[4], site_summary$quant[i]  <- 4,
                            "error"))))
                      }

site_summary$quant <- with(site_summary, factor(quant, 
                                                levels = c(1,2,3,4), labels = c("coral", "rocky", "mixed","macroalgae")))


# Plot habitat characteristics
colour <- c("steelblue4","steelblue",   "seagreen3",  "seagreen4")

hab_1 <- site_summary %>%
  filter(quant == "coral") %>%
  dplyr:: select(-"complex") %>%
  pivot_longer(c(coral_cover,algal_cover)) %>% 
  ggplot( aes(x = name, y = value, fill = name)) +
  geom_boxplot() +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.5)) +
  theme_bw() +
  scale_fill_manual(values=rep(colour[1],2)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=c('Algal cover', 'Coral cover')) +
  coord_flip(ylim = c(0, 80)) +
    labs(x = "",
       y = "'Coral habitats' (% cover)") +
  theme(strip.background = element_blank())

hab_2 <- site_summary %>%
  filter(quant == "rocky") %>%
  dplyr:: select(-"complex") %>%
  pivot_longer(c(coral_cover,algal_cover)) %>% 
  ggplot( aes(x = name, y = value, fill = name)) +
  geom_boxplot() +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.5)) +
  coord_flip(ylim = c(0, 80)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) + # suppress axis labels in all but final plot
  scale_fill_manual(values=rep(colour[2],3)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=c('Algal cover','Coral cover')) +
  labs(x = "",
       y = "'Rocky habitats' (% cover)") +
  theme(strip.background = element_blank())

hab_3 <- site_summary %>%
  filter(quant == "mixed") %>%
  dplyr:: select(-"complex") %>%
  pivot_longer(c(coral_cover,algal_cover)) %>% 
  ggplot( aes(x = name, y = value, fill = name)) +
  geom_boxplot() +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.5)) +
  coord_flip(ylim = c(0, 80)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) + # suppress axis labels in all but final plot
  scale_fill_manual(values=rep(colour[3],3)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=c('Algal cover', 'Coral cover')) +
  labs(x = "",
       y = "'Mixed habitats' (% cover)") +
  theme(strip.background = element_blank())

hab_4 <- site_summary %>%
  filter(quant == "macroalgae") %>%
  dplyr:: select(-"complex") %>%
  pivot_longer(c(coral_cover,algal_cover)) %>% 
  ggplot( aes(x = name, y = value, fill = name)) +
  geom_boxplot() +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.5)) +
  coord_flip(ylim = c(0, 80)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) + # suppress axis labels in all but final plot
  scale_fill_manual(values=rep(colour[4],3)) +
  theme(legend.position = "none") +
  scale_x_discrete(labels=c('Algal cover','Coral cover')) +
  labs(x = "",
       y = "'Macroagal habitats' (% cover)") +
  theme(strip.background = element_blank())


################################################################################
# Allocate fish species to quantiles by selecting the bin in which the majority of observations fall

getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

fish_overlap <- fish_overlap %>% 
       group_by(Species) %>% 
        mutate(mode = getmode(quantile_2))

####################################################################################
# start here for final plots
####################################################################################

# Distribution of all data points (fish) along the PC1 gradient
PC1_vals <- unique(fish_overlap$PC1)[order(unique(fish_overlap$PC1))]
index <- rep(1,123)
sampling <- as.data.frame(cbind(index,PC1_vals))
sampling$Site <- "Site"

#################

 p1_colour_quant <- fish_overlap %>%
  ggplot(aes(x = PC1, 
             y = fct_reorder(fct_reorder(species_n, PC1,.desc = T),mode,.desc=T), 
             fill = as.factor(mode))) + 
  geom_density_ridges(scale = 2, rel_min_height = 0.01) +
  xlim(-4, 6) +
  theme_minimal() +
  scale_fill_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) +
  theme(plot.margin = unit(c(0,1,0,1),"cm") , 
        axis.ticks = element_blank(),
        axis.text.x = element_blank()) + 
  theme(legend.position = "none") +
  xlab("") +
  ylab("") +
  labs(fill = "Habitat") 

####################

p2 <- ggplot(sampling,aes( x = PC1_vals, y = Site) )+
  geom_point(shape=73) +
  xlim(-4, 6) +
  xlab("PC1") +
  ylab("") +
  theme_minimal() +
  theme( 
    axis.ticks = element_blank(),
    axis.title.y = element_text(angle = 0, vjust = 0.5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(-0.5,1,0,1),"cm") )

########################

library(cowplot)

plot_a <- plot_grid(hab_1,hab_2,hab_3,hab_4,ncol=1, labels = "a") #labels = "auto"

plot_b <- plot_grid(p1_colour_quant, p2,ncol=1, rel_widths = c(1,1.5), rel_heights = c(6,.5), align = "v",labels="b")

grid.arrange(grobs=list(plot_a, plot_b),
             nrow=1, ncol=5,
             layout_matrix=cbind(c(1),
                                 c(1),
                                 c(2),
                                 c(2),
                                 c(2)) )
