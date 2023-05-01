

library("tidyverse")
library("factoextra")
library("gridExtra")

##############################################

load("../dat/Benthic_dat.Rdata")

benth_PCA <- benth_dat[,c("complexity","hard.coral","macroalgae")]

results.pca <- prcomp(benth_PCA, scale = T, center = T)

########################################################################################

# Plot PC1 v substrate variables

benth_PCA$PC1 <- results.pca$x[,"PC1"]

benth_PCA <- benth_PCA %>%
  mutate(quant = ifelse(PC1 < -0.808, "Coral",
                        ifelse(PC1 < -0.041, "Rocky",
                               ifelse(PC1 < 1.194, "Mixed", "Macroalgal"))))

benth_PCA$quant <- factor(benth_PCA$quant , levels=c("Coral" ,  "Rocky", "Mixed" , "Macroalgal"))

fviz(results.pca, 
     element="ind", 
     geom="point",  
     #point = 16,
     pointshape = 16,
     pointsize = 4, 
     habillage=benth_PCA$quant, 
     legend.title = "",
     #addEllipses=TRUE, ellipse.level=0.95, 
     palette = c("steelblue4","steelblue",   "seagreen3",  "seagreen4"),
     invisible="quali") 

# Plot against all variables
 
 benth_PCA$sand <- benth_dat$sand
 benth_PCA$rubble <- benth_dat$rubble
 benth_PCA$rock <- benth_dat$rock
 
 P1 <- qplot(PC1, hard.coral, data = benth_PCA)+
   labs(x="PC1", y = "% hard coral cover") +
   theme(legend.position = "none")  +
   geom_smooth() +
   theme_bw()
 
 # % macroalgal cover
 
 P2 <- qplot(PC1, macroalgae, data = benth_PCA) +
   labs(x="PC1", y = "% macroalgal cover")+
   theme(legend.position = "none")  +
   geom_smooth() +
   theme_bw()
 
 # complexity
 
 P3 <- qplot(PC1, complexity, data = benth_PCA) +
   labs(x="PC1", y = "Structural complexity") +
   geom_smooth() +
   theme_bw()
 
 # % sand cover
 
 P4 <- qplot(PC1, sand, data = benth_PCA) + #, colour = state) +
    labs(x="PC1", y = "% sand cover") +
    theme(legend.position = "none")  +
    geom_smooth()+
    theme_bw()
 
 # % rubble cover
 
 P5 <- qplot(PC1, rubble, data = benth_PCA) + #, colour = state) +
    labs(x="PC1", y = "% rubble cover")+
    theme(legend.position = "none")  +
    geom_smooth()+
    theme_bw()
 
 # rock cover
 
 P6 <- qplot(PC1, rock, data = benth_PCA) + # to fit a separate line for each group, add colour = state
    labs(x="PC1", y = "% rock cover")+
    theme(legend.position = "none") +
    geom_smooth() +
    theme_bw()
 
 grid.arrange(P1,P2,P3,P4,P5,P6, nrow=2)

save(benth_PCA,file = "../dat/Benthic_PC1.Rdata")

