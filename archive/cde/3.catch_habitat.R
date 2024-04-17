
# Link fishing operations to habitats by species-habitat associations

catch <- read.csv("../dat/Catch_dat_all_2017_2020.csv")

# Add habitat association quantiles by species 
mode <- read.csv("../dat/spp_quantiles_PC1.csv")
catch$quantile <- mode$quantile[match(catch$species, mode$Species)]

#######################################################################
# Loop to identify dominant species in each operation (by weight)
catch$dom_spp <- NA
sets <- c()
op <- unique(catch$operation_id)  

for (i in op) {
  dom <- catch[catch$operation_id==i,"species_code"][which(catch[catch$operation_id==i,"catch_kg"] == 
                                                             max(catch[catch$operation_id==i,"catch_kg"]))]
  if(length(dom) == 1){
    catch[catch$operation_id==i,"dom_spp"] <- dom
    sets <- c(sets,dom)
  }else{
        dom2 <- dom[1]
           for(j in 2:length(dom)){
           dom2 <-  paste(dom2,dom[j],sep="")
            }
        catch[catch$operation_id==i,"dom_spp"] <- dom2
      sets <- c(sets,dom2)
      }
    }

# < 0.4 % of catch is from sets allocated to multiple dominant species
catch$dom_spp <- substr(catch$dom_spp, 1, 3)
catch$dom_sp_name <- Species_overlap$species_latin  [match(catch$dom_spp, Species_overlap$species_code )]

# Allocate each set to a habitat quantile based on the association of the dominant species caught
catch$set_quantile <- mode$quantile_2  [match(catch$dom_sp_name , mode$Species )]

write.csv(catch,"../dat/catch_habitat.csv", row.names = FALSE)
