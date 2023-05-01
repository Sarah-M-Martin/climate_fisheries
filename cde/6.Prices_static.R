library(janitor)

####################
# Price data
####################

prices <- read.csv('../dat/Packets.csv', encoding='latin1') %>%
  clean_names() %>% 
  add_count(id_packet) 

single_spp <- prices %>%
  filter(n==1) %>%    
  mutate(avg_packaging_price = coalesce(avg_packaging_price,total_price), 
  no_packets = round(quantite_vente / 3.5,0), # Average packet weight = 3.5 kg
  ppkg =  avg_packaging_price / 3.5) 

# Add reef species index for filtering
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

# remove 3 outliers (of 19073) (> 200)
single_spp <- single_spp[-c(which(single_spp$ppkg > 200)),]

# Add species-habitat quantile 
catch <- read.csv("../dat/catch_habitat.csv")
single_spp$quantile <- catch$quantile [match(single_spp$species, catch$species)]

# Plot in USD based on 2017-2020 exchange rate of 0.073 (www.xe.com)
single_spp$USDppkg <- single_spp$ppkg * 0.073

single_spp %>%
  filter(reef==1) %>%
  mutate(species = fct_reorder(species,USDppkg,mean,na.rm=T)) %>%
  ggplot(aes(species,USDppkg, fill = as.factor(quantile))) + 
  geom_boxplot() + 
  scale_fill_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) +
  theme_bw() +
  coord_flip() +
  ylab(bquote(Price~ (USD ~ kg^-1))) + 
  xlab("") +
  theme(legend.position = "none") 

################################################################################

# Table of species mean prices per kg
spp_price <- single_spp %>%
  group_by(species_label) %>%
  summarise(mean_price = mean(ppkg,na.rm=T)) %>%
   arrange(desc(mean_price)) %>%
  as.data.frame()

write.csv(spp_price,"../dat/spp_price", row.names = FALSE)

###########################################################################################################
###########################################################################################################

# add prices to catch data
# Filter to exactly the same cleaned and filtered data as static trap cpue analysis 
load("../dat/Static.Rdata") # 3362 operations 

trap_catch <- catch %>%
  semi_join(Static, by = "operation_id")

# Add mean price per kg to each species in catch dataset
trap_catch$price_kg <- spp_price$mean_price [match(trap_catch$species_label , spp_price$species_label)]
trap_catch$revenue_set <- trap_catch$price_kg * trap_catch$cpue

# Add reef fish filter
trap_catch$reef <- NA
for (i in 1: length(trap_catch$reef)){
  ifelse((trap_catch$species_code[i] %in% reef_species),
         trap_catch$reef[i] <- 1, 
         trap_catch$reef[i] <- 0)
}

################################################################################
# Sum across species for each set
Static_rev <- trap_catch %>%
  group_by(trip_id, operation_id, year, month,  area, 
  location_code, gear_code, tar_spp, vessel_group, set_quantile) %>%
  dplyr::summarise(set_cpue = sum(cpue), set_rev = sum(revenue_set)) %>%   
  drop_na(trip_id,operation_id, year, month, area, location_code,gear_code,tar_spp, vessel_group,set_quantile, set_cpue, set_rev)

write.csv(Static_rev, "../dat/set_revenue", row.names = FALSE)

########################################################################################
 # Explore reponse
########################################################################################

 par(mfrow=c(2,2))
 hist(Static_rev$set_rev, breaks=50) 
 hist(sqrt(Static_rev$set_rev),breaks=50,prob=T) 
 x <- seq(min(sqrt(Static_rev$set_rev)), max(sqrt(Static_rev$set_rev)), length = 40)
 f <- dnorm(x, mean = mean(sqrt(Static_rev$set_rev)), sd = sd(sqrt(Static_rev$set_rev)))
 lines(x, f, col = "red", lwd = 2)

################################################################################
 # Model selection step 1
################################################################################
 # Find optimal random effects structure (REML) using AIC or BIC
 
library(nlme)
RS1 <- lme(sqrt(set_rev) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
            method = "REML", 
           (~1|location_code/area), data = Static_rev)

 RS2 <- lme(sqrt(set_rev) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
            method = "REML", 
            (~1|location_code), data = Static_rev)
 AIC(RS1,RS2) 
 
 ##################################################################
 # Model selection step 2
 ##################################################################
 # Find the optimal fixed effects structure (ML)u sing LRT
 
  RS3 <- lme(sqrt(set_rev) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year) +
            as.factor(month) * as.factor(set_quantile),
            method = "ML", 
            (~1|location_code/area), data = Static_rev)
 
   drop1(RS3,test="Chisq")
 
 ############ Final model ###############

rev.mod <- lme(sqrt(set_rev) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
            method = "REML", 
            (~1|location_code/area), data = Static_rev)
 summary(rev.mod)
 
 # Validation

 plot(rev.mod)
 qqnorm(resid(rev.mod)) ; qqline(resid(rev.mod))
 hist(resid(rev.mod),breaks = 100,prob=T)  
 x <- seq(min(resid(rev.mod)), max(resid(rev.mod)), length = 40)
 f <- dnorm(x, mean = mean(resid(rev.mod)), sd = sd(resid(rev.mod)))
 lines(x, f, col = "red", lwd = 2)
 
 Static_rev$resid <- resid(rev.mod)
 par(mfrow=c(2,4))
 boxplot(Static_rev$resid ~ Static_rev$year, ylab="Residuals", xlab="", col="sky blue")
 boxplot(Static_rev$resid ~ Static_rev$month, ylab="Residuals", xlab="", col="sky blue")
 boxplot(Static_rev$resid ~ Static_rev$area, ylab="Residuals", xlab="", col="sky blue",las=2)
 boxplot(Static_rev$resid ~ Static_rev$set_quantile, ylab="Residuals", xlab="", col="sky blue")
 boxplot(Static_rev$resid ~ Static_rev$location_code, ylab="Residuals", xlab="", col="sky blue")
 boxplot(Static_rev$resid ~ Static_rev$tar_spp, ylab="Residuals", xlab="", col="sky blue")
 boxplot(Static_rev$resid ~ Static_rev$vessel_group, ylab="Residuals", xlab="", col="sky blue")
 
 # plot predictions
 Static_rev$fit <- (fitted(rev.mod))^2
 windows()
 par(mfrow=c(2,4))
 boxplot(Static_rev$fit ~ Static_rev$year,ylab="Revenue (SCR trap-1)", xlab="", col="sky blue")
 boxplot(Static_rev$fit ~ Static_rev$month, ylab="Revenue (SCR trap-1)", xlab="",col="sky blue")
 boxplot(Static_rev$fit ~ Static_rev$area,ylab="Revenue (SCR trap-1)", xlab="", col="sky blue",las=2)
 boxplot(Static_rev$fit ~ Static_rev$set_quantile, ylab="Revenue (SCR trap-1)", xlab="",col="sky blue")
 boxplot(Static_rev$fit ~ Static_rev$location_code, ylab="Revenue (SCR trap-1)", xlab="",col="sky blue")
 boxplot(Static_rev$fit ~ Static_rev$tar_spp,ylab="Revenue (SCR trap-1)", xlab="", col="sky blue")
 boxplot(Static_rev$fit ~ Static_rev$vessel_group, ylab="Revenue (SCR trap-1)", xlab="",col="sky blue")

 Static_rev$fit_USD <- Static_rev$fit * 0.073
 habitats <- c("Coral", "Rocky",  "Mixed",  "Macroalgal")
 
 rev_box_static <- ggplot(Static_rev, aes(x = as.factor(set_quantile), y = fit_USD, color = as.factor(set_quantile))) +
   geom_boxplot() +
   geom_point(position = position_jitter(seed = 1, width = 0.35),alpha=0.1) +
   scale_color_manual(values = c(  "steelblue4",
                                   "steelblue", 
                                   "seagreen3",  
                                   "seagreen4") ) +
   theme_bw() +
   ylab(bquote("Fisher revenue "(USD ~ trap^-1))) +
   xlab("")+
   scale_x_discrete(labels = habitats) +
   theme(legend.position="none") 
 