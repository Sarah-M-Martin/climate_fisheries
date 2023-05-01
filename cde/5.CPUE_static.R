
#######################################################################
# Comparing cpue among habitat types
#######################################################################

catch <- read.csv("../dat/catch_habitat.csv")  

# sum across species for each set
traps_cpue_sets <- catch %>%
    filter(gear_code == "FIXS") %>%
    mutate(cpue = catch_kg/nb_gear) %>%
    group_by(trip_id, operation_id, year, month,  area, 
           location_code, gear_code, tar_spp, vessel_group, set_quantile) %>%
           dplyr::summarise(cpue = sum(cpue)) %>%  # Do not remove NAs here - the whole set should be NA and then removed
           drop_na(trip_id, operation_id, year, month, area, location_code,gear_code,tar_spp, vessel_group,set_quantile, cpue) %>%
           filter(cpue!=0, !is.infinite(cpue))  # drops all rows with zero cpue (these are not real zeros) and Inf

# Remove outliers for cpue by set

  for(i in unique(traps_cpue_sets$gear_code)){
    subs <- traps_cpue_sets$cpue[which( traps_cpue_sets$gear_code == i)]
    a <- as.vector(round(quantile(subs, c(0.95), na.rm = T),2))
    subs[which(subs > a)] <- NA
    traps_cpue_sets$cpue[which(traps_cpue_sets$gear_code == i)] <- subs
  }

    Static <- traps_cpue_sets %>%
    drop_na(cpue) 

save(Static, file =  "../dat/Static.Rdata")

################################################################################
# Model selection step 1
################################################################################
# Find optimal random effects structure  (REML) using AIC or BIC

library(nlme)
mS1 <- lme(sqrt(cpue) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year) + 
             as.factor(month)* as.factor(set_quantile),
           method = "REML", 
           (~1|location_code/area), data = Static)


mS2 <- lme(sqrt(cpue) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year) + 
             as.factor(month)* as.factor(set_quantile),
           method = "REML", 
           (~1|location_code), data = Static)

AIC(mS1,mS2) 

##################################################################
# Model selection step 2
##################################################################

# Find the optimal fixed effects structure (ML) using LRT model comparison

mS3 <- lme(sqrt(cpue) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year) + 
             as.factor(month)* as.factor(set_quantile),
           method = "ML", 
           (~1|location_code/area), data = Static)

drop1(mS3,test="Chisq")

############ Final model ###############

static.mod <- lme(sqrt(cpue) ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
           method = "REML", 
           (~1|location_code/area), data = Static)

summary(static.mod)

# Validation
plot(static.mod)
qqnorm(resid(static.mod)) ; qqline(resid(static.mod))
hist(resid(static.mod),breaks = 100,prob=T)  
x <- seq(min(resid(static.mod)), max(resid(static.mod)), length = 40)
f <- dnorm(x, mean = mean(resid(static.mod)), sd = sd(resid(static.mod)))
lines(x, f, col = "red", lwd = 2)

Static$resid <- resid(static.mod)
par(mfrow=c(2,4))
boxplot(Static$resid ~ Static$year, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static$resid ~ Static$month, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static$resid ~ Static$area, ylab="Residuals", xlab="", col="sky blue",las=2)
boxplot(Static$resid ~ Static$set_quantile, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static$resid ~ Static$location_code, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static$resid ~ Static$tar_spp, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static$resid ~ Static$vessel_group, ylab="Residuals", xlab="", col="sky blue")

# plot predictions
Static$fit <- (fitted(static.mod))^2

year <- Static %>%
  select(year, fit) %>%
ggplot(aes(x = as.factor(year), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("year") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() 

month <- Static %>%
  select(month, fit) %>%
  ggplot(aes(x = as.factor(month), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("month") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() 

LS <- Static %>%
  select(area, fit) %>%
  ggplot(aes(x = as.factor(area), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("Landing site") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() +
  theme( axis.text.x = element_text(angle = 90)) 
 
location <- Static %>%
  select(location_code, fit) %>%
  ggplot(aes(x = as.factor(location_code), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("Location") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() 

tar_spp <- Static %>%
  select(tar_spp, fit) %>%
  ggplot(aes(x = as.factor(tar_spp), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("Target species") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() 

vessel <- Static %>%
  select(vessel_group, fit) %>%
  ggplot(aes(x = as.factor(vessel_group), y = fit) ) +
  geom_boxplot(fill = ("light blue") ) +
  xlab("Vessel type") +
  ylab(bquote(CPUE (kg ~ trap^-1))) +
  theme_bw() 

library(cowplot)

predictions <- plot_grid(year, month, LS, location, tar_spp, vessel,
          labels = c('a', 'b', 'c', 'd', 'e', 'f'), 
          label_fontface = "bold",
          label_size = 12)

habitats <- c("Coral", "Rocky",  "Mixed",  "Macroalgal")

ggplot(Static, aes(x = as.factor(set_quantile), y = fit, color = as.factor(set_quantile))) +
  geom_boxplot() +
  scale_color_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) +
  geom_point(alpha = 0.1,position = position_jitter(seed = 1, width = 0.35)) +
    theme_bw() +
  ylab(bquote(cpue (kg ~ trap^-1))) +
  xlab("")+
  scale_x_discrete(labels = habitats) +
  theme(legend.position="none") 

# POst hoc test for significance among factor levels
library(emmeans)
    emm <- emmeans(static.mod,"set_quantile")
    pairs(emm, adjust = "fdr")
    contrast(regrid(emm))
 
#############################################################
 # Total catch - static traps
#############################################################

new_catch <-  catch %>%
      group_by(operation_id) %>%
      summarise(tot_catch = sum(catch_kg)) %>%
      dplyr::select(tot_catch,operation_id) %>%
      right_join(traps_cpue_sets)  # Filter 
  
annual_catch_sets <-  new_catch %>%
    group_by ( year, gear_code, set_quantile  ) %>%
    dplyr::  summarise(catch_kg = sum(tot_catch,na.rm = TRUE))  %>%
    group_by (gear_code, set_quantile ) %>%
    dplyr:: summarise( tot_catch= mean( catch_kg,na.rm = TRUE))
  
annual_catch_sets$set_quantile <- with(annual_catch_sets, factor(set_quantile, 
                                       levels = c(1,2,3,4), labels = c("Coral", "Rocky", "Mixed","Macroalgal")))

 annual_catch_sets  %>%
  filter(!is.na(set_quantile)) %>%
  filter(gear_code=="FIXS") %>%
  group_by (gear_code) %>%
  mutate(percentage = tot_catch/sum(tot_catch)*100, s_e = (sqrt((percentage/100)*(1-(percentage/100))/4))*100) %>%
  mutate(set_quantile = as.factor(set_quantile)) %>%
  ggplot(aes(set_quantile ,
             percentage, 
             fill = set_quantile  )) +
  geom_col() +
  geom_errorbar(aes(ymax = percentage + s_e, ymin=ifelse(percentage - s_e >= 0, percentage - s_e, 0)),width=0.1) + 
  scale_fill_manual(values = c(  "steelblue4",
                                 "steelblue", 
                                 "seagreen3",  
                                 "seagreen4") ) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Mean annual catch (%) 2017-2020") +
  xlab("") 
