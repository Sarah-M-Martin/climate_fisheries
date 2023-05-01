
########################################################################################
# Affordability - static traps
########################################################################################

## Price by habitat per fixed quantity of catch in USD
# consumption = 58kg year-1 capita-1 (FAO, 2019)
# 4.83 kg per person per month (use monthly rate to match monthly salary data)

Static_rev <- read.csv("../dat/set_revenue")  

# price per 4.83kg of fish in each set
Static_rev$price_month <- (Static_rev$set_rev/Static_rev$set_cpue) * 4.83

# Leptokurtic distribution (heavy tails)

library(LambertW) # Goerg 2015
test_norm(Static_rev$price_month) # not normal

# Gaussianize distribution
trans <- Gaussianize(Static_rev$price_month, return.tau.mat = T, type = "h")
Static_rev$price_month_trans <- as.numeric(Gaussianize(Static_rev$price_month, type = "h"))
test_norm(Static_rev$price_month_trans)

################################################################################
# Model selection step 1
################################################################################
# Find optimal random effects structure  (REML)

AS1 <- lme(price_month_trans ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
           method = "REML", 
           (~1|location_code/area), data = Static_rev)

AS2 <- lme(price_month_trans ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
           method = "REML", 
           (~1|location_code), data = Static_rev)

AIC(AS1,AS2)

##################################################################
# Model selection step 2
##################################################################

# Find  optimal fixed effects structure (ML)

AS3 <- lme(price_month_trans ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
           method = "ML", 
           (~1|location_code/area), data = Static_rev)

summary(AS3)
drop1(AS3,test = "Chisq")

############ Final model ###############

afford.static.mod <- lme(price_month_trans ~ as.factor(set_quantile) + as.factor(month) + vessel_group + tar_spp + as.factor(year),
                         method = "ML", 
                         (~1|location_code/area), data = Static_rev)
summary(afford.static.mod)

# Validation
plot(afford.static.mod)
qqnorm(resid(afford.static.mod)) ; qqline(resid(afford.static.mod))
hist(resid(afford.static.mod),breaks = 100,prob=T) 
x <- seq(min(resid(afford.static.mod)), max(resid(afford.static.mod)), length = 40)
f <- dnorm(x, mean = mean(resid(afford.static.mod)), sd = sd(resid(afford.static.mod)))
lines(x, f, col = "red", lwd = 2)

Static_rev$resid <- resid(afford.static.mod)
par(mfrow=c(2,4))
boxplot(Static_rev$resid ~ Static_rev$year, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static_rev$resid ~ Static_rev$month, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static_rev$resid ~ Static_rev$area, ylab="Residuals", xlab="", col="sky blue",las=2)
boxplot(Static_rev$resid ~ Static_rev$set_quantile, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static_rev$resid ~ Static_rev$location_code, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static_rev$resid ~ Static_rev$tar_spp, ylab="Residuals", xlab="", col="sky blue")
boxplot(Static_rev$resid ~ Static_rev$vessel_group, ylab="Residuals", xlab="", col="sky blue")

# Backtransform
Static_rev$fit <- as.numeric(Gaussianize(fitted(afford.static.mod), tau.mat = trans$tau.mat, inverse = T, type = "h"))

# plot predictions
windows()
par(mfrow=c(2,4))
boxplot(Static_rev$fit ~ Static_rev$year,ylab="Price per daily portion (SCR Daily portion-1)", xlab="", col="sky blue")
boxplot(Static_rev$fit ~ Static_rev$month, ylab="Price per daily portion (SCR Daily portion-1)", xlab="",col="sky blue")
boxplot(Static_rev$fit ~ Static_rev$area,ylab="Price per daily portion (SCR Daily portion-1)", xlab="", col="sky blue",las=2)
boxplot(Static_rev$fit ~ Static_rev$set_quantile, ylab="Price per daily portion (SCR Daily portion-1)", xlab="",col="sky blue")
boxplot(Static_rev$fit ~ Static_rev$location_code, ylab="Price per daily portion (SCR Daily portion-1)", xlab="",col="sky blue")
boxplot(Static_rev$fit ~ Static_rev$tar_spp,ylab="Price per daily portion (SCR Daily portion-1)", xlab="", col="sky blue")
boxplot(Static_rev$fit ~ Static_rev$vessel_group, ylab="Price per daily portion (SCR Daily portion-1)", xlab="",col="sky blue")

# Plot in USD based on 2017-2020 exchange rate of 0.073 (www.xe.com)
Static_rev$fit_USD <- Static_rev$fit * 0.073
habitats <- c("Coral", "Rocky",  "Mixed",  "Macroalgal")

aff_box_st_month <- ggplot(Static_rev, aes(x = as.factor(set_quantile), y = fit_USD, color = as.factor(set_quantile))) +
  geom_boxplot() +
  geom_point(position = position_jitter(seed = 1, width = 0.35),alpha=0.05) +
    scale_color_manual(values = c(  "steelblue4",
                                  "steelblue", 
                                  "seagreen3",  
                                  "seagreen4") ) +
  theme_bw() +
  ylab("Cost per month (USD)") +
  xlab("") +
  scale_x_discrete(labels = habitats) +
  theme(legend.position="none") 

## Add lines for salary * proportion of income expenditure on fish (1.48 %)

salary <- read.csv("../dat/Average_wages_ILO.csv") %>%
  clean_names() %>%
  filter(time==2018) %>% # most recent year available
  dplyr::select(economic_activity, amount_spent_on_fish) %>%
  filter(economic_activity!= "Total" & economic_activity!= "X. Not elsewhere classified") %>%
  arrange(amount_spent_on_fish)

aff_box_st_month_salary <- aff_box_st_month +
  geom_hline(yintercept = salary[6,2], colour = "grey",linetype = 2) + 
  annotate("text", x=0.5, salary[6,2], vjust = 1,  label = "P") +
  geom_hline(yintercept = salary[7,2], colour = "grey",linetype = 2) + 
  annotate("text", x=0.5, salary[7,2], vjust = 1, label = "O") +
  geom_hline(yintercept = salary[8,2], colour = "grey",linetype = 2) + 
  annotate("text", x=0.5, salary[8,2], vjust = 1, label = "N") +
  geom_hline(yintercept = salary[9,2], colour = "grey",linetype = 2) + 
  annotate("text", x=0.5, salary[9,2], vjust = 1, label = "M")

