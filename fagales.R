########################
# Load data sets
########################

library(dplyr)
library(readr)
# Load environmental data; choosing PD as the most high resolution reported variable
env <- list.files(path="./climate_data/spatial_data_climate/PD_P_50km", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 
env <- data.frame(env)
env[env == -9999] <- NA
env$x <- round(env$x, digit = 1)
env$y <- round(env$y, digit = 1)
env %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> env
env <- as.data.frame(env)

# Add RPD randomizations
rand_RPD <- read.csv("./Fagales_CSVs_ToShare/rand_RPD_50km.csv")
rand_RPD$x <- round(rand_RPD$x, digit = 1)
rand_RPD$y <- round(rand_RPD$y, digit = 1)
rand_RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> rand_RPD
rand_RPD <- as.data.frame(rand_RPD)
# Add significance column
rand_RPD$RPD_significance <- as.factor(ifelse(rand_RPD$value < 0.05, "Yes", "No"))
rand_RPD$value <- NULL

combined <- merge(rand_RPD, env, by = c("x", "y"))

# Add RPD
RPD <- read.csv("./Fagales_CSVs_ToShare/rand_RPD_50km.csv")
names(RPD) <- c("x", "y", "RPD")
RPD$x <- round(RPD$x, digit = 1)
RPD$y <- round(RPD$y, digit = 1)
RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> RPD
RPD <- as.data.frame(RPD)

combined <- merge(combined, RPD, by = c("x", "y"))

# Add CANAPE
CANAPE <- read.csv("./Fagales_CSVs_ToShare/CANAPE.csv")
names(CANAPE) <- c("x", "y", "CANAPE")
CANAPE$x <- round(CANAPE$x, digit = 1)
CANAPE$y <- round(CANAPE$y, digit = 1)
CANAPE %>% group_by(x, y) %>% summarize_if(is.character, max) -> CANAPE
CANAPE <- as.data.frame(CANAPE)
CANAPE$CANAPE <- as.factor(CANAPE$CANAPE)

combined <- merge(combined, CANAPE, by = c("x", "y"))


# There is something wrong with the nod data coordinates
## Add nodulation
#proportion_nodulating <- read.csv("./Fagales_CSVs_ToShare/prop_nod.csv")
#names(proportion_nodulating) <- c("x", "y", "proportion_nodulating")
#proportion_nodulating$x <- round(proportion_nodulating$x, digit = 1)
#proportion_nodulating$y <- round(proportion_nodulating$y, digit = 1)
#proportion_nodulating %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> proportion_nodulating
#proportion_nodulating <- as.data.frame(proportion_nodulating)
#
#
#combined <- merge(combined, proportion_nodulating, by = c("x", "y"))

# Normalize entire data frame
combined.scaled <- rapply(combined, scale, c("numeric","integer"), how="replace")
combined.scaled <- as.data.frame(combined.scaled)







# RPD model
summary(lm(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined.scaled)) # Significant, Adjusted R-squared:  0.1271 
# Precipitation most important, then aridity and temperature seasonality ~equally important

# RPD significance model
summary(lm(aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced ~ RPD_significance, data = combined.scaled)) # Significant, Adjusted R-squared:  0.1271 

# CANAPE significance model
summary(lm(aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced ~ CANAPE, data = combined.scaled)) # Significant, Adjusted R-squared:  0.1271 
summary.aov(manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_12, BIOCLIM_7, BIOCLIM_17, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced) ~ CANAPE, data = combined.scaled)) # Significant, Adjusted R-squared:  0.1271 



########################
# Some exploratory environment plots
########################

library(ggplot2)
ggplot(combined, aes(x = RPD_significance, y = aridity_index_UNEP, fill = RPD_significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))
ggplot(combined, aes(x = RPD_significance, y = BIOCLIM_1, fill = significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. RPD significance", x="Significance", y = "Bio1") + ylim(quantile(combined$BIOCLIM_1, 0.025, na.rm = TRUE), quantile(combined$BIOCLIM_1, 0.975, na.rm = TRUE))
ggplot(combined, aes(x = RPD_significance, y = BIOCLIM_12, fill = significance)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Mean annual temperature vs. RPD significance", x="Significance", y = "Bio1") + ylim(quantile(combined$BIOCLIM_12, 0.025, na.rm = TRUE), quantile(combined$BIOCLIM_12, 0.975, na.rm = TRUE))

ggplot(combined, aes(x = CANAPE, y = aridity_index_UNEP, fill = CANAPE)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. RPD significance", x="RPD_significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))


# Assess alternative measures of community age -- node height and tip length essentially measure the same thing
summary(lm(median_tl.normalized ~ median_nh.normalized, data = combined))

# Assess species richness vs. node height -- confounding variable? Removed node/tip info, still somewhat significant
summary(lm(alpha ~ median_nh.normalized, data = combined))

# Mean annual temperature vs. median node height
lm.temp_tl <- lm(median_tl ~ bio1, data = combined.scaled)
summary(lm.temp_tl)




########################
# Reload nodulation data
########################

proportion_nodulating_env <- list.files(path="./climate_data/spatial_data_climate/proportion_nodulating", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 
proportion_nodulating_env <- data.frame(proportion_nodulating_env)
proportion_nodulating <- read.csv("./Fagales_CSVs_ToShare/proportion_nodulating.csv")

combined_nod <- merge(proportion_nodulating, proportion_nodulating_env, by = c("x", "y"))
combined_nod[combined_nod == -9999] <- NA

combined_nod$x <- round(combined_nod$x, digit = 1)
combined_nod$y <- round(combined_nod$y, digit = 1)
combined_nod %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> combined_nod
combined_nod <- as.data.frame(combined_nod)


# Normalize entire data frame
combined_nod.scaled <- scale(combined_nod[3:41])
combined_nod.scaled <- as.data.frame(combined_nod.scaled)

########################
# Nodulation plots
########################

# Bin size control + color palette
library(ggplot2)
ggplot(combined_nod, aes(x = BIOCLIM_12, y = prop_nod) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw()

ggplot(combined_nod, aes(x = aridity_index_UNEP, y = prop_nod) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw() + ylim(0.01, 0.75) + geom_smooth(method='lm', formula = y ~ x) + labs(title="Aridity vs.\nproportion nodulating", x="Aridity index", y = "Proportion nodulating")
summary(lm(prop_nod ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined_nod)) # not significantsummary(lm(prop_nod ~ aridity_index_UNEP, data = combined_nod.scaled))






## Percent bio1 that are impossible 
#length(na.omit(combined[combined$bio1_biotaphy_5min_global < -50, ]$bio1_biotaphy_5min_global))/length(na.omit(combined$bio1_biotaphy_5min_global)) * 100

########################
# Build environment vs. phylogenetic stats models
########################

# combined model
lm.combined.tl <- lm(median_tl ~ bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combined.tl)

# Much better explanatory power for oldest divergences, but correlated with species richness
# Consider normalizing by species richness
lm.combined.nl97_5 <- lm(X97.5_per_tl ~ bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combined.nl97_5)

# Much better explanatory power for youngest divergences, but correlated with species richness
# Consider normalizing by species richness
lm.combined.tl2_5 <- lm(X2.5_per_tl ~ bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combined.tl2_5)


lm.combined.richness <- lm(alpha ~ bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combined.richness)

lm.combined.nh <- lm(median_nh.normalized ~ alpha + bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combined.nh)

########################
# Some exploratory trait plots 
########################

# Assess woodiness vs. tip length -- are woody-containing communities older?
# Coeff positive (more woodiness in older communities), R2 adj 0.4009
summary(lm(woodiness.proportional ~ median_tl, data = combined.scaled))

# Assess chloranthoid teeth vs. tip length
# Coeff negative (less chloranthoid teeth in older communities), R2 adj 0.3154 
summary(lm(Chloranthoid.teeth.proportional ~ median_tl, data = combined.scaled))

# Assess ethereal oil cells vs. tip length
# Coeff positive (more ethereal oil cells in older communities), R2 adj 0.3329 
summary(lm(Ethereal.oil.cells.proportional ~ median_tl, data = combined.scaled))

# Assess vessels vs. tip length
# Coeff positive (more ethereal oil cells in older communities), R2 adj 0.01162 -- significant but very weak 
summary(lm(Vessels.proportional ~ median_tl, data = combined.scaled))


# Bin size control + color palette
library(ggplot2)
ggplot(combined, aes(x=woodiness.proportional, y=median_tl) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw()
ggplot(combined, aes(x=Chloranthoid.teeth.proportional, y=median_tl) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw()
ggplot(combined, aes(Ethereal.oil.cells.proportional, y=median_tl) ) + geom_hex(bins = 70) + scale_fill_continuous(type = "viridis") + theme_bw() + xlim(0.01, 0.3) + ylim(0, 15) # zero-inflated so need to exclude


########################
# Build environment vs. phylogenetic stats models
########################

# combined model
# adj r2 0.5972, all significant except pollen and perianth merosity, chloranthoid most important, followed by stamen phyllotaxis and woodiness. Unexpected direction and large effect size for chloranthoid teeth, carpel form
lm.combinedtraits.tl <- lm(median_tl ~ Vessels.proportional + Chloranthoid.teeth.proportional + Ethereal.oil.cells.proportional + Perianth.phyllotaxis.proportional + Perianth.merosity.proportional + Stamen.phyllotaxis.proportional + Laminar.stamens.proportional + Pollen.features.proportional + Carpel.form.proportional + Postgenital.sealing.of.the.carpel.proportional + woodiness.proportional, data = combined.scaled)
options(scipen = 999)
# options(scipen = 0)
summary(lm.combinedtraits.tl)

# Add in environmental data to understand how much is increased by adding traits (see environment only above)
# Chloranthoid teeth was most important, bio1 narrowly second, stamen phyllotaxis narrowly third
# Comparing environment only model, adj. R2 0.663 --> 0.696. Modest increase, but traits important in combined model. Presumably high internal correlation among predictors.
lm.combinedtraitsenv.tl <- lm(median_tl ~ Vessels.proportional + Chloranthoid.teeth.proportional + Ethereal.oil.cells.proportional + Perianth.phyllotaxis.proportional + Perianth.merosity.proportional + Stamen.phyllotaxis.proportional + Laminar.stamens.proportional + Pollen.features.proportional + Carpel.form.proportional + Postgenital.sealing.of.the.carpel.proportional + woodiness.proportional + bio1 + bio12 + bio17 + elevation + slope + coarse_frag + sand + soil + phx10 + needleleaf + herbaceous, data = combined.scaled)
summary(lm.combinedtraitsenv.tl)

