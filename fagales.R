########################
# Load data sets
########################
# Make sure you are in the correct working directory
# Load libraries
library(dplyr)
library(readr)

# Set the working directory to the cloned repository -- Botany 2023 Workshop folder (not subfolders)

# Load environmental data
# We will practice reconciling data sources

# Load a set of CSV spreadsheets, one per variable, and combine -- each CSV as one column
env <- list.files(path="./climate_data/PD_P_50km", full.names = TRUE) %>% lapply(read_csv) %>% bind_rows 
env <- data.frame(env) # Convert to dataframe
env[env == -9999] <- NA # Recode missing data, which is coded as -9999, as NA

# Let's peek at our spreadsheet, to see if it works
head(env)
# Discuss spreadsheet -- the types of variables: temperature and precipitation, soil, landcover (vegetation types), elevation/slope/aspect

# Soon we will combine environmental data with phylogenetic diversity
# Combining grid cells could fail due to rounding error. We will re-round and aggregate grid cells by lat and long
env$x <- round(env$x, digit = 1) # Round longitude to nearest 0.1 degree
env$y <- round(env$y, digit = 1) # Round latitude to nearest 0.1 degree
env %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> env # Aggregate grid cells with identical coordinates, averaging duplicates
env <- as.data.frame(env) # Convert dplyr output back to data frame


# Add RPD -- relative phylogenetic diversity to identify locations with unusual concentrations of long or short branches 
# So this is the phylogenetic diversity of each grid cell, divided by the phylogenetic diversity where the same tree has equal branch lengths
# Low values mean short branches (compared to the equal branch length tree) and large values mean long branches (compared to the equal branch length tree) 
RPD <- read.csv("./Fagales_CSVs_ToShare/rand_RPD_50km.csv") # Load CSV
names(RPD) <- c("x", "y", "RPD") # Fix the column names
# From here on, repeat aggregation steps (previous block, lines 21-26) from above (here not actually going back to last steps)
RPD$x <- round(RPD$x, digit = 1) # Round longitude to nearest 0.1 degree
RPD$y <- round(RPD$y, digit = 1) # Round latitude to nearest 0.1 degree
RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> RPD # Aggregate grid cells with identical coordinates, averaging duplicates
RPD <- as.data.frame(RPD) # Convert dplyr output back to data frame

combined <- merge(env, RPD, by = c("x", "y")) # Combined environmental dataframe with RPD dataframe

# Check result
head(combined)
# We can see that a new column called RPD was added
# Also, we see that there is much less missing data (NaN) -- this is due to the aggregation step

# Add RPD randomizations -- these are essentially p-values, that we will transform into significance categories
# From here on, repeat aggregation steps (previous block, lines 21-26) from above (here not actually going back to last steps)
rand_RPD <- read.csv("./Fagales_CSVs_ToShare/rand_RPD_50km.csv")
rand_RPD$x <- round(rand_RPD$x, digit = 1) # Round longitude to nearest 0.1 degree
rand_RPD$y <- round(rand_RPD$y, digit = 1) # Round latitude to nearest 0.1 degree
rand_RPD %>% group_by(x, y) %>% summarize_if(is.numeric, mean, na.rm = TRUE) -> rand_RPD # Aggregate grid cells with identical coordinates, averaging duplicates
rand_RPD <- as.data.frame(rand_RPD) # Convert dplyr output back to data frame
# Add significance column. Here we use logicals to recode the data as significance categories
rand_RPD$RPD_significance <- as.factor(ifelse(rand_RPD$value < 0.025, "Low", ifelse(rand_RPD$value > 0.975, "High", "NS"))) # NS is not significant, high is upper tail, low is lower tail
# We no longer need the raw p-values, so we remove
rand_RPD$value <- NULL

combined <- merge(combined, rand_RPD, by = c("x", "y")) # We are combining the combined dataframe with rand_RPD dataframe



# Add CANAPE. This is a different type of PD randomization that tests for neo- and paleoendemism. So we use not only the tree but range size data which captures endemism
# From here on, repeat aggregation steps (previous block, lines 21-26) from above (here not actually going back to last steps)
CANAPE <- read.csv("./Fagales_CSVs_ToShare/CANAPE.csv")
names(CANAPE) <- c("x", "y", "CANAPE")
CANAPE$x <- round(CANAPE$x, digit = 1) # Round longitude to nearest 0.1 degree
CANAPE$y <- round(CANAPE$y, digit = 1) # Round latitude to nearest 0.1 degree
CANAPE %>% group_by(x, y) %>% summarize_if(is.character, max) -> CANAPE # Aggregate grid cells with identical coordinates, averaging duplicates
CANAPE <- as.data.frame(CANAPE) # Convert dplyr output back to data frame
CANAPE$CANAPE <- as.factor(CANAPE$CANAPE) # Setting the CANAPE column in the CANAPE dataframe as factors

combined <- merge(combined, CANAPE, by = c("x", "y")) # We are combining the combined dataframe with CANAPE dataframe

# Check final dataset
head(combined)


# Normalize/standardize entire data frame
# Normalizing is useful in a regression framework for interpreting variables independent of the different scale and variance of different types of measurements
# Some statistical analyses may be affected by normalized data
combined.scaled <- rapply(combined, scale, c("numeric","integer"), how="replace")
combined.scaled <- as.data.frame(combined.scaled)



########################
# Now we are ready to analyze
########################

# Let's understand the relationship between RPD (treated as the response) and the environment (treated as the predictor). 
# We will use a selection of just 8 of the environmental variables.
env_RPD <- lm(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced, data = combined.scaled)
summary(env_RPD) 
# Let's look at the output. Is the model significant overall? 
# What predictors are significant? How much variance does the model explain in the data? (R2)
# Any surprises?


```
Practical for workshop attendees. Examine p value of the model, variables; similarly with R2. Surprising or not? Interpret the variables. Discuss whether their 
significance makes sense. Normalized/standardized coefficients -- interpret these. 
```

# CANAPE significance model. Let's build it, just like above for RPD, and also look at the model summaries
env_CANAPE <- lm(aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced ~ CANAPE, data = combined.scaled)
summary.aov(manova(cbind(aridity_index_UNEP, BIOCLIM_1, BIOCLIM_12, BIOCLIM_7, BIOCLIM_17, ISRICSOILGRIDS_new_average_nitrogen_reduced, ISRICSOILGRIDS_new_average_phx10percent_reduced, ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced) ~ CANAPE, data = combined.scaled)) # Significant, Adjusted R-squared:  0.1271 
# Here the output is a little different because CANAPE is a factor. 
# The variables are numbered in the same order as the variables in the model. So response 1 is aridity.
# Response 2 is BIO 1.


# Let's look at mixed models. This is a slightly different class of regressions we might use when we are trying to control for something
# like grid cells that are similar, or species that are related, having correlated values. Here, just to try something, we will use latitude
library(lme4)
# Same style model as above, but latitude as a random effect
mixed_model_complex <- lmer(RPD ~ aridity_index_UNEP + BIOCLIM_1 + BIOCLIM_12 + BIOCLIM_7 + BIOCLIM_17 + ISRICSOILGRIDS_new_average_nitrogen_reduced + ISRICSOILGRIDS_new_average_phx10percent_reduced + ISRICSOILGRIDS_new_average_soilorganiccarboncontent_reduced + (1 | y), na.action = na.omit, data = combined.scaled)
# We will also take advantage of the variable importance above, and fit a simple model with the best 3 predictors
mixed_model_simple <- lmer(RPD ~ aridity_index_UNEP + BIOCLIM_12 + BIOCLIM_7 + (1 | y), na.action = na.omit, data = combined.scaled)

# Now let's examine model fits and perform model selection
# We can use standard AIC. Remember, low AIC is favored
AIC(mixed_model_simple)
AIC(mixed_model_complex) 
AIC(env_RPD)
# Okay, which model was best? How can we interpret mixed model vs. standard linear model?
# What about simple vs. complex model?



########################
# An exploratory environment plot
########################

library(ggplot2) # Load standard graphics library
# Let's investigate how CANAPE significance categories respond to environment
ggplot(combined, aes(x = CANAPE, y = aridity_index_UNEP, fill = CANAPE)) + geom_violin(trim=FALSE) + geom_boxplot(width=0.1, fill="white") + labs(title="Aridity vs. CANAPE significance", x="CANAPE significance", y = "UNEP aridity index") + ylim(quantile(combined$aridity_index_UNEP, 0.025, na.rm = TRUE), quantile(combined$aridity_index_UNEP, 0.975, na.rm = TRUE))
# Categories -- mixed is a mix of paleo and neoendemism
# So what is the relationship we recovered
# REMINDER: Aridity index -- LOW is arid
