########## HERBIVORE COMPARISON PROJECT: LABRIDAE SUBSET ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species from the family Labridae and compares their density 
# estimates between SVC and transect surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-08-11
##########
##########


# Set-Up =======================================================================

# packages
library(plyr) 
library(tidyverse)
library(nlme)
library(car)
library(arm)
library(MuMIn)
library(doBy)
library(ggplot2)
library(here)

# data
herbivores <- read_csv(here("./dataframes/herbivore_dataframe.csv"))


# Labridae Subset ==============================================================

# The following subsets the herbivore dataframe for species from the family 
# Labridae.

# subset for Labridae
labridae <- herbivores[herbivores$family == "Labridae",]


# Global Labridae Linear Mixed Effects Model ===================================

# The following section creates a global linear mixed effects model to compare 
# Labridae density differences between SVC and transect surveys in relation 
# to species, habitat, and survey traits. It explores collinearity by examining 
# VIF values of predictors and determines model fit using random effects plots,
# residual plots, qq plots, and model plots. 

# global model
labridae_global <- lme(log_difference~octocoral+stony+relief_cm
                           +cryptic_behaviour+max_length+average_depth+
                           size_bin_lengths, 
                           random = list(~1|site, ~1|species_order), 
                           labridae) 
# removed shape because all are fusiform

# summary without behaviour
summary(labridae_global)

# VIF values 
vif(labridae_global)
# no collinearity 


# Dredging =====================================================================

# The following dredges the global linear mixed effects model in order to
# determine the combinations of predictors that result in the models with the 
# highest likelihood.

# dredge
labridae_dredge <- dredge(labridae_global)
labridae_dredge

# subset for delta AIC < 4
labridae_dredge_sub <- subset(labridae_dredge, delta < 4)

# model average of top models 
labridae_model_avg <- model.avg(labridae_dredge_sub)
summary(labridae_model_avg)

# confidence intervals of top model covariates
confint(labridae_model_avg)

# save dredge outputs 
saveRDS(labridae_dredge, here("./outputs/labridae_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# octocoral plot
plot(labridae$log_difference ~ labridae$octocoral)
# significantly positive

# cryptic behaviour boxplot
labridae$cryptic_char <- as.character(labridae$cryptic_behaviour)
ggplot(labridae, aes(x = cryptic_char, y = log_difference, 
                       fill = cryptic_char)) +
  geom_boxplot() +
  theme_classic() + xlab("Presence of Cryptic Behaviour") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# significantly negative

# size bin boxplot
labridae$size_bin_char <- as.character(labridae$size_bin)
ggplot(labridae, aes(x = size_bin_char, y = log_difference, 
                         fill = size_bin_char)) +
  geom_boxplot() +
  theme_classic() + xlab("Size Bin") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# significantly positive