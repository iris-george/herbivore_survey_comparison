########## HERBIVORE SURVEY COMPARISON: SMALL SPECIES ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species between 0-15cm in length (size bins 1-3) and compares their 
# density estimates between SVC and transect surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-06-11
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


# Size Bin Subsets =============================================================

# The following subsets the herbivore dataframe for small-bodied species (size 
# bins 1-3).

# subset for small species
small_herbivores <- herbivores[herbivores$size_bin %in% c(1, 2, 3),]


# Small Herbivore Global Model =================================================

# The following section creates a global linear mixed effects model to compare 
# small herbivore density differences between SVC and transect surveys in 
# relation to species, habitat, and survey traits. It explores collinearity by 
# examining VIF values of predictors and determines model fit using random 
# effects plots, residual plots, qq plots, and model plots. 

# full model 
small_herbivore_global <- lme(log_difference~habitat+octocoral+stony+relief_cm
                        +max_length+cryptic_behaviour
                        +average_depth+size_bin_lengths+shape, 
                        random = list(~1|site, ~1|species_order), 
                        small_herbivores) 
# removed size_bin*shape interaction because it was insignificant 

# summary without behaviour
summary(small_herbivore_global)

# VIF values 
vif(small_herbivore_global)
# no collinearity 


# Small Herbivore Dredge =======================================================

# The following dredges the global linear mixed effects model in order to 
# determine the combinations of predictors that result in the models with the 
# highest likelihood.

# dredge
small_herbivore_dredge <- dredge(small_herbivore_global)
small_herbivore_dredge

# subset for delta AIC < 4
small_herbivore_dredge_sub <- subset(small_herbivore_dredge, delta < 4)

# model average of top models 
small_herbivore_model_avg <- model.avg(small_herbivore_dredge_sub)
summary(small_herbivore_model_avg)

# confidence intervals of top model covariates
confint(small_herbivore_model_avg)

# save dredge outputs 
saveRDS(small_herbivore_dredge, here("./outputs/small_herbivore_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# reef type boxplot
ggplot(small_herbivores, aes(x = habitat, y = log_difference, 
                       fill = habitat)) +
  geom_boxplot() +
  theme_classic() + xlab("Reef Type") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# continuous to patch significantly negative

# octocoral plot
plot(small_herbivores$log_difference ~ small_herbivores$octocoral)
# significantly positive

# stony coral plot
plot(small_herbivores$log_difference ~ small_herbivores$stony)
# significantly positive

# maximum length plot
plot(small_herbivores$log_difference ~ small_herbivores$max_length)
# significantly positive

# cryptic behaviour boxplot
small_herbivores$cryptic_char <- 
  as.character(small_herbivores$cryptic_behaviour)
ggplot(small_herbivores, aes(x = cryptic_char, y = log_difference, 
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
# insignificant

# size bin boxplot
small_herbivores$size_bin_char <- as.character(small_herbivores$size_bin)
ggplot(small_herbivores, aes(x = size_bin_char, y = log_difference, 
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

# shape boxplot
ggplot(small_herbivores, aes(x = shape, y = log_difference, fill = shape)) +
  geom_boxplot() +
  theme_classic() + xlab("Body Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  scale_fill_brewer(palette = "Dark2") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# fusiform significantly negative