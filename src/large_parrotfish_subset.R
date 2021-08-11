########## HERBIVORE SURVEY COMPARISON: LARGE PARROTFISH ##########
########## 
##########
# This file subsets available survey data to look specifically at parrotfish 
# species > 16cm in length (size bins 4-6) and compares their density estimates 
# between SVC and transect surveys. 
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


# Parrotfish Subsets ===========================================================

# The following subsets the parrotfish dataframe for large-bodied species (size 
# bins 4-6).

# subset for parrotfish
parrotfish <- herbivores[herbivores$family == "Labridae",]

# subset for large species
large_parrotfish <- parrotfish[parrotfish$size_bin %in% c(4, 5, 6),]


# Large Parrotfish Global Model ================================================

# The following section creates a global linear mixed effects model to compare 
# large parrotfish density differences between SVC and transect surveys in 
# relation to species, habitat, and survey traits. It explores collinearity by 
# examining VIF values of predictors and determines model fit using random 
# effects plots, residual plots, qq plots, and model plots. 

# full model 
large_parrotfish_global <- lme(log_difference~habitat+octocoral+stony+relief_cm
                              +max_length+cryptic_behaviour
                              +average_depth+size_bin_lengths, 
                              random = list(~1|site, ~1|species_order), 
                              large_parrotfish) 
# removed shape because all are fusiform

# summary without behaviour
summary(large_parrotfish_global)

# VIF values 
vif(large_parrotfish_global)
# no collinearity 


# Large Parrotfish Dredge ======================================================

# The following dredges the global linear mixed effects model in order to 
# determine the combinations of predictors that result in the models with the 
# highest likelihood.

# dredge
large_parrotfish_dredge <- dredge(large_parrotfish_global)
large_parrotfish_dredge

# subset for delta AIC < 4
large_parrotfish_dredge_sub <- subset(large_parrotfish_dredge, delta < 4)

# model average of top models 
large_parrotfish_model_avg <- model.avg(large_parrotfish_dredge_sub)
summary(large_parrotfish_model_avg)

# confidence intervals of top model covariates
confint(large_parrotfish_model_avg)

# save dredge outputs 
saveRDS(large_parrotfish_dredge, here("./outputs/large_parrotfish_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# reef type boxplot
ggplot(large_parrotfish, aes(x = habitat, y = log_difference, 
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
# insignificant

# cryptic behaviour boxplot
large_parrotfish$cryptic_char <- 
  as.character(large_parrotfish$cryptic_behaviour)
ggplot(large_parrotfish, aes(x = cryptic_char, y = log_difference, 
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
large_parrotfish$size_bin_char <- as.character(large_parrotfish$size_bin)
ggplot(large_parrotfish, aes(x = size_bin_char, y = log_difference, 
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