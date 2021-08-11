########## HERBIVORE COMPARISON PROJECT: POMACENTRIDAE SUBSET ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species from the family Pomacentridae and compares their density 
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


# Pomacentridae Subset =========================================================

# The following subsets the herbivore dataframe for species from the family 
# Pomacentridae.

# subset for Pomacentridae
pomacentridae <- herbivores[herbivores$family == "Pomacentridae",]


# Global Pomacentridae Linear Mixed Effects Model ==============================

# The following section creates a global linear mixed effects model to compare 
# Pomacentridae density differences between SVC and transect surveys in relation 
# to species, habitat, and survey traits. It explores collinearity by examining 
# VIF values of predictors and determines model fit using random effects plots,
# residual plots, qq plots, and model plots. 

# global model
pomacentridae_global <- lme(log_difference~habitat+octocoral+stony+relief_cm
                        +max_length+average_depth+size_bin_lengths, 
                        random = list(~1|site, ~1|species_order), 
                        pomacentridae) 
# removed cryptic behaviour because none display it
# removed shape because all are compressiform

# summary without behaviour
summary(pomacentridae_global)

# VIF values 
vif(pomacentridae_global)
# no collinearity 


# Dredging =====================================================================

# The following dredges the global linear mixed effects model in order to
# determine the combinations of predictors that result in the models with the 
# highest likelihood.

# dredge
pomacentridae_dredge <- dredge(pomacentridae_global)
pomacentridae_dredge

# subset for delta AIC < 4
pomacentridae_dredge_sub <- subset(pomacentridae_dredge, delta < 4)

# model average of top models 
pomacentridae_model_avg <- model.avg(pomacentridae_dredge_sub)
summary(pomacentridae_model_avg)

# confidence intervals of top model covariates
confint(pomacentridae_model_avg)

# save dredge outputs 
saveRDS(pomacentridae_dredge, here("./outputs/pomacentridae_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# reef type boxplot
ggplot(pomacentridae, aes(x = habitat, y = log_difference, 
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

# maximum length plot
plot(pomacentridae$log_difference ~ pomacentridae$max_length)
# significantly positive