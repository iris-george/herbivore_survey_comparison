########## HERBIVORE COMPARISON PROJECT: ACANTHURIDAE SUBSET ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species from the family Acanthuridae and compares their density 
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


# Acanthuridae Subset =========================================================

# The following subsets the herbivore dataframe for species from the family 
# acanthuridae.

# subset for Acanthuridae
acanthuridae <- herbivores[herbivores$family == "Acanthuridae",]


# Global acanthuridae Linear Mixed Effects Model ==============================

# The following section creates a global linear mixed effects model to compare 
# acanthuridae density differences between SVC and transect surveys in relation 
# to species, habitat, and survey traits. It explores collinearity by examining 
# VIF values of predictors and determines model fit using random effects plots,
# residual plots, qq plots, and model plots. 

# global model
acanthuridae_global <- lme(log_difference~octocoral+stony+relief_cm
                            +max_length+average_depth+size_bin_lengths, 
                            random = list(~1|site, ~1|species_order), 
                            acanthuridae) 
# removed cryptic behaviour because none display it
# removed shape because all are compressiform
# habitat VIF = 5.462706 and insignificant, so removed

# summary without behaviour
summary(acanthuridae_global)

# VIF values 
vif(acanthuridae_global)
# no collinearity 


# Dredging =====================================================================

# The following dredges the global linear mixed effects model in order to
# determine the combinations of predictors that result in the models with the 
# highest likelihood.

# dredge
acanthuridae_dredge <- dredge(acanthuridae_global)
acanthuridae_dredge

# subset for delta AIC < 4
acanthuridae_dredge_sub <- subset(acanthuridae_dredge, delta < 4)

# model average of top models 
acanthuridae_model_avg <- model.avg(acanthuridae_dredge_sub)
summary(acanthuridae_model_avg)

# confidence intervals of top model covariates
confint(acanthuridae_model_avg)

# save dredge outputs 
saveRDS(acanthuridae_dredge, here("./outputs/acanthuridae_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# octocoral plot
plot(acanthuridae$log_difference ~ acanthuridae$octocoral)
# significantly positive

# maximum length plot
plot(acanthuridae$log_difference ~ acanthuridae$max_length)
# insignificant

# average depth plot
plot(acanthuridae$log_difference ~ acanthuridae$average_depth)
# significantly positive

# size bin boxplot
acanthuridae$size_bin_char <- as.character(acanthuridae$size_bin)
ggplot(acanthuridae, aes(x = size_bin_char, y = log_difference, 
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
# significantly negative 