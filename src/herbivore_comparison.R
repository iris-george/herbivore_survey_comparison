########## SURVEY COMPARISON PROJECT HERBIVORE SUBSET ##########
########## 
##########
# This file subsets available survey data to look specifically at herbivorous 
# fish species and compares their density estimates between SVC and transect
# surveys. 
##########
##########
# AUTHOR: Iris M. George
# DATE OF CREATION: 2021-05-28
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
SVCprey_model_data <- read_csv(here("./dataframes/SVCprey_dataframe.csv"))
prey_fish <- read_csv(here("./clean_data/prey_fish_data.csv"))
prey_meta <- read_csv(here("./clean_data/prey_metadata.csv"))
SVC_meta <- read_csv(here("./clean_data/SVC_data.csv"))
SVC_lengths <- read_csv(here("./clean_data/SVC_lengths.csv"))
traits <- read_csv(here("./clean_data/fish_traits.csv"))


# Select Herbivorous Species ===================================================

# The following section filters the dataframe comparing all fish species 
# detected on SVC and transect surveys to contain only herbivorous species for
# further analysis.

# filter whole dataset for herbivores
herbivores <- filter(SVCprey_model_data, trophic_position == "herbivore")

# change cryptic behaviour to categorical
herbivores$cryptic_behaviour <- as.character(herbivores$cryptic_behaviour)

# export dataframe
write.csv(herbivores, here("./dataframes/herbivore_dataframe.csv"))


# Global Herbivore Linear Mixed Effects Model ==================================

# The following section creates a global linear mixed effects model to compare 
# herbivore density differences between SVC and transect surveys in relation to
# species, habitat, and survey traits. It explores collinearity by examining 
# VIF values of predictors and determines model fit using random effects plots,
# residual plots, qq plots, and model plots. 

# global lme model
herbivore_full <- lme(log_difference~habitat+octocoral+stony+relief_cm
                      +max_length+behavior+cryptic_behaviour
                      +average_depth+size_bin_lengths*shape, 
                      random = list(~1|site, ~1|species_order), 
                      herbivores) 
# response is log density difference, random effects are site and species order
# removed colouration*size_bin interaction because of single levels in camo
# removed nocturnal because all are not
# removed position because all are demersal 
# tried size_bin*colouration but was marginally insignificant (p = 0.0511)

# model summary
summary(herbivore_full) 
# AIC = 6524.376
# significant predictors: octocoral, stony, max_length, cryptic_behaviour, 
# size_bin, fusiform, size_bin*fusiform

# covariate VIF values
vif(herbivore_full)
# habitat GVIF = 5.272865
# behaviour GVIF = 14.420305
# size bin GVIF = 5.395349
# shape GVIF = 25.652292

# model without aggregation behaviour
herbivore_global <- lme(log_difference~habitat+octocoral+stony+relief_cm
                        +max_length+cryptic_behaviour
                        +average_depth+size_bin_lengths*shape, 
                        random = list(~1|site, ~1|species_order), 
                        herbivores) 

# summary without behaviour
summary(herbivore_global)

# VIF values 
vif(herbivore_global)
# model without aggregation behaviour improves fit, using this as global 


# Dredging =====================================================================

# The following dredges the global linear mixed effects model not including 
# behaviour as a predictor in order to determine the combinations of predictors
# that result in the models with the highest likelihood.

# dredge
herbivore_dredge <- dredge(herbivore_global)
herbivore_dredge

# subset for delta AIC < 4
herbivore_dredge_sub <- subset(herbivore_dredge, delta < 4)

# model average of top models 
herbivore_model_avg <- model.avg(herbivore_dredge_sub)
summary(herbivore_model_avg)

# confidence intervals of top model covariates
confint(herbivore_model_avg)

# save dredge outputs 
saveRDS(herbivore_dredge, here("./outputs/herbivore_dredge.rds"))


# Covariate Plots ==============================================================

# The following creates scatterplots and boxplots of all significant predictors 
# from the mixed linear effects model. 

# reef type boxplot
ggplot(herbivores, aes(x = habitat, y = log_difference, 
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
plot(herbivores$log_difference ~ herbivores$octocoral)
# significantly positive

# stony coral plot
plot(herbivores$log_difference ~ herbivores$stony)
# significantly positive

# maximum length plot
plot(herbivores$log_difference ~ herbivores$max_length)
# significantly positive

# cryptic behaviour boxplot
ggplot(herbivores, aes(x = cryptic_behaviour, y = log_difference, 
                       fill = cryptic_behaviour)) +
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
herbivores$size_bin_char <- as.character(herbivores$size_bin)
ggplot(herbivores, aes(x = size_bin_char, y = log_difference, 
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

# shape boxplot
ggplot(herbivores, aes(x = shape, y = log_difference, fill = shape)) +
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

# shape*size_bin interaction boxplot
ggplot(herbivores, aes(shape, log_difference, fill = size_bin_char)) + 
  geom_boxplot(show.legend = TRUE) + 
  theme_classic() + 
  scale_fill_brewer(name = "Size Bin", palette = "YlGnBu") + 
  xlab("Shape") + 
  ylab(bquote("Log Density Difference " (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 18)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "grey40")
# size_bin*fusiform significantly positive


# Density Barplot ==============================================================

# The following creates a barplot of the average density differences of each 
# herbivore species.

# select transect species and session columns
prey_species <- prey_fish[,c(1,3)]

# aggregate species by session
prey_species <- prey_fish %>% group_by(session, species) %>% tally()

# rename abundance column
prey_species <- prey_species %>% rename(prey_abundance = n)

# select transect session and area columns
prey_area <- prey_meta[,c(1,15)]

# aggregate area by session
prey_area <- aggregate(.~session, prey_area, sum)

# join area to fish data
prey_density <- join(prey_species, prey_area, by = NULL, type = "full", 
                     match = "all")

# select SVC session, species, and abundance columns
SVC_density <- SVC_lengths[,c(1,2,5)]

# select SVC area column
SVC_area <- SVC_meta[,c(1,12)]

# join SVC_area and SVC_density
SVC_density <- join(SVC_density, SVC_area, by = NULL, type = "left", 
                    match = "first")

# transect density calculation
prey_density$prey_density <- 
  (prey_density$prey_abundance)/(prey_density$prey_tran_area)

# SVC density calculation 
SVC_density$SVC_density <- 
  (SVC_density$SVC_abundance)/(SVC_density$SVC_cylinder_area)

# join SVC to transect data
SVCprey_fishdens <- join(prey_density, SVC_density, by = NULL, type = "full", 
                         match = "all")

# select session, species, and density columns
SVCprey_fishdens <- SVCprey_fishdens[,c(1,2,5,8)]

# replace NA values with 0
SVCprey_fishdens[is.na(SVCprey_fishdens)] = 0

# SVC vs. transect survey density difference calculation
SVCprey_fishdens$density_difference <- 
  SVCprey_fishdens$SVC_density-SVCprey_fishdens$prey_density

# select trophic categories
fnt_group <- traits[,c(4,70)]

# re-name species column
fnt_group <- fnt_group %>% rename(species = common_name)

# join trophic categories to dataframe
SVCprey_fishdens <- join(SVCprey_fishdens, fnt_group, by = NULL, type = "full", 
                         match = "first")

# filter for herbivores
herbivore_bar <- filter(SVCprey_fishdens, trophic_position == "herbivore")

# select species and density difference columns from SVC vs. transect data
herbivore_bar <- herbivore_bar[,c(1,5)]

# aggregate by species 
herbivore_bar <- summaryBy(density_difference~species, data=herbivore_bar, 
                         FUN=c(mean,sd))

# rename columns
herbivore_bar <- herbivore_bar %>% 
  rename(avg_density_dif = density_difference.mean)
herbivore_bar <- herbivore_bar %>% 
  rename(sd_density_dif = density_difference.sd)

# replace NA values with 0
herbivore_bar[is.na(herbivore_bar)] <- 0

# SVC vs. transect survey herbivore barplot
herbivore_barplot <- ggplot(data=herbivore_bar, 
                          aes(x=species, y=avg_density_dif)) +
  ylim(-0.2, 0.1) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + xlab("Species") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
herbivore_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black")

# barplot with error bars
herbivore_error_barplot <- ggplot(data=herbivore_bar, 
                                aes(x=species, y=avg_density_dif)) +
  geom_bar(stat="identity", fill="blue") +
  theme_classic() + xlab("Family") + 
  ylab(bquote("Mean Density Difference" (individuals/m^2))) +
  theme(axis.title = element_text(size = 20)) +
  theme(axis.text= element_text(size = 14)) +
  theme(legend.text = element_text(size = 18)) +
  theme(legend.title = element_text(size = 20)) 
herbivore_error_barplot + coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black") +
  geom_errorbar(aes(x=species, ymin=avg_density_dif-sd_density_dif, 
                    ymax=avg_density_dif+sd_density_dif), width = 0.2, 
                colour = "black", alpha = 0.9, size = 1.3)