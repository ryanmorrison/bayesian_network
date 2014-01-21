#### Load libraries ####
library(reshape2)
library(ggplot2)
library(scales) # Library for modifying scales on plots
library(grid) # Library for developing nonstandard plotting themes

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### Source common functions ####
source('~/Dropbox/Gila Bayesian/bayesian_network/1-functions.R')

site1_data <- load.probs("site1")
E_site1 <- data.frame(combine.probs.E(), scenario = "existing")
CUFA_150_site1 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
CUFA_nomin_site1 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
combined_site1 <- data.frame(rbind(E_site1, CUFA_150_site1, CUFA_nomin_site1), site = "site1")

site2_data <- load.probs("site2")
E_site2 <- data.frame(combine.probs.E(), scenario = "existing")
CUFA_150_site2 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
CUFA_nomin_site2 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
combined_site2 <- data.frame(rbind(E_site2, CUFA_150_site2, CUFA_nomin_site2), site = "site2")

site4_data <- load.probs("site4")
E_site4 <- data.frame(combine.probs.E(), scenario = "existing")
CUFA_150_site4 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
CUFA_nomin_site4 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
combined_site4 <- data.frame(rbind(E_site4, CUFA_150_site4, CUFA_nomin_site4), site = "site4")

combined_all_sites <- rbind(combined_site1, combined_site2, combined_site4)

load("output/combined_site1.Rdata")
load("output/combined_site2.Rdata")
load("output/combined_site4.Rdata")

combined_all_sites <- rbind(combined_site1, combined_site2, combined_site4)

test <- ggplot(data = combined_all_sites, aes(x = scenario, y = util_prob))
test <- test + geom_boxplot()
test <- test + facet_grid(site~Q_bin)
test <- test + stat_summary(fun.y= mean, geom = "point", color = "red")
test

test <- ggplot(data = combined_all_sites, aes(x = scenario, y = util_prob))
test <- test + geom_boxplot()
test <- test + facet_grid(site~ .)
test <- test + stat_summary(fun.y= mean, geom = "point", color = "red", size = 2.5)
test

