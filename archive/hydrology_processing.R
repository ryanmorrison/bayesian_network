#### Hydrologic Data Processessing ####
library(xts)
library(prob)
library(TTR)
library(bigmemory)
source("R/functions.R")

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### WINDOWS WORKING DIRECTORY####
# setwd("/Users/rmorriso/Documents/bayesian_network")

#### Process USGS hydrology data ####
# gila <- usgs.process("data/usgs_09430500_daily.txt")
# mogollon <- usgs.process("data/usgs_09430600_daily.txt")

# # Trim Gila gage data before 1967-02-21 (start date of Mogollon data)
# startindex <- grep("1967-02-21", gila$date)
# gila <- gila[startindex:nrow(gila),]

# # Trim Mogollon gage data after 2013-09-24 (end date of Gila data)
# endindex <- grep("2013-09-24", mogollon$date)
# mogollon <- mogollon[1:endindex,]

# # Double check that all the dates align
# doublecheck <- count(mogollon$date == gila$date)

# # Add the Gila and Mogollon dataset to produce hydrology at the research sites
# hydro <- data.frame(date=gila$date)
# hydro$cfs <- gila$cfs + mogollon$cfs
hydro <- usgs.process("data/usgs_09430500_daily.txt")

#### Add stage values (feet and cm) to data frame based on a regression equation ####
# The regression equation was developed using historical gage data, and changes depending on the site location

# SITE 1
head_conv <- function(Q) 0.0011*Q

# SITE 2
# head_conv <- function(Q) 0.0011*Q

# SITE 3
# head_conv <- function(Q) 0.056*(Q^0.5612)

# SITE 4
# head_conv <- function(Q) 0.0014*(Q^0.99)

# SITE 5
# head_conv <- function(Q) 0.1123*(Q^0.4963)

hydro_stage <- stage(hydro, head_conv)
hydro <- data.frame(hydro, hydro_stage)

#### Calculate recession rates ####
# The 1-, 2-, ... , 7-day, 14-day recession rates were computed
recess_1d <- recession.rate(hydro, 1)
# recess_2d <- recession.rate(hydro, 2)
# recess_3d <- recession.rate(hydro, 3)
# recess_4d <- recession.rate(hydro, 4)
# recess_5d <- recession.rate(hydro, 5)
# recess_6d <- recession.rate(hydro, 6)
# recess_7d <- recession.rate(hydro, 7)
# recess_14d <- recession.rate(hydro, 14)
# recess_45d <- recession.rate(hydro, 45)

hydro <- cbind(hydro, recess_1d)
recess_2d <- as.data.frame(SMA(hydro[,5], 2))
recess_3d <- as.data.frame(SMA(hydro[,5], 3))
recess_4d <- as.data.frame(SMA(hydro[,5], 4))
recess_5d <- as.data.frame(SMA(hydro[,5], 5))
recess_6d <- as.data.frame(SMA(hydro[,5], 6))
recess_7d <- as.data.frame(SMA(hydro[,5], 7))
recess_14d <- as.data.frame(SMA(hydro[,5], 14))
recess_45d <- as.data.frame(SMA(hydro[,5], 45))

hydro <- cbind(hydro, recess_2d, recess_3d, recess_4d, recess_5d, recess_6d, recess_7d, recess_14d, recess_45d)
colnames(hydro)[6:13] <- c("recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")

#### Process Q-model bins ####
# Model Q-bins were selected so that inundation areas increased by less than 15% between the discharge associated with each bin
# q_bin <- c(719, 1209, 2100, 4000, 6296, 11300)
q_bin <- c(1000, 1200, 1400, 1600, 1800, 2000, 2200)

# Calculate probability of flooding in each bin for the entire year
q_bin_prob <- q.prob(hydro, q_bin)

# Calculate probabilities in each bin according to discrete timing states
# The discrete timing states are April-May, June-July, August-September
hydro_monthly <- subset.month(hydro)

apr_may <- list.to.df(hydro_monthly[[1]])
jun_jul <- list.to.df(hydro_monthly[[2]])
aug_sep <- list.to.df(hydro_monthly[[3]])

q_prob_apr_may <- q.prob(apr_may, q_bin)
q_prob_jun_jul <- q.prob(jun_jul, q_bin)
q_prob_aug_sep <- q.prob(aug_sep, q_bin)

q_prob_all <- data.frame(q_prob_apr_may, q_prob_jun_jul, q_prob_aug_sep)
# write.table(q_prob_all, "output/q_prob_all.txt", sep="\t")

# Test calculations
# R <- probspace(apr_may)
# Y <- prob(R, cfs>=719)
#
# C <- count(apr_may$cfs>=719)

#### Calculate probabilities for recession rates ####
# Test calculations
# S <- probspace(apr_may)
# P <- prob(S, recess_6d>=6)
# X <- S[["recess_1d"]]

# Set discrete states for recession rates (in cm/day)
recess_rates <- c(0, 1, 3, 6)

# Calculate recession rate probabilities for April-May
recess_1d_apr_may <- r.prob(apr_may, "recess_1d", recess_rates)
recess_2d_apr_may <- r.prob(apr_may, "recess_2d", recess_rates)
recess_3d_apr_may <- r.prob(apr_may, "recess_3d", recess_rates)
recess_4d_apr_may <- r.prob(apr_may, "recess_4d", recess_rates)
recess_5d_apr_may <- r.prob(apr_may, "recess_5d", recess_rates)
recess_6d_apr_may <- r.prob(apr_may, "recess_6d", recess_rates)
recess_7d_apr_may <- r.prob(apr_may, "recess_7d", recess_rates)
recess_14d_apr_may <- r.prob(apr_may, "recess_14d", recess_rates)
recess_45d_apr_may <- r.prob(apr_may, "recess_45d", recess_rates)

recess_all_apr_may <- cbind(recess_1d_apr_may, recess_2d_apr_may, recess_3d_apr_may, recess_4d_apr_may, recess_5d_apr_may, recess_6d_apr_may, recess_7d_apr_may, recess_14d_apr_may, recess_45d_apr_may)
colnames(recess_all_apr_may) <- c("recess_1d", "recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")
rownames(recess_all_apr_may) <- c("<0", "0-1", "1-3", "3-6", ">6")
recess_all_apr_may <- t(recess_all_apr_may)
# write.table(recess_all_apr_may, "output/recess_all_apr_may.txt", sep="\t")

# Calculate recession rate probabilities for June-July
recess_1d_jun_jul <- r.prob(jun_jul, "recess_1d", recess_rates)
recess_2d_jun_jul <- r.prob(jun_jul, "recess_2d", recess_rates)
recess_3d_jun_jul <- r.prob(jun_jul, "recess_3d", recess_rates)
recess_4d_jun_jul <- r.prob(jun_jul, "recess_4d", recess_rates)
recess_5d_jun_jul <- r.prob(jun_jul, "recess_5d", recess_rates)
recess_6d_jun_jul <- r.prob(jun_jul, "recess_6d", recess_rates)
recess_7d_jun_jul <- r.prob(jun_jul, "recess_7d", recess_rates)
recess_14d_jun_jul <- r.prob(jun_jul, "recess_14d", recess_rates)
recess_45d_jun_jul <- r.prob(jun_jul, "recess_45d", recess_rates)

recess_all_jun_jul <- cbind(recess_1d_jun_jul, recess_2d_jun_jul, recess_3d_jun_jul, recess_4d_jun_jul, recess_5d_jun_jul, recess_6d_jun_jul, recess_7d_jun_jul, recess_14d_jun_jul, recess_45d_jun_jul)
colnames(recess_all_jun_jul) <- c("recess_1d", "recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")
rownames(recess_all_jun_jul) <- c("<0", "0-1", "1-3", "3-6", ">6")
recess_all_jun_jul <- t(recess_all_jun_jul)
# write.table(recess_all_jun_jul, "output/recess_all_jun_jul.txt", sep="\t")

# Calculate recession rate probabilities for August-September
recess_1d_aug_sep <- r.prob(aug_sep, "recess_1d", recess_rates)
recess_2d_aug_sep <- r.prob(aug_sep, "recess_2d", recess_rates)
recess_3d_aug_sep <- r.prob(aug_sep, "recess_3d", recess_rates)
recess_4d_aug_sep <- r.prob(aug_sep, "recess_4d", recess_rates)
recess_5d_aug_sep <- r.prob(aug_sep, "recess_5d", recess_rates)
recess_6d_aug_sep <- r.prob(aug_sep, "recess_6d", recess_rates)
recess_7d_aug_sep <- r.prob(aug_sep, "recess_7d", recess_rates)
recess_14d_aug_sep <- r.prob(aug_sep, "recess_14d", recess_rates)
recess_45d_aug_sep <- r.prob(aug_sep, "recess_45d", recess_rates)

recess_all_aug_sep <- cbind(recess_1d_aug_sep, recess_2d_aug_sep, recess_3d_aug_sep, recess_4d_aug_sep, recess_5d_aug_sep, recess_6d_aug_sep, recess_7d_aug_sep, recess_14d_aug_sep, recess_45d_aug_sep)
colnames(recess_all_aug_sep) <- c("recess_1d", "recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")
rownames(recess_all_aug_sep) <- c("<0", "0-1", "1-3", "3-6", ">6")
recess_all_aug_sep <- t(recess_all_aug_sep)
# recess_all_aug_sep[,5] <- recess_all_aug_sep[,5] + 0.0002154 # Add small number to get probability sums to equal 1
# write.table(recess_all_aug_sep, "output/recess_all_aug_sep.txt", sep="\t")

recess_prob_all <- cbind(recess_all_apr_may, recess_all_jun_jul, recess_all_aug_sep)
# write.table(recess_prob_all, "output/recess_prob_all.txt", sep="\t")
# recess_prob_all[,5] <- recess_prob_all[,5] + 0.00215

#### Populate network states based on scenarios ####
# Discrete states (1, 2, 3) based on the timing
timing_state <- timing.states(hydro)

# Discrete states (Y, N) based on inundation
q1_state <- q.states(hydro, q_bin, 1)
q2_state <- q.states(hydro, q_bin, 2)
q3_state <- q.states(hydro, q_bin, 3)
q4_state <- q.states(hydro, q_bin, 4)
q5_state <- q.states(hydro, q_bin, 5)
q6_state <- q.states(hydro, q_bin, 6)

# Discrete states based on recession rates
recess_state <- recess.states(hydro, recess_rates, "recess_7d")

# Combine discrete states into single data frame
# q1_all_states <- cbind(timing_state, q1_state, recess_state)
# q2_all_states <- cbind(timing_state, q2_state, recess_state)
# q3_all_states <- cbind(timing_state, q3_state, recess_state)
# q4_all_states <- cbind(timing_state, q4_state, recess_state)
# q5_all_states <- cbind(timing_state, q5_state, recess_state)
# q6_all_states <- cbind(timing_state, q6_state, recess_state)

# # Remove evidence when FLOODING == N or RECESSION == 1 or RECESSION == 5
# q1_trim_states <- subset(q1_all_states, TIMING != "NA" & FLOOD == "Y")
# q2_trim_states <- subset(q2_all_states, TIMING != "NA" & FLOOD == "Y")
# q3_trim_states <- subset(q3_all_states, TIMING != "NA" & FLOOD == "Y")
# q4_trim_states <- subset(q4_all_states, TIMING != "NA" & FLOOD == "Y")
# q5_trim_states <- subset(q5_all_states, TIMING != "NA" & FLOOD == "Y")
# q6_trim_states <- subset(q6_all_states, TIMING != "NA" & FLOOD == "Y")

# q1_trim_states <- subset(q1_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q2_trim_states <- subset(q2_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q3_trim_states <- subset(q3_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q4_trim_states <- subset(q4_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q5_trim_states <- subset(q5_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q6_trim_states <- subset(q6_all_states, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")

# all_trim_states <- list(q1_trim_states, q2_trim_states, q3_trim_states, q4_trim_states, q5_trim_states, q6_trim_states)
# all_trim_states