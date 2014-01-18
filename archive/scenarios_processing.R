#### CUFA 150 ####
# This scenario does not permit diversion withdrawals in the Gila River when natural flows are less than 150 cfs

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

# Read data for the scenario
CUFA_150 <- scenario.process("data/scenarios.csv", 3)
head(CUFA_150)

#### Add stage values (feet and cm) to data frame based on a regression equation ####
# The regression equation was developed using historical gage data
CUFA_150_stage <- stage(CUFA_150, head_conv)
CUFA_150 <- data.frame(CUFA_150, CUFA_150_stage)

#### Calculate recession rates ####
# The 1-, 2-, ... , 7-day recession rates were computed
recess_1d_s1 <- recession.rate(CUFA_150, 1)
# recess_2d_s1 <- recession.rate(CUFA_150, 2)
# recess_3d_s1 <- recession.rate(CUFA_150, 3)
# recess_4d_s1 <- recession.rate(CUFA_150, 4)
# recess_5d_s1 <- recession.rate(CUFA_150, 5)
# recess_6d_s1 <- recession.rate(CUFA_150, 6)
# recess_7d_s1 <- recession.rate(CUFA_150, 7)
# recess_14d_s1 <- recession.rate(CUFA_150, 14)

CUFA_150 <- cbind(CUFA_150, recess_1d_s1)
recess_2d_s1 <- as.data.frame(SMA(CUFA_150[,5], 2))
recess_3d_s1 <- as.data.frame(SMA(CUFA_150[,5], 3))
recess_4d_s1 <- as.data.frame(SMA(CUFA_150[,5], 4))
recess_5d_s1 <- as.data.frame(SMA(CUFA_150[,5], 5))
recess_6d_s1 <- as.data.frame(SMA(CUFA_150[,5], 6))
recess_7d_s1 <- as.data.frame(SMA(CUFA_150[,5], 7))
recess_14d_s1 <- as.data.frame(SMA(CUFA_150[,5], 14))
recess_45d_s1 <- as.data.frame(SMA(CUFA_150[,5], 45))

CUFA_150 <- cbind(CUFA_150, recess_2d_s1, recess_3d_s1, recess_4d_s1, recess_5d_s1, recess_6d_s1, recess_7d_s1, recess_14d_s1, recess_45d_s1)
colnames(CUFA_150)[6:13] <- c("recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")

#### Populate network states based on scenarios ####
# Discrete states (1, 2, 3) based on the timing
timing_state_s1 <- timing.states(CUFA_150)

# Discrete states (Y, N) based on inundation
q1_state_s1 <- q.states(CUFA_150, q_bin, 1)
q2_state_s1 <- q.states(CUFA_150, q_bin, 2)
q3_state_s1 <- q.states(CUFA_150, q_bin, 3)
q4_state_s1 <- q.states(CUFA_150, q_bin, 4)
q5_state_s1 <- q.states(CUFA_150, q_bin, 5)
q6_state_s1 <- q.states(CUFA_150, q_bin, 6)

# Discrete states based on recession rates
recess_state_s1 <- recess.states(CUFA_150, recess_rates, "recess_7d")

# Combine discrete states into single data frame
# q1_all_states_s1 <- cbind(timing_state_s1, q1_state_s1, recess_state_s1)
# q2_all_states_s1 <- cbind(timing_state_s1, q2_state_s1, recess_state_s1)
# q3_all_states_s1 <- cbind(timing_state_s1, q3_state_s1, recess_state_s1)
# q4_all_states_s1 <- cbind(timing_state_s1, q4_state_s1, recess_state_s1)
# q5_all_states_s1 <- cbind(timing_state_s1, q5_state_s1, recess_state_s1)
# q6_all_states_s1 <- cbind(timing_state_s1, q6_state_s1, recess_state_s1)

# # Remove evidence when FLOODING == N or RECESSION == 1 or RECESSION == 5
# q1_trim_states_s1 <- subset(q1_all_states_s1, TIMING != "NA" & FLOOD == "Y")
# q2_trim_states_s1 <- subset(q2_all_states_s1, TIMING != "NA" & FLOOD == "Y")
# q3_trim_states_s1 <- subset(q3_all_states_s1, TIMING != "NA" & FLOOD == "Y")
# q4_trim_states_s1 <- subset(q4_all_states_s1, TIMING != "NA" & FLOOD == "Y")
# q5_trim_states_s1 <- subset(q5_all_states_s1, TIMING != "NA" & FLOOD == "Y")
# q6_trim_states_s1 <- subset(q6_all_states_s1, TIMING != "NA" & FLOOD == "Y")

# q1_trim_states_s1 <- subset(q1_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q2_trim_states_s1 <- subset(q2_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q3_trim_states_s1 <- subset(q3_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q4_trim_states_s1 <- subset(q4_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q5_trim_states_s1 <- subset(q5_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q6_trim_states_s1 <- subset(q6_all_states_s1, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")

# all_trim_states_s1 <- list(q1_trim_states_s1, q2_trim_states_s1, q3_trim_states_s1, q4_trim_states_s1, q5_trim_states_s1, q6_trim_states_s1)


#### CUFA no minimum ####
# This scenario permits diversion withdrawals in the Gila River during all discharges

# Read data for the scenario
CUFA_nomin <- scenario.process("data/scenarios.csv", 4)
head(CUFA_nomin)

#### Add stage values (feet and cm) to data frame based on a regression equation ####
# The regression equation was developed using historical gage data
CUFA_nomin_stage <- stage(CUFA_nomin, head_conv)
CUFA_nomin <- data.frame(CUFA_nomin, CUFA_nomin_stage)

#### Calculate recession rates ####
# The 1-, 2-, ... , 7-day recession rates were computed
recess_1d_s2 <- recession.rate(CUFA_nomin, 1)
# recess_2d_s2 <- recession.rate(CUFA_nomin, 2)
# recess_3d_s2 <- recession.rate(CUFA_nomin, 3)
# recess_4d_s2 <- recession.rate(CUFA_nomin, 4)
# recess_5d_s2 <- recession.rate(CUFA_nomin, 5)
# recess_6d_s2 <- recession.rate(CUFA_nomin, 6)
# recess_7d_s2 <- recession.rate(CUFA_nomin, 7)
# recess_14d_s2 <- recession.rate(CUFA_nomin, 14)

CUFA_nomin <- cbind(CUFA_nomin, recess_1d_s2)
recess_2d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 2))
recess_3d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 3))
recess_4d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 4))
recess_5d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 5))
recess_6d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 6))
recess_7d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 7))
recess_14d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 14))
recess_45d_s2 <- as.data.frame(SMA(CUFA_nomin[,5], 45))

CUFA_nomin <- cbind(CUFA_nomin, recess_2d_s2, recess_3d_s2, recess_4d_s2, recess_5d_s2, recess_6d_s2, recess_7d_s2, recess_14d_s2, recess_45d_s2)
colnames(CUFA_nomin)[6:13] <- c("recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")

#### Populate network states based on scenarios ####
# Discrete states (1, 2, 3) based on the timing
timing_state_s2 <- timing.states(CUFA_nomin)

# Discrete states (Y, N) based on inundation
q1_state_s2 <- q.states(CUFA_nomin, q_bin, 1)
q2_state_s2 <- q.states(CUFA_nomin, q_bin, 2)
q3_state_s2 <- q.states(CUFA_nomin, q_bin, 3)
q4_state_s2 <- q.states(CUFA_nomin, q_bin, 4)
q5_state_s2 <- q.states(CUFA_nomin, q_bin, 5)
q6_state_s2 <- q.states(CUFA_nomin, q_bin, 6)

# Discrete states based on recession rates
recess_state_s2 <- recess.states(CUFA_nomin, recess_rates, "recess_7d")

# Combine discrete states into single data frame
# q1_all_states_s2 <- cbind(timing_state_s2, q1_state_s2, recess_state_s2)
# q2_all_states_s2 <- cbind(timing_state_s2, q2_state_s2, recess_state_s2)
# q3_all_states_s2 <- cbind(timing_state_s2, q3_state_s2, recess_state_s2)
# q4_all_states_s2 <- cbind(timing_state_s2, q4_state_s2, recess_state_s2)
# q5_all_states_s2 <- cbind(timing_state_s2, q5_state_s2, recess_state_s2)
# q6_all_states_s2 <- cbind(timing_state_s2, q6_state_s2, recess_state_s2)

# # Remove evidence when FLOODING == N or RECESSION == 1 or RECESSION == 5
# q1_trim_states_s2 <- subset(q1_all_states_s2, TIMING != "NA" & FLOOD == "Y")
# q2_trim_states_s2 <- subset(q2_all_states_s2, TIMING != "NA" & FLOOD == "Y")
# q3_trim_states_s2 <- subset(q3_all_states_s2, TIMING != "NA" & FLOOD == "Y")
# q4_trim_states_s2 <- subset(q4_all_states_s2, TIMING != "NA" & FLOOD == "Y")
# q5_trim_states_s2 <- subset(q5_all_states_s2, TIMING != "NA" & FLOOD == "Y")
# q6_trim_states_s2 <- subset(q6_all_states_s2, TIMING != "NA" & FLOOD == "Y")

# q1_trim_states_s2 <- subset(q1_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q2_trim_states_s2 <- subset(q2_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q3_trim_states_s2 <- subset(q3_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q4_trim_states_s2 <- subset(q4_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q5_trim_states_s2 <- subset(q5_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# q6_trim_states_s2 <- subset(q6_all_states_s2, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")

# all_trim_states_s2 <- list(q1_trim_states_s2, q2_trim_states_s2, q3_trim_states_s2, q4_trim_states_s2, q5_trim_states_s2, q6_trim_states_s2)

# c(all_trim_states, all_trim_states_s1, all_trim_states_s2)

# #### San Juaquin Post-dam ####
# 
# # Read data for the scenario
# postdam <- usgs.process("data/usgs_11251000_daily_postdam.txt")
# head(postdam)
# 
# #### Add stage values (feet and cm) to data frame based on a regression equation ####
# # The regression equation was developed using historical gage data
# postdam_stage <- stage(postdam, head_conv)
# postdam <- data.frame(postdam, postdam_stage)
# 
# #### Calculate recession rates ####
# # The 1-, 2-, ... , 7-day recession rates were computed
# recess_1d_pd <- recession.rate(postdam, 1)
# # recess_2d_pd <- recession.rate(postdam, 2)
# # recess_3d_pd <- recession.rate(postdam, 3)
# # recess_4d_pd <- recession.rate(postdam, 4)
# # recess_5d_pd <- recession.rate(postdam, 5)
# # recess_6d_pd <- recession.rate(postdam, 6)
# # recess_7d_pd <- recession.rate(postdam, 7)
# # recess_14d_pd <- recession.rate(postdam, 14)
# 
# postdam <- cbind(postdam, recess_1d_pd)
# recess_2d_pd <- as.data.frame(SMA(postdam[,5], 2))
# recess_3d_pd <- as.data.frame(SMA(postdam[,5], 3))
# recess_4d_pd <- as.data.frame(SMA(postdam[,5], 4))
# recess_5d_pd <- as.data.frame(SMA(postdam[,5], 5))
# recess_6d_pd <- as.data.frame(SMA(postdam[,5], 6))
# recess_7d_pd <- as.data.frame(SMA(postdam[,5], 7))
# recess_14d_pd <- as.data.frame(SMA(postdam[,5], 14))
# recess_45d_pd <- as.data.frame(SMA(postdam[,5], 45))
# 
# postdam <- cbind(postdam, recess_2d_pd, recess_3d_pd, recess_4d_pd, recess_5d_pd, recess_6d_pd, recess_7d_pd, recess_14d_pd, recess_45d_pd)
# colnames(postdam)[6:13] <- c("recess_2d", "recess_3d", "recess_4d", "recess_5d", "recess_6d", "recess_7d", "recess_14d", "recess_45d")
# 
# #### Populate network states based on scenarios ####
# # Discrete states (1, 2, 3) based on the timing
# timing_state_pd <- timing.states(postdam)
# 
# # Discrete states (Y, N) based on inundation
# q1_state_pd <- q.states(postdam, q_bin, 1)
# q2_state_pd <- q.states(postdam, q_bin, 2)
# q3_state_pd <- q.states(postdam, q_bin, 3)
# q4_state_pd <- q.states(postdam, q_bin, 4)
# q5_state_pd <- q.states(postdam, q_bin, 5)
# q6_state_pd <- q.states(postdam, q_bin, 6)
# 
# # Discrete states based on recession rates
# recess_state_pd <- recess.states(postdam, recess_rates, "recess_7d")
# 
# # Combine discrete states into single data frame
# # q1_all_states_pd <- cbind(timing_state_pd, q1_state_pd, recess_state_pd)
# # q2_all_states_pd <- cbind(timing_state_pd, q2_state_pd, recess_state_pd)
# # q3_all_states_pd <- cbind(timing_state_pd, q3_state_pd, recess_state_pd)
# # q4_all_states_pd <- cbind(timing_state_pd, q4_state_pd, recess_state_pd)
# # q5_all_states_pd <- cbind(timing_state_pd, q5_state_pd, recess_state_pd)
# # q6_all_states_pd <- cbind(timing_state_pd, q6_state_pd, recess_state_pd)
# 
# # # Remove evidence when FLOODING == N or RECESSION == 1 or RECESSION == 5
# # q1_trim_states_pd <- subset(q1_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# # q2_trim_states_pd <- subset(q2_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# # q3_trim_states_pd <- subset(q3_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# # q4_trim_states_pd <- subset(q4_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# # q5_trim_states_pd <- subset(q5_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# # q6_trim_states_pd <- subset(q6_all_states_pd, TIMING != "NA" & FLOOD == "Y")
# 
# # q1_trim_states_pd <- subset(q1_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# # q2_trim_states_pd <- subset(q2_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# # q3_trim_states_pd <- subset(q3_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# # q4_trim_states_pd <- subset(q4_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# # q5_trim_states_pd <- subset(q5_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# # q6_trim_states_pd <- subset(q6_all_states_pd, TIMING != "NA" & FLOOD == "Y" & RECESSION != "1" & RECESSION != "5")
# 
# # all_trim_states_pd <- list(q1_trim_states_pd, q2_trim_states_pd, q3_trim_states_pd, q4_trim_states_pd, q5_trim_states_pd, q6_trim_states_pd)
