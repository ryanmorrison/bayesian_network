#### EXISTING CONDITIONS ####

p_q1_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q1_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q1_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q1_e <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q2_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q2_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q2_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q2_e <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q3_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q3_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q3_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q3_e <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q4_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q4_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q4_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q4_e <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q5_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q5_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q5_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q5_e <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q6_d1_e <- vector() # Vector for groundwater depth "1" probabilities
p_q6_d2_e <- vector() # Vector for groundwater depth "2" probabilities
p_q6_d3_e <- vector() # Vector for groundwater depth "3" probabilities
p_q6_e <- vector() # Vector for probabilities with no evidence of groundwater depth

# Calculate probabilities for bin Q1 #
for (i in 1:nrow(q1_trim_states)) p_q1_d1_e[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q1_trim_states)) p_q1_d2_e[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q1_trim_states)) p_q1_d3_e[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q1_trim_states)) p_q1_e[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states[i,]), method="lw")
p_q1_e_all <- as.data.frame(matrix(c(p_q1_e, p_q1_d1_e, p_q1_d2_e, p_q1_d3_e), ncol=4))
colnames(p_q1_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q1_e_mean <- apply(p_q1_e_all, 2, mean, na.rm=TRUE)

# Calculate probabilities for bin Q2 #
for (i in 1:nrow(q2_trim_states)) p_q2_d1_e[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q2_trim_states)) p_q2_d2_e[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q2_trim_states)) p_q2_d3_e[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q2_trim_states)) p_q2_e[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=as.list(q2_trim_states[i,]), method="lw")
p_q2_e_all <- as.data.frame(matrix(c(p_q2_e, p_q2_d1_e, p_q2_d2_e, p_q2_d3_e), ncol=4))
colnames(p_q2_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q2_e_mean <- apply(p_q2_e_all, 2, mean, na.rm=TRUE)

# Calculate probabilities for bin Q3 #
for (i in 1:nrow(q3_trim_states)) p_q3_d1_e[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q3_trim_states)) p_q3_d2_e[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q3_trim_states)) p_q3_d3_e[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q3_trim_states)) p_q3_e[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=as.list(q3_trim_states[i,]), method="lw")
p_q3_e_all <- as.data.frame(matrix(c(p_q3_e, p_q3_d1_e, p_q3_d2_e, p_q3_d3_e), ncol=4))
colnames(p_q3_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q3_e_mean <- apply(p_q3_e_all, 2, mean, na.rm=TRUE)

# Calculate probabilities for bin Q4 #
for (i in 1:nrow(q4_trim_states)) p_q4_d1_e[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q4_trim_states)) p_q4_d2_e[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q4_trim_states)) p_q4_d3_e[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q4_trim_states)) p_q4_e[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=as.list(q4_trim_states[i,]), method="lw")
p_q4_e_all <- as.data.frame(matrix(c(p_q4_e, p_q4_d1_e, p_q4_d2_e, p_q4_d3_e), ncol=4))
colnames(p_q4_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q4_e_mean <- apply(p_q4_e_all, 2, mean, na.rm=TRUE)

# Calculate probabilities for bin Q5 #
for (i in 1:nrow(q5_trim_states)) p_q5_d1_e[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q5_trim_states)) p_q5_d2_e[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q5_trim_states)) p_q5_d3_e[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q5_trim_states)) p_q5_e[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=as.list(q5_trim_states[i,]), method="lw")
p_q5_e_all <- as.data.frame(matrix(c(p_q5_e, p_q5_d1_e, p_q5_d2_e, p_q5_d3_e), ncol=4))
colnames(p_q5_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q5_e_mean <- apply(p_q5_e_all, 2, mean, na.rm=TRUE)

# Calculate probabilities for bin Q6 #
for (i in 1:nrow(q6_trim_states)) p_q6_d1_e[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q6_trim_states)) p_q6_d2_e[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q6_trim_states)) p_q6_d3_e[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q6_trim_states)) p_q6_e[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=as.list(q6_trim_states[i,]), method="lw")
p_q6_e_all <- as.data.frame(matrix(c(p_q6_e, p_q6_d1_e, p_q6_d2_e, p_q6_d3_e), ncol=4))
colnames(p_q6_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q6_e_mean <- apply(p_q6_e_all, 2, mean, na.rm=TRUE)

p_q_e_all <- rbind(p_q1_e_mean, p_q2_e_mean, p_q3_e_mean, p_q4_e_mean, p_q5_e_mean, p_q6_e_mean)

#### SCENARIO 1 ####

p_q1_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q1_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q1_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q1_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q2_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q2_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q2_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q2_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q3_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q3_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q3_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q3_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q4_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q4_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q4_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q4_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q5_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q5_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q5_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q5_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q6_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
p_q6_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
p_q6_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
p_q6_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth

for (i in 1:nrow(q1_trim_states_s1)) p_q1_d1_s1[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q1_trim_states_s1)) p_q1_d2_s1[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q1_trim_states_s1)) p_q1_d3_s1[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q1_trim_states_s1)) p_q1_s1[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states_s1[i,]), method="lw")
p_q1_s1_all <- as.data.frame(matrix(c(p_q1_s1, p_q1_d1_s1, p_q1_d2_s1, p_q1_d3_s1), ncol=4))
colnames(p_q1_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q1_s1_mean <- apply(p_q1_s1_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q2_trim_states_s1)) p_q2_d1_s1[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q2_trim_states_s1)) p_q2_d2_s1[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q2_trim_states_s1)) p_q2_d3_s1[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q2_trim_states_s1)) p_q2_s1[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=as.list(q2_trim_states_s1[i,]), method="lw")
p_q2_s1_all <- as.data.frame(matrix(c(p_q2_s1, p_q2_d1_s1, p_q2_d2_s1, p_q2_d3_s1), ncol=4))
colnames(p_q2_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q2_s1_mean <- apply(p_q2_s1_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q3_trim_states_s1)) p_q3_d1_s1[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q3_trim_states_s1)) p_q3_d2_s1[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q3_trim_states_s1)) p_q3_d3_s1[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q3_trim_states_s1)) p_q3_s1[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=as.list(q3_trim_states_s1[i,]), method="lw")
p_q3_s1_all <- as.data.frame(matrix(c(p_q3_s1, p_q3_d1_s1, p_q3_d2_s1, p_q3_d3_s1), ncol=4))
colnames(p_q3_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q3_s1_mean <- apply(p_q3_s1_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q4_trim_states_s1)) p_q4_d1_s1[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q4_trim_states_s1)) p_q4_d2_s1[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q4_trim_states_s1)) p_q4_d3_s1[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q4_trim_states_s1)) p_q4_s1[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=as.list(q4_trim_states_s1[i,]), method="lw")
p_q4_s1_all <- as.data.frame(matrix(c(p_q4_s1, p_q4_d1_s1, p_q4_d2_s1, p_q4_d3_s1), ncol=4))
colnames(p_q4_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q4_s1_mean <- apply(p_q4_s1_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q5_trim_states_s1)) p_q5_d1_s1[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q5_trim_states_s1)) p_q5_d2_s1[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q5_trim_states_s1)) p_q5_d3_s1[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q5_trim_states_s1)) p_q5_s1[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=as.list(q5_trim_states_s1[i,]), method="lw")
p_q5_s1_all <- as.data.frame(matrix(c(p_q5_s1, p_q5_d1_s1, p_q5_d2_s1, p_q5_d3_s1), ncol=4))
colnames(p_q5_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q5_s1_mean <- apply(p_q5_s1_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q6_trim_states_s1)) p_q6_d1_s1[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q6_trim_states_s1)) p_q6_d2_s1[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q6_trim_states_s1)) p_q6_d3_s1[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q6_trim_states_s1)) p_q6_s1[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=as.list(q6_trim_states_s1[i,]), method="lw")
p_q6_s1_all <- as.data.frame(matrix(c(p_q6_s1, p_q6_d1_s1, p_q6_d2_s1, p_q6_d3_s1), ncol=4))
colnames(p_q6_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q6_s1_mean <- apply(p_q6_s1_all, 2, mean, na.rm=TRUE)

p_q_s1_all <- rbind(p_q1_s1_mean, p_q2_s1_mean, p_q3_s1_mean, p_q4_s1_mean, p_q5_s1_mean, p_q6_s1_mean)

#### SCENARIO 2 ####

p_q1_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q1_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q1_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q1_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q2_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q2_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q2_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q2_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q3_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q3_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q3_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q3_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q4_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q4_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q4_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q4_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q5_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q5_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q5_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q5_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

p_q6_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
p_q6_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
p_q6_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
p_q6_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth

for (i in 1:nrow(q1_trim_states_s2)) p_q1_d1_s2[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q1_trim_states_s2)) p_q1_d2_s2[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q1_trim_states_s2)) p_q1_d3_s2[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q1_trim_states_s2)) p_q1_s2[i] <- cpquery(riparian.fit1, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states_s2[i,]), method="lw")
p_q1_s2_all <- as.data.frame(matrix(c(p_q1_s2, p_q1_d1_s2, p_q1_d2_s2, p_q1_d3_s2), ncol=4))
colnames(p_q1_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q1_s2_mean <- apply(p_q1_s2_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q2_trim_states_s2)) p_q2_d1_s2[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q2_trim_states_s2)) p_q2_d2_s2[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q2_trim_states_s2)) p_q2_d3_s2[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q2_trim_states_s2)) p_q2_s2[i] <- cpquery(riparian.fit2, (POTENTIAL == "Y"), evidence=as.list(q2_trim_states_s2[i,]), method="lw")
p_q2_s2_all <- as.data.frame(matrix(c(p_q2_s2, p_q2_d1_s2, p_q2_d2_s2, p_q2_d3_s2), ncol=4))
colnames(p_q2_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q2_s2_mean <- apply(p_q2_s2_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q3_trim_states_s2)) p_q3_d1_s2[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q3_trim_states_s2)) p_q3_d2_s2[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q3_trim_states_s2)) p_q3_d3_s2[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q3_trim_states_s2)) p_q3_s2[i] <- cpquery(riparian.fit3, (POTENTIAL == "Y"), evidence=as.list(q3_trim_states_s2[i,]), method="lw")
p_q3_s2_all <- as.data.frame(matrix(c(p_q3_s2, p_q3_d1_s2, p_q3_d2_s2, p_q3_d3_s2), ncol=4))
colnames(p_q3_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q3_s2_mean <- apply(p_q3_s2_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q4_trim_states_s2)) p_q4_d1_s2[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q4_trim_states_s2)) p_q4_d2_s2[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q4_trim_states_s2)) p_q4_d3_s2[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q4_trim_states_s2)) p_q4_s2[i] <- cpquery(riparian.fit4, (POTENTIAL == "Y"), evidence=as.list(q4_trim_states_s2[i,]), method="lw")
p_q4_s2_all <- as.data.frame(matrix(c(p_q4_s2, p_q4_d1_s2, p_q4_d2_s2, p_q4_d3_s2), ncol=4))
colnames(p_q4_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q4_s2_mean <- apply(p_q4_s2_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q5_trim_states_s2)) p_q5_d1_s2[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q5_trim_states_s2)) p_q5_d2_s2[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q5_trim_states_s2)) p_q5_d3_s2[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q5_trim_states_s2)) p_q5_s2[i] <- cpquery(riparian.fit5, (POTENTIAL == "Y"), evidence=as.list(q5_trim_states_s2[i,]), method="lw")
p_q5_s2_all <- as.data.frame(matrix(c(p_q5_s2, p_q5_d1_s2, p_q5_d2_s2, p_q5_d3_s2), ncol=4))
colnames(p_q5_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q5_s2_mean <- apply(p_q5_s2_all, 2, mean, na.rm=TRUE)

for (i in 1:nrow(q6_trim_states_s2)) p_q6_d1_s2[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="1"), method="lw")
for (i in 1:nrow(q6_trim_states_s2)) p_q6_d2_s2[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="2"), method="lw")
for (i in 1:nrow(q6_trim_states_s2)) p_q6_d3_s2[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="3"), method="lw")
for (i in 1:nrow(q6_trim_states_s2)) p_q6_s2[i] <- cpquery(riparian.fit6, (POTENTIAL == "Y"), evidence=as.list(q6_trim_states_s2[i,]), method="lw")
p_q6_s2_all <- as.data.frame(matrix(c(p_q6_s2, p_q6_d1_s2, p_q6_d2_s2, p_q6_d3_s2), ncol=4))
colnames(p_q6_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
p_q6_s2_mean <- apply(p_q6_s2_all, 2, mean, na.rm=TRUE)

p_q_s2_all <- rbind(p_q1_s2_mean, p_q2_s2_mean, p_q3_s2_mean, p_q4_s2_mean, p_q5_s2_mean, p_q6_s2_mean)

p_all <- list(p_q_e_all, p_q_s1_all, p_q_s2_all)
names(p_all) <- c("Existing", "Scenario1", "Scenario2")

# Calculate probabilities for bin Q1 #
# p_q1_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q1_d1_e <- vector()
# p_q1_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q1_d2_e <- vector()
# p_q1_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q1_d3_e <- vector()
# # p_q1_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q1_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d1_e[j] <- mean(p_q1_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d2_e[j] <- mean(p_q1_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d3_e[j] <- mean(p_q1_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_e[j] <- mean(p_q1_e, na.rm=TRUE)
# }
# p_q1_e_all <- as.data.frame(matrix(c(mn_q1_e, mn_q1_d1_e, mn_q1_d2_e, mn_q1_d3_e), ncol=4))
# colnames(p_q1_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q1_e_mean <- apply(p_q1_e_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q2 #
# p_q2_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q2_d1_e <- vector()
# p_q2_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q2_d2_e <- vector()
# p_q2_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q2_d3_e <- vector()
# p_q2_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q2_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d1_e[j] <- mean(p_q2_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d2_e[j] <- mean(p_q2_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d3_e[j] <- mean(p_q2_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_e[j] <- mean(p_q2_e, na.rm=TRUE)
# }
# p_q2_e_all <- as.data.frame(matrix(c(mn_q2_e, mn_q2_d1_e, mn_q2_d2_e, mn_q2_d3_e), ncol=4))
# colnames(p_q2_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q2_e_mean <- apply(p_q2_e_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q3 #
# p_q3_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q3_d1_e <- vector()
# p_q3_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q3_d2_e <- vector()
# p_q3_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q3_d3_e <- vector()
# p_q3_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q3_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d1_e[j] <- mean(p_q3_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d2_e[j] <- mean(p_q3_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d3_e[j] <- mean(p_q3_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_e[j] <- mean(p_q3_e, na.rm=TRUE)
# }
# p_q3_e_all <- as.data.frame(matrix(c(mn_q3_e, mn_q3_d1_e, mn_q3_d2_e, mn_q3_d3_e), ncol=4))
# colnames(p_q3_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q3_e_mean <- apply(p_q3_e_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q4 #
# p_q4_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q4_d1_e <- vector()
# p_q4_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q4_d2_e <- vector()
# p_q4_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q4_d3_e <- vector()
# p_q4_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q4_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d1_e[j] <- mean(p_q4_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d2_e[j] <- mean(p_q4_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d3_e[j] <- mean(p_q4_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_e[j] <- mean(p_q4_e, na.rm=TRUE)
# }
# p_q4_e_all <- as.data.frame(matrix(c(mn_q4_e, mn_q4_d1_e, mn_q4_d2_e, mn_q4_d3_e), ncol=4))
# colnames(p_q4_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q4_e_mean <- apply(p_q4_e_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q5 #
# p_q5_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q5_d1_e <- vector()
# p_q5_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q5_d2_e <- vector()
# p_q5_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q5_d3_e <- vector()
# p_q5_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q5_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d1_e[j] <- mean(p_q5_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d2_e[j] <- mean(p_q5_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d3_e[j] <- mean(p_q5_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_e[j] <- mean(p_q5_e, na.rm=TRUE)
# }
# p_q5_e_all <- as.data.frame(matrix(c(mn_q5_e, mn_q5_d1_e, mn_q5_d2_e, mn_q5_d3_e), ncol=4))
# colnames(p_q5_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q5_e_mean <- apply(p_q5_e_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q6 #
# p_q6_d1_e <- vector() # Vector for groundwater depth "1" probabilities
# mn_q6_d1_e <- vector()
# p_q6_d2_e <- vector() # Vector for groundwater depth "2" probabilities
# mn_q6_d2_e <- vector()
# p_q6_d3_e <- vector() # Vector for groundwater depth "3" probabilities
# mn_q6_d3_e <- vector()
# p_q6_e <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q6_e <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d1_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d1_e[j] <- mean(p_q6_d1_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d2_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d2_e[j] <- mean(p_q6_d2_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d3_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d3_e[j] <- mean(p_q6_d3_e, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_e[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_e[j] <- mean(p_q6_e, na.rm=TRUE)
# }
# p_q6_e_all <- as.data.frame(matrix(c(mn_q6_e, mn_q6_d1_e, mn_q6_d2_e, mn_q6_d3_e), ncol=4))
# colnames(p_q6_e_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q6_e_mean <- apply(p_q6_e_all, 2, mean, na.rm=TRUE)


# Calculate probabilities for bin Q1 #
# p_q1_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q1_d1_s1 <- vector()
# p_q1_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q1_d2_s1 <- vector()
# p_q1_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q1_d3_s1 <- vector()
# p_q1_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q1_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d1_s1[j] <- mean(p_q1_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d2_s1[j] <- mean(p_q1_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d3_s1[j] <- mean(p_q1_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_s1[j] <- mean(p_q1_s1, na.rm=TRUE)
# }

# # Calculate probabilities for bin Q2 #
# p_q2_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q2_d1_s1 <- vector()
# p_q2_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q2_d2_s1 <- vector()
# p_q2_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q2_d3_s1 <- vector()
# p_q2_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q2_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d1_s1[j] <- mean(p_q2_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d2_s1[j] <- mean(p_q2_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d3_s1[j] <- mean(p_q2_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_s1[j] <- mean(p_q2_s1, na.rm=TRUE)
# }
# p_q2_s1_all <- as.data.frame(matrix(c(mn_q2_s1, mn_q2_d1_s1, mn_q2_d2_s1, mn_q2_d3_s1), ncol=4))
# colnames(p_q2_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q2_s1_mean <- apply(p_q2_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q3 #
# p_q3_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q3_d1_s1 <- vector()
# p_q3_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q3_d2_s1 <- vector()
# p_q3_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q3_d3_s1 <- vector()
# p_q3_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q3_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d1_s1[j] <- mean(p_q3_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d2_s1[j] <- mean(p_q3_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d3_s1[j] <- mean(p_q3_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_s1[j] <- mean(p_q3_s1, na.rm=TRUE)
# }
# p_q3_s1_all <- as.data.frame(matrix(c(mn_q3_s1, mn_q3_d1_s1, mn_q3_d2_s1, mn_q3_d3_s1), ncol=4))
# colnames(p_q3_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q3_s1_mean <- apply(p_q3_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q4 #
# p_q4_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q4_d1_s1 <- vector()
# p_q4_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q4_d2_s1 <- vector()
# p_q4_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q4_d3_s1 <- vector()
# p_q4_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q4_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d1_s1[j] <- mean(p_q4_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d2_s1[j] <- mean(p_q4_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d3_s1[j] <- mean(p_q4_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_s1[j] <- mean(p_q4_s1, na.rm=TRUE)
# }
# p_q4_s1_all <- as.data.frame(matrix(c(mn_q4_s1, mn_q4_d1_s1, mn_q4_d2_s1, mn_q4_d3_s1), ncol=4))
# colnames(p_q4_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q4_s1_mean <- apply(p_q4_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q5 #
# p_q5_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q5_d1_s1 <- vector()
# p_q5_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q5_d2_s1 <- vector()
# p_q5_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q5_d3_s1 <- vector()
# p_q5_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q5_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d1_s1[j] <- mean(p_q5_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d2_s1[j] <- mean(p_q5_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d3_s1[j] <- mean(p_q5_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_s1[j] <- mean(p_q5_s1, na.rm=TRUE)
# }
# p_q5_s1_all <- as.data.frame(matrix(c(mn_q5_s1, mn_q5_d1_s1, mn_q5_d2_s1, mn_q5_d3_s1), ncol=4))
# colnames(p_q5_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q5_s1_mean <- apply(p_q5_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q6 #
# p_q6_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q6_d1_s1 <- vector()
# p_q6_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q6_d2_s1 <- vector()
# p_q6_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q6_d3_s1 <- vector()
# p_q6_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q6_s1 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d1_s1[j] <- mean(p_q6_d1_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d2_s1[j] <- mean(p_q6_d2_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d3_s1[j] <- mean(p_q6_d3_s1, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_s1[j] <- mean(p_q6_s1, na.rm=TRUE)
# }
# p_q6_s1_all <- as.data.frame(matrix(c(mn_q6_s1, mn_q6_d1_s1, mn_q6_d2_s1, mn_q6_d3_s1), ncol=4))
# colnames(p_q6_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q6_s1_mean <- apply(p_q6_s1_all, 2, mean, na.rm=TRUE)

# p_q_s1_all <- rbind(p_q1_s1_mean, p_q2_s1_mean, p_q3_s1_mean, p_q4_s1_mean, p_q5_s1_mean, p_q6_s1_mean)

# # Calculate probabilities for bin Q1 #
# p_q1_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q1_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q1_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q1_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states_s1[i,]), method="lw")
# p_q1_s1_all <- as.data.frame(matrix(c(p_q1_s1, p_q1_d1_s1, p_q1_d2_s1, p_q1_d3_s1), ncol=4))
# colnames(p_q1_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q1_s1_mean <- apply(p_q1_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q2 #
# p_q2_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q2_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q2_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q2_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q2_trim_states_s1[i,]), method="lw")
# p_q2_s1_all <- as.data.frame(matrix(c(p_q2_s1, p_q2_d1_s1, p_q2_d2_s1, p_q2_d3_s1), ncol=4))
# colnames(p_q2_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q2_s1_mean <- apply(p_q2_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q3 #
# p_q3_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q3_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q3_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q3_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q3_trim_states_s1[i,]), method="lw")
# p_q3_s1_all <- as.data.frame(matrix(c(p_q3_s1, p_q3_d1_s1, p_q3_d2_s1, p_q3_d3_s1), ncol=4))
# colnames(p_q3_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q3_s1_mean <- apply(p_q3_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q4 #
# p_q4_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q4_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q4_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q4_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q4_trim_states_s1[i,]), method="lw")
# p_q4_s1_all <- as.data.frame(matrix(c(p_q4_s1, p_q4_d1_s1, p_q4_d2_s1, p_q4_d3_s1), ncol=4))
# colnames(p_q4_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q4_s1_mean <- apply(p_q4_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q5 #
# p_q5_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q5_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q5_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q5_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q5_trim_states_s1[i,]), method="lw")
# p_q5_s1_all <- as.data.frame(matrix(c(p_q5_s1, p_q5_d1_s1, p_q5_d2_s1, p_q5_d3_s1), ncol=4))
# colnames(p_q5_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q5_s1_mean <- apply(p_q5_s1_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q6 #
# p_q6_d1_s1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q6_d2_s1 <- vector() # Vector for groundwater depth "2" probabilities
# p_q6_d3_s1 <- vector() # Vector for groundwater depth "3" probabilities
# p_q6_s1 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_d1_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_d2_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_d3_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_s1[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q6_trim_states_s1[i,]), method="lw")
# p_q6_s1_all <- as.data.frame(matrix(c(p_q6_s1, p_q6_d1_s1, p_q6_d2_s1, p_q6_d3_s1), ncol=4))
# colnames(p_q6_s1_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q6_s1_mean <- apply(p_q6_s1_all, 2, mean, na.rm=TRUE)

# p_q_s1_all <- rbind(p_q1_s1_mean, p_q2_s1_mean, p_q3_s1_mean, p_q4_s1_mean, p_q5_s1_mean, p_q6_s1_mean)


#### SCENARIO 2 ####

# # Calculate probabilities for bin Q1 #
# p_q1_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q1_d1_s2 <- vector()
# p_q1_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q1_d2_s2 <- vector()
# p_q1_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q1_d3_s2 <- vector()
# p_q1_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q1_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d1_s2[j] <- mean(p_q1_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d2_s2[j] <- mean(p_q1_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_d3_s2[j] <- mean(p_q1_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q1_trim_states)) p_q1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q1_s2[j] <- mean(p_q1_s2, na.rm=TRUE)
# }
# # for (i in 1:nrow(q1_trim_states)) p_q1_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="2"), method="lw")
# # for (i in 1:nrow(q1_trim_states)) p_q1_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states[i,]), DEPTH="3"), method="lw")
# # for (i in 1:nrow(q1_trim_states)) p_q1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states[i,]), method="lw")
# p_q1_s2_all <- as.data.frame(matrix(c(mn_q1_s2, mn_q1_d1_s2, mn_q1_d2_s2, mn_q1_d3_s2), ncol=4))
# colnames(p_q1_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q1_s2_mean <- apply(p_q1_s2_all, 2, mean, na.rm=TRUE)


# # Calculate probabilities for bin Q2 #
# p_q2_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q2_d1_s2 <- vector()
# p_q2_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q2_d2_s2 <- vector()
# p_q2_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q2_d3_s2 <- vector()
# p_q2_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q2_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d1_s2[j] <- mean(p_q2_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d2_s2[j] <- mean(p_q2_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_d3_s2[j] <- mean(p_q2_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q2_trim_states)) p_q2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q2_s2[j] <- mean(p_q2_s2, na.rm=TRUE)
# }
# p_q2_s2_all <- as.data.frame(matrix(c(mn_q2_s2, mn_q2_d1_s2, mn_q2_d2_s2, mn_q2_d3_s2), ncol=4))
# colnames(p_q2_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q2_s2_mean <- apply(p_q2_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q3 #
# p_q3_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q3_d1_s2 <- vector()
# p_q3_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q3_d2_s2 <- vector()
# p_q3_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q3_d3_s2 <- vector()
# p_q3_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q3_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d1_s2[j] <- mean(p_q3_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d2_s2[j] <- mean(p_q3_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_d3_s2[j] <- mean(p_q3_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q3_trim_states)) p_q3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q3_s2[j] <- mean(p_q3_s2, na.rm=TRUE)
# }
# p_q3_s2_all <- as.data.frame(matrix(c(mn_q3_s2, mn_q3_d1_s2, mn_q3_d2_s2, mn_q3_d3_s2), ncol=4))
# colnames(p_q3_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q3_s2_mean <- apply(p_q3_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q4 #
# p_q4_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q4_d1_s2 <- vector()
# p_q4_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q4_d2_s2 <- vector()
# p_q4_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q4_d3_s2 <- vector()
# p_q4_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q4_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d1_s2[j] <- mean(p_q4_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d2_s2[j] <- mean(p_q4_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_d3_s2[j] <- mean(p_q4_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q4_trim_states)) p_q4_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q4_s2[j] <- mean(p_q4_s2, na.rm=TRUE)
# }
# p_q4_s2_all <- as.data.frame(matrix(c(mn_q4_s2, mn_q4_d1_s2, mn_q4_d2_s2, mn_q4_d3_s2), ncol=4))
# colnames(p_q4_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q4_s2_mean <- apply(p_q4_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q5 #
# p_q5_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q5_d1_s2 <- vector()
# p_q5_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q5_d2_s2 <- vector()
# p_q5_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q5_d3_s2 <- vector()
# p_q5_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q5_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d1_s2[j] <- mean(p_q5_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d2_s2[j] <- mean(p_q5_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_d3_s2[j] <- mean(p_q5_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q5_trim_states)) p_q5_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q5_s2[j] <- mean(p_q5_s2, na.rm=TRUE)
# }
# p_q5_s2_all <- as.data.frame(matrix(c(mn_q5_s2, mn_q5_d1_s2, mn_q5_d2_s2, mn_q5_d3_s2), ncol=4))
# colnames(p_q5_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q5_s2_mean <- apply(p_q5_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q6 #
# p_q6_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# mn_q6_d1_s2 <- vector()
# p_q6_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# mn_q6_d2_s2 <- vector()
# p_q6_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# mn_q6_d3_s2 <- vector()
# p_q6_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# mn_q6_s2 <- vector()
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d1_s2[j] <- mean(p_q6_d1_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d2_s2[j] <- mean(p_q6_d2_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_d3_s2[j] <- mean(p_q6_d3_s2, na.rm=TRUE)
# }
# for (j in 1:25) {
# 	for (i in 1:nrow(q6_trim_states)) p_q6_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states[i,]), DEPTH="1"), method="lw")
# 		mn_q6_s2[j] <- mean(p_q6_s2, na.rm=TRUE)
# }
# p_q6_s2_all <- as.data.frame(matrix(c(mn_q6_s2, mn_q6_d1_s2, mn_q6_d2_s2, mn_q6_d3_s2), ncol=4))
# colnames(p_q6_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q6_s2_mean <- apply(p_q6_s2_all, 2, mean, na.rm=TRUE)

# p_q_s2_all <- rbind(p_q1_s2_mean, p_q2_s2_mean, p_q3_s2_mean, p_q4_s2_mean, p_q5_s2_mean, p_q6_s2_mean)


# # Calculate probabilities for bin Q1 #
# p_q1_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q1_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q1_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q1_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q1_trim_states_s2)) p_q1_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q1_trim_states_s2)) p_q1_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q1_trim_states_s2)) p_q1_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q1_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q1_trim_states_s2)) p_q1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q1_trim_states_s2[i,]), method="lw")
# p_q1_s2_all <- as.data.frame(matrix(c(p_q1_s2, p_q1_d1_s2, p_q1_d2_s2, p_q1_d3_s2), ncol=4))
# colnames(p_q1_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q1_s2_mean <- apply(p_q1_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q2 #
# p_q2_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q2_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q2_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q2_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q2_trim_states_s2)) p_q2_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q2_trim_states_s2)) p_q2_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q2_trim_states_s2)) p_q2_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q2_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q2_trim_states_s2)) p_q2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q2_trim_states_s2[i,]), method="lw")
# p_q2_s2_all <- as.data.frame(matrix(c(p_q2_s2, p_q2_d1_s2, p_q2_d2_s2, p_q2_d3_s2), ncol=4))
# colnames(p_q2_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q2_s2_mean <- apply(p_q2_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q3 #
# p_q3_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q3_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q3_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q3_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q3_trim_states_s2)) p_q3_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q3_trim_states_s2)) p_q3_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q3_trim_states_s2)) p_q3_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q3_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q3_trim_states_s2)) p_q3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q3_trim_states_s2[i,]), method="lw")
# p_q3_s2_all <- as.data.frame(matrix(c(p_q3_s2, p_q3_d1_s2, p_q3_d2_s2, p_q3_d3_s2), ncol=4))
# colnames(p_q3_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q3_s2_mean <- apply(p_q3_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q4 #
# p_q4_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q4_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q4_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q4_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q4_trim_states_s2)) p_q4_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q4_trim_states_s2)) p_q4_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q4_trim_states_s2)) p_q4_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q4_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q4_trim_states_s2)) p_q4_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q4_trim_states_s2[i,]), method="lw")
# p_q4_s2_all <- as.data.frame(matrix(c(p_q4_s2, p_q4_d1_s2, p_q4_d2_s2, p_q4_d3_s2), ncol=4))
# colnames(p_q4_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q4_s2_mean <- apply(p_q4_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q5 #
# p_q5_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q5_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q5_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q5_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q5_trim_states_s2)) p_q5_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q5_trim_states_s2)) p_q5_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q5_trim_states_s2)) p_q5_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q5_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q5_trim_states_s2)) p_q5_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q5_trim_states_s2[i,]), method="lw")
# p_q5_s2_all <- as.data.frame(matrix(c(p_q5_s2, p_q5_d1_s2, p_q5_d2_s2, p_q5_d3_s2), ncol=4))
# colnames(p_q5_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q5_s2_mean <- apply(p_q5_s2_all, 2, mean, na.rm=TRUE)

# # Calculate probabilities for bin Q6 #
# p_q6_d1_s2 <- vector() # Vector for groundwater depth "1" probabilities
# p_q6_d2_s2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q6_d3_s2 <- vector() # Vector for groundwater depth "3" probabilities
# p_q6_s2 <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q6_trim_states_s2)) p_q6_d1_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q6_trim_states_s2)) p_q6_d2_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q6_trim_states_s2)) p_q6_d3_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=c(as.list(q6_trim_states_s2[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q6_trim_states_s2)) p_q6_s2[i] <- cpquery(riparian.fit, (POTENTIAL == "Y"), evidence=as.list(q6_trim_states_s2[i,]), method="lw")
# p_q6_s2_all <- as.data.frame(matrix(c(p_q6_s2, p_q6_d1_s2, p_q6_d2_s2, p_q6_d3_s2), ncol=4))
# colnames(p_q6_s2_all) <- c("Prob_NoEvidence", "Prob_Depth1", "Prob_Depth2", "Prob_Depth3")
# p_q6_s2_mean <- apply(p_q6_s2_all, 2, mean, na.rm=TRUE)

# p_q_s2_all <- rbind(p_q1_s2_mean, p_q2_s2_mean, p_q3_s2_mean, p_q4_s2_mean, p_q5_s2_mean, p_q6_s2_mean)



# # Create empty vectors for "M" recruitment potential
# p_q1_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q1_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q1_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q1_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q1_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q1_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q1_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q1_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q1_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q1_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q1_trim_states_s1)) p_q1_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q1_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q1_all1 <- as.data.frame(matrix(c(p_q1_h1, p_q1_m1, p_q1_l1), ncol=3))
# p_q1_all2 <- as.data.frame(matrix(c(p_q1_h2, p_q1_m2, p_q1_l2), ncol=3))
# p_q1_all3 <- as.data.frame(matrix(c(p_q1_h3, p_q1_m3, p_q1_l3), ncol=3))
# p_q1_all <- as.data.frame(matrix(c(p_q1_h, p_q1_m, p_q1_l), ncol=3))
# # Name columns
# colnames(p_q1_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q1_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q1_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q1_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q1_all1, 2, mean, na.rm=TRUE)
# apply(p_q1_all2, 2, mean, na.rm=TRUE)
# apply(p_q1_all3, 2, mean, na.rm=TRUE)
# apply(p_q1_all, 2, mean, na.rm=TRUE)

# #### Calculate probabilities for bin Q2 ####
# # Create empty vectors for "H" recruitment potential
# p_q2_h1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q2_h2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q2_h3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q2_h <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_h1[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_h2[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_h3[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q2_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "M" recruitment potential
# p_q2_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q2_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q2_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q2_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q2_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q2_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q2_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q2_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q2_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q2_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q2_trim_states_s1)) p_q2_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q2_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q2_all1 <- as.data.frame(matrix(c(p_q2_h1, p_q2_m1, p_q2_l1), ncol=3))
# p_q2_all2 <- as.data.frame(matrix(c(p_q2_h2, p_q2_m2, p_q2_l2), ncol=3))
# p_q2_all3 <- as.data.frame(matrix(c(p_q2_h3, p_q2_m3, p_q2_l3), ncol=3))
# p_q2_all <- as.data.frame(matrix(c(p_q2_h, p_q2_m, p_q2_l), ncol=3))
# # Name columns
# colnames(p_q2_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q2_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q2_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q2_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q2_all1, 2, mean, na.rm=TRUE)
# apply(p_q2_all2, 2, mean, na.rm=TRUE)
# apply(p_q2_all3, 2, mean, na.rm=TRUE)
# apply(p_q2_all, 2, mean, na.rm=TRUE)

# #### Calculate probabilities for bin Q3 ####
# # Create empty vectors for "H" recruitment potential
# p_q3_h1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q3_h2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q3_h3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q3_h <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_h1[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_h2[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_h3[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q3_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "M" recruitment potential
# p_q3_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q3_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q3_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q3_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q3_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q3_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q3_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q3_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q3_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q3_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q3_trim_states_s1)) p_q3_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q3_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q3_all1 <- as.data.frame(matrix(c(p_q3_h1, p_q3_m1, p_q3_l1), ncol=3))
# p_q3_all2 <- as.data.frame(matrix(c(p_q3_h2, p_q3_m2, p_q3_l2), ncol=3))
# p_q3_all3 <- as.data.frame(matrix(c(p_q3_h3, p_q3_m3, p_q3_l3), ncol=3))
# p_q3_all <- as.data.frame(matrix(c(p_q3_h, p_q3_m, p_q3_l), ncol=3))
# # Name columns
# colnames(p_q3_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q3_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q3_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q3_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q3_all1, 2, mean, na.rm=TRUE)
# apply(p_q3_all2, 2, mean, na.rm=TRUE)
# apply(p_q3_all3, 2, mean, na.rm=TRUE)
# apply(p_q3_all, 2, mean, na.rm=TRUE)

# #### Calculate probabilities for bin Q4 ####
# # Create empty vectors for "H" recruitment potential
# p_q4_h1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q4_h2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q4_h3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q4_h <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_h1[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_h2[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_h3[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q4_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "M" recruitment potential
# p_q4_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q4_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q4_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q4_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q4_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q4_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q4_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q4_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q4_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q4_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q4_trim_states_s1)) p_q4_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q4_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q4_all1 <- as.data.frame(matrix(c(p_q4_h1, p_q4_m1, p_q4_l1), ncol=3))
# p_q4_all2 <- as.data.frame(matrix(c(p_q4_h2, p_q4_m2, p_q4_l2), ncol=3))
# p_q4_all3 <- as.data.frame(matrix(c(p_q4_h3, p_q4_m3, p_q4_l3), ncol=3))
# p_q4_all <- as.data.frame(matrix(c(p_q4_h, p_q4_m, p_q4_l), ncol=3))
# # Name columns
# colnames(p_q4_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q4_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q4_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q4_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q4_all1, 2, mean, na.rm=TRUE)
# apply(p_q4_all2, 2, mean, na.rm=TRUE)
# apply(p_q4_all3, 2, mean, na.rm=TRUE)
# apply(p_q4_all, 2, mean, na.rm=TRUE)

# #### Calculate probabilities for bin Q5 ####
# # Create empty vectors for "H" recruitment potential
# p_q5_h1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q5_h2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q5_h3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q5_h <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_h1[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_h2[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_h3[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q5_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "M" recruitment potential
# p_q5_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q5_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q5_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q5_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q5_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q5_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q5_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q5_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q5_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q5_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q5_trim_states_s1)) p_q5_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q5_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q5_all1 <- as.data.frame(matrix(c(p_q5_h1, p_q5_m1, p_q5_l1), ncol=3))
# p_q5_all2 <- as.data.frame(matrix(c(p_q5_h2, p_q5_m2, p_q5_l2), ncol=3))
# p_q5_all3 <- as.data.frame(matrix(c(p_q5_h3, p_q5_m3, p_q5_l3), ncol=3))
# p_q5_all <- as.data.frame(matrix(c(p_q5_h, p_q5_m, p_q5_l), ncol=3))
# # Name columns
# colnames(p_q5_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q5_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q5_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q5_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q5_all1, 2, mean, na.rm=TRUE)
# apply(p_q5_all2, 2, mean, na.rm=TRUE)
# apply(p_q5_all3, 2, mean, na.rm=TRUE)
# apply(p_q5_all, 2, mean, na.rm=TRUE)

# #### Calculate probabilities for bin Q6 ####
# # Create empty vectors for "H" recruitment potential
# p_q6_h1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q6_h2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q6_h3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q6_h <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_h1[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_h2[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_h3[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q6_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "M" recruitment potential
# p_q6_m1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q6_m2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q6_m3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q6_m <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_m1[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_m2[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_m3[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q6_trim_states_s1[i,]), method="lw")

# # Create empty vectors for "L" recruitment potential
# p_q6_l1 <- vector() # Vector for groundwater depth "1" probabilities
# p_q6_l2 <- vector() # Vector for groundwater depth "2" probabilities
# p_q6_l3 <- vector() # Vector for groundwater depth "3" probabilities
# p_q6_l <- vector() # Vector for probabilities with no evidence of groundwater depth
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_l1[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="1"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_l2[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="2"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_l3[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=c(as.list(q6_trim_states_s1[i,]), DEPTH="3"), method="lw")
# for (i in 1:nrow(q6_trim_states_s1)) p_q6_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q6_trim_states_s1[i,]), method="lw")

# # Combine probabilities into data frames
# p_q6_all1 <- as.data.frame(matrix(c(p_q6_h1, p_q6_m1, p_q6_l1), ncol=3))
# p_q6_all2 <- as.data.frame(matrix(c(p_q6_h2, p_q6_m2, p_q6_l2), ncol=3))
# p_q6_all3 <- as.data.frame(matrix(c(p_q6_h3, p_q6_m3, p_q6_l3), ncol=3))
# p_q6_all <- as.data.frame(matrix(c(p_q6_h, p_q6_m, p_q6_l), ncol=3))
# # Name columns
# colnames(p_q6_all1) <- c("p.H1", "p.M1", "p.L1")
# colnames(p_q6_all2) <- c("p.H2", "p.M2", "p.L2")
# colnames(p_q6_all3) <- c("p.H3", "p.M3", "p.L3")
# colnames(p_q6_all) <- c("p.H", "p.M", "p.L")
# # Calculate means of all probabilites
# apply(p_q6_all1, 2, mean, na.rm=TRUE)
# apply(p_q6_all2, 2, mean, na.rm=TRUE)
# apply(p_q6_all3, 2, mean, na.rm=TRUE)
# apply(p_q6_all, 2, mean, na.rm=TRUE)



# #### Test functions ####
# # test <- list()
# # for (i in 1:nrow(q1_trim_states_s1)) test[i] <-table(cpdist(riparian.fit, "POTENTIAL", evidence=as.list(q1_trim_states_s1[i,]), method="lw"))
# #
# # what <- table(cpdist(riparian.fit, "POTENTIAL", evidence=as.list(q1_trim_states_s1[1,]), method="lw"))

# # # Calculate probabilities for bin Q1 using all time steps
# # p_q1_h2 <- vector()
# # for (i in 1:nrow(q1_all_states_s1)) p_q1_h[i] <- cpquery(riparian.fit, (POTENTIAL == "H"), evidence=as.list(q1_all_states_s1[i,]), method="lw", n=500000)
# # p_q1_m2 <- vector()
# # for (i in 1:nrow(q1_all_states_s1)) p_q1_m[i] <- cpquery(riparian.fit, (POTENTIAL == "M"), evidence=as.list(q1_all_states_s1[i,]), method="lw", n=500000)
# # p_q1_l2 <- vector()
# # for (i in 1:nrow(q1_all_states_s1)) p_q1_l[i] <- cpquery(riparian.fit, (POTENTIAL == "L"), evidence=as.list(q1_all_states_s1[i,]), method="lw", n=500000)
# # p_q1_all2 <- as.data.frame(matrix(c(p_q1_h, p_q1_m, p_q1_l), ncol=3))
# # colnames(p_q1_all2) <- c("p.H", "p.M", "p.L")
