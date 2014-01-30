#### Read results from SRH-2D model ####

set.seed(140)

# Change input table depending on the site number
E_alldata <- read.table("data/site1_1000_to_4000.txt", header=FALSE, skip=0)

colnames(E_alldata) <- c("cell", "flood_cfs")

#### Separate grid data according to the Q-bins previously defined ####
E_q1_cells <- subset(E_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
E_q2_cells <- subset(E_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])
E_q3_cells <- subset(E_alldata, flood_cfs>=q_bin[3] & flood_cfs<q_bin[4])
E_q4_cells <- subset(E_alldata, flood_cfs>=q_bin[4] & flood_cfs<q_bin[5])
E_q5_cells <- subset(E_alldata, flood_cfs>=q_bin[5] & flood_cfs<q_bin[6])
E_q6_cells <- subset(E_alldata, flood_cfs>=q_bin[6] & flood_cfs<q_bin[7])
E_q7_cells <- subset(E_alldata, flood_cfs>=q_bin[7])

#### Combine hydrology time series, timing states, and recession rate states ####
E_hydro_states <- cbind(hydro, timing_state, recess_state)
#### Trim data frame based on timing and recession rates ####
E_hydro_states <- subset(E_hydro_states, TIMING != "NA" & RECESSION != 1)
E_timing_subset <- E_hydro_states[,15]
E_recess_subset <- E_hydro_states[,16]

save(E_hydro_states, file="output/site3/E_hydro_states.Rdata")

#### Allow dimension naming using the "bigmemory" package ####
options(bigmemory.allow.dimnames=TRUE)

#### Create empty matrix to fill with data based on the Q-bin ####
E_emptymatrix1 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q1_cells[,1]))))
E_emptymatrix2 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q2_cells[,1]))))
E_emptymatrix3 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q3_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q3_cells[,1]))))
E_emptymatrix4 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q4_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q4_cells[,1]))))
E_emptymatrix5 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q5_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q5_cells[,1]))))
E_emptymatrix6 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q6_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q6_cells[,1]))))
E_emptymatrix7 <- big.matrix(nrow=nrow(E_hydro_states), ncol=(nrow(E_q7_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(E_q6_cells[,1]))))

# USE FOR WINDOWS #
# emptymatrix1 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q1_cells[,1]))))
# emptymatrix2 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q2_cells[,1]))))
# emptymatrix3 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q3_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q3_cells[,1]))))
# emptymatrix4 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q4_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q4_cells[,1]))))
# emptymatrix5 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q5_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q5_cells[,1]))))
# emptymatrix6 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q6_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q6_cells[,1]))))

#### Populate empty matrices
E_emptymatrix1[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix1[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q1_cells)) {
    E_emptymatrix1[,i+2] <- E_q1_cells[i,2]<=E_hydro_states[,2] & E_q1_cells[i,2]>E_hydro_states[-1,2] 
}

E_emptymatrix2[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix2[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q2_cells)) {
  E_emptymatrix2[,i+2] <- E_q2_cells[i,2]<=E_hydro_states[,2] & E_q2_cells[i,2]>E_hydro_states[-1,2]
}

E_emptymatrix3[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix3[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q3_cells)) {
  E_emptymatrix3[,i+2] <- E_q3_cells[i,2]<=E_hydro_states[,2] & E_q3_cells[i,2]>E_hydro_states[-1,2]
}

E_emptymatrix4[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix4[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q4_cells)) {
  E_emptymatrix4[,i+2] <- E_q4_cells[i,2]<=E_hydro_states[,2] & E_q4_cells[i,2]>E_hydro_states[-1,2]
}

E_emptymatrix5[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix5[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q5_cells)) {
  E_emptymatrix5[,i+2] <- E_q5_cells[i,2]<=E_hydro_states[,2] & E_q5_cells[i,2]>E_hydro_states[-1,2]
}

E_emptymatrix6[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix6[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q6_cells)) {
  E_emptymatrix6[,i+2] <- E_q6_cells[i,2]<=E_hydro_states[,2] & E_q6_cells[i,2]>E_hydro_states[-1,2]
}

E_emptymatrix7[,1] <- as.vector(as.matrix(E_timing_subset))
E_emptymatrix7[,2] <- as.vector(as.matrix(E_recess_subset))
for (i in 1:nrow(E_q7_cells)) {
  E_emptymatrix7[,i+2] <- E_q7_cells[i,2]<=E_hydro_states[,2] & E_q7_cells[i,2]>E_hydro_states[-1,2]
}

# a <- vector("list", nrow(q1_cells))
# names(a) <- (q1_cells[,1])
# a[[1]] <- data.frame(matrix(test1[,1:3][test1[,3]==1], ncol=3))
# a[[1]] <- lapply(a[[1]], factor)
# names(a[[1]]) <- c("TIMING", "RECESSION", "FLOOD")
# w <- a[[1]][1]
# x <- a[[1]][2]
# v <- a[[1]][3]
# a[[1]] <- data.frame(w,x,v)

#### Populate empty matrices with evidence ####
E_evidence1 <- populate.evidence(E_q1_cells, E_emptymatrix1)
E_evidence2 <- populate.evidence(E_q2_cells, E_emptymatrix2)
E_evidence3 <- populate.evidence(E_q3_cells, E_emptymatrix3)
E_evidence4 <- populate.evidence(E_q4_cells, E_emptymatrix4)
E_evidence5 <- populate.evidence(E_q5_cells, E_emptymatrix5)
E_evidence6 <- populate.evidence(E_q6_cells, E_emptymatrix6)
E_evidence7 <- populate.evidence(E_q7_cells, E_emptymatrix7)
save(E_evidence1, file="output/site3/E_evidence1.Rdata")
save(E_evidence2, file="output/site3/E_evidence2.Rdata")
save(E_evidence3, file="output/site3/E_evidence3.Rdata")
save(E_evidence4, file="output/site3/E_evidence4.Rdata")
save(E_evidence5, file="output/site3/E_evidence5.Rdata")
save(E_evidence6, file="output/site3/E_evidence6.Rdata")
save(E_evidence7, file="output/site3/E_evidence7.Rdata")


# a <- vector("list", nrow(q1_cells))
# names(a) <- (q1_cells[,1])
# for (i in 1:nrow(q1_cells)) {
# 	a[[i]] <- data.frame(matrix(test1[,c(1:2,i+2)][test1[,i+2]==1], ncol=3))
# 	a[[i]] <- lapply(a[[i]], factor)
# 	names(a[[i]]) <- c("TIMING", "RECESSION", "FLOOD")
# 	w <- a[[i]][1]
# 	x <- a[[i]][2]
# 	v <- a[[i]][3]
# 	a[[i]] <- data.frame(w,x,v)
# }
## USE THIS FOR WINDOWS ##
# a <- vector("list", nrow(q1_cells))
# names(a) <- (q1_cells[,1])
# for (i in 1:nrow(q1_cells)) {
# 	a[[i]] <- data.frame(matrix(test1[,c(1:2,i+2)][test1[,i+2]=="TRUE"], ncol=3))
# 	a[[i]] <- lapply(a[[i]], factor)
# 	names(a[[i]]) <- c("TIMING", "RECESSION", "FLOOD")
# 	w <- a[[i]][1]
# 	x <- a[[i]][2]
# 	v <- a[[i]][3]
# 	a[[i]] <- data.frame(w,x,v)
# }

# matrix1 <- matrix(4:11, nrow=2)
# matrix2 <- matrix(5:12, nrow=2)
# exlist <- list(matrix1, matrix2)

# lapply(exlist, apply, 1, mean)

# testlist <- a[1:10]

# q_test <- lapply(testlist, apply, 1, function(x) cpquery(riparian.fit1, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
# write.csv(q_test, file="q_test.csv")

#### Calculate recruitment probability by instantiating the BN with evidence ####
system.time(E_q1_cell_probs <- lapply(E_evidence1, apply, 1, function(x) cpquery(riparian.fit1, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q1_cell_probs, file="output/site3/E_q1_cell_probs.Rdata")
system.time(E_q2_cell_probs <- lapply(E_evidence2, apply, 1, function(x) cpquery(riparian.fit2, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q2_cell_probs, file="output/site3/E_q2_cell_probs.Rdata")
system.time(E_q3_cell_probs <- lapply(E_evidence3, apply, 1, function(x) cpquery(riparian.fit3, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q3_cell_probs, file="output/site3/E_q3_cell_probs.Rdata")
system.time(E_q4_cell_probs <- lapply(E_evidence4, apply, 1, function(x) cpquery(riparian.fit4, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q4_cell_probs, file="output/site3/E_q4_cell_probs.Rdata")
system.time(E_q5_cell_probs <- lapply(E_evidence5, apply, 1, function(x) cpquery(riparian.fit5, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q5_cell_probs, file="output/site3/E_q5_cell_probs.Rdata")
system.time(E_q6_cell_probs <- lapply(E_evidence6, apply, 1, function(x) cpquery(riparian.fit6, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q6_cell_probs, file="output/site3/E_q6_cell_probs.Rdata")
system.time(E_q7_cell_probs <- lapply(E_evidence7, apply, 1, function(x) cpquery(riparian.fit7, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
save(E_q7_cell_probs, file="output/site3/E_q7_cell_probs.Rdata")

E_q1_cell_prob_mn <- t(data.frame(lapply(E_q1_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q2_cell_prob_mn <- t(data.frame(lapply(E_q2_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q3_cell_prob_mn <- t(data.frame(lapply(E_q3_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q4_cell_prob_mn <- t(data.frame(lapply(E_q4_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q5_cell_prob_mn <- t(data.frame(lapply(E_q5_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q6_cell_prob_mn <- t(data.frame(lapply(E_q6_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
E_q7_cell_prob_mn <- t(data.frame(lapply(E_q7_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
if (exists("E_q7_cell_prob_mn")) E_q_all_prob_mn <- rbind(E_q1_cell_prob_mn, E_q2_cell_prob_mn, E_q3_cell_prob_mn, E_q4_cell_prob_mn, E_q5_cell_prob_mn, E_q6_cell_prob_mn, E_q7_cell_prob_mn)
if (!exists("E_q7_cell_prob_mn")) E_q_all_prob_mn <- rbind(E_q1_cell_prob_mn, E_q2_cell_prob_mn, E_q3_cell_prob_mn, E_q4_cell_prob_mn, E_q5_cell_prob_mn, E_q6_cell_prob_mn)

# q1_norm <- normalize(hydro_states, q1_cell_probs)
# q2_norm <- normalize(hydro_states, q2_cell_probs)
# q3_norm <- normalize(hydro_states, q3_cell_probs)
# q4_norm <- normalize(hydro_states, q4_cell_probs)
# q5_norm <- normalize(hydro_states, q5_cell_probs)
# q6_norm <- normalize(hydro_states, q6_cell_probs)

# q1_prob_mn_norm <- q1_cell_prob_mn * q1_norm
# q2_prob_mn_norm <- q2_cell_prob_mn * q2_norm
# q3_prob_mn_norm <- q3_cell_prob_mn * q3_norm
# q4_prob_mn_norm <- q4_cell_prob_mn * q4_norm
# q5_prob_mn_norm <- q5_cell_prob_mn * q5_norm
# q6_prob_mn_norm <- q6_cell_prob_mn * q6_norm

# row.names(q1_prob_mn_norm) <- row.names(q1_cell_prob_mn)
# row.names(q2_prob_mn_norm) <- row.names(q2_cell_prob_mn)
# row.names(q3_prob_mn_norm) <- row.names(q3_cell_prob_mn)
# row.names(q4_prob_mn_norm) <- row.names(q4_cell_prob_mn)
# row.names(q5_prob_mn_norm) <- row.names(q5_cell_prob_mn)
# row.names(q6_prob_mn_norm) <- row.names(q6_cell_prob_mn)

# if (exists("q6_prob_mn_norm")) q_all_mn_norm_e <- rbind(q1_prob_mn_norm, q2_prob_mn_norm, q3_prob_mn_norm, q4_prob_mn_norm, q5_prob_mn_norm, q6_prob_mn_norm)
# if (!exists("q6_prob_mn_norm")) q_all_mn_norm_e <- rbind(q1_prob_mn_norm, q2_prob_mn_norm, q3_prob_mn_norm, q4_prob_mn_norm, q5_prob_mn_norm)

#### Write csv file output ####
# write.csv(q1_prob_mn_norm, file="output/site3/q1_prob_mn_norm_e.csv")
# write.csv(q2_prob_mn_norm, file="output/site3/q2_prob_mn_norm_e.csv")
# write.csv(q3_prob_mn_norm, file="output/site3/q3_prob_mn_norm_e.csv")
# write.csv(q4_prob_mn_norm, file="output/site3/q4_prob_mn_norm_e.csv")
# write.csv(q5_prob_mn_norm, file="output/site3/q5_prob_mn_norm_e.csv")
# write.csv(q6_prob_mn_norm, file="output/site3/q6_prob_mn_norm_e.csv")

# Export File
write.csv(E_q_all_prob_mn, file="output/site3/E_q_all_prob_mn.csv")
E_q_all_prob_mn2 <- read.csv("output/site3/E_q_all_prob_mn.csv")
colnames(E_q_all_prob_mn2) <- c("cell","mean_prob")
E_q_all_prob_mn2 <- E_q_all_prob_mn2[order(E_q_all_prob_mn2$cell),]
write.csv(E_q_all_prob_mn2, file="output/site3/E_q_all_prob_mn.csv", row.names=FALSE)

# Site 2
# write.csv(q_all_mn_norm_e, file="output/site3/q_all_mn_norm_site3_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site3/q_all_mn_norm_site3_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site3/q_all_mn_norm_site3_e.csv", row.names=FALSE)

# Site 3
# write.csv(q_all_mn_norm_e, file="output/site3/q_all_mn_norm_site3_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site3/q_all_mn_norm_site3_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site3/q_all_mn_norm_site3_e.csv", row.names=FALSE)

# # Site 4
# write.csv(q_all_mn_norm_e, file="output/site3/q_all_mn_norm_site3_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site3/q_all_mn_norm_site3_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site3/q_all_mn_norm_site3_e.csv", row.names=FALSE)

# Site 5
# write.csv(q_all_mn_norm_e, file="output/site3/q_all_mn_norm_site3_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site3/q_all_mn_norm_site3_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site3/q_all_mn_norm_site3_e.csv", row.names=FALSE)