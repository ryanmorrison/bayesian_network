#### Read results from SRH-2D model ####
# Change input table depending on the site number

# Site 1
alldata <- read.table("data/s1_1000_to_2000_trim.txt", header=FALSE, skip=0)

# Site 2
# alldata <- read.table("data/s2_tiny_Qs.txt", header=FALSE, skip=0)

# Site 3
# alldata <- read.table("data/s3_1000_to_2000.txt", header=FALSE, skip=0)

# Site 4
# alldata <- read.table("data/s4_1000_to_2000.txt", header=FALSE, skip=0)

# Site 5
# alldata <- read.table("data/s5_1000_to_2000.txt", header=FALSE, skip=0)

colnames(alldata) <- c("cell", "flood_cfs")

#### Separate grid data according to the Q-bins previously defined ####
q1_cells <- subset(alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
q2_cells <- subset(alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])
q3_cells <- subset(alldata, flood_cfs>=q_bin[3] & flood_cfs<q_bin[4])
q4_cells <- subset(alldata, flood_cfs>=q_bin[4] & flood_cfs<q_bin[5])
q5_cells <- subset(alldata, flood_cfs>=q_bin[5] & flood_cfs<q_bin[6])
q6_cells <- subset(alldata, flood_cfs>=q_bin[6])

#### Combine hydrology time series, timing states, and recession rate states ####
hydro_states <- cbind(hydro, timing_state, recess_state)
#### Trim data frame based on timing and recession rates ####
hydro_states <- subset(hydro_states, TIMING != "NA" & RECESSION != 1)
timing_subset <- hydro_states[,14]
recess_subset <- hydro_states[,15]

#### Allow dimension naming using the "bigmemory" package ####
options(bigmemory.allow.dimnames=TRUE)

#### Create empty matrix to fill with data based on the Q-bin ####
emptymatrix1 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q1_cells[,1]))))
emptymatrix2 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q2_cells[,1]))))
emptymatrix3 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q3_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q3_cells[,1]))))
emptymatrix4 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q4_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q4_cells[,1]))))
emptymatrix5 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q5_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q5_cells[,1]))))
emptymatrix6 <- big.matrix(nrow=nrow(hydro_states), ncol=(nrow(q6_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q6_cells[,1]))))

# USE FOR WINDOWS #
# emptymatrix1 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q1_cells[,1]))))
# emptymatrix2 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q2_cells[,1]))))
# emptymatrix3 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q3_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q3_cells[,1]))))
# emptymatrix4 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q4_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q4_cells[,1]))))
# emptymatrix5 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q5_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q5_cells[,1]))))
# emptymatrix6 <- matrix(nrow=nrow(hydro_states), ncol=(nrow(q6_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(q6_cells[,1]))))

#### Populate empty matrices
emptymatrix1[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix1[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q1_cells)) {
  emptymatrix1[,i+2] <- q1_cells[i,2]<=hydro_states[,2]
}

emptymatrix2[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix2[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q2_cells)) {
  emptymatrix2[,i+2] <- q2_cells[i,2]<=hydro_states[,2]
}

emptymatrix3[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix3[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q3_cells)) {
  emptymatrix3[,i+2] <- q3_cells[i,2]<=hydro_states[,2]
}

emptymatrix4[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix4[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q4_cells)) {
  emptymatrix4[,i+2] <- q4_cells[i,2]<=hydro_states[,2]
}

emptymatrix5[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix5[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q5_cells)) {
  emptymatrix5[,i+2] <- q5_cells[i,2]<=hydro_states[,2]
}

emptymatrix6[,1] <- as.vector(as.matrix(timing_subset))
emptymatrix6[,2] <- as.vector(as.matrix(recess_subset))
for (i in 1:nrow(q6_cells)) {
  emptymatrix6[,i+2] <- q6_cells[i,2]<=hydro_states[,2]
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
evidence1 <- populate.evidence(q1_cells, emptymatrix1)
evidence2 <- populate.evidence(q2_cells, emptymatrix2)
evidence3 <- populate.evidence(q3_cells, emptymatrix3)
evidence4 <- populate.evidence(q4_cells, emptymatrix4)
evidence5 <- populate.evidence(q5_cells, emptymatrix5)
evidence6 <- populate.evidence(q6_cells, emptymatrix6)


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
system.time(q1_cell_probs <- lapply(evidence1, apply, 1, function(x) cpquery(riparian.fit1, (POTENTIAL=="Y"), evidence=as.list(x), method="lw")))
q2_cell_probs <- lapply(evidence2, apply, 1, function(x) cpquery(riparian.fit2, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
q3_cell_probs <- lapply(evidence3, apply, 1, function(x) cpquery(riparian.fit3, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
q4_cell_probs <- lapply(evidence4, apply, 1, function(x) cpquery(riparian.fit4, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
q5_cell_probs <- lapply(evidence5, apply, 1, function(x) cpquery(riparian.fit5, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
q6_cell_probs <- lapply(evidence6, apply, 1, function(x) cpquery(riparian.fit6, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))

q1_cell_prob_mn <- t(data.frame(lapply(q1_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
q2_cell_prob_mn <- t(data.frame(lapply(q2_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
q3_cell_prob_mn <- t(data.frame(lapply(q3_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
q4_cell_prob_mn <- t(data.frame(lapply(q4_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
q5_cell_prob_mn <- t(data.frame(lapply(q5_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
q6_cell_prob_mn <- t(data.frame(lapply(q6_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
if (exists("q6_cell_prob_mn")) q_all_prob_mn <- rbind(q1_cell_prob_mn, q2_cell_prob_mn, q3_cell_prob_mn, q4_cell_prob_mn, q5_cell_prob_mn, q6_cell_prob_mn)
if (!exists("q6_cell_prob_mn")) q_all_prob_mn <- rbind(q1_cell_prob_mn, q2_cell_prob_mn, q3_cell_prob_mn, q4_cell_prob_mn, q5_cell_prob_mn)

q1_norm <- normalize(hydro_states, q1_cell_probs)
q2_norm <- normalize(hydro_states, q2_cell_probs)
q3_norm <- normalize(hydro_states, q3_cell_probs)
q4_norm <- normalize(hydro_states, q4_cell_probs)
q5_norm <- normalize(hydro_states, q5_cell_probs)
q6_norm <- normalize(hydro_states, q6_cell_probs)

q1_prob_mn_norm <- q1_cell_prob_mn * q1_norm
q2_prob_mn_norm <- q2_cell_prob_mn * q2_norm
q3_prob_mn_norm <- q3_cell_prob_mn * q3_norm
q4_prob_mn_norm <- q4_cell_prob_mn * q4_norm
q5_prob_mn_norm <- q5_cell_prob_mn * q5_norm
q6_prob_mn_norm <- q6_cell_prob_mn * q6_norm

row.names(q1_prob_mn_norm) <- row.names(q1_cell_prob_mn)
row.names(q2_prob_mn_norm) <- row.names(q2_cell_prob_mn)
row.names(q3_prob_mn_norm) <- row.names(q3_cell_prob_mn)
row.names(q4_prob_mn_norm) <- row.names(q4_cell_prob_mn)
row.names(q5_prob_mn_norm) <- row.names(q5_cell_prob_mn)
row.names(q6_prob_mn_norm) <- row.names(q6_cell_prob_mn)

if (exists("q6_prob_mn_norm")) q_all_mn_norm_e <- rbind(q1_prob_mn_norm, q2_prob_mn_norm, q3_prob_mn_norm, q4_prob_mn_norm, q5_prob_mn_norm, q6_prob_mn_norm)
if (!exists("q6_prob_mn_norm")) q_all_mn_norm_e <- rbind(q1_prob_mn_norm, q2_prob_mn_norm, q3_prob_mn_norm, q4_prob_mn_norm, q5_prob_mn_norm)

#### Write csv file output ####
write.csv(q1_prob_mn_norm, file="output/site1/q1_prob_mn_norm_e.csv")
write.csv(q2_prob_mn_norm, file="output/site1/q2_prob_mn_norm_e.csv")
write.csv(q3_prob_mn_norm, file="output/site1/q3_prob_mn_norm_e.csv")
write.csv(q4_prob_mn_norm, file="output/site1/q4_prob_mn_norm_e.csv")
write.csv(q5_prob_mn_norm, file="output/site1/q5_prob_mn_norm_e.csv")
write.csv(q6_prob_mn_norm, file="output/site1/q6_prob_mn_norm_e.csv")

# Site 1
write.csv(q_all_mn_norm_e, file="output/site1/q_all_mn_norm_site1_e.csv")
q_all_mn_norm_e2 <- read.csv("output/site1/q_all_mn_norm_site1_e.csv")
colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
write.csv(q_all_mn_norm_e2, file="output/site1/q_all_mn_norm_site1_e.csv", row.names=FALSE)

# Site 2
# write.csv(q_all_mn_norm_e, file="output/site2/q_all_mn_norm_site2_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site2/q_all_mn_norm_site2_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site2/q_all_mn_norm_site2_e.csv", row.names=FALSE)

# Site 3
# write.csv(q_all_mn_norm_e, file="output/site3/q_all_mn_norm_site3_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site3/q_all_mn_norm_site3_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site3/q_all_mn_norm_site3_e.csv", row.names=FALSE)

# # Site 4
# write.csv(q_all_mn_norm_e, file="output/site4/q_all_mn_norm_site4_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site4/q_all_mn_norm_site4_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site4/q_all_mn_norm_site4_e.csv", row.names=FALSE)

# Site 5
# write.csv(q_all_mn_norm_e, file="output/site5/q_all_mn_norm_site5_e.csv")
# q_all_mn_norm_e2 <- read.csv("output/site5/q_all_mn_norm_site5_e.csv")
# colnames(q_all_mn_norm_e2) <- c("cell","mean_prob")
# q_all_mn_norm_e2 <- q_all_mn_norm_e2[order(q_all_mn_norm_e2$cell),]
# write.csv(q_all_mn_norm_e2, file="output/site5/q_all_mn_norm_site5_e.csv", row.names=FALSE)