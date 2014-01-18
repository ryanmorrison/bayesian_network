# Existing

e_alldata <- read.table("data/s1_1000_to_2000_trim.txt", header=FALSE, skip=0)
colnames(e_alldata) <- c("cell", "flood_cfs")

e_q1_cells <- subset(e_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
e_q2_cells <- subset(e_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])

e_hydro_states <- cbind(hydro, timing_state, recess_state)
e_hydro_states <- subset(e_hydro_states, TIMING != "NA" & RECESSION != 1)
e_timing_subset <- e_hydro_states[,14]
e_recess_subset <- e_hydro_states[,15]

options(bigmemory.allow.dimnames=TRUE)

e_emptymatrix1 <- big.matrix(nrow=nrow(e_hydro_states), ncol=(nrow(e_q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(e_q1_cells[,1]))))
e_emptymatrix2 <- big.matrix(nrow=nrow(e_hydro_states), ncol=(nrow(e_q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(e_q2_cells[,1]))))

e_emptymatrix1[,1] <- as.vector(as.matrix(e_timing_subset))
e_emptymatrix1[,2] <- as.vector(as.matrix(e_recess_subset))
for (i in 1:nrow(e_q1_cells)) {
  e_emptymatrix1[,i+2] <- e_q1_cells[i,2]<=e_hydro_states[,2]
}
e_emptymatrix2[,1] <- as.vector(as.matrix(e_timing_subset))
e_emptymatrix2[,2] <- as.vector(as.matrix(e_recess_subset))
for (i in 1:nrow(e_q2_cells)) {
  e_emptymatrix2[,i+2] <- e_q2_cells[i,2]<=e_hydro_states[,2]
}

e_evidence1 <- populate.evidence(e_q1_cells, e_emptymatrix1)
e_evidence2 <- populate.evidence(e_q2_cells, e_emptymatrix2)

e_q1_cell_probs <- lapply(e_evidence1, apply, 1, function(x) cpquery(riparian.fit1, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
e_q2_cell_probs <- lapply(e_evidence2, apply, 1, function(x) cpquery(riparian.fit2, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))

e_q1_cell_prob_mn <- t(data.frame(lapply(e_q1_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
e_q2_cell_prob_mn <- t(data.frame(lapply(e_q2_cell_probs, mean, na.rm=TRUE), check.names=FALSE))

e_q1_norm <- normalize(e_hydro_states, e_q1_cell_probs)
e_q2_norm <- normalize(e_hydro_states, e_q2_cell_probs)

e_q1_prob_mn_norm <- e_q1_cell_prob_mn * e_q1_norm
e_q2_prob_mn_norm <- e_q2_cell_prob_mn * e_q2_norm

row.names(e_q1_prob_mn_norm) <- row.names(e_q1_cell_prob_mn)
row.names(e_q2_prob_mn_norm) <- row.names(e_q2_cell_prob_mn)



# Scenario 1

s1_alldata <- read.table("data/s1_1000_to_2000_trim.txt", header=FALSE, skip=0)
colnames(s1_alldata) <- c("cell", "flood_cfs")

s1_q1_cells <- subset(s1_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
s1_q2_cells <- subset(s1_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])

s1_hydro_states <- cbind(scenario1, timing_state_s1, recess_state_s1)
s1_hydro_states <- subset(s1_hydro_states, TIMING != "NA" & RECESSION != 1)
s1_timing_subset <- s1_hydro_states[,14]
s1_recess_subset <- s1_hydro_states[,15]

options(bigmemory.allow.dimnames=TRUE)

s1_emptymatrix1 <- big.matrix(nrow=nrow(s1_hydro_states), ncol=(nrow(s1_q1_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(s1_q1_cells[,1]))))
s1_emptymatrix2 <- big.matrix(nrow=nrow(s1_hydro_states), ncol=(nrow(s1_q2_cells)+2), dimnames=list(NULL, c("timing", "recess", as.character(s1_q2_cells[,1]))))

s1_emptymatrix1[,1] <- as.vector(as.matrix(s1_timing_subset))
s1_emptymatrix1[,2] <- as.vector(as.matrix(s1_recess_subset))
for (i in 1:nrow(s1_q1_cells)) {
  s1_emptymatrix1[,i+2] <- s1_q1_cells[i,2]<=s1_hydro_states[,2]
}
s1_emptymatrix2[,1] <- as.vector(as.matrix(s1_timing_subset))
s1_emptymatrix2[,2] <- as.vector(as.matrix(s1_recess_subset))
for (i in 1:nrow(s1_q2_cells)) {
  s1_emptymatrix2[,i+2] <- s1_q2_cells[i,2]<=s1_hydro_states[,2]
}

s1_evidence1 <- populate.evidence(s1_q1_cells, s1_emptymatrix1)
s1_evidence2 <- populate.evidence(s1_q2_cells, s1_emptymatrix2)

s1_q1_cell_probs <- lapply(s1_evidence1, apply, 1, function(x) cpquery(riparian.fit1, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))
s1_q2_cell_probs <- lapply(s1_evidence2, apply, 1, function(x) cpquery(riparian.fit2, (POTENTIAL=="Y"), evidence=as.list(x), method="lw"))

s1_q1_cell_prob_mn <- t(data.frame(lapply(s1_q1_cell_probs, mean, na.rm=TRUE), check.names=FALSE))
s1_q2_cell_prob_mn <- t(data.frame(lapply(s1_q2_cell_probs, mean, na.rm=TRUE), check.names=FALSE))

s1_q1_normalize <- normalize2(e_hydro_states, s1_hydro_states, e_q1_cell_probs, s1_q1_cell_probs)
s1_q2_normalize <- normalize2(e_hydro_states, s1_hydro_states, e_q2_cell_probs, s1_q2_cell_probs)

s1_q1_prob_mn_norm <- s1_q1_cell_prob_mn * s1_q1_normalize
s1_q2_prob_mn_norm <- s1_q2_cell_prob_mn * s1_q2_normalize

row.names(s1_q1_prob_mn_norm) <- row.names(s1_q1_cell_prob_mn)
row.names(s1_q2_prob_mn_norm) <- row.names(s1_q2_cell_prob_mn)

nrow(s1_hydro_states)
nrow(e_hydro_states)

nrow(e_q1_prob_mn_norm)
nrow(s1_q1_prob_mn_norm)

nrow(e_q2_prob_mn_norm)
nrow(s1_q2_prob_mn_norm)

q1_diff <- (s1_q1_prob_mn_norm - e_q1_cell_prob_mn)/e_q1_cell_prob_mn*100
mean(q1_diff[,1])
q1_diff <- (s1_q1_cell_prob_mn - e_q1_cell_prob_mn)/e_q1_cell_prob_mn*100
q2_diff <- (s1_q2_prob_mn_norm - e_q2_cell_prob_mn)/e_q2_cell_prob_mn*100
mean(q2_diff[,1])

q2_diff <- (s1_q2_cell_prob_mn - e_q2_cell_prob_mn)/e_q2_cell_prob_mn*100



E_site1 <- read.csv("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network/output/site1/E_q_all_prob_mn.csv", header=TRUE)
S1_site1 <- read.csv("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network/output/site1/S1_q_all_prob_mn.csv", header=TRUE)
S2_site1 <- read.csv("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network/output/site1/S2_q_all_prob_mn.csv", header=TRUE)

S1_diff <- (S1_site1 - E_site1)*100
mean(S1_diff[,2])
S2_diff <- (S2_site1 - E_site1)*100
mean(S2_diff[,2])