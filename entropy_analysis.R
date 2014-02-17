# Entropy reduction calculations for riparian recruitment potential
set.seed(271)

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit1, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit1, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit1, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit1, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit1, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit1, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit1, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit1, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit1, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit1, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit1, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit1, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit1, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit1, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit1, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit1, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit1, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit1, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit1, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit1, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit1, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit1, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit1, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit1, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit1, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit1, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit1, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit1, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit1, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit1, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit1, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit1, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit1, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit1, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit1, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit1, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit1, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit1, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit1, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit1, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit1, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit1, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit1, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit1, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit1, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit1, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit1, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q1 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q1



## Bin Q2

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit2, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit2, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit2, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit2, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit2, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit2, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit2, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit2, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit2, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit2, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit2, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit2, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit2, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit2, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit2, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit2, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit2, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit2, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit2, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit2, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit2, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit2, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit2, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit2, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit2, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit2, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit2, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit2, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit2, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit2, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit2, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit2, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit2, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit2, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit2, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit2, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit2, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit2, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit2, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit2, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit2, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit2, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit2, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit2, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit2, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit2, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit2, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q2 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q2



## Bin Q3

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit3, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit3, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit3, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit3, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit3, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit3, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit3, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit3, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit3, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit3, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit3, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(post_belief11, 0.00001, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit3, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit3, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit3, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit3, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit3, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit3, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit3, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit3, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit3, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit3, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit3, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit3, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit3, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit3, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit3, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit3, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit3, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit3, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit3, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit3, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit3, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit3, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit3, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit3, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit3, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit3, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit3, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit3, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit3, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit3, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit3, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit3, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit3, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit3, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit3, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit3, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q3 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q3



## Bin Q4

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit4, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit4, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit4, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit4, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit4, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit4, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit4, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit4, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit4, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit4, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit4, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(post_belief11, 0.00001, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit4, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit4, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit4, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit4, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit4, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit4, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit4, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit4, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit4, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit4, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit4, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit4, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit4, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit4, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit4, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit4, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit4, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit4, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit4, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit4, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit4, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit4, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit4, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit4, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit4, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit4, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit4, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit4, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit4, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit4, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit4, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit4, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit4, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit4, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit4, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit4, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q4 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q4



## Bin Q5

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit5, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit5, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit5, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit5, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit5, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit5, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit5, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit5, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit5, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit5, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit5, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(post_belief11, 0.00001, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit5, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit5, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit5, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit5, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit5, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit5, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit5, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit5, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit5, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit5, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit5, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit5, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit5, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit5, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit5, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit5, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit5, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit5, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit5, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit5, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit5, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit5, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit5, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit5, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit5, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit5, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit5, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit5, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit5, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit5, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit5, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit5, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit5, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit5, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit5, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit5, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q5 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q5



## Bin Q6

# Calculate the prior entropy reduction of recruitment potential given no evidence of other network nodes
prior_belief1 <- cpquery(riparian.fit6, (POTENTIAL=="Y"), TRUE)
prior_belief2 <- cpquery(riparian.fit6, (POTENTIAL=="N"), TRUE)
prior_entropy <- -1*(prior_belief1*log2(prior_belief1)+prior_belief2*log2(prior_belief2))
prior_entropy 	# ~0.031

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for TIMING
post_belief1 <- cpquery(riparian.fit6, TIMING=="1", TRUE)
post_belief2 <- cpquery(riparian.fit6, TIMING=="2", TRUE)
post_belief3 <- cpquery(riparian.fit6, TIMING=="3", TRUE)

prob_timing <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit6, POTENTIAL=="Y", TIMING=="1")
post_belief12 <- cpquery(riparian.fit6, POTENTIAL=="Y", TIMING=="2")
post_belief13 <- cpquery(riparian.fit6, POTENTIAL=="Y", TIMING=="3")

post_belief21 <- cpquery(riparian.fit6, POTENTIAL=="N", TIMING=="1")
post_belief22 <- cpquery(riparian.fit6, POTENTIAL=="N", TIMING=="2")
post_belief32 <- cpquery(riparian.fit6, POTENTIAL=="N", TIMING=="3")

prob_cond <- matrix(c(0.00001, 0.00001, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_timing <- -1*(prob_timing[1] * sum(prob_cond_prod[1,]) + prob_timing[2] * sum(prob_cond_prod[2,]) + prob_timing[3] * sum(prob_cond_prod[3,]))
post_entropy_timing 	# ~0.029

entropy_red_timing <- (prior_entropy - post_entropy_timing) # ~0.0002

(prior_entropy - post_entropy_timing)/prior_entropy*100 	# ~4.95%


# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for FLOOD
post_belief1 <- cpquery(riparian.fit6, FLOOD=="1", TRUE)
post_belief2 <- cpquery(riparian.fit6, FLOOD=="0", TRUE)

prob_flood <- c(post_belief1, post_belief2)

post_belief11 <- cpquery(riparian.fit6, POTENTIAL=="Y", FLOOD=="1")
post_belief12 <- cpquery(riparian.fit6, POTENTIAL=="Y", FLOOD=="0")

post_belief21 <- cpquery(riparian.fit6, POTENTIAL=="N", FLOOD=="1")
post_belief22 <- cpquery(riparian.fit6, POTENTIAL=="N", FLOOD=="0")

prob_cond <- matrix(c(post_belief11, 0.000001, post_belief21, post_belief22), nrow=2) # The post_belief12 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_flood <- -1*(prob_flood[1] * sum(prob_cond_prod[1,]) + prob_flood[2] * sum(prob_cond_prod[2,]))
post_entropy_flood 	# ~0.007

entropy_red_flood <- (prior_entropy - post_entropy_flood) # ~0.021

(prior_entropy - post_entropy_flood)/prior_entropy*100 	# ~74.4%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for RECESSION
post_belief1 <- cpquery(riparian.fit6, RECESSION=="1", TRUE)
post_belief2 <- cpquery(riparian.fit6, RECESSION=="2", TRUE)
post_belief3 <- cpquery(riparian.fit6, RECESSION=="3", TRUE)
post_belief4 <- cpquery(riparian.fit6, RECESSION=="4", TRUE)
post_belief5 <- cpquery(riparian.fit6, RECESSION=="5", TRUE)

prob_recession <- c(post_belief1, post_belief2, post_belief3, post_belief4, post_belief5)
prob_recession

post_belief11 <- cpquery(riparian.fit6, POTENTIAL=="Y", RECESSION=="1")
post_belief12 <- cpquery(riparian.fit6, POTENTIAL=="Y", RECESSION=="2")
post_belief13 <- cpquery(riparian.fit6, POTENTIAL=="Y", RECESSION=="3")
post_belief14 <- cpquery(riparian.fit6, POTENTIAL=="Y", RECESSION=="4")
post_belief15 <- cpquery(riparian.fit6, POTENTIAL=="Y", RECESSION=="5")

post_belief21 <- cpquery(riparian.fit6, POTENTIAL=="N", RECESSION=="1")
post_belief22 <- cpquery(riparian.fit6, POTENTIAL=="N", RECESSION=="2")
post_belief32 <- cpquery(riparian.fit6, POTENTIAL=="N", RECESSION=="3")
post_belief42 <- cpquery(riparian.fit6, POTENTIAL=="N", RECESSION=="4")
post_belief52 <- cpquery(riparian.fit6, POTENTIAL=="N", RECESSION=="5")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief13, post_belief14, 0.00001, post_belief21, post_belief22, post_belief32, post_belief42, post_belief52), nrow=5) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_recession <- -1*(prob_recession[1] * sum(prob_cond_prod[1,]) + prob_recession[2] * sum(prob_cond_prod[2,]) + prob_recession[3] * sum(prob_cond_prod[3,]) + prob_recession[4] * sum(prob_cond_prod[4,]) + prob_recession[5] * sum(prob_cond_prod[5,]))
post_entropy_recession 	# ~0.028

entropy_red_recession <- (prior_entropy - post_entropy_recession) # ~0.0007

(prior_entropy - post_entropy_recession)/prior_entropy*100 	# ~2.46%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for HYDRO
post_belief1 <- cpquery(riparian.fit6, HYDRO=="L", TRUE)
post_belief2 <- cpquery(riparian.fit6, HYDRO=="H", TRUE)

prob_hydro <- c(post_belief1, post_belief2)
prob_hydro

post_belief11 <- cpquery(riparian.fit6, POTENTIAL=="Y", HYDRO=="L")
post_belief12 <- cpquery(riparian.fit6, POTENTIAL=="Y", HYDRO=="H")

post_belief21 <- cpquery(riparian.fit6, POTENTIAL=="N", HYDRO=="L")
post_belief22 <- cpquery(riparian.fit6, POTENTIAL=="N", HYDRO=="H")

prob_cond <- matrix(c(0.000001, post_belief12, post_belief21, post_belief22), nrow=2) # The post_belief11 variable was replaced with 0.000001 because the log2 of 0 is negative infinity
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_hydro <- -1*(prob_hydro[1] * sum(prob_cond_prod[1,]) + prob_hydro[2] * sum(prob_cond_prod[2,]))
post_entropy_hydro 	# ~0.0032

entropy_red_hydro <- (prior_entropy - post_entropy_hydro) # ~0.0025

(prior_entropy - post_entropy_hydro)/prior_entropy*100 	# ~89.56%

# Calculate the posterior entropy of recruitment POTENTIAL given each state of evidence for DEPTH
post_belief1 <- cpquery(riparian.fit6, DEPTH=="1", TRUE)
post_belief2 <- cpquery(riparian.fit6, DEPTH=="2", TRUE)
post_belief3 <- cpquery(riparian.fit6, DEPTH=="3", TRUE)

prob_depth <- c(post_belief1, post_belief2, post_belief3)

post_belief11 <- cpquery(riparian.fit6, POTENTIAL=="Y", DEPTH=="1")
post_belief12 <- cpquery(riparian.fit6, POTENTIAL=="Y", DEPTH=="2")
post_belief13 <- cpquery(riparian.fit6, POTENTIAL=="Y", DEPTH=="3")

post_belief21 <- cpquery(riparian.fit6, POTENTIAL=="N", DEPTH=="1")
post_belief22 <- cpquery(riparian.fit6, POTENTIAL=="N", DEPTH=="2")
post_belief32 <- cpquery(riparian.fit6, POTENTIAL=="N", DEPTH=="3")

prob_cond <- matrix(c(post_belief11, post_belief12, post_belief13, post_belief21, post_belief22, post_belief32), nrow=3)
prob_cond
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_entropy_depth <- -1*(prob_depth[1] * sum(prob_cond_prod[1,]) + prob_depth[2] * sum(prob_cond_prod[2,]) + prob_depth[3] * sum(prob_cond_prod[3,]))
post_entropy_depth 	# ~0.029

entropy_red_depth <- (prior_entropy - post_entropy_depth) # ~0.00035

(prior_entropy - post_entropy_depth)/prior_entropy*100 	# ~1.23%

entropy_reduction_q6 <- data.frame(node=c("timing", "flood", "recession", "hydro_state", "depth"), reduction=c(entropy_red_timing, entropy_red_flood, entropy_red_recession, entropy_red_hydro, entropy_red_depth))
entropy_reduction_q6

entropy_reduction_all <- cbind(entropy_reduction_q1, entropy_reduction_q2[2], entropy_reduction_q3[2], entropy_reduction_q4[2], entropy_reduction_q5[2], entropy_reduction_q6[2])
entropy_reduction_all

entropy_avg <- as.data.frame(apply(entropy_reduction_all[,2:7], 1, mean, na.rm=TRUE))
entropy_avg <- cbind(entropy_reduction_all$node, entropy_avg)
colnames(entropy_avg) <- c("node", "entropy_reduction")
entropy_avg