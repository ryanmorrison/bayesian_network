#### Load packages ####
# Load "bnlearn" package
library(bnlearn)

# Name network variables
riparian_var <- c("TIMING", "FLOOD", "RECESSION", "POTENTIAL")

# Create empty network
riparian_dag <- empty.graph(riparian_var)

riparian_test <- model2network("[TIMING][FLOOD|TIMING][RECESSION|TIMING][POTENTIAL|FLOOD:RECESSION]")

# Connect variables with directed arcs
arcs(riparian_dag) <- matrix(
	c("TIMING", "FLOOD", "TIMING", "RECESSION", "RECESSION", "POTENTIAL", "FLOOD", "POTENTIAL"),
	ncol = 2, byrow = TRUE,
	dimnames = list(c(), c("from", "to")))

#### Discrete States ####
# Set discrete states for each node

# Key to timing statess: 1 = May-June; 2 = July-August, 3 = September-October
dim_TIMING <- c(1, 2, 3)
# Key to flooding states: Y = yes; N = no
dim_FLOOD <- c("Y", "N")
# Key to recession states: 1 = < 0cm/day 2 = 0-1 cm/day; 3 = 1-3 cm/day; 4 = 3-6 cm/day, 5 = > 6 cm/day
dim_RECESSION <- c(1, 2, 3, 4, 5)
# Key to recruitment potential states: L=low; M=medium; H=high
dim_POTENTIAL <- c("L", "M", "H")

#### CPT Assignments ####
cpt_TIMING <- matrix(c(0.33, 0.33, 0.34), ncol=3, dimnames=list(NULL, "TIMING"=dim_TIMING))
cpt_FLOOD <- matrix(as.matrix(q_prob_all[2,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_RECESSION <- matrix(recess_prob_all[7,], ncol=3, dimnames=list("RECESSION"=dim_RECESSION, "TIMING"=dim_TIMING))
# cpt_POTENTIAL <- matrix(c(1, 0, 0, 0.05, 0.35, 0.6, 0.1, 0.4, 0.5, 0.1, 0.6, 0.3, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0, 1, 0, 0))
cpt_POTENTIAL <- matrix(c(1, 0, 0, 1, 0, 0, 0.05, 0.35, 0.6, 1, 0, 0, 0.1, 0.5, 0.4, 1, 0, 0, 0.1, 0.6, 0.3, 1, 0, 0, 1, 0, 0, 1, 0, 0))
dim(cpt_POTENTIAL) <- c(3, 2, 5)
dimnames(cpt_POTENTIAL) <- list("POTENTIAL"=dim_POTENTIAL, "FLOOD"=dim_FLOOD, "RECESSION"=dim_RECESSION)

riparian.fit <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD, RECESSION=cpt_RECESSION, POTENTIAL=cpt_POTENTIAL))

replicate(table(cpdist(riparian.fit, "POTENTIAL", (TIMING=="1" & FLOOD=="Y" & RECESSION=="3"))), n=10)

# ti <- as.factor(rep(1, each=10))
# q <- c("Y", "N", "N", "N", "Y", "Y", "Y", "Y", "N", "Y")
# re <- as.factor(rep(1:5, each=2))
# tdf <- data.frame(TIMING=ti, FLOOD=q, RECESSION=re)


