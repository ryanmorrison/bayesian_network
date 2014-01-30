#### Load packages ####
# Load "bnlearn" package
library(bnlearn)

# Name network variables
riparian_var <- c("TIMING", "FLOOD", "RECESSION", "HYDRO", "DEPTH", "POTENTIAL")

# Create empty network
riparian_dag <- empty.graph(riparian_var)

# riparian_test <- model2network("[TIMING][DEPTH][FLOOD|TIMING][RECESSION|TIMING][HYDRO|FLOOD:RECESSION][POTENTIAL|HYDRO:DEPTH]")

# Connect variables with directed arcs
arcs(riparian_dag) <- matrix(
	c("TIMING", "FLOOD", "TIMING", "RECESSION", "RECESSION", "HYDRO", "FLOOD", "HYDRO", "DEPTH", "POTENTIAL", "HYDRO", "POTENTIAL"),
	ncol = 2, byrow = TRUE,
	dimnames = list(c(), c("from", "to")))

#### Discrete States ####
# Set discrete states for each node

# Key to timing statess: 1 = May-June; 2 = July-August, 3 = September-October
dim_TIMING <- c(1, 2, 3)
# Key to flooding states: Y = yes; N = no
# dim_FLOOD <- c("Y", "N")
dim_FLOOD <- c(1, 0)
# dim_FLOOD <- c("TRUE", "FALSE") # USE FOR WINDOWS
# Key to recession states: 1 = < 0cm/day 2 = 0-1 cm/day; 3 = 1-3 cm/day; 4 = 3-6 cm/day, 5 = > 6 cm/day
dim_RECESSION <- c(1, 2, 3, 4, 5)
# Key to hydrologic condition states: L=low; M=medium; H=high
# dim_HYDRO <- c("L", "M", "H")
dim_HYDRO <- c("L", "H")
# Key to groundwater depth states: 1 = < 50 cm; 2 = 50-200 cm; 3 = > 200 cm
dim_DEPTH <- c(1, 2, 3)
# Key to recruitment potential states: L=low; M=medium; H=high
# dim_POTENTIAL <- c("L", "M", "H")
dim_POTENTIAL <- c("Y", "N")


#### CPT Assignments ####
cpt_TIMING <- matrix(c(0.65, 0.25, 0.1), ncol=3, dimnames=list(NULL, "TIMING"=dim_TIMING))
cpt_FLOOD1 <- matrix(as.matrix(q_prob_all[2, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD2 <- matrix(as.matrix(q_prob_all[3, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD3 <- matrix(as.matrix(q_prob_all[4, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD4 <- matrix(as.matrix(q_prob_all[5, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD5 <- matrix(as.matrix(q_prob_all[6, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD6 <- matrix(as.matrix(q_prob_all[7, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD7 <- matrix(as.matrix(q_prob_all[8, ]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_RECESSION <- matrix(recess_prob_all[8, ], ncol=3, dimnames=list("RECESSION"=dim_RECESSION, "TIMING"=dim_TIMING)) # Row 8 refers to the 14-day avg forward recession rate
cpt_RECESSION <- cpt.roundoff(cpt_RECESSION)
# cpt_HYDRO <- matrix(c(1, 0, 0, 1, 0, 0, 0.05, 0.35, 0.6, 1, 0, 0, 0.1, 0.5, 0.4, 1, 0, 0, 0.1, 0.6, 0.3, 1, 0, 0, 1, 0, 0, 1, 0, 0))
# cpt_HYDRO <- matrix(c(0.9, 0.05, 0.05, 0.9, 0.05, 0.05, 0.05, 0.35, 0.6, 0.9, 0.05, 0.05, 0.1, 0.4, 0.5, 0.9, 0.05, 0.05, 0.1, 0.6, 0.3, 0.9, 0.05, 0.05, 0.9, 0.05, 0.05, 0.9, 0.05, 0.05))
# cpt_HYDRO <- matrix(c(1, 0, 0, 1, 0, 0, 0.05, 0.35, 0.6, 1, 0, 0, 0.1, 0.4, 0.5, 1, 0, 0, 0.1, 0.6, 0.3, 1, 0, 0, 1, 0, 0, 1, 0, 0))
cpt_HYDRO <- matrix(c(1, 0, 1, 0, 0.05, 0.95, 1, 0, 0.1, 0.9, 1, 0, 0.3, 0.7, 1, 0, 0.9, 0.1, 1, 0))
dim(cpt_HYDRO) <- c(2, 2, 5)
dimnames(cpt_HYDRO) <- list("HYDRO"=dim_HYDRO, "FLOOD"=dim_FLOOD, "RECESSION"=dim_RECESSION)
cpt_DEPTH <- matrix(c(0.33, 0.33, 0.34), ncol=3, dimnames=list(NULL, "DEPTH"=dim_DEPTH))
# cpt_POTENTIAL <- matrix(c(0.3, 0.7, 0.4, 0.6, 0.5, 0.5, 0.5, 0.5, 0.6, 0.4, 0.7, 0.3, 0.3,0.7, 0.4, 0.6, 0.5, 0.5), ncol=3)
# cpt_POTENTIAL <- matrix(c(0, 1, 0.4, 0.6, 0.5, 0.5, 0.5, 0.5, 0.6, 0.4, 0.7, 0.3, 0, 1, 0.4, 0.6, 0.5, 0.5), ncol=3)
cpt_POTENTIAL <- matrix(c(0, 1, 0.7, 0.3, 0, 1, 0.9, 0.1, 0, 1, 0.7, 0.3), ncol=3)
dim(cpt_POTENTIAL) <- c(2, 2, 3)
dimnames(cpt_POTENTIAL) <- list("POTENTIAL"=dim_POTENTIAL, "HYDRO"=dim_HYDRO, "DEPTH"=dim_DEPTH)
# cpt_POTENTIAL <- matrix(c(0.8, 0.1, 0.1, 0.3, 0.5, 0.2, 0.1, 0.3, 0.6, 0.75, 0.15, 0.1, 0.2, 0.4, 0.4, 0.05, 0.2, 0.75, 0.9, 0.05, 0.05, 0.3, 0.5, 0.2, 0.2, 0.4, 0.4))
# dim(cpt_POTENTIAL) <- c(3, 3, 3)
# dimnames(cpt_POTENTIAL) <- list("POTENTIAL"=dim_POTENTIAL, "HYDRO"=dim_HYDRO, "DEPTH"=dim_DEPTH)

riparian.fit1 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD1, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit2 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD2, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit3 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD3, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit4 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD4, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit5 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD5, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit6 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD6, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))
riparian.fit7 <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD7, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO, DEPTH=cpt_DEPTH, POTENTIAL=cpt_POTENTIAL))

# plot(riparian_dag)

# replicate(table(cpdist(riparian.fit, "POTENTIAL", (TIMING=="1" & FLOOD=="Y" & RECESSION=="3"))), n=10)

# ti <- as.factor(rep(1, each=10))
# q <- c("Y", "N", "N", "N", "Y", "Y", "Y", "Y", "N", "Y")
# re <- as.factor(rep(1:5, each=2))
# tdf <- data.frame(TIMING=ti, FLOOD=q, RECESSION=re)

# ev <- list(FLOOD="1", RECESSION="2", TIMING="2")
# cpquery(riparian.fit1, POTENTIAL=="Y", evidence=ev, method="lw")

