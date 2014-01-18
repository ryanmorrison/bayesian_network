#### Load packages ####
# Load "bnlearn" package
library(bnlearn)

set.seed(132)

# Name network variables
riparian_var <- c("TIMING", "FLOOD", "RECESSION", "HYDRO")

# Create empty network
riparian_dag <- empty.graph(riparian_var)

# Connect variables with directed arcs
arcs(riparian_dag) <- matrix(
	c("TIMING", "FLOOD", "TIMING", "RECESSION", "RECESSION", "HYDRO", "FLOOD", "HYDRO"),
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
dim_HYDRO <- c("L", "H")


#### CPT Assignments ####
cpt_TIMING <- matrix(c(0.33, 0.33, 0.34), ncol=3, dimnames=list(NULL, "TIMING"=dim_TIMING))
cpt_FLOOD1 <- matrix(as.matrix(q_prob_all[2,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD2 <- matrix(as.matrix(q_prob_all[3,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD3 <- matrix(as.matrix(q_prob_all[4,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD4 <- matrix(as.matrix(q_prob_all[5,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD5 <- matrix(as.matrix(q_prob_all[6,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD6 <- matrix(as.matrix(q_prob_all[7,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_FLOOD7 <- matrix(as.matrix(q_prob_all[8,]), ncol=3, dimnames=list("FLOOD"=dim_FLOOD, "TIMING"=dim_TIMING))
cpt_RECESSION <- matrix(recess_prob_all[7,], ncol=3, dimnames=list("RECESSION"=dim_RECESSION, "TIMING"=dim_TIMING)) # Row 7 refers to the 7-day avg recession rate
# cpt_HYDRO <- matrix(c(1, 0, 0, 1, 0, 0, 0.05, 0.35, 0.6, 1, 0, 0, 0.1, 0.5, 0.4, 1, 0, 0, 0.1, 0.6, 0.3, 1, 0, 0, 1, 0, 0, 1, 0, 0))
cpt_HYDRO <- matrix(c(1, 0, 0, 1, 0, 0, 0.05, 0.35, 0.6, 1, 0, 0, 0.1, 0.4, 0.5, 1, 0, 0, 0.1, 0.6, 0.3, 1, 0, 0, 0.9, 0.1, 0, 1, 0, 0))
cpt_HYDRO <- matrix(c(1, 0, 1, 0, 0.05, 0.95, 1, 0, 0.1, 0.9, 1, 0, 0.3, 0.7, 1, 0, 0.9, 0.1, 1, 0))
dim(cpt_HYDRO) <- c(2, 2, 5)
dimnames(cpt_HYDRO) <- list("HYDRO"=dim_HYDRO, "FLOOD"=dim_FLOOD, "RECESSION"=dim_RECESSION)

riparian.fitX <- custom.fit(riparian_dag, dist=list(TIMING=cpt_TIMING, FLOOD=cpt_FLOOD1, RECESSION=cpt_RECESSION, HYDRO=cpt_HYDRO))

# plot(riparian_dag)

# replicate(table(cpdist(riparian.fit, "POTENTIAL", (TIMING=="1" & FLOOD=="Y" & RECESSION=="3"))), n=10)

# ti <- as.factor(rep(1, each=10))
# q <- c("Y", "N", "N", "N", "Y", "Y", "Y", "Y", "N", "Y")
# re <- as.factor(rep(1:5, each=2))
# tdf <- data.frame(TIMING=ti, FLOOD=q, RECESSION=re)

ev <- list(FLOOD="1", RECESSION="3", TIMING="2")
cpquery(riparian.fitX, HYDRO=="H", evidence=ev, method="lw")
