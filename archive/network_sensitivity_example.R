#### Load packages ####
# Load "bnlearn" package
library(bnlearn)

# Name network variables
cancer_var <- c("Pollution", "Smoker", "Cancer", "Xray", "Dyspnoea")

# Create empty network
cancer_dag <- empty.graph(cancer_var)

# riparian_test <- model2network("[TIMING][DEPTH][FLOOD|TIMING][RECESSION|TIMING][HYDRO|FLOOD:RECESSION][POTENTIAL|HYDRO:DEPTH]")

# Connect variables with directed arcs
arcs(cancer_dag) <- matrix(
	c("Pollution", "Cancer", "Smoker", "Cancer", "Cancer", "Xray", "Cancer", "Dyspnoea"),
	ncol = 2, byrow = TRUE,
	dimnames = list(c(), c("from", "to")))

#### Discrete States ####
# Set discrete states for each node

dim_Pollution <- c("L", "H")
dim_Smoker <- c("T", "F")
dim_Cancer <- c("T", "F")
dim_Xray <- c("Pos", "Neg")
dim_Dyspnoea <- c("T", "F")


#### CPT Assignments ####
cpt_Pollution <- matrix(c(0.9, 0.1), ncol=2, dimnames=list(NULL, "Pollution"=dim_Pollution))
cpt_Smoker <- matrix(c(0.3, 0.7), ncol=2, dimnames=list(NULL, "Smoker"=dim_Smoker))
cpt_Cancer <- matrix(c(0.03, 0.97, 0.001, 0.999, 0.05, 0.95, 0.02, 0.98))
dim(cpt_Cancer) <- c(2, 2, 2)
dimnames(cpt_Cancer) <- list("Cancer"=dim_Cancer, "Smoker"=dim_Smoker, "Pollution"=dim_Pollution)
cpt_Xray <- matrix(c(0.9, 0.1, 0.2, 0.8), ncol=2, dimnames=list("Xray"=dim_Xray,"Cancer"=dim_Cancer))
cpt_Dyspnoea <- matrix(c(0.65, 0.35, 0.3, 0.7), ncol=2, dimnames=list("Dyspnoea"=dim_Dyspnoea, "Cancer"=dim_Cancer))

cancer_fit <- custom.fit(cancer_dag, dist=list(Pollution=cpt_Pollution, Smoker=cpt_Smoker, Cancer=cpt_Cancer, Xray=cpt_Xray, Dyspnoea=cpt_Dyspnoea))

#### CPT Assignments ####
cpt_Pollution <- matrix(c(0.9, 0.1), ncol=2, dimnames=list(NULL, "Pollution"=dim_Pollution))
cpt_Smoker <- matrix(c(0.3, 0.7), ncol=2, dimnames=list(NULL, "Smoker"=dim_Smoker))
cpt_Cancer <- matrix(c(0.03, 0.97, 0.001, 0.999, 0.05, 0.95, 0.02, 0.98))
dim(cpt_Cancer) <- c(2, 2, 2)
dimnames(cpt_Cancer) <- list("Cancer"=dim_Cancer, "Smoker"=dim_Smoker, "Pollution"=dim_Pollution)
cpt_Xray <- matrix(c(0.9, 0.1, 0.2, 0.8), ncol=2, dimnames=list("Xray"=dim_Xray,"Cancer"=dim_Cancer))
cpt_Dyspnoea <- matrix(c(0.65, 0.35, 0.3, 0.7), ncol=2, dimnames=list("Dyspnoea"=dim_Dyspnoea, "Cancer"=dim_Cancer))

cancer_fit <- custom.fit(cancer_dag, dist=list(Pollution=cpt_Pollution, Smoker=cpt_Smoker, Cancer=cpt_Cancer, Xray=cpt_Xray, Dyspnoea=cpt_Dyspnoea))


# Xray Entropy Reduction
prior_bel1 <- cpquery(cancer_fit, (Cancer=="T"), TRUE)
prior_bel2 <- cpquery(cancer_fit, (Cancer=="F"), TRUE)
prior_ent <- -1*(prior_bel1*log2(prior_bel1)+prior_bel2*log2(prior_bel2))

post_bel1 <- cpquery(cancer_fit, Xray=="Pos", TRUE)
post_bel2 <- cpquery(cancer_fit, Xray=="Neg", TRUE)

prob_xray <- c(post_bel1, post_bel2)

post_bel11 <- cpquery(cancer_fit, Cancer=="T", Xray=="Pos")
post_bel12 <- cpquery(cancer_fit, Cancer=="T", Xray=="Neg")

post_bel21 <- cpquery(cancer_fit, Cancer=="F", Xray=="Pos")
post_bel22 <- cpquery(cancer_fit, Cancer=="F", Xray=="Neg")

prob_cond <- matrix(c(post_bel11, post_bel12, post_bel21, post_bel22), nrow=2)
prob_cond_log <- log2(prob_cond)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_ent <- -1*(prob_xray[1] * sum(prob_cond_prod[1,]) + prob_xray[2] * sum(prob_cond_prod[2,]))

(prior_ent - post_ent)

(prior_ent - post_ent)/prior_ent*100

# Smoking Entropy Reduction
post_bel1 <- cpquery(cancer_fit, Smoker=="T", TRUE)
post_bel2 <- cpquery(cancer_fit, Smoker=="F", TRUE)

prob_smoker <- c(post_bel1, post_bel2)

post_bel11 <- cpquery(cancer_fit, Cancer=="T", Smoker=="T")
post_bel12 <- cpquery(cancer_fit, Cancer=="T", Smoker=="F")

post_bel21 <- cpquery(cancer_fit, Cancer=="F", Smoker=="T")
post_bel22 <- cpquery(cancer_fit, Cancer=="F", Smoker=="F")

prob_cond <- matrix(c(post_bel11, post_bel12, post_bel21, post_bel22), nrow=2)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_ent <- -1*(prob_smoker[1] * sum(prob_cond_prod[1,]) + prob_smoker[2] * sum(prob_cond_prod[2,]))

(prior_ent - post_ent)

# Pollution Entropy Reduction
post_bel1 <- cpquery(cancer_fit, Pollution=="L", TRUE)
post_bel2 <- cpquery(cancer_fit, Pollution=="H", TRUE)

prob_pollution <- c(post_bel1, post_bel2)

post_bel11 <- cpquery(cancer_fit, Cancer=="T", Pollution=="L")
post_bel12 <- cpquery(cancer_fit, Cancer=="T", Pollution=="H")

post_bel21 <- cpquery(cancer_fit, Cancer=="F", Pollution=="L")
post_bel22 <- cpquery(cancer_fit, Cancer=="F", Pollution=="H")

prob_cond <- matrix(c(post_bel11, post_bel12, post_bel21, post_bel22), nrow=2)
prob_cond_prod <- prob_cond * log2(prob_cond)

post_ent <- -1*(prob_pollution[1] * sum(prob_cond_prod[1,]) + prob_pollution[2] * sum(prob_cond_prod[2,]))

(prior_ent - post_ent)

# Correct example
y <- c(0.108, 0.27, 0.453, 0.170)
x <- c(0.25, 0.25, 0.25, 0.25)
x_given_y <- matrix(c(0.284, 0.221, 0.355, 0.0114, 0.406, 0.232, 0.293, 0.0648, 0.221, 0.0868, 0.172, 0.736, 0.089, 0.47, 0.180, 0.188), nrow=4)
prior_entropy <- -1*sum(x*log2(x))
post_entropy1 <- x_given_y * log2(x_given_y)
post_entropy2 <- -1*(y[1]*sum(post_entropy1[1,]) + y[2]*sum(post_entropy1[2,]) + y[3]*sum(post_entropy1[3,]) + y[4]*sum(post_entropy1[4,]))
reduction <- prior_entropy - post_entropy2



bel <- cpquery(cancer_fit, (Cancer=="T"), TRUE)
min_bel <- cpquery(cancer_fit, Cancer=="T", Cancer=="F")
max_bel <- cpquery(cancer_fit, Cancer=="T", Cancer=="T")

test1 <- cpquery(cancer_fit, Cancer=="T", Xray=="Neg")
test2 <- cpquery(cancer_fit, Cancer=="T", Xray=="Pos")
test3 <- cpquery(cancer_fit, Cancer=="F", Xray=="Pos")
test4 <- cpquery(cancer_fit, Cancer=="F", Xray=="Neg")
test5 <- -1*(test1*log(test1)+test2*log(test2))
test6 <- -1*(test3*log(test3)+test4*log(test4))

what1 <- cpquery(cancer_fit, Cancer=="T", Smoker=="T")
what2 <- cpquery(cancer_fit, Cancer=="T", Smoker=="F")
what3 <- cpquery(cancer_fit, Cancer=="F", Smoker=="T")
what4 <- cpquery(cancer_fit, Cancer=="F", Smoker=="F")
what5 <- -1*(what1*log(what1)+what2*log(what2))
what6 <- -1*(what3*log(what3)+what4*log(what4))
what7 <- what1*log(what1)+what3*log(what3)
what8 <- what2*log(what2)+what4*log(what4)
what7-what8
what6-what5

log(2)
0.001*log(0.001)+0.050*log(0.050)+0.95*log(0.95)+0.999*log(0.999)