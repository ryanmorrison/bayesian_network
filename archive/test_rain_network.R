#### Load packages ####
# Load "bnlearn" package
library(bnlearn)

# Name network variables
rain_var <- c("cloudy", "sprinkler", "rain", "wetgrass")

# Create empty network
rain_dag <- empty.graph(rain_var)

# Connect variables with directed arcs
arcs(rain_dag) <- matrix(
	c("cloudy", "sprinkler", "cloudy", "rain", "rain", "wetgrass", "sprinkler", "wetgrass"),
	ncol = 2, byrow = TRUE,
	dimnames = list(c(), c("from", "to")))

#### Discrete States ####
# Set discrete states for each node

dim_cloudy <- c("T", "F")
dim_sprinkler <- c("T", "F")
dim_rain <- c("T", "F")
dim_wetgrass <- c("T", "F")


#### CPT Assignments ####
cpt_cloudy <- matrix(c(0.5, 0.5), ncol=2, dimnames=list(NULL, "cloudy"=dim_cloudy))
cpt_sprinkler <- matrix(c(0.1, 0.9, 0.5, 0.5), ncol=2, dimnames=list("sprinkler"=dim_sprinkler, "cloudy"=dim_cloudy))
cpt_rain <- matrix(c(0.8, 0.2, 0.2, 0.8), ncol=2, dimnames=list("rain"=dim_rain, "cloudy"=dim_cloudy))
cpt_wetgrass <- matrix(c(0.99, 0.01, 0.9, 0.1, 0.9, 0.1, 0.01, 0.99))
dim(cpt_wetgrass) <- c(2, 2, 2)
dimnames(cpt_wetgrass) <- list("wetgrass"=dim_wetgrass, "rain"=dim_rain, "sprinkler"=dim_sprinkler)

rain.fit <- custom.fit(rain_dag, dist=list(cloudy=cpt_cloudy, sprinkler=cpt_sprinkler, rain=cpt_rain, wetgrass=cpt_wetgrass))

evid <- list(sprinkler="F", rain="F")
cpquery(rain.fit, wetgrass=="T", evidence=evid, method="lw")





library(bnlearn)

set.seed(42)

# a BN using expert knowledge
net <- model2network("[A][S][T|A][L|S][B|S][E|T:L][X|E][D|B:E]")
yn <- c("yes", "no")
cptA <- matrix(c(0.01, 0.99), ncol=2, dimnames=list(NULL, yn))
cptS <- matrix(c(0.5, 0.5), ncol=2, dimnames=list(NULL, yn))
cptT <- matrix(c(0.05, 0.95, 0.01, 0.99),
               ncol=2, dimnames=list("T"=yn, "A"=yn))
cptL <- matrix(c(0.1, 0.9, 0.01, 0.99),
               ncol=2, dimnames=list("L"=yn, "S"=yn))
cptB <- matrix(c(0.6, 0.4, 0.3, 0.7),
               ncol=2, dimnames=list("B"=yn, "S"=yn))
# cptE and cptD are 3-d matrices, which don't exist in R, so
# need to build these manually as below.
cptE <- c(1.0, 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0)
dim(cptE) <- c(2, 2, 2)
dimnames(cptE) <- list("E"=yn, "L"=yn, "T"=yn)
cptX <- matrix(c(0.98, 0.02, 0.05, 0.95),
               ncol=2, dimnames=list("X"=yn, "E"=yn))
cptD <- c(0.9, 0.1, 0.7, 0.3, 0.8, 0.2, 0.1, 0.9)
dim(cptD) <- c(2, 2, 2)
dimnames(cptD) <- list("D"=yn, "E"=yn, "B"=yn)
net.disc <- custom.fit(net, dist=list(A=cptA, S=cptS, T=cptT, L=cptL,
                                      B=cptB, E=cptE, X=cptX, D=cptD))

# Unit test: Given no evidence, the chances of tuberculosis is about 1%
cpquery(net.disc, (T=="yes"), TRUE)
cpquery(net.disc, (L=="yes"), TRUE)
cpquery(net.disc, (B=="yes"), TRUE)
# [1] 0.01084444
# [1] 0.05428889
# [1] 0.4501667

# Question 1:
# Patient has recently visited Asia and does not smoke. Which is most
# likely?
# (a) the patient is more likely to have tuberculosis then anything else.
# (b) the chance that the patient has lung cancer is higher than he/she
#     having tuberculosis
# (c) the patient is more likely to have bronchitis then anything else
# (d) the chance that the patient has tuberculosis is higher than he/she
#     having bronchitis
cpquery(net.disc, (T=="yes"), (A=="yes" & S=="no"))
cpquery(net.disc, (L=="yes"), (A=="yes" & S=="no"))
cpquery(net.disc, (B=="yes"), (A=="yes" & S=="no"))
# [1] 0.04988124
# [1] 0.00462963
# [1] 0.316092
# shows that (c) is correct.

# Question 2
# The patient has recently visited Asia, does not smoke, is not
# complaining of dyspnoea, but his/her x-ray shows a positive shadow
# (a) the patient most likely has tuberculosis, but lung cancer is
#     almost equally likely
# (b) the patient most likely has tuberculosis as compared to any of
#     the other choices
# (c) the patient most likely has bronchitis, and tuberculosis is
#     almost equally likely
# (d) the patient most likely has tuberculosis, but bronchitis is
#     almost equally likely
cpquery(net.disc, (T=="yes"), (A=="yes" & S=="no" & D=="no" & X=="yes"))
cpquery(net.disc, (L=="yes"), (A=="yes" & S=="no" & D=="no" & X=="yes"))
cpquery(net.disc, (B=="yes"), (A=="yes" & S=="no" & D=="no" & X=="yes"))
# [1] 0.2307692
# [1] 0.04166667
# [1] 0.2105263
# shows that (d) is correct