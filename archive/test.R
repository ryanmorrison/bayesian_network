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