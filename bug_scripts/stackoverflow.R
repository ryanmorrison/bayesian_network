library(bnlearn)
data(learning.test)
fitted = bn.fit(hc(learning.test), learning.test)

bn.function <- function(network, evidence_data) {
  a <- NULL
  b <- nrow(evidence_data)
  for (i in 1:b) {
    evi <- paste("(", names(evidence_data), "=='",
               sapply(evidence_data[i,], as.character), "')",
               sep = "", collapse = " & ")
    a[i] <- cpquery(network, (C=='c'), eval(parse(text=evi)))
  }
  return(a)
}

test <- bn.function(fitted, learning.test)