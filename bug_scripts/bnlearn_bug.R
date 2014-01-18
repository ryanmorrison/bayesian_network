#### Create network based on cquery documentation example
data(learning.test)
fitted = bn.fit(hc(learning.test), learning.test)

#### Function to create list of evidence. The function requires the data set of evidence.
evidence.function <- function(evidence_data) {
  a <- list()
  b <- nrow(evidence_data)
  for (i in 1:b) {
    a[i] <- paste("(", names(evidence_data), "=='",
                  sapply(evidence_data[i,], as.character), "')",
                  sep = "", collapse = " & ")
  }
  return(a)
}

#### Function to computes conditional probabilities using the cpquery function. A network and the list of evidence created with the "evidence.function" are required inputs.
cp.function <- function(network, evidence_list) {
  b <- length(evidence_list)
  a <- vector(length=b)
  for (i in 1:b) {
    a[i] <- cpquery(network, (C == "c"), eval(parse(text = evidence_list[[i]])), method="lw")
  }
  return(a)
}

#### Test the functions

# Create list of evidence
test1 <- evidence.function(learning.test)
# Trim evidence to first 25 entries for brevity
test1 <- test1[1:25]
# Compute probabililities
test2 <- cp.function(fitted, test1)
# The following error is produced: 
# Error in parse(text = evidence_list[[i]]) : 
# object 'evidence_list' not found

#### However, when these computation are performed outside of the user-defined functions, it appears the correct results are produced
v <- list()
for (j in 1:nrow(learning.test)) { v[j] <- paste("(", names(learning.test), "=='", sapply(learning.test[j,], as.character), "')", sep="", collapse=" & ")}
v <- v[1:25]
q <- vector()
for (k in 1:length(vi)) {q[k] <- cpquery(fitted, (C == "c"), eval(parse(text=v[k])))}

# Any feedback you could provide would be great!
# Thanks,
# Ryan Morrison, Unversity of New Mexico