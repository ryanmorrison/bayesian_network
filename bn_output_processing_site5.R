#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### Load model results ####
E_input <- read.table("data/s5_1000_to_2000.txt", header=FALSE)
E_output <- read.csv("output/site5/E_q_all_prob_mn.csv", header=TRUE)
CUFA_150_output <- read.csv("output/site5/CUFA_150_q_all_prob_mn.csv", header=TRUE)
CUFA_nomin_output <- read.csv("output/site5/CUFA_nomin_q_all_prob_mn.csv", header=TRUE)

#### Match the number of cells between existing conditions and each scenario ####
E_output_1 <- E_output[E_output[,1]%in%CUFA_150_output[,1],]
E_output_2 <- E_output[E_output[,1]%in%CUFA_nomin_output[,1],]

#### Calculate the mean probability difference between existing conditons and each scenario ####
CUFA_150_diff <- (CUFA_150_output - E_output_1)*100
mean(CUFA_150_diff[,2], na.rm=TRUE)
CUFA_nomin_diff <- (CUFA_nomin_output - E_output_2)*100
mean(CUFA_nomin_diff[,2], na.rm=TRUE)

#### Quick histogram plots of the probability differences ####
hist(CUFA_150_diff[,2])
hist(CUFA_nomin_diff[,2])

#### Load all the probabilities for each Q-bin ####
load("output/site5/E_q1_cell_probs.Rdata")
load("output/site5/E_q2_cell_probs.Rdata")
load("output/site5/E_q3_cell_probs.Rdata")
load("output/site5/E_q4_cell_probs.Rdata")
load("output/site5/E_q5_cell_probs.Rdata")
load("output/site5/E_q6_cell_probs.Rdata")

load("output/site5/CUFA_150_q1_cell_probs.Rdata")
load("output/site5/CUFA_150_q2_cell_probs.Rdata")
load("output/site5/CUFA_150_q3_cell_probs.Rdata")
load("output/site5/CUFA_150_q4_cell_probs.Rdata")
load("output/site5/CUFA_150_q5_cell_probs.Rdata")
load("output/site5/CUFA_150_q6_cell_probs.Rdata")

load("output/site5/CUFA_nomin_q1_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q2_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q3_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q4_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q5_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q6_cell_probs.Rdata")

load("output/site5/E_evidence1.Rdata")
load("output/site5/E_evidence2.Rdata")
load("output/site5/E_evidence3.Rdata")
load("output/site5/E_evidence4.Rdata")
load("output/site5/E_evidence5.Rdata")
load("output/site5/E_evidence6.Rdata")

load("output/site5/CUFA_150_evidence1.Rdata")
load("output/site5/CUFA_150_evidence2.Rdata")
load("output/site5/CUFA_150_evidence3.Rdata")
load("output/site5/CUFA_150_evidence4.Rdata")
load("output/site5/CUFA_150_evidence5.Rdata")
load("output/site5/CUFA_150_evidence6.Rdata")

load("output/site5/CUFA_nomin_evidence1.Rdata")
load("output/site5/CUFA_nomin_evidence2.Rdata")
load("output/site5/CUFA_nomin_evidence3.Rdata")
load("output/site5/CUFA_nomin_evidence4.Rdata")
load("output/site5/CUFA_nomin_evidence5.Rdata")
load("output/site5/CUFA_nomin_evidence6.Rdata")

#### Calculate the number of events (length) of each cell ####
# Existing conditions
E_all_probs <- c(E_q1_cell_probs, E_q2_cell_probs, E_q3_cell_probs, E_q4_cell_probs, E_q5_cell_probs, E_q6_cell_probs) # Combine probabilities for all cells
head(E_all_probs) # Examine cell probs
E_lengths <- as.data.frame(sapply(E_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
E_lengths <- data.frame(c(as.numeric(row.names(E_lengths))), E_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(E_lengths) <- c("cell", "length") # Name columns
head(E_lengths) # Preview the data table
nrow(E_lengths) # Count number of cells (rows)
E_mean <- mean(E_lengths[,2]) # Calculate the mean number of events for all cells
E_mean # View mean

# CUFA 150
CUFA_150_all_probs <- c(CUFA_150_q1_cell_probs, CUFA_150_q2_cell_probs, CUFA_150_q3_cell_probs, CUFA_150_q4_cell_probs, CUFA_150_q5_cell_probs, CUFA_150_q6_cell_probs) # Combine probabilities for all cells
head(CUFA_150_all_probs) # Examine cell probs
CUFA_150_lengths <- as.data.frame(sapply(CUFA_150_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
CUFA_150_lengths <- data.frame(c(as.numeric(row.names(CUFA_150_lengths))), CUFA_150_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(CUFA_150_lengths) <- c("cell", "length") # Name columns
head(CUFA_150_lengths) # Preview the data table
nrow(CUFA_150_lengths) # Count number of cells (rows)
CUFA_150_mean <- mean(CUFA_150_lengths[,2]) # Calculate the mean number of events for all cells
CUFA_150_mean # View mean

# CUFA no minimum
CUFA_nomin_all_probs <- c(CUFA_nomin_q1_cell_probs, CUFA_nomin_q2_cell_probs, CUFA_nomin_q3_cell_probs, CUFA_nomin_q4_cell_probs, CUFA_nomin_q5_cell_probs, CUFA_nomin_q6_cell_probs) # Combine probabilities for all cells
head(CUFA_nomin_all_probs) # Examine cell probs
CUFA_nomin_lengths <- as.data.frame(sapply(CUFA_nomin_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
CUFA_nomin_lengths <- data.frame(c(as.numeric(row.names(CUFA_nomin_lengths))), CUFA_nomin_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(CUFA_nomin_lengths) <- c("cell", "length") # Name columns
head(CUFA_nomin_lengths) # Preview the data table
nrow(CUFA_nomin_lengths) # Count number of cells (rows)
CUFA_nomin_mean <- mean(CUFA_nomin_lengths[,2]) # Calculate the mean number of events for all cells
CUFA_nomin_mean # View mean

#### Calculate the difference in events between existing conditions and the scenario for each cell ####

E_lengths_1 <- E_lengths[E_lengths[,1]%in%CUFA_150_lengths[,1],] # Match cells in existing conditions and CUFA 150
E_lengths_2 <- E_lengths[E_lengths[,1]%in%CUFA_nomin_lengths[,1],] # Match cells in existing conditions and CUFA no mininum

# CUFA 150
CUFA_150_pct_diff <- as.data.frame((E_lengths_1[,2]-CUFA_150_lengths[,2])/E_lengths_1[,2]*100)
CUFA_150_pct_diff <- data.frame((CUFA_150_lengths[,1]), CUFA_150_pct_diff[,1])
colnames(CUFA_150_pct_diff) <- c("cell","pct_decrease")
CUFA_150_pct_diff <- CUFA_150_pct_diff[order(CUFA_150_pct_diff$cell),]
head(CUFA_150_pct_diff)
mean(CUFA_150_pct_diff[,2])

# CUFA no minimum
CUFA_nomin_pct_diff <- as.data.frame((E_lengths_2[,2]-CUFA_nomin_lengths[,2])/E_lengths_2[,2]*100)
CUFA_nomin_pct_diff <- data.frame((CUFA_nomin_lengths[,1]), CUFA_nomin_pct_diff[,1])
colnames(CUFA_nomin_pct_diff) <- c("cell","pct_decrease")
CUFA_nomin_pct_diff <- CUFA_nomin_pct_diff[order(CUFA_nomin_pct_diff$cell),]
head(CUFA_nomin_pct_diff)
mean(CUFA_nomin_pct_diff[,2])

#### Define a utility function that accounts for a decrease in events for a particular cell ####
utility_ftn <- function(X) (100-X)/100

#### Apply the utility function to each scenario ####
# CUFA 150
CUFA_150_utility <- utility_ftn(CUFA_150_pct_diff[,2])
CUFA_150_utility <- cbind(CUFA_150_pct_diff[,1], CUFA_150_utility)
colnames(CUFA_150_utility) <- c("cell","pct_of_network_output")
head(CUFA_150_utility)

CUFA_nomin_utility <- utility_ftn(CUFA_nomin_pct_diff[,2])
CUFA_nomin_utility <- cbind(CUFA_nomin_pct_diff[,1], CUFA_nomin_utility)
colnames(CUFA_nomin_utility) <- c("cell","pct_of_network_output")
head(CUFA_nomin_utility)

CUFA_150_utility_probs <- CUFA_150_output[,2] * CUFA_150_utility[,2]
CUFA_150_utility_probs <- cbind(CUFA_150_pct_diff[,1], CUFA_150_utility_probs)
colnames(CUFA_150_utility_probs) <- c("cell","mean_prob")
head(CUFA_150_utility_probs)
head(CUFA_150_output)
mean(CUFA_150_utility_probs[,2], na.rm=TRUE)
mean(E_output[,2], na.rm=TRUE)

CUFA_nomin_utility_probs <- CUFA_nomin_output[,2] * CUFA_nomin_utility[,2]
CUFA_nomin_utility_probs <- cbind(CUFA_nomin_pct_diff[,1], CUFA_nomin_utility_probs)
colnames(CUFA_nomin_utility_probs) <- c("cell","mean_prob")
head(CUFA_nomin_utility_probs)
head(CUFA_nomin_output)
mean(CUFA_nomin_utility_probs[,2], na.rm=TRUE)
mean(E_output[,2], na.rm=TRUE)



