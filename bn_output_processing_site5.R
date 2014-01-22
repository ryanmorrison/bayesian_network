#### This script is used to compare model outputs of each scenario to existing conditions. Two approaches are used: ####
# 1) Not including cells that are no longer inundated into the analyses
# 2) Assigning cells that are no longer inundated a zero percent recruitment potential and including the cells in the analyses

#### Approach #1 ####

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

# Load libraries
library(sp)  # vector data
library(raster)  # raster data
library(rgdal)  # input/output, projections
library(graphics)

#### Load model results ####
E_input <- read.table("data/site5_1000_to_4000.txt", header=FALSE)
E_output <- read.csv("output/site5/E_q_all_prob_mn.csv", header=TRUE)
CUFA_150_output <- read.csv("output/site5/CUFA_150_q_all_prob_mn.csv", header=TRUE)
CUFA_nomin_output <- read.csv("output/site5/CUFA_nomin_q_all_prob_mn.csv", header=TRUE)
nrow(E_output)
nrow(CUFA_150_output)
nrow(CUFA_nomin_output)

#### Match the number of cells between existing conditions and each scenario ####
E_output_1 <- E_output[E_output[,1]%in%CUFA_150_output[,1],]
E_output_2 <- E_output[E_output[,1]%in%CUFA_nomin_output[,1],]

#### Calculate the mean probability difference between existing conditons and each scenario ####
# Disregarding cells that are no longer inundated
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
load("output/site5/E_q7_cell_probs.Rdata")

load("output/site5/CUFA_150_q1_cell_probs.Rdata")
load("output/site5/CUFA_150_q2_cell_probs.Rdata")
load("output/site5/CUFA_150_q3_cell_probs.Rdata")
load("output/site5/CUFA_150_q4_cell_probs.Rdata")
load("output/site5/CUFA_150_q5_cell_probs.Rdata")
load("output/site5/CUFA_150_q6_cell_probs.Rdata")
load("output/site5/CUFA_150_q7_cell_probs.Rdata")

load("output/site5/CUFA_nomin_q1_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q2_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q3_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q4_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q5_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q6_cell_probs.Rdata")
load("output/site5/CUFA_nomin_q7_cell_probs.Rdata")

load("output/site5/E_evidence1.Rdata")
load("output/site5/E_evidence2.Rdata")
load("output/site5/E_evidence3.Rdata")
load("output/site5/E_evidence4.Rdata")
load("output/site5/E_evidence5.Rdata")
load("output/site5/E_evidence6.Rdata")
load("output/site5/E_evidence7.Rdata")

load("output/site5/CUFA_150_evidence1.Rdata")
load("output/site5/CUFA_150_evidence2.Rdata")
load("output/site5/CUFA_150_evidence3.Rdata")
load("output/site5/CUFA_150_evidence4.Rdata")
load("output/site5/CUFA_150_evidence5.Rdata")
load("output/site5/CUFA_150_evidence6.Rdata")
load("output/site5/CUFA_150_evidence7.Rdata")

load("output/site5/CUFA_nomin_evidence1.Rdata")
load("output/site5/CUFA_nomin_evidence2.Rdata")
load("output/site5/CUFA_nomin_evidence3.Rdata")
load("output/site5/CUFA_nomin_evidence4.Rdata")
load("output/site5/CUFA_nomin_evidence5.Rdata")
load("output/site5/CUFA_nomin_evidence6.Rdata")
load("output/site5/CUFA_nomin_evidence7.Rdata")

#### Calculate the number of events (length) of each cell ####
# Existing conditions
E_all_probs <- c(E_q1_cell_probs, E_q2_cell_probs, E_q3_cell_probs, E_q4_cell_probs, E_q5_cell_probs, E_q6_cell_probs, E_q7_cell_probs) # Combine probabilities for all cells
head(E_all_probs) # Examine cell probs
E_lengths <- as.data.frame(sapply(E_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
E_lengths <- data.frame(c(as.numeric(row.names(E_lengths))), E_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(E_lengths) <- c("cell", "length") # Name columns
head(E_lengths) # Preview the data table
nrow(E_lengths) # Count number of cells (rows)
E_mean <- mean(E_lengths[,2]) # Calculate the mean number of events for all cells
E_mean # View mean

# CUFA 150
CUFA_150_all_probs <- c(CUFA_150_q1_cell_probs, CUFA_150_q2_cell_probs, CUFA_150_q3_cell_probs, CUFA_150_q4_cell_probs, CUFA_150_q5_cell_probs, CUFA_150_q6_cell_probs, CUFA_150_q7_cell_probs) # Combine probabilities for all cells
head(CUFA_150_all_probs) # Examine cell probs
CUFA_150_lengths <- as.data.frame(sapply(CUFA_150_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
CUFA_150_lengths <- data.frame(c(as.numeric(row.names(CUFA_150_lengths))), CUFA_150_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(CUFA_150_lengths) <- c("cell", "length") # Name columns
head(CUFA_150_lengths) # Preview the data table
nrow(CUFA_150_lengths) # Count number of cells (rows)
CUFA_150_mean <- mean(CUFA_150_lengths[,2]) # Calculate the mean number of events for all cells
CUFA_150_mean # View mean

# CUFA no minimum
CUFA_nomin_all_probs <- c(CUFA_nomin_q1_cell_probs, CUFA_nomin_q2_cell_probs, CUFA_nomin_q3_cell_probs, CUFA_nomin_q4_cell_probs, CUFA_nomin_q5_cell_probs, CUFA_nomin_q6_cell_probs, CUFA_nomin_q7_cell_probs) # Combine probabilities for all cells
head(CUFA_nomin_all_probs) # Examine cell probs
CUFA_nomin_lengths <- as.data.frame(sapply(CUFA_nomin_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
CUFA_nomin_lengths <- data.frame(c(as.numeric(row.names(CUFA_nomin_lengths))), CUFA_nomin_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
colnames(CUFA_nomin_lengths) <- c("cell", "length") # Name columns
head(CUFA_nomin_lengths) # Preview the data table
nrow(CUFA_nomin_lengths) # Count number of cells (rows)
CUFA_nomin_mean <- mean(CUFA_nomin_lengths[,2]) # Calculate the mean number of events for all cells
CUFA_nomin_mean # View mean

#### Calculate the difference in events between existing conditions and the scenario for each cell ####

E_lengths_1 <- E_lengths[E_lengths[ ,1] %in% CUFA_150_lengths[ ,1], ] # Match cells in existing conditions and CUFA 150
E_lengths_2 <- E_lengths[E_lengths[ ,1] %in% CUFA_nomin_lengths[ ,1], ] # Match cells in existing conditions and CUFA no mininum

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
mean(E_output[,2])
mean(CUFA_150_utility_probs[,2], na.rm=TRUE) - mean(E_output[,2])

CUFA_nomin_utility_probs <- CUFA_nomin_output[,2] * CUFA_nomin_utility[,2]
CUFA_nomin_utility_probs <- cbind(CUFA_nomin_pct_diff[,1], CUFA_nomin_utility_probs)
colnames(CUFA_nomin_utility_probs) <- c("cell","mean_prob")
head(CUFA_nomin_utility_probs)
head(CUFA_nomin_output)
mean(CUFA_nomin_utility_probs[,2], na.rm=TRUE)
mean(E_output[,2])
mean(CUFA_nomin_utility_probs[,2], na.rm=TRUE) - mean(E_output[,2])

#### Prepare data for GIS mapping ####
CUFA_150_prob_diffs <- data.frame(cell = E_output[ ,1], mean_prob_diff = CUFA_150_utility_probs[ ,2] - E_output[ ,2])
hist(CUFA_150_prob_diffs[ ,2])

CUFA_nomin_prob_diffs <- data.frame(cell = E_output[ ,1], mean_prob_diff = CUFA_nomin_utility_probs[ ,2] - E_output[ ,2])
hist(CUFA_nomin_prob_diffs[ ,2])

# Read x,y coordinate for the site
site5_xy <- read.table("data/coordinates/site5_coords_1000_to_4000.txt", header=TRUE)

# Trim x,y coordinate data so only cells with model outputs are included
site5_xy_trim <- site5_xy[site5_xy[ ,3] %in% E_output[ ,1],]

# Combine the coordinate and results data
CUFA_150_gis <- data.frame(site5_xy_trim, 
                           existing_prob = E_output[, 2], 
                           CUFA_150_prob = CUFA_150_utility_probs[, 2], 
                           prob_diff = CUFA_150_prob_diffs[ ,2], 
                           e_events = E_lengths[ ,2], 
                           CUFA_150_events = CUFA_150_lengths[ ,2]
)
head(CUFA_150_gis)

# Create a spatial data frame by assigning spatial coordinates to the x,y columns
coordinates(CUFA_150_gis) <- c("x", "y")

# X,Y coordinates are in New Mexico West state plane NAD83 (EPSG code 2904). This needs to be defined in the new object.
CUFA_150_gis@proj4string # Verify that no projection is currently specified
NMWSP <- CRS("+init=epsg:2904") # NMWSP stands for New Mexico West state plane
proj4string(CUFA_150_gis) <- NMWSP # Assign the proj4string slot the correct projection
CUFA_150_gis@proj4string # Verify that projection has been specified

# Export data as shapefile
writeOGR(CUFA_150_gis, "/Users/Morrison/Documents/GIS/Projects/gila bn/shapefiles/site5", "CUFA_150_gis", driver="ESRI Shapefile", overwrite_layer=TRUE)




#### Combine all scenarios into single data frame for analysis ####
# Combine model ouput
E_site5 <- data.frame(combine.probs.E(), scenario = "existing")
CUFA_150_site5 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
CUFA_nomin_site5 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
combined_site5 <- data.frame(rbind(E_site5, CUFA_150_site5, CUFA_nomin_site5), site = "site5")

# Calculate and combine evidence in each cell
E_lengths_site5 <- combine.lengths.E()
CUFA_150_lengths_site5 <- combine.lengths.CUFA.150()
CUFA_nomin_lengths_site5 <- combine.lengths.CUFA.nomin()

# Calculate difference in events
E_lengthsdiff_site5 <- (E_lengths_site5[ ,1] - E_lengths_site5[ ,1])/E_lengths_site5[ ,1] * 100
CUFA_150_lengthsdiff_site5 <- (E_lengths_site5[ ,1] - CUFA_150_lengths_site5[ ,1])/E_lengths_site5[ ,1] * 100
CUFA_nomin_lengthsdiff_site5 <- (E_lengths_site5[ ,1] - CUFA_nomin_lengths_site5[ ,1])/E_lengths_site5[ ,1] * 100

# Apply utility function to calculate percent decrease in probabilities
E_utility_site5 <- utility_ftn(E_lengthsdiff_site5)
CUFA_150_utility_site5 <- utility_ftn(CUFA_150_lengthsdiff_site5)
CUFA_nomin_utility_site5 <- utility_ftn(CUFA_nomin_lengthsdiff_site5)

# Apply utility function decreases to probabilities
E_util_site5 <- E_site5[ ,1] * E_utility_site5
CUFA_150_util_site5 <- CUFA_150_site5[ ,1] * CUFA_150_utility_site5
CUFA_nomin_util_site5 <- CUFA_nomin_site5[ ,1] * CUFA_nomin_utility_site5

# Make one large data frame with all the data
E_site5 <- data.frame(prob = E_site5$prob, util_prob = E_util_site5, evidence = E_lengths_site5$evidence, Q_bin = E_site5$Q_bin, scenario = E_site5$scenario)
CUFA_150_site5 <- data.frame(prob = CUFA_150_site5$prob, util_prob = CUFA_150_util_site5, evidence = CUFA_150_lengths_site5$evidence, Q_bin = CUFA_150_site5$Q_bin, scenario = CUFA_150_site5$scenario)
CUFA_nomin_site5 <- data.frame(prob = CUFA_nomin_site5$prob, util_prob = CUFA_nomin_util_site5, evidence = CUFA_nomin_lengths_site5$evidence, Q_bin = CUFA_nomin_site5$Q_bin, scenario = CUFA_nomin_site5$scenario)
combined_site5 <- data.frame(rbind(E_site5, CUFA_150_site5, CUFA_nomin_site5), site = "site5")

# Export as .Rdata
save(combined_site5, file="output/combined_site5.Rdata")






# #### Approach #2 ####
# 
# #### Account for cells that are not inundated and have zero probability ####
# CUFA_150_diffcells <- setdiff(E_output[,1], CUFA_150_output[,1]) # Finds cell numbers in existing that aren't in CUFA 150
# CUFA_nomin_diffcells <- setdiff(E_output[,1], CUFA_nomin_output[,1]) # Finds cell numbers in existing that aren't in CUFA no minimum
# 
# #### Assign zero probabilities to cell numbers ####
# CUFA_150_zeroprobs <- data.frame(CUFA_150_diffcells, rep(0, times=length(CUFA_150_diffcells)))
# CUFA_nomin_zeroprobs <- data.frame(CUFA_nomin_diffcells, rep(0, times=length(CUFA_nomin_diffcells)))
# colnames(CUFA_150_zeroprobs) <- c("cell", "mean_prob")
# colnames(CUFA_nomin_zeroprobs) <- c("cell", "mean_prob")
# 
# #### Combine model output to cells with zero probability ####
# CUFA_150_output_supp <- rbind(CUFA_150_output, CUFA_150_zeroprobs)
# CUFA_nomin_output_supp <- rbind(CUFA_nomin_output, CUFA_nomin_zeroprobs)
# 
# #### Sort by cell number ####
# CUFA_150_output_supp <- CUFA_150_output_supp[order(CUFA_150_output_supp$cell),]
# CUFA_nomin_output_supp <- CUFA_nomin_output_supp[order(CUFA_nomin_output_supp$cell),]
# 
# #### Assign zero lengths to cells ####
# CUFA_150_zerolengths <- data.frame(CUFA_150_diffcells, rep(0, times=length(CUFA_150_diffcells)))
# CUFA_nomin_zerolengths <- data.frame(CUFA_nomin_diffcells, rep(0, times=length(CUFA_nomin_diffcells)))
# 
# colnames(CUFA_150_zerolengths) <- c("cell", "length")
# colnames(CUFA_nomin_zerolengths) <- c("cell", "length")
# 
# #### Calculate the mean probability difference between existing conditons and each scenario ####
# CUFA_150_diff <- (CUFA_150_output_supp - E_output)*100
# mean(CUFA_150_diff[,2], na.rm=TRUE)
# CUFA_nomin_diff <- (CUFA_nomin_output_supp - E_output)*100
# mean(CUFA_nomin_diff[,2], na.rm=TRUE)
# 
# #### Quick histogram plots of the probability differences ####
# hist(CUFA_150_diff[,2])
# hist(CUFA_nomin_diff[,2])
# 
# #### Calculate the number of events (length) of each cell ####
# # CUFA 150
# CUFA_150_all_probs <- c(CUFA_150_q1_cell_probs, CUFA_150_q2_cell_probs, CUFA_150_q3_cell_probs, CUFA_150_q4_cell_probs, CUFA_150_q5_cell_probs, CUFA_150_q6_cell_probs) # Combine probabilities for all cells
# head(CUFA_150_all_probs) # Examine cell probs
# CUFA_150_lengths <- as.data.frame(sapply(CUFA_150_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
# CUFA_150_lengths <- data.frame(c(as.numeric(row.names(CUFA_150_lengths))), CUFA_150_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
# colnames(CUFA_150_lengths) <- c("cell", "length") # Name columns
# CUFA_150_lengths <- rbind(CUFA_150_lengths, CUFA_150_zerolengths) # Add cells to data frame that have zero length (no events)
# head(CUFA_150_lengths) # Preview the data table
# nrow(CUFA_150_lengths) # Count number of cells (rows)
# CUFA_150_mean <- mean(CUFA_150_lengths[,2]) # Calculate the mean number of events for all cells
# CUFA_150_mean # View mean
# 
# # CUFA no minimum
# CUFA_nomin_all_probs <- c(CUFA_nomin_q1_cell_probs, CUFA_nomin_q2_cell_probs, CUFA_nomin_q3_cell_probs, CUFA_nomin_q4_cell_probs, CUFA_nomin_q5_cell_probs, CUFA_nomin_q6_cell_probs) # Combine probabilities for all cells
# head(CUFA_nomin_all_probs) # Examine cell probs
# CUFA_nomin_lengths <- as.data.frame(sapply(CUFA_nomin_all_probs, length)) # Calculate the number of events in each cell and convert to data frame
# CUFA_nomin_lengths <- data.frame(c(as.numeric(row.names(CUFA_nomin_lengths))), CUFA_nomin_lengths[,1]) # Create new data frame with cell number in first column and number of events in second colume
# colnames(CUFA_nomin_lengths) <- c("cell", "length") # Name columns
# CUFA_nomin_lengths <- rbind(CUFA_nomin_lengths, CUFA_nomin_zerolengths) # Add cells to data frame that have zero length (no events)
# head(CUFA_nomin_lengths) # Preview the data table
# nrow(CUFA_nomin_lengths) # Count number of cells (rows)
# CUFA_nomin_mean <- mean(CUFA_nomin_lengths[,2]) # Calculate the mean number of events for all cells
# CUFA_nomin_mean # View mean
# 
# 
# #### Calculate the difference in events between existing conditions and the scenario for each cell ####
# # Sort data frames according to cell number
# E_lengths <- E_lengths[order(E_lengths$cell),]
# CUFA_150_lengths <- CUFA_150_lengths[order(CUFA_150_lengths$cell),]
# CUFA_nomin_lengths <- CUFA_nomin_lengths[order(CUFA_nomin_lengths$cell),]
# 
# # CUFA 150
# CUFA_150_pct_diff <- as.data.frame((E_lengths[,2]-CUFA_150_lengths[,2])/E_lengths[,2]*100)
# CUFA_150_pct_diff <- data.frame((CUFA_150_lengths[,1]), CUFA_150_pct_diff[,1])
# colnames(CUFA_150_pct_diff) <- c("cell","pct_decrease")
# head(CUFA_150_pct_diff)
# mean(CUFA_150_pct_diff[,2])
# 
# # CUFA no minimum
# CUFA_nomin_pct_diff <- as.data.frame((E_lengths[,2]-CUFA_nomin_lengths[,2])/E_lengths[,2]*100)
# CUFA_nomin_pct_diff <- data.frame((CUFA_nomin_lengths[,1]), CUFA_nomin_pct_diff[,1])
# colnames(CUFA_nomin_pct_diff) <- c("cell","pct_decrease")
# head(CUFA_nomin_pct_diff)
# mean(CUFA_nomin_pct_diff[,2])
# 
# #### Apply the utility function to each scenario ####
# # CUFA 150
# CUFA_150_utility <- utility_ftn(CUFA_150_pct_diff[,2])
# CUFA_150_utility <- cbind(CUFA_150_pct_diff[,1], CUFA_150_utility)
# colnames(CUFA_150_utility) <- c("cell","pct_of_network_output")
# head(CUFA_150_utility)
# 
# CUFA_nomin_utility <- utility_ftn(CUFA_nomin_pct_diff[,2])
# CUFA_nomin_utility <- cbind(CUFA_nomin_pct_diff[,1], CUFA_nomin_utility)
# colnames(CUFA_nomin_utility) <- c("cell","pct_of_network_output")
# head(CUFA_nomin_utility)
# 
# CUFA_150_utility_probs_supp <- CUFA_150_output_supp[,2] * CUFA_150_utility[,2]
# CUFA_150_utility_probs_supp <- cbind(CUFA_150_pct_diff[,1], CUFA_150_utility_probs_supp)
# colnames(CUFA_150_utility_probs_supp) <- c("cell","mean_prob")
# head(CUFA_150_utility_probs_supp)
# head(CUFA_150_output_supp)
# mean(CUFA_150_utility_probs_supp[,2], na.rm=TRUE)
# mean(E_output[,2])
# 
# CUFA_nomin_utility_probs_supp <- CUFA_nomin_output_supp[,2] * CUFA_nomin_utility[,2]
# CUFA_nomin_utility_probs_supp <- cbind(CUFA_nomin_pct_diff[,1], CUFA_nomin_utility_probs_supp)
# colnames(CUFA_nomin_utility_probs_supp) <- c("cell","mean_prob")
# head(CUFA_nomin_utility_probs_supp)
# head(CUFA_nomin_output_supp)
# mean(CUFA_nomin_utility_probs_supp[,2], na.rm=TRUE)
# mean(E_output[,2])
# 
# #### Calculate the relative difference between existing and scenarios on a cell-by-cell basis####
# CUFA_150_cell_diff <- (CUFA_150_utility_probs_supp[,2] - E_output[,2])/E_output[,2]*100
# CUFA_150_cell_diff <- cbind(CUFA_150_utility_probs_supp[,1], CUFA_150_cell_diff)
# colnames(CUFA_150_cell_diff) <- c("cell","relative_diff")
# head(CUFA_150_cell_diff)
# 
# CUFA_nomin_cell_diff <- (CUFA_nomin_utility_probs_supp[,2] - E_output[,2])/E_output[,2]*100
# CUFA_nomin_cell_diff <- cbind(CUFA_nomin_utility_probs_supp[,1], CUFA_nomin_cell_diff)
# colnames(CUFA_nomin_cell_diff) <- c("cell","relative_diff")
# head(CUFA_nomin_cell_diff)
# 
# E_evidence2[["2"]]
# CUFA_150_evidence2[["2"]]
# CUFA_nomin_evidence2[["2"]]
# 
# E_evidence4[["15"]]
# CUFA_150_evidence4[["15"]]
# CUFA_nomin_evidence4[["15"]]
