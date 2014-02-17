#### CUFA No Minimum ####

set.seed(140)

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### Source common functions ####
source('~/Dropbox/Gila Bayesian/bayesian_network/1-functions.R')


#### Read results from SRH-2D model ####
# Change input table depending on the site number

# Site 1
CUFA_nomin_alldata <- read.table("data/s1_1000_to_2000_trim2.txt", header=FALSE, skip=0)

# Site 2
# CUFA_nomin_alldata <- read.table("data/s2_1000_to_2000.txt", header=FALSE, skip=0)

# Site 3
# CUFA_nomin_alldata <- read.table("data/s3_1000_to_2000.txt", header=FALSE, skip=0)

# Site 4
# CUFA_nomin_alldata <- read.table("data/s4_1000_to_2000.txt", header=FALSE, skip=0)

# Site 5
# CUFA_nomin_alldata <- read.table("data/s5_1000_to_2000.txt", header=FALSE, skip=0)

colnames(CUFA_nomin_alldata) <- c("cell", "flood_cfs")

#### Separate grid data according to the Q-bins previously defined ####
CUFA_nomin_q1_cells <- subset(CUFA_nomin_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
CUFA_nomin_q2_cells <- subset(CUFA_nomin_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])
CUFA_nomin_q3_cells <- subset(CUFA_nomin_alldata, flood_cfs>=q_bin[3] & flood_cfs<q_bin[4])
CUFA_nomin_q4_cells <- subset(CUFA_nomin_alldata, flood_cfs>=q_bin[4] & flood_cfs<q_bin[5])
CUFA_nomin_q5_cells <- subset(CUFA_nomin_alldata, flood_cfs>=q_bin[5] & flood_cfs<q_bin[6])
CUFA_nomin_q6_cells <- subset(CUFA_nomin_alldata, flood_cfs>=q_bin[6])

#### Combine hydrology time series, timing states, and recession rate states ####
CUFA_nomin_hydro_states <- cbind(CUFA_nomin, timing_state_s2, recess_state_s2)
#### Trim data frame based on timing and recession rates ####
CUFA_nomin_hydro_states <- subset(CUFA_nomin_hydro_states, TIMING != "NA" & RECESSION != 1)
# CUFA_nomin_timing_subset <- CUFA_nomin_hydro_states[,14]
# CUFA_nomin_recess_subset <- CUFA_nomin_hydro_states[,15]

# Pull out cell #2
querycell <- subset(CUFA_nomin_q2_cells, CUFA_nomin_q2_cells$cell==2)

CUFA_nomin_hydro_subset <- subset(CUFA_nomin_hydro_states, CUFA_nomin_hydro_states[,2]>=querycell[,2] & CUFA_nomin_hydro_states[-1,2]<querycell[,2])






# Site 1
CUFA_150_alldata <- read.table("data/site1_1000_to_4000.txt", header=FALSE, skip=0)

# Site 2
# CUFA_nomin_alldata <- read.table("data/s2_1000_to_2000.txt", header=FALSE, skip=0)

# Site 3
# CUFA_nomin_alldata <- read.table("data/s3_1000_to_2000.txt", header=FALSE, skip=0)

# Site 4
# CUFA_nomin_alldata <- read.table("data/s4_1000_to_2000.txt", header=FALSE, skip=0)

# Site 5
# CUFA_nomin_alldata <- read.table("data/s5_1000_to_2000.txt", header=FALSE, skip=0)

colnames(CUFA_150_alldata) <- c("cell", "flood_cfs")

#### Separate grid data according to the Q-bins previously defined ####
CUFA_150_q1_cells <- subset(CUFA_150_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
CUFA_150_q2_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])
CUFA_150_q3_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[3] & flood_cfs<q_bin[4])
CUFA_150_q4_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[4] & flood_cfs<q_bin[5])
CUFA_150_q5_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[5] & flood_cfs<q_bin[6])
CUFA_150_q6_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[6] & flood_cfs<q_bin[7])
CUFA_150_q7_cells <- subset(CUFA_150_alldata, flood_cfs>=q_bin[7])

#### Combine hydrology time series, timing states, and recession rate states ####
CUFA_150_hydro_states <- cbind(CUFA_150, timing_state_s1, recess_state_s1)
#### Trim data frame based on timing and recession rates ####
CUFA_150_hydro_states <- subset(CUFA_150_hydro_states, TIMING != "NA" & RECESSION != 1)
# CUFA_150_timing_subset <- CUFA_150_hydro_states[,14]
# CUFA_150_recess_subset <- CUFA_150_hydro_states[,15]

# Pull out cell #2
querycell <- subset(CUFA_150_q3_cells, CUFA_150_q3_cells$cell==66)

CUFA_150_hydro_subset <- subset(CUFA_150_hydro_states, CUFA_150_hydro_states[,2]>=querycell[,2] & CUFA_150_hydro_states[-1,2]<querycell[,2])


#### Read results from SRH-2D model ####

set.seed(140)

# Change input table depending on the site number

# Site 1
E_alldata <- read.table("data/site1_1000_to_4000.txt", header=FALSE, skip=0)

# Site 2
# E_alldata <- read.table("data/s2_1000_to_2000.txt", header=FALSE, skip=0)

# Site 3
# E_alldata <- read.table("data/s3_1000_to_2000.txt", header=FALSE, skip=0)

# Site 4
# E_alldata <- read.table("data/s4_1000_to_2000.txt", header=FALSE, skip=0)

# Site 5
# E_alldata <- read.table("data/s5_1000_to_2000.txt", header=FALSE, skip=0)

colnames(E_alldata) <- c("cell", "flood_cfs")

#### Separate grid data according to the Q-bins previously defined ####
E_q1_cells <- subset(E_alldata, flood_cfs>q_bin[1] & flood_cfs<q_bin[2])
E_q2_cells <- subset(E_alldata, flood_cfs>=q_bin[2] & flood_cfs<q_bin[3])
E_q3_cells <- subset(E_alldata, flood_cfs>=q_bin[3] & flood_cfs<q_bin[4])
E_q4_cells <- subset(E_alldata, flood_cfs>=q_bin[4] & flood_cfs<q_bin[5])
E_q5_cells <- subset(E_alldata, flood_cfs>=q_bin[5] & flood_cfs<q_bin[6])
E_q6_cells <- subset(E_alldata, flood_cfs>=q_bin[6] & flood_cfs<q_bin[7])
E_q7_cells <- subset(E_alldata, flood_cfs>=q_bin[7])

#### Combine hydrology time series, timing states, and recession rate states ####
E_hydro_states <- cbind(hydro, timing_state, recess_state)
#### Trim data frame based on timing and recession rates ####
E_hydro_states <- subset(E_hydro_states, TIMING != "NA" & RECESSION != 1)
E_timing_subset <- E_hydro_states[,14]
E_recess_subset <- E_hydro_states[,15]

# Pull out cell #2
E_querycell <- subset(E_q3_cells, E_q3_cells$cell==66)

E_hydro_subset <- subset(E_hydro_states, E_hydro_states[,2]>=querycell[,2] & E_hydro_states[-1,2]<querycell[,2])

subset(E_hydro_states, date>"1972-09-01" & date<"1972-09-30")
subset(hydro, date=="1975-09-05")
subset(CUFA_nomin_hydro_states, date>"1975-09-01" & date<"1975-09-30")

as.Date(setdiff(E_hydro_subset$date, CUFA_nomin_hydro_subset$date))
