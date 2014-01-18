#### Process USGS Hydrology Data ####
# This function processes USGS gage data saved from the USGS website as a text file. The first 28 lines of the file are skipped (includes description of the data), and the data is processed so only a data frame with the date and discharge (cfs) is returned.
usgs.process <- function(rawdata) {
  a <- read.table(rawdata,
                  skip=28, #Skip 3,255 lines to start at 10/1/1936 to match scenario time series; otherwise, skip 28 lines
                  colClasses=c("character", "numeric", "factor", "numeric", "character"),
                  header=FALSE)
  a$V3 <- as.Date(a$V3, format="%Y-%m-%d")
  a <- a[,3:4]
  colnames(a) <- c("date", "cfs")
  return(a)
}

#### Process Scenario Flow Data ####
# This function processes scenario data provided by TNC. Before using this function the data must be in csv format, and the date column must be in the format 10/02/1999 The first 2 lines of the file are skipped, and the data is processed so only a data frame date and discharge (cfs) is returned. The user must be familiar with the scenario csv file so he can select the appropriate column to process (separate columns for each scenario).
scenario.process <- function(rawdata, scenario_column) {
  a <- read.csv(rawdata,
                skip=2,
                colClasses=c("factor", "numeric", "numeric", "numeric"),
                header=FALSE)
  a$V1 <- as.Date(a$V1, format="%m/%d/%Y")
  a1 <- a[,1]
  a2 <- a[,scenario_column]
  a <- data.frame(a1, a2)
  colnames(a) <- c("date", "cfs")
  return(a)
}

#### Add Stage to Hydrology Data ####
# This function adds 2 columns to the hydrology data frame. The first is the stage in feet. This column is calculated using a user-define regression equation for stage based on discharge. The second column is the stage converted to centimeters.
stage <- function(df, eq) {
  a <- apply(df[2], 2, eq)
  b <- a*12*2.54
  c <- cbind(a, b)
  colnames(c) <- c("ft", "cm")
  return(c)
}

#### Calculate Recession Rates ####
# This function requires a data frame (df) with hydrograph stage values (df$cm) and the user-defined number of days to calculate recesssion rates. The function assumes recession rates are linear between the number of days specified. If the stage values are in cm, the returned data frame has units of cm/day.
recession.rate <- function(df, days) {
  r <- nrow(df)
  a <- NULL
  for (i in 1:r) {
    if (i <= days) {
      a[i] <- NA
    }
    else
    a[i] <- (df$cm[i-days]-df$cm[i])/days
  }
  a <- as.data.frame(a)
  colnames(a) <- paste("recess_", days, "d", sep="")
  return(a)
}

# This function requires a data frame (df) with hydrograph stage values (df$cm) and the user-defined number of days to calculate recesssion rates. The function assumes recession rates are linear between the number of days specified, and calculates the forward recession rate. If the stage values are in cm, the returned data frame has units of cm/day.
recession.rate.forward <- function(df, days) {
  r <- nrow(df)
  a <- NULL
  for (i in 1:r) {
    if (i >= r-days) {
      a[i] <- NA
    }
    else
      a[i] <- (df$cm[i+days]-df$cm[i])/days*-1
  }
  a <- as.data.frame(a)
  colnames(a) <- paste("recess_", days, "d", sep="")
  return(a)
}


#### Split Hydrology Data ####
# This function reads in a data frame that contains columns for date, discharge, and stage, and splits the data according to discrete time frames. The time frames represent April-May, June-July, and August-September. These are important discrete states that are used in the Bayesian Network.
subset.month <- function (data) {
  n <- ncol(data)
  a <- as.xts(data[,2:n], order.by=data[,1], header=TRUE)
  apr <- a[.indexmon(a)==(3)]
  may <- a[.indexmon(a)==(4)]
  jun <- a[.indexmon(a)==(5)]
  jul <- a[.indexmon(a)==(6)]
  aug <- a[.indexmon(a)==(7)]
  sep <- a[.indexmon(a)==(8)]
  apr_may <- rbind(apr, may)
  jun_jul <- rbind(jun, jul)
  aug_sep <- rbind(aug, sep)
  return(list(apr_may, jun_jul, aug_sep))
}

#### Convert list element into a data frame ####
list.to.df <- function(list_element) {
  a <- as.data.frame(list_element)
  b <- data.frame(date=rownames(a), a)
  rownames(b) <- index(b)
  return(b)
}

#### Calculate inundation probabilities ####
# This function calculates the frequency of occurence for a specific flow rate. The required inputs are a hydrologic data frame and a vector of flow rates that represent flow "bins". Results from the function indicate how frequently a specified discharge creates inundation conditions.
q.prob <- function(df, bin) {
  a <- NULL
  s <- probspace(df)
  for (i in 1:(length(bin)+1)) {
    if (i==1) {
      a[i] <- prob(s, cfs<bin[i])
    }
    else {
      a[i] <- prob(s, cfs>=bin[i-1])
    }
  }
  a <- as.data.frame(a)
  colnames(a) <- "YES"
  a$NO <- 1 - a$YES
  rownames(a) <- paste("Q_bin_", 0:(length(bin)), sep="")
  return(a)
}

#### Calculate recession rate probabilities ####
# This function calculates the frequency of recession rates for specified discrete states. The required inputs are a hydrologic data frame, the column of the data frame that contains calculated recession rates, and a vector of recession rate values that segregate discrete states. Results from the function indicate how frequently the recession rate occurs within a specified discrete state.
r.prob <- function(df, col, bin) {
  a <- NULL
  s <- probspace(df)
  for (i in 1:(length(bin)+1)) {
    if (i==1) {
      a[i] <- prob(s, s[[col]]<bin[i])
    }
    else if (i==(length(bin)+1)) {
      a[i] <- prob(s, s[[col]]>=bin[i-1])
    }
    else {
      a[i] <- prob(s, s[[col]]>=bin[i-1] & s[[col]]<bin[i])
    }
  }
  a <- as.data.frame(a)
  colnames(a) <- "CP"
  return(a)
}

q.states <- function(data, bin, bin_column) {
  b <- nrow(data)
  a <- NULL
  for (i in 1:b) {
    if (data[i,2] >= bin[bin_column]) a[i] <- "Y"
    else a[i] <- "N"
  }
  a <- as.data.frame(as.factor(a))
  colnames(a) <- "FLOOD"
  return(a)
}

timing.states <- function(data) {
  b <- nrow(data)
  a <- NULL
  c <- NULL
  for (i in 1:b) a[i] <- format(data[i,1], "%m")
  for (i in 1:b) {
  if (a[i] == "04" | a[i] == "05") c[i] <- 1
  else if (a[i] == "06" | a[i] == "07") c[i] <- 2
  else if (a[i] == "08" | a[i] == "09") c[i] <- 3
  else c[i] <- NA
  }
  c <- as.data.frame(as.factor(c))
  colnames(c) <- "TIMING"
  return(c)
}

recess.states <- function(data, bin, column) {
  b <- nrow(data)
  a <- NULL
  for (i in 1:b) {
    if (is.na(data[i,column])==TRUE) a[i] <- NA
    else if (data[i,column] < bin[1]) a[i] <- 1
    else if (data[i,column] > bin[1] & data[i,column] < bin[2]) a[i] <- 2
    else if (data[i,column] > bin[2] & data[i,column] < bin[3]) a[i] <- 3
    else if (data[i,column] > bin[3] & data[i,column] < bin[4]) a[i] <- 4
    else a[i] <- 5
  }
  a <- as.data.frame(as.factor(a))
  colnames(a) <- "RECESSION"
  return(a)
}

#### Populate empty matrices with evidence in the cell-by-cell analysis ####
populate.evidence <- function(q_cells, emptymatrix) {
  a <- vector("list", nrow(q_cells))
  names(a) <- (q_cells[,1])
  for (i in 1:nrow(q_cells)) {
    a[[i]] <- data.frame(matrix(emptymatrix[,c(1:2,i+2)][emptymatrix[,i+2]==1], ncol=3))
    a[[i]] <- lapply(a[[i]], factor)
#     a[[i]] <- lapply(a[[i]], rep, 3)
    names(a[[i]]) <- c("TIMING", "RECESSION", "FLOOD")
    w <- a[[i]][1]
    x <- a[[i]][2]
    v <- a[[i]][3]
    a[[i]] <- data.frame(w,x,v)
  }
    a <- Filter(function(x) dim(x)[1] > 0, a)
  return(a)
}

normalize <- function(states, computed_probs) {
  a <- vector()
  c <- nrow(states)
  for (i in 1:length(computed_probs)) {
    a[i] <- length(computed_probs[[i]])
  }
  a <- as.data.frame(a)
  b <- a/c
  return(b)
}

normalize2 <- function(existing_state, scenario_state, existing_probs, scenario_probs) {
  cell_diff <- vector()
  b <- nrow(existing_state)
  c <- nrow(scenario_state)
  total_diff <- b-c
  for (i in 1:length(existing_probs)) {
    cell_diff[i] <- length(existing_probs[[i]]) - length(scenario_probs[[i]])
  }
  cell_diff <- as.data.frame(cell_diff)
  fraction_diff <- 1-cell_diff/total_diff
  return(fraction_diff)
}

# This function correct round off errors so that all CTPs equal 1.
cpt.roundoff <- function(cpt_table) {
  c <- ncol(cpt_table)
  r <- nrow(cpt_table)
  for (i in 1:c) {
    if (sum(cpt_table[,i]!=1)) {
      cpt_table[r,i] <- cpt_table[r,i] + (1-sum(cpt_table[,i]))
    }
  }
  return (cpt_table)
}


# test.function.p3 <- function(network, evidence_table) {
#   b <- nrow(evidence_table)
#   a <- vector(length=b)
#   for (i in 1:b) a[i] <- cpquery(network, event = (POTENTIAL == "H"), evidence = as.list(evidence_table[i,]))
#   return(a)
# }

#### Old functions ####

# q.count <- function(df) {
#   a <- c(q1=count(df$cfs>=q_bin[1]),
#          q2=count(df$cfs>=q_bin[2]),
#          q3=count(df$cfs>=q_bin[3]),
#          q4=count(df$cfs>=q_bin[4]),
#          q5=count(df$cfs>=q_bin[5]),
#          q6=count(df$cfs>=q_bin[6]),
#          q7=count(df$cfs>=q_bin[7]),
#          q8=count(df$cfs>=q_bin[8]),
#          q9=count(df$cfs>=q_bin[9])
#          )
#   return(a)
# }

# r.prob.single <- function(df, variable, state1, state2, bin_number) {
#   w <- probspace(df)
#   x <- w[w[[variable]]>state1 & w[[variable]]<=state2, ]
#   y <- w[w[["cfs"]]>=q_bin[bin_number], ]
#   z <- prob(x, given=y)
#   return(z)
# }

# r.prob.vector <- function(df, recess_col, states, bin_variable) {
#   a <- data.frame()
#   w <- probspace(df)
#   for(j in 1:(length(states)-1)) {
#     x <- w[w[[recess_col]]<=states[j+1] & w[[recess_col]]>states[j], ]
#     for(i in 1:length(bin_variable)) {
#       y <- w[w[["cfs"]]>=bin_variable[i], ]
#       z <- prob(x, given=y)
#       a[i,j] <- z
#       colnames(a)[j] <- paste(states[j], "_", states[j+1], "_cm/d", sep="")
#     }
#   }
#   n <- length(bin_variable)
#   rownames(a) <- paste("q_bin_", 1:n, sep="")
#   return(a)
# }

# test.function <- function(network, state_table) {
#   a <- list()
#   b <- nrow(state_table)
#   des <- NULL
#   for (i in 1:b) {
#     des <- paste("(", names(state_table), "=='",
#                  sapply(state_table[i,], as.character), "')",
#                  sep = "", collapse = " & ")
#     a[i] <- cpquery(network, (POTENTIAL=="H"), eval(parse(text = des)))
#   }
#   return(a)
# }

# test.function.p1 <- function(state_table) {
#   a <- list()
#   b <- nrow(state_table)
#   for (i in 1:b) {
#     a[i] <- paste("(", names(state_table), "=='",
#                  sapply(state_table[i,], as.character), "')",
#                  sep = "", collapse = " & ")
#   }
# #   for (k in 1:b) {
# #     c[k] <- cpquery(network, (POTENTIAL=="H"), eval(parse(text = a[[k]])))
# #   }
#   return(a)
# }

# test.function.p2 <- function(network, evidence_list) {
#   b <- length(evidence_list)
#   a <- vector(length=b)
#   for (i in 1:b) {
# #       a[i] <- cpquery(network, (POTENTIAL == "H"), eval(parse(text = evidence_list[[i]])))
#     a[i] <- cpquery(network, (POTENTIAL == "H"), as.list(evidence_list[[i]]), method="lw")
#     }
#   return(a)
# }

