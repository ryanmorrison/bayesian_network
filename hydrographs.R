library(reshape2)
library(scales)
Existing <- read.table("data/Gila-Gila.txt", header=FALSE)
Existing$V1 <- as.Date(Existing$V1, format="%m/%d/%Y")
colnames(Existing) <- c("Date", "CFS")

CUFA_150 <- read.table("data/CUFA_150.txt", header=FALSE)
CUFA_150$V1 <- as.Date(CUFA_150$V1, format="%m/%d/%Y")
colnames(CUFA_150) <- c("Date", "CFS")

CUFA_nomin <- read.table("data/CUFA_nomin.txt", header=FALSE)
CUFA_nomin$V1 <- as.Date(CUFA_nomin$V1, format="%m/%d/%Y")
colnames(CUFA_nomin) <- c("Date", "CFS")

combined_hydro <- cbind(Existing, CUFA_150[,2], CUFA_nomin[,2])
colnames(combined_hydro) <- c("Date", "Existing", "CUFA_150", "CUFA_nomin")
hydro_melt <- melt(combined_hydro, id="Date")

hydrograph <- ggplot(hydro_melt, aes(Date, value, color=variable)) + geom_line() 
hydrograph <- hydrograph + ylim(c(0,4000)) 
hydrograph <- hydrograph + scale_x_date(breaks="5 day", labels=date_format("%m/%d/%y"),limits = as.Date(c('1972-09-01','1972-10-01')))
hydrograph



# Existing_xts <- xts(Existing[,2], order.by=Existing[,1])
# CUFA_150_xts <- xts(CUFA_150[,2], order.by=CUFA_150[,1])
# CUFA_nomin_xts <- xts(CUFA_nomin[,2], order.by=CUFA_nomin[,1])
# 
# Subset_range <- '195709'
# 
# Existing_xts[Subset_range]
# 
# plot_range <- 
# 
# plot.xts(Existing_xts[Subset_range])
# plot.xts(CUFA_150_xts[Subset_range])
