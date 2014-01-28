#### Load libraries ####
library(reshape2)
library(ggplot2)
library(scales) # Library for modifying scales on plots
library(grid) # Library for developing nonstandard plotting themes
library(extrafont)      # Library for embedding system fonds in pdf plots
loadfonts()             # Loads system fonts into R. If you want to output to .ps files instead of .pdf, use:
                        # loadfonts(device="postscript")

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### Source common functions ####
source('~/Dropbox/Gila Bayesian/bayesian_network/1-functions.R')

#### Load hydrology data ####

# Add the Gila at Gila USGS gage data
hydro <- usgs.process("data/usgs_09430500_daily.txt")

# Read data for the scenarios
CUFA_150 <- scenario.process("data/scenarios.csv", 3)
CUFA_nomin <- scenario.process("data/scenarios.csv", 4)

#### Organize hydrology data ####

# The datasets have different lengths and need to match, so the hydro dataset is trimmed
hydro <- hydro[hydro[ ,1] %in% CUFA_150[ ,1], ]

# Combine all datasets into one data frame
combined_hydro <- data.frame(date = hydro$date, Gila_Gila = hydro$cfs, CUFA_150 = CUFA_150$cfs, CUFA_nomin = CUFA_nomin$cfs)

# Add day of year to the data frame so flows can be averaged across the time series
doy <- as.numeric(strftime(combined_hydro$date, format = "%j")) # Get day of the year value from dates
combined_hydro <- cbind(doy, combined_hydro[ ,2:4], doy)

# Arrange the data frame according to day of the year
combined_hydro_melt <- melt(combined_hydro, id.vars = "doy")
head(combined_hydro_melt)

# Calculate mean discharge for each day of the year
combined_hydro_means <- dcast(combined_hydro_melt, doy~variable, mean)
combined_hydro_means

# Calculate difference in means between Gila-Gila and each scenario
combined_mean_diffs_150 <- combined_hydro_means$CUFA_150 - combined_hydro_means$Gila_Gila
combined_mean_diffs_nomin <- combined_hydro_means$CUFA_nomin - combined_hydro_means$Gila_Gila

combined_hydro_means <- data.frame(combined_hydro_means, CUFA_150_diff = combined_mean_diffs_150, CUFA_nomin_diff = combined_mean_diffs_nomin)

# Arrange data according to day of the year
combined_hydro_means <- melt(combined_hydro_means, id.vars = "doy")

# Convert cfs to cms
combined_hydro_means$value <- combined_hydro_means$value * 0.0283168466
head(combined_hydro_means)

#### Plot the data ####
# Custom theme
# Specify a custom theme for plotting.
theme_tufte <- function(ticks=TRUE, base_family="Lato", base_size=11) {
  ret <- theme_bw(base_family=base_family, base_size=base_size) +
    theme(
      legend.background  = element_blank(),
      legend.key        	= element_blank(),
      panel.background  	= element_blank(),
      panel.border      	= element_blank(),
      strip.background  	= element_blank(),
      plot.background   	= element_blank(),
      axis.line         	= element_line(color="black", size=0.35),

      panel.margin 	  	= unit(1.5, "lines"))
  if (!ticks) {
    ret <- ret + theme(axis.ticks = element_blank())
  }
  ret
}

# Create ggplot

# hydroplot <- ggplot(data = combined_hydro_means, aes(x = doy, y = value, group = variable, linetype = variable))
# hydroplot <- hydroplot + geom_line()
# hydroplot <- hydroplot + scale_y_continuous(breaks = seq(-50, 500, 50)) + labs(x="Day of the Year", y="Mean Discharge (cfs)")
# hydroplot <- hydroplot + theme_tufte()
# hydroplot

hydroplot <- ggplot(data = combined_hydro_means, aes(x = doy, y = value, group = variable, linetype = variable))
hydroplot <- hydroplot + geom_line()
hydroplot <- hydroplot + scale_linetype_manual(name = "Scenario", values= c("Gila_Gila" = 1, "CUFA_150" = 2, "CUFA_nomin" = 3, "CUFA_150_diff" = 2, "CUFA_nomin_diff" = 3))
hydroplot <- hydroplot + scale_y_continuous(breaks = seq(from = -4, to = 14, by = 2)) + labs(x="Day of the Year", y="Mean Discharge (cms)")
hydroplot <- hydroplot + theme_tufte()
hydroplot
ggsave("figs/hydroplot.pdf", hydroplot, width=20, height=8)

# hydroplot2 <- ggplot(data = combined_hydro_melt, aes(x = doy, y = value, group = variable, color = variable))
# hydroplot2 <- hydroplot2 + stat_summary(fun.y = "mean", geom = "line")
# hydroplot2
