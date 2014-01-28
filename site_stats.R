#### Load libraries ####
library(reshape2)
library(ggplot2)
library(dplyr)
library(scales) # Library for modifying scales on plots
library(grid) # Library for developing nonstandard plotting themes
library(extrafont)      # Library for embedding system fonds in pdf plots
loadfonts()             # Loads system fonts into R. If you want to output to .ps files instead of .pdf, use:
# loadfonts(device="postscript")

#### Set working directory ####
setwd("/Users/Morrison/Dropbox/Gila Bayesian/bayesian_network")

#### Source common functions ####
source('~/Dropbox/Gila Bayesian/bayesian_network/1-functions.R')

# site1_data <- load.probs("site1")
# E_site1 <- data.frame(combine.probs.E(), scenario = "existing")
# CUFA_150_site1 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
# CUFA_nomin_site1 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
# combined_site1 <- data.frame(rbind(E_site1, CUFA_150_site1, CUFA_nomin_site1), site = "site1")
# 
# site2_data <- load.probs("site2")
# E_site2 <- data.frame(combine.probs.E(), scenario = "existing")
# CUFA_150_site2 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
# CUFA_nomin_site2 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
# combined_site2 <- data.frame(rbind(E_site2, CUFA_150_site2, CUFA_nomin_site2), site = "site2")
# 
# site4_data <- load.probs("site4")
# E_site4 <- data.frame(combine.probs.E(), scenario = "existing")
# CUFA_150_site4 <- data.frame(combine.probs.CUFA.150(), scenario = "CUFA_150")
# CUFA_nomin_site4 <- data.frame(combine.probs.CUFA.nomin(), scenario = "CUFA_nomin")
# combined_site4 <- data.frame(rbind(E_site4, CUFA_150_site4, CUFA_nomin_site4), site = "site4")
# 
# combined_all_sites <- rbind(combined_site1, combined_site2, combined_site4)

load("output/combined_site1.Rdata")
load("output/combined_site2.Rdata")
load("output/combined_site3.Rdata")
load("output/combined_site4.Rdata")
load("output/combined_site5.Rdata")

combined_all_sites <- rbind(combined_site1, combined_site2, combined_site3, combined_site4, combined_site5)

# Specify a custom theme for plotting.
theme_tufte <- function(ticks=TRUE, base_family="Lato", base_size=13) {
  ret <- theme_bw(base_family=base_family, base_size=base_size) +
    theme(
      legend.background  = element_blank(),
      legend.key          = element_blank(),
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

# Plots

Q_boxplots <- ggplot(data = combined_all_sites, aes(x = scenario, y = util_prob))
Q_boxplots <- Q_boxplots + geom_boxplot()
Q_boxplots <- Q_boxplots + facet_grid(site~Q_bin)
Q_boxplots <- Q_boxplots + stat_summary(fun.y= mean, geom = "point", color = "red")
Q_boxplots <- Q_boxplots + theme_tufte()
Q_boxplots
ggsave("figs/Q_boxplots.pdf", Q_boxplots, width = 20, height = 14)

site_boxplots <- ggplot(data = combined_all_sites, aes(x = scenario, y = util_prob))
site_boxplots <- site_boxplots + geom_boxplot()
site_boxplots <- site_boxplots + facet_grid(site~ .)
site_boxplots <- site_boxplots + stat_summary(fun.y= mean, geom = "point", color = "red", size = 2.5)
site_boxplots <- site_boxplots + theme_tufte()
site_boxplots
ggsave("figs/site_boxplots.pdf", site_boxplots, width = 20, height = 14)

# Calculate the mean and standard devation of probability differences between existing conditions and each scenario
diffstats <- ddply(.data = combined_all_sites, .variables = c("scenario", "site"), summarize, meandiff = mean(prob_diff, na.rm=TRUE), sddiff = sd(prob_diff, na.rm=TRUE), mediandiff = median(prob_diff, na.rm=TRUE), twentyfive_percent = quantile(prob_diff, 0.25, na.rm=TRUE), seventyfive_percent = quantile(prob_diff, 0.75, na.rm=TRUE))

# Plot mean and standard devation of changes in probabilities for each scenario
dodge <- position_dodge(width=0.9)
site_diff_bars <- ggplot(data = diffstats[6:15 ,], aes(x = scenario, y = mediandiff, fill = site))
site_diff_bars <- site_diff_bars + geom_bar(position = "dodge", stat = "identity")
site_diff_bars <- site_diff_bars + geom_errorbar(aes(ymin = twentyfive_percent, ymax = seventyfive_percent), stat = "identity", position =dodge, width = 0.25, linetype = 2)
site_diff_bars <- site_diff_bars + scale_fill_grey() + theme_tufte()
site_diff_bars <- site_diff_bars + ylab("Median Relative Decrease in Posterior Probability of Recruitment") + xlab("")
site_diff_bars
ggsave("figs/site_diff_bars.pdf", site_diff_bars, width = 16, height = 12)

# Calculate the average number of events during each time state
eventstats <- ddply(.data = combined_all_sites, .variables = c("scenario", "site"), summarize, mean_apr_may = mean(apr_may), mean_jun_jul = mean(jun_jul), mean_aug_sep = mean(aug_sep))

# Calculate difference in events each season for each scenario
eventdiffs <- as.data.frame(matrix(nrow = 5, ncol = 3))
for (i in 1:5){
  eventdiffs$V1[i] <- (eventstats$mean_apr_may[i+5]-eventstats$mean_apr_may[i])/eventstats$mean_apr_may[i] * 100
  eventdiffs$V2[i] <- (eventstats$mean_jun_jul[i+5]-eventstats$mean_jun_jul[i])/eventstats$mean_jun_jul[i] * 100
  eventdiffs$V3[i] <- (eventstats$mean_aug_sep[i+5]-eventstats$mean_aug_sep[i])/eventstats$mean_aug_sep[i] * 100
}

eventmelt <- melt(data = eventstats, id.vars = c("site", "scenario"))

# Plot the number of events in each month
event_plot <- ggplot(data = eventmelt, aes(x = scenario, y = value, fill = variable))
event_plot <- event_plot + geom_bar(position = "dodge", stat = "identity")
event_plot <- event_plot + facet_grid(site ~ .)
event_plot <- event_plot + scale_fill_grey() + theme_tufte()
event_plot
ggsave("figs/event_plot.pdf", event_plot, width = 12, height = 15)

# Plot the mean probability at each site
site_boxplots <- ggplot(data = combined_all_sites, aes(x = scenario, y = util_prob))
site_boxplots <- site_boxplots + geom_boxplot()
site_boxplots <- site_boxplots + facet_grid(site~ .)
site_boxplots <- site_boxplots + stat_summary(fun.y= mean, geom = "point", color = "red", size = 2.5)
site_boxplots <- site_boxplots + theme_tufte()
site_boxplots
ggsave("figs/site_boxplots.pdf", site_boxplots, width = 20, height = 14)

removed_existing <- subset(combined_all_sites, combined_all_sites$scenario != "existing")
prob_histogram <- ggplot(data = removed_existing, aes(x = prob_diff))
prob_histogram <- prob_histogram + geom_histogram(stat = "bin", binwidth = 0.005)
prob_histogram <- prob_histogram + facet_grid(site ~ scenario)
prob_histogram <- prob_histogram + theme_tufte()
prob_histogram
ggsave("figs/prob_histogram.pdf", prob_histogram, width = 18, height = 12)


only_positive <- subset(combined_all_sites, combined_all_sites$prob_diff > 0)
