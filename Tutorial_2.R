library(lubridate)
library(here)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)


# Section 1: Save out subset zebrafish dataframe --------------------------

fish <- read.csv("C:/Users/ameli/Downloads/zebrafish_behavior.csv")

fish <- fish %>% filter(month(time) == 8)
table(fish$genotype, fish$experiment)
fish <- subset(fish, select = -c(experiment, unique_trackID))

write.csv(fish, here("zebrafish_behaviour_subset.csv"))


# Section 2: First attempt at proportion plot -----------------------------

fish <- read.csv("C:/Users/ameli/Downloads/zebrafish_behavior.csv")
#fish <- fish %>% filter(month(time) == 8) #optional: subset for only august observations
fish_WT <- fish %>% filter(genotype == "WT")
fish_WT$hour <- hour(fish_WT$time)
fish_counts <- fish_WT %>% count(hour, state)
fish_wake <- fish_counts[fish_counts$state == "Wake",]
fish_sleep <- fish_counts[fish_counts$state == "Sleep",]
fish_wake$totals <- fish_WT %>% count(hour)
fish_sleep$totals <- fish_WT %>% count(hour)
fish_sleep$proportion <- fish_sleep$n/fish_sleep$totals$n
fish_wake$proportion <- fish_wake$n/fish_wake$totals$n

WT_wake <- ggplot(fish_wake, aes(x = hour, y = proportion)) + geom_line() + geom_point() 
WT_sleep <- ggplot(fish_sleep, aes(x = hour, y = proportion)) + geom_line() + geom_point()

fish_WT <- fish %>% filter(genotype == "Het")
fish_WT$hour <- hour(fish_WT$time)
fish_counts <- fish_WT %>% count(hour, state)
fish_wake <- fish_counts[fish_counts$state == "Wake",]
fish_sleep <- fish_counts[fish_counts$state == "Sleep",]
fish_wake$totals <- fish_WT %>% count(hour)
fish_sleep$totals <- fish_WT %>% count(hour)
fish_sleep$proportion <- fish_sleep$n/fish_sleep$totals$n
fish_wake$proportion <- fish_wake$n/fish_wake$totals$n

Het_wake <- ggplot(fish_wake, aes(x = hour, y = proportion)) + geom_line() + geom_point() 
Het_sleep <- ggplot(fish_sleep, aes(x = hour, y = proportion)) + geom_line() + geom_point()

fish_WT <- fish %>% filter(genotype == "Hom")
fish_WT$hour <- hour(fish_WT$time)
fish_counts <- fish_WT %>% count(hour, state)
fish_wake <- fish_counts[fish_counts$state == "Wake",]
fish_sleep <- fish_counts[fish_counts$state == "Sleep",]
fish_wake$totals <- fish_WT %>% count(hour)
fish_sleep$totals <- fish_WT %>% count(hour)
fish_sleep$proportion <- fish_sleep$n/fish_sleep$totals$n
fish_wake$proportion <- fish_wake$n/fish_wake$totals$n

Hom_wake <- ggplot(fish_wake, aes(x = hour, y = proportion)) + geom_line() + geom_point() 
Hom_sleep <- ggplot(fish_sleep, aes(x = hour, y = proportion)) + geom_line() + geom_point()

WT_wake
WT_sleep
Hom_sleep
Hom_wake
Het_sleep
Het_wake

grid.arrange(WT_wake, Hom_wake, Het_wake, nrow = 2, ncol = 2)
grid.arrange(WT_sleep, Hom_sleep, Het_sleep, nrow = 2, ncol = 2)


# Section 2: Second attempt at proportion plots ---------------------------
fish <- read.csv("C:/Users/ameli/Downloads/zebrafish_behavior.csv") 
fish <- fish %>% filter(month(time) == 8)
fish <- subset(fish, select = -c(experiment, unique_trackID))
fish$hour <- hour(fish$time)
fish_WT <- fish %>% filter(genotype == "WT")

fish_counts <- fish %>% count(hour, state, genotype)

#plot total counts
#ggplot(fish_counts, aes(x = hour, y = n, color = genotype)) + geom_point() + geom_line() + facet_wrap(~state)

fish_totals <- fish %>% count(hour, genotype)
fish_totals <- cbind(fish_totals, rep(row.names(fish_totals), each = 2))
fish_counts <- fish_counts %>% arrange(state)
fish_counts$totals <- fish_totals$n
fish_counts$proportions <- fish_counts$n/fish_counts$totals

#plot proportions
august_plot <- ggplot(fish_counts, aes(x = hour, y = proportions, color = genotype)) + geom_point() + geom_line() + facet_wrap(~state)
august_plot

#for each individual (trackID) what is the amount of time spent in each state (wake or sleep)
#since each row is the call made off 6 seconds of observations we can use the number of rows as a measure of time (x6sec)
fish_counts <- fish_WT %>% count(hour, state, trackID)

#to find the proportion we need the total time recorded in sleep and wake for each individual
fish_totals <- fish_counts %>% group_by(hour, trackID) %>% summarise(total = sum(n))

#create a column of unique identifiers we can use to merge
fish_totals$hourtrackID <- paste(fish_totals$hour, fish_totals$trackID, sep = "_")
fish_counts$hourtrackID <- paste(fish_counts$hour, fish_counts$trackID, sep = "_")
#merge the dataframes into one
fish_counts <- merge(fish_counts,fish_totals) 

#take the proportion
fish_counts$proportion <- fish_counts$n/fish_counts$total

#take the mean of all of the trackID proportions for each state and each hour
fish_1 <- fish_counts %>% group_by(hour, state) %>% summarise(mean_proportion = mean(proportion))
#take the standard deviation of each individuals proportion of time awake and asleep (ie how much do they vary from the mean)
fish_2 <- fish_counts %>% group_by(hour, state) %>% summarise(SD = sd(proportion), SD_n = sum(n))
#SEM = SD/sqrt(n), where n is the number of observation (ie the number of timepoints - rows per individual per state per hour) 
fish_2$SEM <- fish_2$SD/sqrt(fish_2$SD_n)

fish_1 <- merge(fish_1, fish_2)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SD, ymax = mean_proportion + SD)) + facet_wrap(~state)
ggplot(fish_1, aes(x = hour, y = mean_proportion)) + geom_point() + geom_line() + geom_errorbar(aes(ymin = mean_proportion - SEM, ymax = mean_proportion + SEM)) + facet_wrap(~state)

#add the standard error of the mean (SEM) which is the standard deviation /N 
install.packages("plotrix")
library(plotrix)

fish_WT <- fish %>% filter(genotype == "WT")
data <- fish_WT$Dist_6s
std.error(data)
fish_WT %>% group_by(hour) %>% std.error()
lapply(unique(fish_WT$hour), function(x) {std.error(x)})

# Section 4: Sleep in day vs night zebrafish ------------------------------


