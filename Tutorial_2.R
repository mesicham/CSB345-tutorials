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
#fish <- fish %>% filter(month(time) == 8)
fish <- subset(fish, select = -c(experiment, unique_trackID))
fish$hour <- hour(fish$time)
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

all_month_plot <- ggplot(fish_counts, aes(x = hour, y = proportions, color = genotype)) + geom_point() + geom_line() + facet_wrap(~state)
all_month_plot #not a very big difference so we can probably stick with just using august data


# Section 4: Sleep in day vs night zebrafish ------------------------------


