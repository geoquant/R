rm(list = ls(all = TRUE))

library(xts)
library(dplyr)
library(scales)
library(ggplot2)
library(gridExtra)

setwd("~/dev/fbook")

# load data ---------------------------------------------------------------
# 'data.frame':	326 obs. of  2 variables:
# $ year  : int 
# $ person: chr  
data <- read.csv("hbd_2016.csv", header = TRUE, as.is = TRUE, sep = ",")


# individual frequency ----------------------------------------------------
indiv_freq  <- count_(unique(data), vars = "person", sort = TRUE)

percent1 <- quantile(indiv_freq$n, 0.99) # 2 posts
percent5 <- quantile(indiv_freq$n, 0.95) # 3 posts

who1 <- indiv_freq[indiv_freq$n >= percent1, ] #  4 people
who5 <- indiv_freq[indiv_freq$n >= percent5, ] # 23 people

# density plot
plot1 <- ggplot(indiv_freq, aes(n)) + 
  geom_density(fill = "black") +
  ggtitle("Kernel Density of FB HBD Posts 2008-2016") +
  xlab("Cumulative Sum of Posts Per Person") +
  ylab("Density")  +
  geom_vline(xintercept = c(percent1, percent5), 
             colour = c("#7fcdbb", "#2c7fb8"), size = 1.05) +
  annotate("text", x = c(3, 5) + 0.25 , y = c(1, 1) + 0.05, 
           label = c("95%", "99%"), 
           colour = c("#2c7fb8", "#7fcdbb"))

# bar plot
plot2 <- ggplot(data = who5, aes(x = reorder(person, n), y = n)) +
  ggtitle("95th Percentile of HBD Posters 2008-2016") +
  ylab("Cumulative Sum of Posts Per Person") +
  xlab(NULL) +
  geom_bar(stat = 'identity') + 
  coord_flip() +
  annotate("text", x = nrow(who5) - nrow(who1) + 0.35, y = 6 + 0.25, 
           label = "99%", colour = "#7fcdbb") +
  geom_vline(xintercept = nrow(who5) - nrow(who1) + 1, 
             colour = "#7fcdbb", size = 1.05)

# plot density and barplot side-by-side
grid.arrange(plot1, plot2, ncol = 2)


# streaks -----------------------------------------------------------------
split_names <- split(data, data$person)
split_dates <- lapply(split_names, `[`, , 1)
split_diff  <- lapply(split_dates, function(x) diff(rev(x)))
split_con   <- lapply(split_diff, function(x) x[!x > 1])
split_sum   <- lapply(split_con, sum)

consecutive_sums <- sort(unlist(split_sum))

# print to console consecutive count of 99% percentile
cbind(consecutive_sums[who1$person])


# annual frequency --------------------------------------------------------
annual_count <- 
  data %>%
  group_by(year) %>%
  summarise(count = n_distinct(person)) %>%
  mutate(count = replace(count, count == 1, 0)) %>%
  arrange(year) 

# line plot
ggplot(data = annual_count, aes(x = year, y = count)) +
  ggtitle("Total HBD Posts Per Year") +
  ylab(NULL) +
  xlab(NULL) +
  geom_line()
