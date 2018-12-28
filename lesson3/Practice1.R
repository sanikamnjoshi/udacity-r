getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson3")

pf = read.csv("pseudo_facebook.tsv", sep = "\t")

names(pf)

library(ggplot2)

ggplot(pf, aes(x = friend_count))+
  geom_histogram(breaks = seq(0,500, by=10), color = "white")


# facet
ggplot(pf, aes(x = dob_day))+
  geom_histogram(binwidth = 1, color = "white")+
  scale_x_continuous(breaks = 1:31)+
  facet_wrap(~dob_month, ncol = 3)


ggplot(subset(pf, !is.na(gender)), aes(x = friend_count))+
  geom_histogram(breaks = seq(0,500, by=10), binwidth = 1, color = "white")+
  facet_wrap(~gender)


# frequency polygon
ggplot(subset(pf, !is.na(gender)), aes(x = friend_count, y = ..count../sum(..count..)))+
  geom_freqpoly(aes(color = gender), binwidth=10)+
  scale_x_continuous(limits = c(0, 1000), breaks = seq(0, 1000, 50))+
  xlab('Friend Count')+
  ylab('Proportion of users with that friend count')


ggplot(subset(pf, !is.na(gender)), aes(x = friend_count, y = ..count../sum(..count..)))+
  geom_freqpoly(aes(color = gender), binwidth=10)+
  scale_x_continuous(limits = c(350, 1000), breaks = seq(0, 1000, 50))+
  xlab('Friend Count')+
  ylab('Proportion of users with that friend count')


# boxplot
ggplot(subset(pf, !is.na(gender)), aes(x = gender, y = friend_count))+
  geom_boxplot(aes(fill = gender))+
  scale_y_continuous(limits = c(0,1000), breaks = seq(0, 1000, 100))


ggplot(subset(pf, !is.na(gender)), aes(x = gender, y = friend_count))+
  geom_boxplot(aes(fill = gender))+
  scale_y_continuous(limits = c(0,250), breaks = seq(0, 250, 10))


# coord cartesian
ggplot(subset(pf, !is.na(gender)), aes(x = gender, y = friend_count))+
  geom_boxplot(aes(fill = gender))+
  coord_cartesian(ylim = c(0,250))


by(pf$friend_count, pf$gender, summary)
# the values in this summary table which 'by' gives
# will only match the values in the plot
# if you set y limits in the coord_cartesian function.
# They won't match the plot in which you pass y limits
# to the scale_y_continuous function.


# on average (NOT overall) who initiated more frndshps - men or women?
ggplot(subset(pf, !is.na(gender)), aes(x = gender, y = friendships_initiated))+
  geom_boxplot(aes(fill = gender))+
  coord_cartesian(ylim = c(0,150))

by(pf$friendships_initiated, pf$gender, median)
by(pf$friendships_initiated, pf$gender, summary)



# overall (NOT on average) who initiated more frndshps - men or women?
by(pf$friendships_initiated, pf$gender, sum)


# scale_X_log10
ggplot(subset(pf, !is.na(gender)), aes(x = www_likes))+
  geom_freqpoly(aes(color=gender))+
  scale_x_continuous(limits = c(1, 15000), breaks = seq(0, 15000, 1000))+
  scale_y_continuous(limits = c(0, 2000))+
  scale_x_log10()


ggplot(subset(pf, !is.na(gender)), aes(x = friend_count))+
  geom_freqpoly(aes(color = gender))+
  scale_x_log10()


# by
by(pf$friend_count, pf$gender, summary)

by(pf$www_likes, pf$gender, sum)


# experimenting with color and fill
ggplot(pf, aes(x = tenure/365)) +
  geom_histogram(binwidth = 1, color = "white", fill = '#ff6a50')+
  scale_x_continuous(breaks = seq(0,7))+
  xlab("No. of years on Facebook")+
  ylab("No. of users")


ggplot(pf, aes(x = age))+
  geom_histogram(color = "white", fill = "#c581ff", binwidth=1)+
  scale_x_continuous(breaks = seq(10,115, by=2))


# multiple graphs, single plot
library(gridExtra)
p1 = ggplot(pf, aes(x = friend_count))+
  geom_histogram(breaks = seq(0,1000, by=10), color = "white", fill = "#c581ff", binwidth=100)+
  xlab("No. of friends")+
  ylab("No. of users")
p1

p2 = ggplot(pf, aes(x = friend_count+1))+
  geom_histogram(color = "white", fill = "#ff7f50", binwidth=0.03)+
  scale_x_log10(breaks = seq(0,1500, by=200))+
  xlab("No. of friends")+
  ylab("No. of users")
p2

p3 = ggplot(pf, aes(x = friend_count))+
  geom_histogram(color = "white", fill = "#ff85bd", binwidth=1)+
  scale_x_sqrt(breaks = seq(0,2500, by=500))+
  xlab("No. of friends")+
  ylab("No. of users")
p3

grid.arrange(p1, p2, p3, ncol=1)


# finding percentage of users who check in from
# their mobile devices based on mobile likes
pf$mobile_checkin = NA
pf$mobile_checkin = ifelse(pf$mobile_likes > 0, TRUE, FALSE)
pf$mobile_checkin = factor(pf$mobile_checkin)
summary(pf$mobile_checkin)
(sum(pf$mobile_checkin==TRUE)) / (length(pf$mobile_checkin))