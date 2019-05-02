getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson3")

library(ggplot2)
pf = read.csv("pseudo_facebook.tsv", sep = "\t")
names(pf)

setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson5")

ggplot(pf, aes(x = age, y = friend_count))+
  geom_point()+
  xlim(13,90)
# ggsave("agefriendsp.jpg")


# avoiding overplotting
# alpha makes points transparent
# also, setting alpha to 1/20 means 1 point represents 20 points
# alpha = 1/1 will yield practically the same plot as no alpha
# alpha 1/200 will make the plot too sparse
ggplot(pf, aes(x = age, y = friend_count))+
  geom_point(alpha = 1/20)+
  xlim(13,90)
# ggsave("agefriendspalpha.jpg")


# geom_jitter instead of geom_point
# also includes alpha
ggplot(pf, aes(x = age, y = friend_count))+
  geom_jitter(alpha = 1/20)+
  xlim(13,90)
# ggsave("agefriendspjitter.jpg")


# coord_trans
ggplot(pf, aes(x = age, y = friend_count))+
  geom_point(alpha = 1/20, position = position_jitter(h = 0))+
  xlim(13,90)+
  coord_trans(y = "sqrt")
# ggsave("coordtrans.jpg")


# Examine the relationship between
# friendships_initiated (y) and age (x)
# using the ggplot syntax.
ggplot(pf, aes(x = age, y = friendships_initiated))+
  geom_point(alpha = 1/10, position = position_jitter(h = 0))+
  xlim(13,90)+
  coord_trans(y = "sqrt")


# overlaying plots one on top of the other
ggplot(pf, aes(x = age, y = friend_count))+
  geom_point(alpha = 1/20, position = position_jitter(h = 0),
             color = "#b4eeb4")+
  xlim(13,90)+
  coord_trans(y = "sqrt")+
  geom_line(stat = "summary", fun.y = mean)+
  geom_line(stat = "summary",
            fun.y = quantile,
            fun.args = list(probs = 0.1),
                            linetype = 2,
                            color = "blue")+
  geom_line(stat = "summary",
            fun.y = quantile,
            fun.args = list(probs = 0.9),
                            linetype = 2,
                            color = "blue")+
  geom_line(stat = "summary",
            fun.y = median,
            color = "blue")
# ggsave("overlays.jpg")


# including a coord_cartesian layer this time
ggplot(pf, aes(x = age, y = friend_count))+
  geom_point(alpha = 1/20, position = position_jitter(h = 0),
             color = "#ffb48a")+
  coord_cartesian(xlim = c(13, 70), ylim = c(0,1000))+
  geom_line(stat = "summary", fun.y = mean)+
  geom_line(stat = "summary",
            fun.y = quantile,
            fun.args = list(probs = 0.1),
            linetype = 2,
            color = "#9e00d6")+
  geom_line(stat = "summary",
            fun.y = quantile,
            fun.args = list(probs = 0.9),
            linetype = 2,
            color = "#9e00d6")+
  geom_line(stat = "summary",
            fun.y = median,
            color = "#9e00d6")
# ggsave("cc-overlays.jpg")


# correlation
cor.test(pf$age, pf$friend_count, method = "pearson")

# same thing, alternate syntax (method 'pearson' is the default)
cor.test(pf$age, pf$friend_count)

# a good thumb rule: a pearson cor value of >0.3 OR <(-0.3)
# means that there is some correlation between the
# two variables
# ~ +/-0.3 means there is some pearson cor, but it is small
# ~ +/-0.5 means there is some pearson cor, and it is moderate
# ~ +/-0.7 means there is some pearson cor, and it is large

# more alternate syntax
with(pf, cor.test(age, friend_count))

# correlation on subsets
with(subset(pf, age <=70),
     cor.test(age, friend_count))


# different correlation methods
# pearson done already

# spearman
with(subset(pf, age <=70),
     cor.test(age, friend_count,
              method = "spearman"))

# kendall
# THIS KENDALL THING IS TAKING WAY TOO MUCH TIME
# VERY COMPUTATIONALLY EXPENSIVE; IT SEEMS
# with(subset(pf, age <=70),
#      cor.test(age, friend_count,
#               method = "kendall"))


# scatterplots of obviously related variables
# here: likes_received vs. www_likes_received
summary(pf)

ggplot(pf, aes(x = www_likes_received, y = likes_received))+
  geom_point(alpha = 1/10,
             color = "#40e0d0",
             position = position_jitter(h = 0))+
  coord_cartesian(xlim = c(0, quantile(pf$www_likes_received, 0.95)),
                  ylim = c(0, quantile(pf$likes_received, 0.95)))+
  # a layer that adds a line that best fits this plot
  # 'lm' stands for 'linear model'
  geom_smooth(method = "lm", color = "#ff3276")
ggsave("correlations.jpg")

with(pf, cor.test(www_likes_received, likes_received))
with(pf, cor.test(likes_received, www_likes_received))
# notice how both of the above (x and y params passed to cor.test)
# yield the exact same cor value


# Mitchell library from the alr3 package
library(alr3)
data(Mitchell)

names(Mitchell)
summary(Mitchell)

ggplot(Mitchell, aes(x = Month, y = Temp))+
  geom_point()

with(Mitchell, cor.test(x = Month, y = Temp))

# arranging the data in a format that makes sense
ggplot(Mitchell, aes(x = ((Month) %% 12), y = Temp))+
  geom_point()+
  scale_x_continuous(breaks = seq(0,11,1))+
  xlab("Months (0 = January, 11 = December)")+
  scale_y_continuous(breaks = seq(-10, 30, 5))


# back to pf data
# age_with_months variable
pf$age_with_months = pf$age + (1 - pf$dob_month/12)


# dplyr
library("dplyr")

# group_by
agegroups = group_by(pf, age)

# summarise
# n()
pf.fc_by_age = summarise(agegroups,
                         friend_count_mean = mean(friend_count),
                         friend_count_median = median(friend_count),
                         n = n())

head(pf.fc_by_age)

rm(agegroups)

# arrange
# arranging by num of people in the group
pf.fc_by_age = arrange(pf.fc_by_age, n)
head(pf.fc_by_age)

# arranging by age (This was the default arrangement,
# at least for me, but it wasn't the default for the tutor.
# His data was seemingly unordered.)
pf.fc_by_age = arrange(pf.fc_by_age, age)
head(pf.fc_by_age)


# creating a line graph to visualize fc_by_age
ggplot(pf.fc_by_age, aes(x = age, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(10, 113, 10), limits = c(13,113))
# ggsave("meanfcbyage.jpg")


ageplot = ggplot(subset(pf.fc_by_age, age <= 70),
                 aes(x = age, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(10, 70, 10),
                     limits = c(13,70))
ageplot

# group_by
agemonthgroups = group_by(pf, age_with_months)

# summarise
# n()
pf.fc_by_age_months = summarise(agemonthgroups,
                                friend_count_mean = mean(friend_count),
                                friend_count_median = median(friend_count),
                                n = n())

head(pf.fc_by_age_months)

rm(agemonthgroups)

# arrange
# arranging by age_with_months
pf.fc_by_age_months = arrange(pf.fc_by_age_months, age_with_months)
head(pf.fc_by_age_months)

# plotting some bits of the new data frame
agemonthplot = ggplot(subset(pf.fc_by_age_months, age_with_months < 71),
                      aes(x = age_with_months, y = friend_count_mean))+
  geom_line()+
  scale_x_continuous(breaks = seq(10, 70, 10),
                     limits = c(13,70))
agemonthplot

# a smooth plot
smoothplot = ggplot(subset(pf, age <= 70),
                     aes(x = (round(age/2) * 2), y = friend_count))+
  geom_line(stat = "summary", fun.y = mean)+
  scale_x_continuous(breaks = seq(10, 70, 10),
                     limits = c(13,70))
smoothplot

# an even smoother plot
smootherplot = ggplot(subset(pf, age <= 70),
                    aes(x = (round(age/5) * 5), y = friend_count))+
  geom_line(stat = "summary", fun.y = mean)+
  scale_x_continuous(breaks = seq(10, 70, 10),
                     limits = c(13,70))
smootherplot

# yeh kuch jyada hi smooth hain aur kaafi useless bhi
smoothestplot = ggplot(subset(pf, age <= 70),
                      aes(x = (round(age/10) * 10), y = friend_count))+
  geom_line(stat = "summary", fun.y = mean)+
  scale_x_continuous(breaks = seq(10, 70, 10),
                     limits = c(13,70))
smoothestplot

library(gridExtra)
grid.arrange(agemonthplot, ageplot, smoothplot, smootherplot, smoothestplot, ncol = 2)
