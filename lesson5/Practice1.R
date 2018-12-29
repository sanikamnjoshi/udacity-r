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


# dplyr
library("dplyr")

# group_by
agegroups = group_by(pf, age)

# summarise
# n()
fc_by_age = summarise(agegroups,
          mean_fc = mean(friend_count),
          median_fc = median(friend_count),
          num_people = n())

head(fc_by_age)

# arrange
# arranging by num of people in the group
fc_by_age = arrange(fc_by_age, num_people)
head(fc_by_age)

# arranging by age (This was the default arrangement,
# at least for me, but it wasn't the default for the tutor.
# His data was seemingly unordered.)
fc_by_age = arrange(fc_by_age, age)
head(fc_by_age)


# creating a line graph to visualize fc_by_age
ggplot(fc_by_age, aes(x = age, y = mean_fc))+
  geom_line()+
  scale_x_continuous(breaks = seq(10, 113, 10), limits = c(13,113))
# ggsave("meanfcbyage.jpg")


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
