# Write code to create a new data frame,
# called 'pf.fc_by_age_gender', that contains
# information on each age AND gender group.

# The data frame should contain the following variables:

#    mean_friend_count,
#    median_friend_count,
#    n (the number of users in each age and gender grouping)

# Here is an example of the structure of your data frame. Your
# data values will be different. Note that if you are grouping by
# more than one variable, you will probably need to call the
# ungroup() function. 

#   age gender mean_friend_count median_friend_count    n
# 1  13 female          247.2953                 150  207
# 2  13   male          184.2342                  61  265
# 3  14 female          329.1938                 245  834
# 4  14   male          157.1204                  88 1201

getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson3")
pf = read.csv("pseudo_facebook.tsv", sep = "\t")
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson7")

library(dplyr)
agegengroups = group_by(subset(pf, !is.na(gender)), age, gender)
pf.fc_by_age_gender = summarise(agegengroups,
                                mean_friend_count = mean(friend_count),
                                median_friend_count = median(friend_count),
                                n = n())
head(pf.fc_by_age_gender)
rm(agegengroups)


# Create a line graph showing the
# median friend count over the ages
# for each gender. Be sure to use
# the data frame you just created,
# pf.fc_by_age_gender.

library(ggplot2)

medianagegenfc = ggplot(pf.fc_by_age_gender, aes(x = age, y = median_friend_count))+
  geom_line(aes(color = gender))
medianagegenfc

# another way of getting same graph as medainagegenfc
ggplot(aes(x = age, y = friend_count),
       data = subset(pf, !is.na(gender))) +
  geom_line(aes(color = gender), stat = 'summary', fun.y = median)

meanagegenfc = ggplot(pf.fc_by_age_gender, aes(x = age, y = mean_friend_count))+
  geom_line(aes(color = gender))
meanagegenfc

library(gridExtra)
grid.arrange(meanagegenfc, medianagegenfc)