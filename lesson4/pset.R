getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson4")

library(ggplot2)
data(diamonds)

names(diamonds)
str(diamonds)
?diamonds


# histogram of price of all diamonds in dataset
summary(diamonds$price)

ggplot(diamonds, aes(x = price))+
  geom_histogram(color = "white", fill = "#ff8cd7", binwidth = 200)+
  scale_x_continuous(breaks = seq(0, 19000, 2000))+
  scale_y_continuous(breaks = seq(0, 6000, 2000))
# ggsave("diamondprices.jpeg")

ggplot(diamonds, aes(x = price))+
  geom_histogram(color = "white", fill = "#ff8cd7", binwidth = 0.02)+
  scale_x_log10()


# diamond prices
sum(diamonds$price < 250)

sum(diamonds$price < 500)

sum(diamonds$price >= 15000)


# exploring the highest peak in diamond prices histogram from earlier
ggplot(diamonds, aes(x = price))+
  geom_histogram(color = "white", fill = "#ff8cd7", binwidth = 20)+
  scale_x_continuous(limits = c(300,1400), breaks = seq(300, 1400, 100))
# ggsave("commonprice.jpg")


# histograms of diamond prices sorted by diamond cut
library(gridExtra)

summary(diamonds$cut)

p1 = ggplot(subset(diamonds, diamonds$cut == "Fair"), aes(x = price))+
  geom_histogram(color = "white", fill = "#000080", binwidth = 200)+
  ylab("Count of 'Fair' cut diamonds")
p1

p2 = ggplot(subset(diamonds, diamonds$cut == "Good"), aes(x = price))+
  geom_histogram(color = "white", fill = "#22a079", binwidth = 200)+
  ylab("Count of 'Good' cut diamonds")
p2

p3 = ggplot(subset(diamonds, diamonds$cut == "Very Good"), aes(x = price))+
  geom_histogram(color = "white", fill = "#9a38e0", binwidth = 200)+
  ylab("Count of 'Very Good' cut diamonds")
p3

p4 = ggplot(subset(diamonds, diamonds$cut == "Premium"), aes(x = price))+
  geom_histogram(color = "white", fill = "#02cdf1", binwidth = 200)+
  ylab("Count of 'Premium' cut diamonds")
p4

p5 = ggplot(subset(diamonds, diamonds$cut == "Ideal"), aes(x = price))+
  geom_histogram(color = "white", fill = "#60004d", binwidth = 200)+
  ylab("Count of 'Ideal' cut diamonds")
p5

grid.arrange(p1, p2, p3, p4, p5, ncol=2)


# a much easier way of going about the same question
# shitty looking, though, and need to manually adjust
# y-scale for uniformity
qplot(x = price, data = diamonds) + facet_wrap(~cut)


# parameter added to automatically scale y
qplot(x = price, data = diamonds) + facet_wrap(~cut, scales = "free_y")


by(diamonds$price, diamonds$cut, summary)


# a histogram of price per carat faceted by cut.
# Adjust the bin width and transform the scale of the x-axis using log10.
ggplot(diamonds, aes(x = price/carat))+
  geom_histogram(color = "white", fill = "#b199ac", binwidth =)+
  facet_wrap(~cut, scales = "free_y")+
  scale_x_log10(breaks = seq(1000, 19000, 5000))


# investigating price of diamond by color
ggplot(diamonds, aes(x = color, y = price))+
  geom_boxplot(aes(fill = color))+
  coord_cartesian(ylim = c(0,8000))+
  scale_y_continuous(breaks = seq(0, 8000, 500))
ggsave("pricebycolor.jpg")

by(diamonds$price, diamonds$color, summary)


# Investigate the price per carat of diamonds across
# the different colors of diamonds using boxplots.
ggplot(diamonds, aes(x = color, y = price/carat))+
  geom_boxplot(aes(fill = color))+
  coord_cartesian(ylim = c(2000,6000))+
  scale_y_continuous(breaks = seq(2000, 6000, 500))
ggsave("pricepercaratbp.jpg")


# frequency polygon for carat
ggplot(diamonds, aes(x = carat))+
  geom_freqpoly(binwidth = 0.01)+
  scale_y_continuous(breaks = seq(0,16000, 1000))+
  scale_x_continuous(breaks = seq(0,5, 0.10), limits = c(0,5))



# the pokemon dataset from kaggle for freestyle practice
getwd()

pokemon = read.csv("pokemon.csv")
summary(pokemon)
str(pokemon)
names(pokemon)

# remove 'Type.2' column which had a lot of blank fields
pokemon = subset(pokemon, select = -Type.2)

# turning pokemon Generation to a factor instead of int
pokemon$Generation = factor(pokemon$Generation)


# counts of pokemon separated by Type.1
ggplot(pokemon, aes(x = Generation))+
  geom_bar(color = "white", aes(fill = Generation))+
  scale_x_discrete(breaks = seq(1,6,1))
# ggsave("gencount.jpg")


# boxplots of speed by generation
ggplot(pokemon, aes(x = Generation, y = Speed))+
  geom_boxplot(aes(fill = Generation))+
  coord_cartesian(ylim = c(25, 120))
# ggsave("genspeedbp.jpg")