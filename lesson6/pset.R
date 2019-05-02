getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson6")

library(ggplot2)
data(diamonds)
str(diamonds)


# scatterplot - price vs. x
ggplot(diamonds, aes(x = x, y = price))+
  geom_point(alpha = 1/20)+
  scale_x_continuous(breaks = seq(3, 10, 0.5), limits = c(3,10))


# correlations bw price and x, y, z
with(diamonds, cor.test(x, price))
with(diamonds, cor.test(y, price))
with(diamonds, cor.test(z, price))


# scatterplot - price vs. depth
ggplot(diamonds, aes(x = depth, y = price))+
  geom_point(alpha = 1/100)


# correlation - depth vs. price
with(diamonds, cor.test(depth, price))


# Create a scatterplot of price vs carat
# and omit the top 1% of price and carat
# values.
ggplot(diamonds, aes(x = carat, y = price))+
  geom_point(alpha = 1/50)+
  coord_cartesian(xlim = c(0, quantile(diamonds$carat, 0.99)),
                  ylim = c(0, quantile(diamonds$price, 0.99)))


# Create a new variable for volume in the diamonds data frame.
# This will be useful in a later exercise.
diamonds$volume = with(diamonds, (x*y*z))

# Create a scatterplot of price vs. volume (x * y * z).
# This is a very rough approximation for a diamond's volume.
# Don't make any adjustments to the plot just yet.
ggplot(diamonds, aes(x = volume, y = price))+
  geom_point()


# Subset the data to exclude diamonds with a volume
# greater than or equal to 800. Also, exclude diamonds
# with a volume of 0. Adjust the transparency of the
# points and add a linear model to the plot. (See the
# Instructor Notes or look up the documentation of
# geom_smooth() for more details about smoothers.)
ggplot(subset(diamonds, diamonds$volume != 0 & diamonds$volume < 800),
       aes(x = volume, y = price))+
  geom_point(alpha = 1/60, color = "#b29f9d")+
  geom_smooth(method = "lm", color = "#ff3276")


# correlation
with(subset(diamonds, diamonds$volume != 0 & diamonds$volume < 800),
     cor.test(price, volume))


# Use the dplyr package
# to create a new data frame containing
# info on diamonds by clarity.

# Name the data frame diamondsByClarity

# The data frame should contain the following
# variables in this order.

#       (1) mean_price
#       (2) median_price
#       (3) min_price
#       (4) max_price
#       (5) n

# where n is the number of diamonds in each
# level of clarity.

library(dplyr)

clarities = group_by(diamonds, clarity)

diamondsByClarity = summarise(clarities,
                              mean_price = mean(price),
                              median_price = median(price),
                              min_price = min(price),
                              max_price = max(price),
                              n = n())

head(diamondsByClarity)

rm(clarities)


# We've created summary data frames with the mean price
# by clarity and color. You can run the code in R to
# verify what data is in the variables diamonds_mp_by_clarity
# and diamonds_mp_by_color.
library(dplyr)

diamonds_by_clarity <- group_by(diamonds, clarity)
diamonds_mp_by_clarity <- summarise(diamonds_by_clarity,
                                    mean_price = mean(price))

diamonds_by_color <- group_by(diamonds, color)
diamonds_mp_by_color <- summarise(diamonds_by_color,
                                  mean_price = mean(price))

# Your task is to write additional code to create two bar plots
# on one output image using the grid.arrange() function from the package
# gridExtra.

rm(diamonds_by_clarity)
rm(diamonds_by_color)

clarityBar = ggplot(diamonds_mp_by_clarity,
                    aes(x = clarity, y = mean_price))+
  geom_bar(stat = "identity", fill = "#9bceee")
clarityBar
ggsave("claritybar.jpg")


colorBar = ggplot(diamonds_mp_by_color,
                  aes(x = color, y = mean_price))+
  geom_bar(stat = "identity", fill = "#ff9292")
colorBar
ggsave("colorbar.jpg")

library(gridExtra)
grid.arrange(clarityBar, colorBar)

diamonds_by_cut = group_by(diamonds, cut)
diamonds_mp_by_cut = summarise(diamonds_by_cut,
                               mean_price = mean(price))
rm(diamonds_by_cut)

cutBar = ggplot(diamonds_mp_by_cut,
                aes(x = cut, y = mean_price))+
  geom_bar(stat = "identity", fill = "#67ffa7")
cutBar
ggsave("cutbar.jpg")

grid.arrange(clarityBar, colorBar, cutBar)