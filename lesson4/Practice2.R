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