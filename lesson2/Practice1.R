getwd()
setwd("C:/Users/sanikamnjoshi/Documents/code/r/udacity-r/lesson2")

statesInfo = read.csv('stateData.csv')

# creating a subset method 1
northEast = subset(statesInfo, state.region==1)
highMurder = subset(statesInfo, murder>4)
length(highMurder)

# creating a subset method 2
gawaarStates = statesInfo[statesInfo$illiteracy>1.0, 1:2]
hushaarStates = statesInfo[statesInfo$highSchoolGrad>60, ]