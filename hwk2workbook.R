datacounts <- read.csv("CovidData-NYStateDailyCounts.csv", header = T)
zipcodes <- read.csv("CovidData-NYCZipcodeCounts.csv", header = T)
library(ggplot2)
library(tidyverse)

#1a  A bar graph showing the number of zipcodes in each NYC borough, just histogram with one variable of boroughs 
qplot(zipcodes$BOROUGH_GROUP,zipcodes$MODIFIED_ZCTA) 
ggplot(zipcodes, mapping=aes(x=BOROUGH_GROUP,y=MODIFIED_ZCTA)) + geom_bar()
ggplot(data=zipcodes,mapping=aes(x=BOROUGH_GROUP)) + geom_bar() + labs(title= "", x="", y="count")

#1b Histogram of the number of Covid cases by zipcode, using a bin width of 100:
case.restrict <-  zipcodes %>% filter(COVID_CASE_COUNT < 5000)
ggplot(data=case.restrict) + geom_histogram(mapping=aes(x=COVID_CASE_COUNT), binwidth=100, stat="bin") + labs(title= "", x="COVID_CASE_COUNT", y="count")

#1c Similar to a histogram of the number of Covid cases by zipcode, using a bin width of 100, but with the bars represented by points instead:

a <- ggplot(zipcodes, aes(COVID_CASE_COUNT)) + xlim(0,4850)
a + stat_bin(binwidth = 100,geom = "point")

#1d Side-by-side boxplots of the total number of tests conducted in each zipcode:
ggplot(zipcodes, mapping=aes(x=BOROUGH_GROUP,y=TOTAL_COVID_TESTS)) + geom_boxplot()

#1e 
ggplot(zipcodes,aes(PERCENT_POSITIVE,COVID_DEATH_COUNT)) + geom_point(aes(shape = BOROUGH_GROUP, color =BOROUGH_GROUP), size = 1)

#1f 
ggplot(zipcodes, aes(x=COVID_CASE_COUNT,y=COVID_DEATH_COUNT)) + facet_grid(BOROUGH_GROUP~.) + geom_point() + geom_smooth(method="lm", formula = y ~ x,  aes(color="red")) + theme(legend.position = "none")

#1gHELP daily number of cases and deaths time plot 
ggplot(datacounts, aes(x=locations, y=Cases)) + geom_point() + geom_line(aes(x=locations,y=Deaths), color='grey') + labs(title="", x= "Day 1 is Feb 29, 2020", y="Number of Cases and Deaths")

nrow(datacounts)
locations <- c(1:127)

#1h Scatter plot of the daily number of deaths vs daily number of cases:

ggplot(datacounts,aes(Cases,Deaths)) + geom_point(aes(color =Hospitalizations), shape= "+", size=2) 

#1i 
propdtoc <- datacounts$Deaths/datacounts$Cases
ggplot(datacounts,aes(Cases,propdtoc)) + geom_point(aes(color =Hospitalizations), na.rm = TRUE, size=1) + labs(title="", x="Cases", y="Proportion of death to cases")

#2a , how can you find the total population ?? 
sum(datacounts$Cases)
sum(zipcodes$POP_DENOMINATOR)
sum(zipcodes$COVID_CASE_COUNT)

#2b
num_col <- unlist(lapply(zipcodes,is.numeric))
num_col
data_num <- zipcodes[ ,num_col]
data_num
sapply(data_num, FUN=summary)

#2c
which(zipcodes=="Manhattan",arr.ind=TRUE)
summary(zipcodes$COVID_CASE_COUNT[1:44])

#2d 
which(zipcodes=="Staten Island",arr.ind=TRUE)
stateni <- summary(zipcodes$COVID_CASE_COUNT[45:56]) 
stateni
which(zipcodes=="Queens",arr.ind=TRUE)
queens <- summary(zipcodes$COVID_CASE_COUNT[82:177])
queens

#2e The highest case rate has a NYC zip-code of 11369 and a neighborhood name of Airport/East Elmhurst
max(zipcodes$COVID_CASE_RATE)
which(zipcodes==4563.63,arr.ind=TRUE)
zipcodes[141,]

#2f 
deathsprop <- datacounts$Deaths/datacounts$Hospitalizations
which.max(deathsprop)
max(deathsprop)
summary(deathsprop)
datacounts[126,]
#3a. 
e <- seq(2,24,by=2)
e[5]

#3b.
rev(e)

#3c
j <- c(rep(1,50), rep(2,26),rep(3,17))
j

#3d
g <- c(1:10)
diag(g,length(g))
g <- diag(seq(1,10))
g
solve(g)
#3e Extract the 5th column of the matrix you created in 3(d). Make this vector (i.e the 5th column) into a 2 × 5 matrix, filling out the matrix by row.

g <- diag(seq(1,10))
g[,5]
k <- g[,5]
matrix(k, nrow=2, ncol = 5, byrow=T)










