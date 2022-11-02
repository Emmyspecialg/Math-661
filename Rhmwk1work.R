# importing csv file 
citibike <- read.csv("201611-citibike-tripdata.csv", header=T)
citibikerides <- read.csv("2016Q4-RidershipMembership.csv", header=T)

#1 summary of csv file, how many bike rides?
summary(citibike)
nrow(citibike)

#2 comparing the amount of men vs women in pie chart, (Zero=unknown; 1=male; 2=female)
citibike.table <- table(citibike$Gender)
citibike.percent<-100*citibike.table/sum(citibike.table)
temp.name <- paste(format(citibike.percent, digits=4, trim=T), "%", sep="")
citibike.names <- paste(names(citibike.table), temp.name)
pie(citibike.table, labels=citibike.names, main="Citibike Gender (1=male,2=female,0=unknown)")
table(citibike$Gender)
table(citibike.percent)

#3 making a plot showing the number of riders vs the birth year 
plot(table(citibike$Birth.Year), main="Birth Year Frequency", xlab="Birth Year", ylab = "Frequency")
summary(citibike$Birth.Year)
min(citibike$Birth.Year, na.rm = TRUE)
table(citibike$Birth.Year)

#4 What is the proportion of customers who are non-annual subscribers? What proportion of rides are made by non-annual subscribers?

citibike.table2 <- table(citibike$User.Type)
citibike.percent<-100*citibike.table2/sum(citibike.table2)
temp.name <- paste(format(citibike.percent, digits=4, trim=T), "%", sep="")
citibike.names <- paste(names(citibike.table2), temp.name)
pie(citibike.table2, labels=citibike.names, main= "User Types Compared to Total Trips ")

day1 <- sum(citibikerides$X24.Hour.Passes.Purchased.Today[1],citibikerides$X3.Day.Passes.Purchased.Today[1])
day1proportion <- paste(format(day1/sum(day1,citibikerides$All.Time.Total.Annual.Members[1])*100),"%", sep="")
day1proportion

day2 <- sum(citibikerides$X24.Hour.Passes.Purchased.Today[46],citibikerides$X3.Day.Passes.Purchased.Today[46], na.rm = FALSE)
day2
day2proportion <- paste(format(day1/sum(day1,citibikerides$All.Time.Total.Annual.Members[46])*100),"%", sep="")
day2proportion

day3 <- sum(citibikerides$X24.Hour.Passes.Purchased.Today[-1],citibikerides$X3.Day.Passes.Purchased.Today[-1], na.rm = FALSE)
day3
day3proportion <- paste(format(day1/sum(day1,citibikerides$All.Time.Total.Annual.Members[-1])*100),"%", sep="")
day3proportion

difftimeproportion <- c(day1proportion,day2proportion,day3proportion)
difftimeproportion

#5 Time Plot
plot(citibikerides$X24.Hour.Passes.Purchased.Today, main = "Customer Purchases", xlab = "Days", ylab = "Number of Passes Purchased", type = 'l')
lines(citibikerides$X3.Day.Passes.Purchased.Today, col = 'blue')
legend("topright", legend=c("24 Hour Passes", "3 Day Passes"), fill = c("black","blue"))

#6 Duration of Trip- making histogram and summary... use IQR to find outliers to make a comment 
summary(citibike$Trip.Duration)
lowerlimit <- 369 - 1.5*IQR(citibike$Trip.Duration)
paste("Lower limit is ", lowerlimit)
higherlimit <- 1017 + 1.5*IQR(citibike$Trip.Duration)
paste("Higher limit is ", higherlimit)
hist(citibike$Trip.Duration)
betterduration <- citibike$Trip.Duration[citibike$Trip.Duration<=1989]
hist(betterduration, main = "Histogram of Trip Duration", xlab = "Seconds")

#7  Produce a time plot of the number of bike trips by day, from Oct 1, 2016 to Dec 31, 2016. Provide summary statistics. Comment.

plot(citibikerides$Trips.today, main = "Trips Per Day", xlab="Days", ylab = "Number of Trips", type = "l")
summary(citibikerides$Trips.today)

#8 Produce a histogram of the daily number of trips for the 4th quarter of 2016, and another histogram for only Oct 2016. Make it so that both histograms show the same range on the x-axis. 

hist(citibikerides$Trips.today, main = "Histogram of Trip Duration", nclass = 10, xlab = "Seconds", xlim = c(20000,70000))
octdur <- citibikerides$Trips.today[1:31]
hist(octdur, nclass=10, main = "October Trip Duration", xlab = "Seconds")

hist(citibikerides$Trips.today, main = "Daily Number of Trips", nclass = 10, xlab = "Trips", xlim = c(20000,70000))
octdur <- citibikerides$Trips.today[1:31]
hist(octdur, nclass=10, main = "October's Daily Number of Trips", xlab = "Trips", xlim = c(20000,70000))
#9a explain birth year and trip duration

#9b you cant do it explain 

#9c How many unique CitiBike stations were there in the dataset ? You should use end station because of there are more unique ones. 
length(table(citibike$Start.Station.Name))
length(unique(citibike$End.Station.Name))
length(table(c(citibike$Start.Station.ID,citibike$End.Station.ID)))

#9d
length(unique(citibike$Bike.ID))

#9e October 17, 2016, the max day 
max(citibikerides$X3.Day.Passes.Purchased.Today)
which.max(citibikerides$X3.Day.Passes.Purchased.Today)
which(citibikerides == 236, arr.ind = TRUE)

#9f cumulative trips before start 

starttriptotal <- citibikerides$Cumulative.trips.since.launch[1] - citibikerides$Trips.today[1]
starttriptotal
  

h