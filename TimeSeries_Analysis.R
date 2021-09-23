########################## Time Series Analysis ############################## 

# Lets import the data
mydata<-read.csv("opsd_germany_daily.txt",header = TRUE,row.names = "Date")
mydata

# Lets see the first six rows
head(mydata)

# Lets see the last six rows
tail(mydata)

# Lets view the data in tabular format
View(mydata)

# Lets retrieve the dimensions of the data
dim(mydata)

# Lets check the datatype of each column in the dataframe
str(mydata)

# Looking at the row names
row.names(mydata)

# Accessing a specific row
mydata["2006-01-01",]
mydata["2007-08-10",]

# Accessing multiple rows
mydata[c("2006-01-01","2007-08-10"),]

# Lets see the summary
summary(mydata)

# Lets import the data again, but this time without parsing the date column
mydata2<-read.csv("opsd_germany_daily.txt",header = TRUE)
mydata2

# Lets look at the date column
str(mydata2$Date)

# Lets convert the Date into date format
x<-as.Date(mydata2$Date)
head(x) # Lets see the first six rows
class(x) # Lets see the class
str(x)   # Lets see the structure

# Lets create the year,month and day column
year<-as.numeric(format(x,"%Y"))
head(year)

month <- as.numeric(format(x,"%m"))
head(month)

day <- as.numeric(format(x,"%d"))
head(day)

# Lets see the actual data frame
head(mydata2)

# Lets add the year,month and day column to the existing data frame
mydata2 <- cbind(mydata2,year,month,day)
head(mydata2)

mydata2[1:3,] # Displays the first three rows

head(sample(mydata2,8)) # Displays a smaple of the data

# Lets create a Line Plot of the full time series of Germany
## Option 1:
plot(mydata2$year,mydata2$Consumption,type = "l",xlab = "Year",ylab = "Consumption" )

# Option2:
par(mfrow=c(1,1))
plot(mydata2[,2])

# Option3:
plot(mydata2[,2],xlab = "Year",ylab = "Consumpton")
plot(mydata2[,2],xlab = "Year",ylab = "Consumpton",type = "l",lwd=2,col = "blue")
plot(mydata2[,2],xlab = "Year",ylab = "Consumpton",type = "l",lwd = 2,xlim=c(0,2018))
plot(mydata2[,2],xlab = "Year",ylab = "Consumpton",type = "l",lwd = 2,xlim=c(2006,2018))
plot(mydata2[,2],xlab = "Year",ylab = "Consumpton",type = "l",lwd = 2,xlim=c(2006,2018),ylim = c(900,2000),main = "Consumption Graph")

# Taking log values of consumption and taking differences of log
plot(10*diff(log(mydata2[,2])),xlab = "Year",ylab = "Consumption",type = "l",lwd = 2,ylim = c(-5,5),main = "Consumption Graph",col = "orange")

# Using ggplot()
install.packages("ggplot2")
library(ggplot2)

# Option1:
ggplot(mydata2,type = "o")+geom_line(aes(x=year,y=Consumption))

# Option2:
ggplot(data = mydata2,aes(x=year,y=Consumption,group=1))+geom_line()+geom_point()

# Option3:
ggplot(data = mydata2,aes(x=year,y=Consumption,group=1))+geom_line(linetype="dashed")+geom_point()
ggplot(data = mydata2,mapping = aes(x=year,y=Consumption,col="red"))+geom_point()

# We can see that the plot() method has chosen pretty good tick locations.
#(every two years) and labels(the years) for the x-axis which is helpful.
# However,with so many data points, the line plot is crowded and hard to read.
# Thus we can go with the plot()

# Lets plot the data considering Solar and wind time series too
## Lets look at the data first
mydata2

# Lets see the min and max of Consumption column
min(mydata2[,2],na.rm=T)
max(mydata2[,2],na.rm=T)
# Lets see the min and max of wind column
min(mydata2[,3],na.rm=T)
max(mydata2[,3],na.rm=T)
# Lets see the min and max of Solar column
min(mydata2[,4],na.rm=T)
max(mydata2[,4],na.rm=T)
# Lets see the min and max of wind + Solar column
min(mydata2[,5],na.rm=T)
max(mydata2[,5],na.rm=T)

# For multiple plots
par(mfrow=c(3,1))

plot1 <- plot(mydata2[,2],xlab = "Year",ylab = "Daily Total",type = "l", lwd = 2,main = "Consumption",col = "orange",xlim = c(2006,2018),ylim=c(840,1750))

plot1 <- plot(mydata2[,1],mydata2[,2],xlab = "Year",ylab = "Daily Total",type = "l", lwd = 2,main = "Consumption",col = "orange",xlim = c(2006,2018),ylim=c(840,1750))


plot2 <- plot(mydata2[,4],xlab = "Year",ylab = "Daily Totals",type = "l",main = "Solar",xlim=c(2006,2018),ylim=c(0,500),col="blue")

plot2 <- plot(mydata2[,1],mydata2[,4],xlab = "Year",ylab = "Daily Totals", type = "l",main = "Solar",xlim(2006,2018),ylim = c(0,500),col = "blue")


plot3 <- plot(mydata2[,3],xlab = "Year", ylab = "Daily Totals", type = "l", lwd = 2, main = "Wind",xlim(2006,2018), ylim = c(0.900),col = "blue")

plot3 <- plot(mydata2[,1],mydata2[,3],xlab = "Year", ylab = "Daily Totals", type = "l", lwd = 2, main = "Wind",xlim(2006,2018), ylim = c(0.900),col = "red")

# Lets plot time series in a single year to investigate further
str(mydata2)
x <- as.Date(mydata2$Date) # Converting into Date
head(x)
class(x)
str(x)

# To convert Date column into date format
moddate <- as.Date(x,format = "%m/%d/%Y")
str(moddate)
mydata3 <- cbind(moddate,mydata2) # adding column

head(mydata3)
str(mydata3)

# Lets extract the data we want
mydata4 = subset(mydata3,subset = mydata3$moddate >= "2017-01-01" & mydata3$moddate <= "2017-12-31")
head(mydata4)

plot4 <- plot(mydata4[,1],mydata4[,3],xlab="Year",ylab="Daily Totals",type="l",lwd=2,main="Consumption",col="orange",)

# Zooming in further
mydata4 = subset(mydata3,subset = mydata3$moddate >= "2017-01-01" & mydata3$moddate <= "2017-02-28")
head(mydata4)

xmin <-min(mydata4[,1],na.rm=T)
xmin
xmax <-max(mydata4[,1],na.rm=T)
xmax
ymin <-min(mydata4[,3],na.rm=T)
ymin
ymax <-max(mydata4[,3],na.rm=T)
ymax

plot4 <- plot(mydata4[,1],mydata4[,3],xlab = "year",ylab = "daily total",type"l",lwd=2,main = "Consumption",col = "orange",xlim=c(xmin,xmax),ylim=c(ymin,ymax))

grid()
# Adding Horizontal line
abline(h=c(13000,1500,1600))
# Adding dashed blue vertical line
abline(v=seq(xmin,xmax,7),lty=2,col="blue")

# Boxplots
boxplot(mydata3$Consumption)
boxplot(mydata3$Solar)
boxplot(mydata3$Wind)

# Boxplot is visual display of 5 number summary
quantile(mydata3$Consumption, probs = c(0,0.25,0.5,0.75,1))

boxplot(mydata3$Consumption,main="Consumption",ylab="Consumption",ylim=c(600,1500))

# Yearly
boxplot(mydata3$Consumption~mydata3$year,main="Consumption",ylab="Consumption",xlab="Years",ylim=c(600,1500))

boxplot(mydata3$Consumption~mydata3$year,main="Consumption",ylab="Consumption",xlab="Years",ylim=c(600,1500),las=1)

# Monthly
boxplot(mydata3$Consumption~mydata3$month,main="Consumption",ylab="Consumption",xlab="Months",ylim=c(600,1500),las=1)

# Multiple plots
par(mfrow=c(3,1))

boxplot(mydata3$Wind~mydata3$month,main="Wind",ylab="Wind",xlab="Months",ylim=c(0,900),las=1,col="blue")

boxplot(mydata3$Consumption~mydata3$month,main="Consumption",ylab="Consumption",xlab="Months",ylim=c(600,1500),las=1,col="red")



boxplot(mydata3$Solar~mydata3$month,main="Solar",ylab="Solar",xlab="Months",ylim=c(0,200),las=1,col="green")

# Days
par(mfrow=c(1,1))

boxplot(mydata3$Consumption~mydata3$day,main="Consumption",ylab="Consumption",xlab="Days",ylim=c(600,1600),las=1,col="green")

# Lets check for null values
mydata3
library("dplyr")
summary(mydata3)
colSums((! is.na(mydata3))) # Shows the not null values
sum(is.na(mydata3$Consumption))
sum(is.na(mydata3$Wind))
sum(is.na(mydata3$Solar))
sum(is.na(mydata3$Wind.Solar))

# Frequency
xmin <- min(mydata3[,1],na.rm=T)
xmin
freq <- seq(from=xmin,by="day",length.out=5)
freq
typeof(freq)
class(freq)

freq2 <- seq(from=xmin,by="month",length.out=5)
freq2

freq3 <- seq(from=xmin,by="year",length.out=5)
freq3

# Lets select data which has NA value for Wind
selwind1 <- mydata3[which(is.na(mydata3$Wind)),names(mydata3) %in% c("moddate","Consumption","Wind","Solar")]
selwind1[1:10,]
View(selwind1)                    
# Lets select data which does not have NA values for wind
selwind2 <- mydata3[which(!is.na(mydata3$Wind)),names(mydata3) %in% c("moddate","Consumption","Wind","Solar")]
selwind2[1:10,]
View(selwind2)

# Looking at the results of the above two we know that year 2011 has wind column with some missing values
selwind3 <- mydata3[which(mydata3$year == "2011"),names(mydata3)%in% c("moddate","Consumption","Wind","Solar")]
selwind3[1:10,]
class(selwind3)
View(selwind3)

# Number of rows in the resultamt dataframe
nrow(selwind3)

# Earlier we checked total number of NA per column
# Lets find the number of NA values for a particular year
sum(is.na(mydata3$Wind[which(mydata3$year == "2011")]))

# Non NA values 
sum(!is.na(mydata3$Wind[which(mydata3$year == "2011")]))

str(selwind1)

# Lets extract that specific row containing the NA value of Wind column for 2011
selwind4 <- selwind3[which(is.na(selwind3$Wind)),names(selwind3) %in% c("modedate","Consumption","Wind","Solar")]
selwind4

# We know that data follows a day wise frequency
# Lets select data which has NA and non NA values
test1 <- selwind3[which(selwind3$moddate > "2011-12-12"& selwind3$moddate < "2011-12-16"), names(selwind3) %in% c("moddate","Consumption","Wind","Solar")]
test1
class(test1)
str(test1)

# Handling missing values
library(tidyr)
test1 %>% fill(Wind) # Missing values gets replaced with forward fill

# Thus we can take care of missing values using direction

install.packages("zoo")
library("zoo")

test_0jda = zoo::rollmean(mydata3$Consumption,k=3,fill=NA)

str(test_0jda)

# Trend Analysis looking at rolling mean
mydata3

threedayTest <- mydata3 %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(test_0jda=zoo::rollmean(Consumption,k=3,fill=NA),)
%>%
dplyr::ungroup()                  
threedayTest

# Lets calculate 7days and 365 days rolling mean for consumption
mydataTest <- mydata3 %>%
  dplyr::arrange(desc(year)) %>%
  dplyr::group_by(year) %>%
  dplyr::mutate(test_0jda=zoo::rollmean(Consumption,k=7,fill=NA),test_365da = zoo::rollmean(Consumption,k=365,fill=NA)) %>%
  dplyr::ungroup()

# Lets check the results
mydataTest %>%
  dplyr::arrange(moddate) %>%
  dplyr::filter(year==2017) %>%
  dplyr::select(Consumption,
                moddate,
                year,
                test_0jda:test_365da) %>%
  utils::head(7)

mydataTest$test_0jda
mydataTest$test_365da

# Lets plot
par(mfrow=c(1,1))
plot(mydataTest$Consumption,xlab = "year",ylab = "Consumption",type="l",lwd=2,col="blue",main="Consumption Graph")

points(mydataTest$test_0jda,type="l",lwd=2,xlim=c(2000,2018),yli=c(900,2000),col="orange")

lines(mydataTest$test_365da,type="l",lwd=5,xlim=c(2000,2018),yli=c(900,2000),col="black")

legend(2500,1600,legend=c("mydataTest$Consumption","mydataTest$test_0jda","mydataTest$test_365da"),col=c("blue","orange","black"),pch=c("o","*","+"),lty=c(1,2,3),mcol=1)