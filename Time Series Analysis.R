
df <- read.csv("C:/Users/Abhishek Benjamin/Desktop/Download -2/Statistics/Learnbay Data Set/opsd_germany_daily.csv", header=T)

str(df$Date)


# Converting Date into Date Format

x <- as.Date(df$Date)

moddate <- as.Date(x,format = "%m/%d/%Y")

df1 <- cbind(moddate,df)

str(df1)
head(df1)

## Another Method for Extracting Year, Month & Date

year <- as.numeric(format(x,"%Y"))
month <- as.numeric(format(x,"%m"))
date <- as.numeric(format(x,"%d"))

df1 <- cbind(df, year, month, date)

head(df1)


# Plotting time Series

min(df1$Wind, na.rm = T)
max(df1$Wind, na.rm=T)

min(df1$Solar, na.rm = T)
max(df1$Solar, na.rm = T)

min(df1$Consumption, na.rm = T)
max(df1$Consumption, na.rm = T)

min(df1$Wind.Solar, na.rm = T)
max(df1$Wind.Solar, na.rm = T)

## Plotting Graphs

plot1 <- plot(df1[,2], xlab = "Time", ylab="Yearly Usage(GwH)", type="l",
     col= "red", xlim = c(2006,2018), ylim= c(840,1710), main= "Annual Consumption ")


# library(ggplot2)
# ggplot(data= df, aes(x = df1$moddate, y = df1$Consumption))+ geom_line(col="red")


plot2 <- plot(df1[,3], xlab = "Time", ylab="Yearly Usage(GwH)", type="l",
     col= "red", xlim = c(2006,2018), ylim= c(0,850), main= "Annual Wind")


plot3 <- plot(df1[,4], xlab = "Time", ylab="Yearly Usage(GwH)", type="l",
     col= "red",ylim= c(0,250), main= "Annual Solar")


plot4 <- plot(df1[,5], xlab = "Time", ylab="Yearly Usage(GwH)", type="l",
     col= "red", ylim= c(10,900), main= "Annual Solar + Wind")



# For Plotting Specific Year or Month Consumption Graph using Subsetting

df_2017 <- subset(df1,subset = moddate>= "2017-01-01" & moddate <="2017-12-31")

df_jan_2017 <- subset(df1, subset = moddate>="2017-01-01" & moddate <="2017-01-31")

head(df_2017)
str(df_2017)

head(df_jan_2017)
str(df_jan_2017) 

# Plotting Consumption for year 2017

plot5 <- plot(df_2017[,2],xlab= "Month", ylab = "Monthly Usage(GwH)",
               type="l",lwd=2, col= "red", main="2017 Consumption Graph")

# Plotting Consumption for Jan 2017

plot6 <- plot(df_jan_2017[,2], xlab = "days", ylab = "Daily Usage(GwH)",
             type="l", lwd=2, col = "red", main ="Jan 2017 Consumption Graph")

boxplot(df_2017$Consumption)
boxplot(df_jan$Consumption)

head(df1)


boxplot(df1$Consumption, main="Consumption-Boxplot", ylim = c(800,1750))

par(mfrow = c(3,1))

# Yearly Boxplot of Consumption

boxplot(df1$Consumption ~ df1$year, main=" Yearly Consumption-Boxplot", ylim = c(800,1750), las=1)


# Monthly Boxplot of Consumption

boxplot(df1$Consumption ~ df1$month, main="Monthly Consumption-Boxplot", ylim = c(800,1750), las=1)


summary(df1)
head(df1)
colSums(!is.na(df1))
colSums(is.na(df1))
sum(is.na(df1))

# Selecting Missing values in Wind Column keeping moddate,Solar, Wind
# & Consumption columns in mind

selwind1 <- df1[which(is.na(df$Wind)), names(df1) %in% c("Wind","Solar","moddate","Consumption")]

selwind1[1:10,]

# Selecting from Wind Column where there is no Missing Values keeping moddate,Solar, Wind
# & Consumption columns in mind

selwind2 <- df1[which(!is.na(df$Wind == "2011")), names(df1) %in% c("Wind","Solar","moddate","Consumption")]

selwind2[1:10,]

selwind3 <- df1[which(df1$year == "2011"), names(df1) %in% c("Wind","Solar","Consumption","moddate")]

selwind3[1:10,]

colSums(is.na(selwind3))

## Finding which Row has missing value

selwind4 <- selwind3[which(is.na(selwind3$Wind)), names(selwind3) %in% c("Wind","Solar","Consumption","moddate")]

selwind4

rm(list=ls())
