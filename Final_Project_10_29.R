# Bee Population Final Project - Brendan Hogan, Erin Ospeck, Casey Samagalsky, Celeste Sowell

# Loading Required Libraries
library(forecast)
library(ggplot2)
library(dplyr)
library(MASS)
library(tidyr)

### PREPROCESSING
##DATA MANIPULATION

# Loading Bee Dataset
bees_state <- read.csv("Bee Population by State 1987-2017.csv", stringsAsFactors=TRUE)
bees_state$Region <- NA
bees_state$Population <- as.numeric(gsub(",", "", bees_state$Population))

## Loading Weather Datasets

# Precipitation
precip <- read.csv('State_Precip.csv')
names(precip)[1] <- "YEAR"
precip$Region <- NA
precip <- gather(precip, State, Precipitation, ALABAMA:WYOMING)
precip <- precip[ -c(2:3) ]

# Temperature
temp <- read.csv('State_Avg_Temp.csv')
names(temp)[1] <- "YEAR"
temp$Region <- NA
temp <- gather(temp, State, Temperature, ALABAMA:WYOMING)
temp <- temp[ -c(2:3) ]

# Creating Region Vectors
Region <- list("Northeast", "Midwest", "South", "West")
Northeast <- c("CONNECTICUT", "MAINE", "MASSACHUSETTS", "NEW HAMPSHIRE", "RHODE ISLAND", "VERMONT", "NEW JERSEY", "NEW YORK", "PENNSYLVANIA")
Midwest <- c("ILLINOIS", "INDIANA", "MICHIGAN", "OHIO", "WISCONSIN", "IOWA", "KANSAS", "MINNESOTA", "MISSOURI", "NEBRASKA", "NORTH DAKOTA", "SOUTH DAKOTA")
South <- c("DELAWARE", "FLORIDA", "GEORGIA", "MARYLAND", "NORTH CAROLINA", "SOUTH CAROLINA", "VIRGINIA", "DISTRICT OF COLUMBIA", "WEST VIRGINIA", "ALABAMA", "KENTUCKY", "MISSISSIPPI", "TENNESSEE", "ARKANSAS", "LOUISIANA", "OKLAHOMA", "TEXAS")
West <- c("ARIZONA", "COLORADO", "IDAHO", "MONTANA", "NEVADA", "NEW MEXICO", "UTAH", "WYOMING", "ALASKA", "CALIFORNIA", "HAWAII", "OREGON", "WASHINGTON")

# Aggregating bee data across regions
for(i in 1:nrow(bees_state)){
  if(bees_state$State[i] %in% Northeast){
    bees_state$Region[i] <- "Northeast" 
  }else if(bees_state$State[i] %in% Midwest){
    bees_state$Region[i] <- "Midwest"
  }else if(bees_state$State[i] %in% South){
    bees_state$Region[i] <- "South"
  }else{
    bees_state$Region[i] <- "West"
  }
}
bees_region <- aggregate(bees_state$Population, by=list(Year=bees_state$Year, Region=bees_state$Region), FUN=sum)
bees_region
bees_total <- aggregate(bees_region$x, by=list(Year=bees_region$Year), FUN=sum)


# Aggregating Weather data across regions

# Precipitation
for(i in 1:nrow(precip)){
  if(precip$State[i] %in% Northeast){
    precip$Region[i] <- "Northeast" 
  }else if(precip$State[i] %in% Midwest){
    precip$Region[i] <- "Midwest"
  }else if(precip$State[i] %in% South){
    precip$Region[i] <- "South"
  }else{
    precip$Region[i] <- "West"
  }
}

precip <- aggregate(precip$Precipitation, by=list(Year=precip$YEAR, Region=precip$Region), FUN=sum)
precip <- rename(precip, c("Precipitation"="x"))
precip

# Temperature

for(i in 1:nrow(temp)){
  if(temp$State[i] %in% Northeast){
    temp$Region[i] <- "Northeast" 
  }else if(temp$State[i] %in% Midwest){
    temp$Region[i] <- "Midwest"
  }else if(temp$State[i] %in% South){
    temp$Region[i] <- "South"
  }else{
    temp$Region[i] <- "West"
  }
}

temp <- aggregate(temp$Temperature, by=list(Year=temp$YEAR, Region=temp$Region), FUN=mean)
temp <- rename(temp, c("Temperature"="x"))
temp

# Joining together weather data and bee data & creating a time series for the entire US. 
weather <- inner_join(temp, precip, by = c('Year','Region'))
bees_weather <- inner_join(bees_region, weather, by = c('Year','Region'))
bees_weather.ts <- ts(bees_weather, start = c(1987), end = c(2017), freq = 1)

# Subsetting each region out of main data set 
bees_midwest <- subset(bees_region, Region == "Midwest", select = c("Year", "x"))
bees_northeast <- subset(bees_region, Region == "Northeast", select = c("Year", "x"))
bees_south <- subset(bees_region, Region == "South", select = c("Year", "x"))
bees_west <- subset(bees_region, Region == "West", select = c("Year", "x"))

# Subsetting regions from weather data set
bees_weather_midwest <- subset(bees_weather, Region == "Midwest", select = c("Year", "x", "Temperature", "Precipitation"))
bees_weather_northeast <- subset(bees_weather, Region == "Northeast", select = c("Year", "x", "Temperature", "Precipitation"))
bees_weather_south <- subset(bees_weather, Region == "South", select = c("Year", "x", "Temperature", "Precipitation"))
bees_weather_west <- subset(bees_weather, Region == "West", select = c("Year", "x", "Temperature", "Precipitation"))


## Creating time series for each region

# Bee Data
bees.ts <- ts(bees_total$x, start = c(1987, 1), end = c(2017, 1), freq = 1)
midwest.ts <- ts(bees_midwest$x, start = c(1987, 1), end = c(2017, 1), freq = 1)
northeast.ts <- ts(bees_northeast$x, start = c(1987, 1), end = c(2017, 1), freq = 1)
south.ts <- ts(bees_south$x, start = c(1987, 1), end = c(2017, 1), freq = 1)
west.ts <- ts(bees_west$x, start = c(1987, 1), end = c(2017, 1), freq = 1)

# Weather Data
midwest_weather.ts <- ts(bees_weather_midwest, start = c(1987), end = c(2017), freq = 1)
northeast_weather.ts <- ts(bees_weather_northeast, start = c(1987), end = c(2017), freq = 1)
south_weather.ts <- ts(bees_weather_south, start = c(1987), end = c(2017), freq = 1)
west_weather.ts <- ts(bees_weather_west, start = c(1987), end = c(2017), freq = 1)


# Visualizing Past Populations
options(scipen=999)
x11()
ts.plot(bees.ts, gpars=list(xlab="Year", ylab="Total Population", 
                            main="Historical Bee Populations (1987-2017)",
                            col="Yellow", lwd=3))

x11()
ts.plot(midwest.ts, northeast.ts, south.ts, west.ts, 
        gpars=list(xlab = "Year", ylab = "Population", 
                   main = "Bee Populations 1987-2017 by Region",
                   ylim=c(0, 1525000), col=1:4, lwd=3))
legend("topright", lty=1, col=1:4, box.lty=0, lwd=3,
       legend=c("Midwest", "Northeast", "South", "West"))
options(scipen=0)


# Creating Valid and Training data for each Region
nValid <- 4
nTrain <- length(south.ts) - nValid

## South
south.train.ts <- window(south.ts, start = c(1987, 1), end = c(1987, nTrain))
south.valid.ts <- window(south.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

south.weather.train.ts <- window(south_weather.ts, start = c(1987, 1), end = c(1987, nTrain))
south.weather.valid.ts <- window(south_weather.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

# Northeast
northeast.train.ts <- window(northeast.ts, start = c(1987, 1), end = c(1987, nTrain))
northeast.valid.ts <- window(northeast.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

northeast.weather.train.ts <- window(northeast_weather.ts, start = c(1987, 1), end = c(1987, nTrain))
northeast.weather.valid.ts <- window(northeast_weather.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

# Midwest
midwest.train.ts <- window(midwest.ts, start = c(1987, 1), end = c(1987, nTrain))
midwest.valid.ts <- window(midwest.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

midwest.weather.train.ts <- window(midwest_weather.ts, start = c(1987, 1), end = c(1987, nTrain))
midwest.weather.valid.ts <- window(midwest_weather.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

# West
west.train.ts <- window(west.ts, start = c(1987, 1), end = c(1987, nTrain))
west.valid.ts <- window(west.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))

west.weather.train.ts <- window(west_weather.ts, start = c(1987, 1), end = c(1987, nTrain))
west.weather.valid.ts <- window(west_weather.ts, start = c(1987, nTrain + 1), end = c(1987, nTrain + nValid))


# Plotting South Population Training and Validation

options(scipen=999)
plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2018), main = "South Bee Population")
axis(1, at = seq(1987, 2018, 1), labels = format(seq(1987, 2018, 1)))

# Add Validation Data 
lines(south.valid.ts)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")


### PREDICTIVE ANALYTICS PROJECT

## SIMPLE METHOD ANALYSIS

## Mean Method

south.mean.pred <- meanf(south.train.ts, h = nValid)

# Plot Population
plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2018), main = "South Bee Population Mean Method")
axis(1, at = seq(1987, 2018, 1), labels = format(seq(1987, 2018, 1)))

# Add Mean Forecast 
lines(south.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Mean Fitted Values 
lines(south.mean.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(south.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")

accuracy(south.mean.pred, south.valid.ts)


## Naive Method 

south.naive.pred <- naive(south.train.ts, h = nValid)

# Plot Population
options(scipen=999)
plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2018), main = "South Bee Population Naive Method")
axis(1, at = seq(1987, 2018, 1), labels = format(seq(1987, 2018, 1)))

# Add Naive Mean Forecast 
lines(south.naive.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Naive Fitted Values 
lines(south.naive.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(south.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")

accuracy(south.naive.pred, south.valid.ts)

## Drift Method

south.drift.pred <- rwf(south.train.ts, drift=TRUE, h= nValid)

# Plot Population
plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2018), main = "South Bee Population Drift Method")
axis(1, at = seq(1987, 2018, 1), labels = format(seq(1987, 2018, 1)))

# Add Drift Mean Forecast 
lines(south.drift.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Drift Fitted Values 
lines(south.drift.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(south.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")

accuracy(south.drift.pred, south.valid.ts)


# Plot All Forecasts

plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2018), main = "South Bee Population")
axis(1, at = seq(1987, 2018, 1), labels = format(seq(1987, 2018, 1)))

# Add Mean Forecast 
lines(south.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Naive Mean Forecast 
lines(south.naive.pred$mean, lwd = 2, col = "green", lty = 2)

# Add Drift Mean Forecast 
lines(south.drift.pred$mean, lwd = 2, col = "red", lty = 2)

# Add Validation Data 
lines(south.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")

# Add Legend
legend(1987, 500000, legend=c("Mean", "Naive", "Drift"),
       col=c("blue", "green", "red"), lty = 2)


# Find which model is most accurate 
accuracy(south.mean.pred, south.valid.ts)
accuracy(south.naive.pred, south.valid.ts)
accuracy(south.drift.pred, south.valid.ts)

## Mean method is most accurate out of simple methods


## Time Series Regression Analysis

# Create the linear model for the South
south.weather.train.lm <- tslm(x ~ trend  + I(trend^2) + Temperature + Precipitation, data=south.weather.train.ts)
south.weather.train.lm.pred <- forecast(south.weather.train.lm$fitted.values, h = nValid)

# Plot the Southern model
options(scipen=999)
plot(south.train.ts,  ylab = "Bee Population", xlab = "Time", 
     bty = "l", xaxt = "n", ylim = c(350000,850000), xlim = c(1987, 2020), main = "South Bee Population Regression")
axis(1, at = seq(1987, 2025, 1), labels = format(seq(1987, 2025, 1)))
lines(south.weather.train.lm.pred$fitted, lwd = 2, col = "blue")
lines(south.weather.train.lm.pred$mean, lwd = 2, col = "blue", lty = 2)
lines(south.valid.ts, col = "grey20", lty = 3)

# Check Southern Accuracy
accuracy(south.weather.train.lm.pred, south.valid.ts)

lines(c(2017 - 3, 2017 - 3), c(0, 800000))
lines(c(2017, 2017), c(0, 800000))

text(1999, 820000, "Training")
text(2015.5, 820000, "Validation")

# Create the linear model for the Midwest
midwest.weather.train.lm <- tslm(x ~ trend + I(trend^2) + Temperature + Precipitation, data=midwest.weather.train.ts)
midwest.weather.train.lm.pred <- forecast(midwest.weather.train.lm$fitted.values, h = nValid)

# Plot the Midwestern model
plot(midwest.train.ts, ylab = "Bee Population", xlab = "Time", 
     bty = "l", xaxt = "n", ylim = c(900000,1400000), xlim = c(1987, 2020), main = "Midwest Bee Population Regresssion")
axis(1, at = seq(1987, 2025, 1), labels = format(seq(1987, 2025, 1)))
lines(midwest.weather.train.lm.pred$fitted, lwd = 2, col = "blue")
lines(midwest.weather.train.lm.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Validation Data 
lines(midwest.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 1350000))
lines(c(2017, 2017), c(0, 1350000))

# Add Text Labels 
text(1999, 1380000, "Training")
text(2015.5, 1380000, "Validation")

# Compute midwest accuracy
accuracy(midwest.weather.train.lm.pred, midwest.valid.ts)


# Create the linear model for the West
west.weather.train.lm <- tslm(x ~ trend + I(trend^2) + Temperature + Precipitation, data=west.weather.train.ts)
west.weather.train.lm.pred <- forecast(west.weather.train.lm$fitted.values, h = nValid)

# Plot the Western model
plot(west.train.ts, ylab = "Bee Population", xlab = "Time", 
     bty = "l", xaxt = "n", ylim = c(800000,1400000), xlim = c(1987, 2020), main = "West Bee Population Regresssion")
axis(1, at = seq(1987, 2025, 1), labels = format(seq(1987, 2025, 1)))
lines(west.weather.train.lm.pred$fitted, lwd = 2, col = "blue")
lines(west.weather.train.lm.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Validation Data 
lines(west.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 1350000))
lines(c(2017, 2017), c(0, 1350000))

# Add Text Labels 
text(1999, 1380000, "Training")
text(2015.5, 1380000, "Validation")

# Compute West accuracy
accuracy(west.weather.train.lm.pred, west.valid.ts)


# Create the linear model for the Northeast
northeast.weather.train.lm <- tslm(x ~ trend + I(trend^2) + Temperature + Precipitation, data=northeast.weather.train.ts)
northeast.weather.train.lm.pred <- forecast(northeast.weather.train.lm$fitted.values, h = nValid)

# Plot the Northeastern model
plot(northeast.train.ts, ylab = "Bee Population", xlab = "Time", 
     bty = "l", xaxt = "n", ylim = c(80000,234000), xlim = c(1987, 2020), main = "Northeast Bee Population Regresssion")
axis(1, at = seq(1987, 2025, 1), labels = format(seq(1987, 2025, 1)))
lines(northeast.weather.train.lm.pred$fitted, lwd = 2, col = "blue")
lines(northeast.weather.train.lm.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Validation Data 
lines(northeast.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 229000))
lines(c(2017, 2017), c(0, 229000))

# Add Text Labels 
text(1999, 233000, "Training")
text(2015.5, 233000, "Validation")

# Compute West accuracy
accuracy(northeast.weather.train.lm.pred, northeast.valid.ts)

## ARIMA
# Creating Model for South
south.arima <- auto.arima(south.train.ts)
south.arima.forecast <- forecast(south.arima)

Acf(south.arima.forecast$residuals, lag.max = 10, ylab="ACF of Arima Residuals")

#Plotting the Model
options(scipen=999)
autoplot(south.arima.forecast, main="Forecasts from ARIMA(0,1,0)  -  South")

#Determining Accuracy
accuracy(south.arima.forecast, south.valid.ts)

# Checking the other Regions
northeast.arima <-auto.arima(northeast.train.ts)
autoplot(forecast(northeast.arima))

west.arima <- auto.arima(west.train.ts)
autoplot(forecast(west.arima))

midwest.arima <- auto.arima(midwest.train.ts)
midwest.arima.forecast <- forecast(midwest.arima)
autoplot(midwest.arima.forecast, main="Forecasts from ARIMA(2,2,0)  -  Midwest")
midwest.arima

Acf(midwest.arima.forecast$residuals, lag.max = 10, ylab="ACF of Arima Residuals")

accuracy(midwest.arima.forecast, midwest.valid.ts)


## CONCLUSION

# Use mean method to forecast bee population in 2018 and 2019 for all regions

## Midwest

midwest.mean.pred <- meanf(midwest.train.ts, h = 6)

# Plot Population
plot(midwest.train.ts, ylim = c(900000, 1300000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2020), main = "Midwest Bee Population Mean Method")
axis(1, at = seq(1987, 2020, 1), labels = format(seq(1987, 2020, 1)))

# Add Mean Forecast 
lines(midwest.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Mean Fitted Values 
lines(midwest.mean.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(midwest.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 1250000))
lines(c(2017, 2017), c(0, 1250000))

# Add Text Labels 
text(1999, 1300000, "Training")
text(2015.5, 1300000, "Validation")
text(2020, 1300000, "Future")

# Find Value 
mean(midwest.valid.ts)


## Northeast

northeast.mean.pred <- meanf(northeast.train.ts, h = 6)

# Plot Population
plot(northeast.train.ts, ylim = c(75000, 225000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2020), main = "Northeast Bee Population Mean Method")
axis(1, at = seq(1987, 2020, 1), labels = format(seq(1987, 2020, 1)))

# Add Mean Forecast 
lines(northeast.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Mean Fitted Values 
lines(northeast.mean.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(northeast.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 200000))
lines(c(2017, 2017), c(0, 200000))

# Add Text Labels 
text(1999, 225000, "Training")
text(2015.5, 225000, "Validation")
text(2020, 225000, "Future")

# Find Value 
mean(northeast.valid.ts)


## South

south.mean.pred <- meanf(south.train.ts, h = 6)

# Plot Population
plot(south.train.ts, ylim = c(400000, 800000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2020), main = "South Bee Population Mean Method")
axis(1, at = seq(1987, 2020, 1), labels = format(seq(1987, 2020, 1)))

# Add Mean Forecast 
lines(south.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Mean Fitted Values 
lines(south.mean.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(south.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 750000))
lines(c(2017, 2017), c(0, 750000))

# Add Text Labels 
text(1999, 800000, "Training")
text(2015.5, 800000, "Validation")
text(2020, 800000, "Future")

# Find Value 
mean(south.valid.ts)


## West

west.mean.pred <- meanf(west.train.ts, h = 6)

# Plot Population
plot(west.train.ts, ylim = c(800000, 1250000), ylab = "Population", xlab = "Time",
     bty = "l", xaxt = "n", xlim = c(1987, 2020), main = "West Bee Population Mean Method")
axis(1, at = seq(1987, 2020, 1), labels = format(seq(1987, 2020, 1)))

# Add Mean Forecast 
lines(west.mean.pred$mean, lwd = 2, col = "blue", lty = 2)

# Add Mean Fitted Values 
lines(west.mean.pred$fitted, lwd = 2, col = "blue")

# Add Validation Data 
lines(west.valid.ts, col = "grey20", lty = 3)

# Section off Validation Data
lines(c(2017 - 3, 2017 - 3), c(0, 1200000))
lines(c(2017, 2017), c(0, 1200000))

# Add Text Labels 
text(1999, 1250000, "Training")
text(2015.5, 1250000, "Validation")
text(2020, 1250000, "Future")

# Find Value 
mean(west.valid.ts)


