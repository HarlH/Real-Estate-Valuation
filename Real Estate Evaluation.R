library(readxl)
library(ggplot2)
library(lubridate)
library(leaps)
library(olsrr)
data <- read_excel("./Real estate valuation data set.xlsx")
colnames(data) <- c("id", "date", "age", "dist", "stores", "lat", "lon", "price")
data$date <- as.Date(round(date_decimal(data$date), "month")) #Dealing with awkward date format in data
data

#Exploratory data analysis
summary(data)
hist(data$price, xlab = "Price (TWD/ping)", main = "Histogram of price")
plot(data$date, data$price, xlab = "Date (2012-2013)", ylab = "Price (TWD/ping)", main = "Price vs. date")
plot(data$age, data$price, xlab = "Age (years)", ylab = "Price (TWD/ping)", main = "Price vs. age") #already suggesting a non-linear relationship between age and price
plot(data$dist, data$price, xlab = "Distance to nearest MRT station (meters)", ylab = "Price (TWD/ping)", main = "Price vs. station distance") #also looks non-linear
plot(data$stores, data$price, xlab = "Convenience stores within 500 meters", ylab = "Price (TWD/ping)", main = "Price vs. nearby convenience stores")
ggplot(data, aes(x = lon, y = lat, colour = price)) + geom_point(size = 2) + scale_colour_viridis_c(option = "D", direction = -1, trans = "log10") + labs(x = "lon", y = "lat", title = "Price by location", colour = "Price (TWD/ping)")

#Additional plots
plot(data$lat, data$price)
plot(data$lon, data$price)
ggplot(data, aes(x = lon, y = lat, colour = date)) + geom_point(size = 2) + scale_colour_viridis_c(option = "D")
ggplot(data, aes(x = lon, y = lat, colour = age)) + geom_point(size = 2) + scale_colour_viridis_c(option = "D", direction = -1)
ggplot(data, aes(x = lon, y = lat, colour = log(dist))) + geom_point(size = 2) + scale_colour_viridis_c(option = "D", direction = -1)
ggplot(data, aes(x = lon, y = lat, colour = stores)) + geom_point(size = 2) + scale_colour_viridis_c(option = "D", direction = -1)

#Starting from a basic linear model with all explanatory variables
#Since lat and lon describe a position in 2 dimensions, they suggest a special treatment - will consider this later

model1 <- lm(price ~ date + age + dist + stores + lat + lon, data = data)
summary(model1) #adj R^2 = 0.5762
plot(model1, which = 1)
plot(data$age, residuals(model1), xlab = "Age (years)", ylab = "Residuals of basic linear model", main = "Residuals vs. age")
plot(data$dist, residuals(model1), xlab = "Distance to nearest MRT station (meters)", ylab = "Residuals of basic linear model", main = "Residuals vs. distance to station")

model2 <- lm(price ~ date + age + I(age^2) + I(sqrt(dist)) + stores + lat + lon, data = data)
summary(model2)
plot(model2, which = 1)

model3 <- lm(I(log(price)) ~ date + age + I(age^2) + I(sqrt(dist)) + stores + lat + lon, data = data)
summary(model3)
plot(model3, which = 1)

fullmodel <- lm(I(log(price)) ~ date + I(as.numeric(date - mean(date))^2) + age + I(age^2) + I(sqrt(dist)) + stores + I(stores^2) + lat + I((lat - mean(lat))^2) + lon + I((lon - mean(lon))^2) + date:age + date:stores + age:stores + I(lat - mean(lat)):I(lon - mean(lon)), data = data)
summary(fullmodel)
withlon <- lm(I(log(price)) ~ date + age + I(age^2) + I(sqrt(dist)) + stores + lat + lon + I(lat - mean(lat)):I(lon - mean(lon)), data = data)
withoutlon <- lm(I(log(price)) ~ date + age + I(age^2) + I(sqrt(dist)) + stores + lat + I(lat - mean(lat)):I(lon - mean(lon)), data = data)
summary(withlon) #adj R^2 = 0.7370
summary(withoutlon) #adj R^2 = 0.7353
AIC(withlon) #-141.6476
AIC(withoutlon) #-139.8648
ols_mallows_cp(withlon, fullmodel) #12.34489
ols_mallows_cp(withoutlon, fullmodel) #14.09309

plot(withlon, which = 1)
plot(withlon, which = 2)
plot(withlon, which = 5)
plot(data$date, residuals(withlon), xlab = "Date (2012-2013)", ylab = "Residuals of final model", main = "Residuals vs. date")
plot(data$age, residuals(withlon), xlab = "Age (years)", ylab = "Residuals of final model", main = "Residuals vs. age")
plot(data$dist, residuals(withlon), xlab = "Distance to the nearest MRT station (meters)", ylab = "Residuals of final model", main = "Residuals vs. distance to station")
plot(data$stores, residuals(withlon), xlab = "Convenience stores within 500 meters", ylab = "Residuals of final model", main = "Residuals vs. nearby stores")
plot(data$lat, residuals(withlon), xlab = "Latitude (degrees)", ylab = "Residuals of final model", main = "Residuals vs. latitude")
plot(data$lon, residuals(withlon), xlab = "Longitude (degrees)", ylab = "Residuals of final model", main = "Residuals vs. longitude")

exp(withlon$coefficients["date"]*365.25)
exp(withlon$coefficients["stores"])

location <- data$lat*9.349 + data$lon*2.265 + (data$lon - mean(data$lon))*(data$lat - mean(data$lat))*191.2
data <- cbind(data, location)
data$location <- data$location - mean(data$location)
ggplot(data, aes(x = lon, y = lat, colour = location)) + geom_point() + scale_colour_viridis_c(option="D") + labs(x = "Longitude", y = "Latitude", title = "Log of effect of location on price per unit area")
