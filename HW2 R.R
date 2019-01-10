##### STA141A HW2 code Ruochen Zhong 912888970

hw2data = readRDS("/Users/apple/Desktop/housing.rds")

library(lubridate)
library(ggplot2)
library(dplyr)
library(MASS)
library(treemap)
library(maps)


##### Q1 #####
##### check the whole structure of the data
str(hw2data)
class(hw2data$date)
##### change the class of the variable "date", "year", and "city"
hw2data$date = as.Date(hw2data$date)
class(hw2data$date)
hw2data$year = as.integer(hw2data$year)
hw2data$city = as.character(hw2data$city)
str(hw2data)


##### Q2 #####
##### Find the timespan of the housing sales
range(hw2data$date)
difftime(max(hw2data$date), min(hw2data$date), units = 'days')
##### Find the range of the construction date
range(hw2data$year, na.rm = TRUE)
sort(hw2data$year)
sort(hw2data$year, decreasing = TRUE)
##### Some observations' construction date is not appropriate, ignoring those observations
appropriate_year <- subset(hw2data, (hw2data$year > 1800) & (hw2data$year < 2100))
##### check the timespan by boxplot
boxplot(appropriate_year$year, horizontal = TRUE,
        col = 'red', main = "range of year", xlab = "year")

text(x = boxplot.stats(hw2data$year)$stats, labels = boxplot.stats(hw2data$year)$stats, y = 1.25)

sort(appropriate_year$year)

##### Q3 #####
##### check there are how many years and months
table(month(hw2data$date))
table(year(hw2data$date))
##### create two new variables years, and month
hw2data$year2 <- year(hw2data$date)
hw2data$month <- month(hw2data$date)

##### For the number of sales over time
##### split by year
data_2003 <- subset(hw2data, year(hw2data$date) == 2003)
data_2004 <- subset(hw2data, year(hw2data$date) == 2004)
data_2005 <- subset(hw2data, year(hw2data$date) == 2005)
data_2006 <- subset(hw2data, year(hw2data$date) == 2006)
##### construct data.frame for each year
dt_2003 <- as.data.frame(table(month(data_2003$date)))
colnames(dt_2003) = c("months", "numbers")

dt_2004 <- as.data.frame(table(month(data_2004$date)))
colnames(dt_2004) = c("months", "numbers")

dt_2005 <- as.data.frame(table(month(data_2005$date)))
colnames(dt_2005) = c("months", "numbers")

dt_2006 <- as.data.frame(table(month(data_2006$date)))
colnames(dt_2006) = c("months", "numbers")

##### draw the ggplot in one graph
p <- ggplot() + geom_line(data=dt_2004, aes(x=months, y = numbers, group=1, colour = 'pink'))+ 
     geom_line(data=dt_2003, aes(x=months, y = numbers, group=1, colour = 'blackblue')) + 
     geom_line(data=dt_2005, aes(x=months, y = numbers, group=1, colour = 'green')) + 
     geom_line(data=dt_2006, aes(x=months, y = numbers, group=1, colour = 'yellow'))

##### correct the legend and add a title
p.update <- p + scale_color_discrete(name = "Years", labels = c("2003", "2004","2005", "2006")) +
            ggtitle("number of sales in different years") +
            theme(plot.title = element_text(hjust = 0.5))
print(p.update)

##### For the average house price
##### create data frame for each year's price
df_price2003 = aggregate(price ~ month(date), data_2003, mean)
colnames(df_price2003) = c("months", "avg_prices")

df_price2004 = aggregate(price ~ month(date), data_2004, mean)
colnames(df_price2004) = c("months", "avg_prices")

df_price2005 = aggregate(price ~ month(date), data_2005, mean)
colnames(df_price2005) = c("months", "avg_prices")

df_price2006 = aggregate(price ~ month(date), data_2006, mean)
colnames(df_price2006) = c("months", "avg_prices")

##### Draw the ggplot
p2 <- ggplot() + geom_line(data=df_price2004, aes(x=months, y = avg_prices, group=1, colour = 'green'))+ 
  geom_line(data=df_price2003, aes(x=months, y = avg_prices, group=1, colour = 'blackblue')) + 
  geom_line(data=df_price2005, aes(x=months, y = avg_prices, group=1, colour = 'pink')) + 
  geom_line(data=df_price2006, aes(x=months, y = avg_prices, group=1, colour = 'yellow'))
##### correct the legend and add a title and labels // need to cite!!!
p2.update <- p2 + scale_color_discrete(name = "Years", labels = c("2003", "2004","2005", "2006")) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  ggtitle("average house price in different years") +
  theme(plot.title = element_text(hjust = 0.5))

print(p2.update)

##### Q4 #####
##### Do some data correcting firstly
summary(hw2data$county)
table(hw2data$county)
##### Correct the county name 
hw2data$county <- gsub("C", "c", hw2data$county)
##### Correct "San Franciscoe"
which(hw2data$county == "San Franciscoe county")
hw2data[1421,]$city
hw2data$county <- gsub("San Franciscoe county", "San Francisco county", hw2data$county)
##### Correct "Alpine county"
which(hw2data$county == "Alpine county")
hw2data[1190,]$city
hw2data$county <- gsub("Alpine county", "San Francisco county", hw2data$county)
##### Convert contra costa to upper letter
hw2data$county <- gsub("contra costa county", "Contra Costa county", hw2data$county)
table(hw2data$county)

##### Create a subset
relation_dt <- subset(hw2data, select=c("county", "br", "price", "date"))
##### Excluding the sale year 2006 and br = NA  
relation_dt<-relation_dt[!(year(relation_dt$date) == 2006),]
relation_dt<-relation_dt[!(is.na(relation_dt$br) == "TRUE"),]
##### Replace br >= 4 with 4+ 
relation_dt$br[relation_dt$br >= 4] <- "4+"
##### Create a variable sale_year
relation_dt$sale_year <- year(relation_dt$date)

##### Use dplyr pakage to group data
relation_update <-relation_dt %>%
                  group_by(county , sale_year, br) %>% 
                  summarise_at(vars(price), mean)
print(relation_update)

##### Draw the ggplot
p3 <- ggplot(relation_update, aes(x = sale_year, y = price, color = county)) + 
      geom_line() +
      geom_point() +
      facet_wrap(~br, scales = 'free')
print(p3)
##### Make the ggplot seems better and add titles
p3.update <- p3 + scale_y_continuous(labels = scales::dollar) +
             labs(x = "Year", y = "average price") +
             ggtitle("Relation Between Bedroom, County, Sale year") +
             theme(plot.title = element_text(hjust = 0.5))
print(p3.update)

##### Q5 ##### 
##### Create a subset to focus on county, city
county_city <- subset(hw2data, select = c("county", "city"))
##### Create a table and caculate the unique match of city and county
table(county_city[c("city","county")])
unique_table <- table(unique(county_city[c("city","county")]))
print(unique_table)
##### Find which city don't match only a unique county
which(rowSums(unique_table)>1)
city_Vallejo <- subset(county_city, city == "Vallejo")
##### Only Vallejo ! it has sales in two counties, table them
table(city_Vallejo$county)

##### Q6 #####
##### Exclude rows whose price = 0 in a subset
which(hw2data$price == 0)
bc_subset <- subset(hw2data, hw2data$price != 0)
which(bc_subset$price == 0)
##### Create a regression model
reg1 <- lm(bc_subset$price ~ bc_subset$bsqft)
summary(reg1)
##### Draw a plot of this regression model 
p4 <- ggplot(bc_subset, aes(x = bsqft, y = price)) + 
      geom_point( alpha = 0.4) +
      geom_smooth(method = "lm", se = FALSE)

print(p4)

p4.update <- p4 + scale_y_continuous(labels = scales::dollar) +
             ggtitle("Building size of house vs. Price") +
             theme(plot.title = element_text(hjust = 0.5))

print(p4.update)
##### check the diagnostics
par(mfrow=c(1,2))
plot(reg1, which = 1)
plot(reg1, which = 2)
##### use box-cox transformation to check 
par(mfrow=c(1,1))
box_cox <- boxcox(bc_subset$price ~ bc_subset$bsqft)
##### find the max value of the lamda
lamda <- box_cox$x
likelihood <- box_cox$y
bc <- cbind(lamda, likelihood)
bc[order(-likelihood),]
##### The best lamda is near 0, so use the lamda = 0 to update regression line and check its normality
bc_subset$price <- as.numeric(bc_subset$price)
reg1.update <- lm(log(bc_subset$price) ~ bc_subset$bsqft)
plot(reg1.update)
##### Compare graphs before and after the box-cox transformation
par(mfrow=c(1,2))

plot(reg1, which = 1, main = "R vs F before transformation")
plot(reg1.update, which = 1, main = "R vs F after transformation")

plot(reg1, which = 2, main = "Q-Q before transformation")
plot(reg1.update, which = 2, main = "Q-Q after transformation")

##### Q7 #####
reg2 <- lm(hw2data$price ~ hw2data$bsqft + hw2data$lsqft)
summary(reg2)
###test Ho: beta(bsqft) - beta(lsqft) >= 0
beta_diff <- 302.4 - (-0.0006119)
var_diff <- (3.041)^2 + (0.0008334)^2
T_value <- (beta_diff) / (sqrt(var_diff))
#### compare to the critical value
-qt(0.95, 19988)

##### result: cannot reject Ho, T_value is larger than the critical value

##### Q8 #####
#### Draw the linear regression line for each county
ind_reg <- subset(hw2data, select=c("county", "bsqft", "price"))
p5 <- ggplot(ind_reg, aes(x = bsqft, y = price, color = county)) + 
      geom_smooth(method="lm", se = FALSE)
#### make the ggplot
p5.update <- p5 + scale_y_continuous(labels = scales::dollar) +
             labs(x = "building size of the house(square feet)", y = "price") +
             ggtitle("Relation of building size and price for each County") +
             theme(plot.title = element_text(hjust = 0.5))
print(p5.update)

##### Q9 #####
##### create a subset to study the city, county and price
data_city <- subset(hw2data, select = c("county", "city", "price"))
##### rank the top 3 cities solds of each county and caculate their mean price
top_three <- data_city %>%
             group_by(county, city) %>% 
             summarise(n = n()) %>%
             top_n(n = 3, wt = n)
##### extract top 3 city's price data from the original subset
data_city$ideal <- (data_city$city %in% top_three$city)&(data_city$county %in% top_three$county)
ideal_data <- subset(data_city, data_city$ideal == 'TRUE')
##### caculate their average price
ideal <- ideal_data %>%
         group_by(city) %>% 
         summarise(avg_price = mean(price))
##### change the order of column and combine them 
top_three <- top_three[order(top_three$city),]
top_three <- transform(top_three, newcol=paste(ideal$avg_price, sep="_"))
colnames(top_three)[4] <- "mean_price"
top_three <- top_three[order(top_three$county),]
top_three$mean_price <- as.numeric(as.character(top_three$mean_price))
##### use the ideal dataset to draw the treemap 
treemap(top_three,
        index=c("county", "city"),
        vSize= "n",
        vColor="mean_price",
        type="value",
        format.legend = list(scientific = FALSE, big.mark = " "),
        title="Average prices for 3 cities with most sales in each county",
        align.labels=list(c("left","top"),c("center","center"))
        )

##### Q10 #####
##### extract data for Sanfrancisco
SFdata <- subset(hw2data, hw2data$county == "San Francisco county")
##### transformation the location variables
SFdata$long2 <- round(SFdata$long, 2)
SFdata$lat2 <- round(SFdata$lat, 2)
##### create appropirate factors and specify the levels
long_range<-range(SFdata$long2,na.rm=TRUE)
long_seq<-seq(long_range[1],long_range[2],.01)

lat_range<-range(SFdata$lat2,na.rm=TRUE)
lat_seq<-seq(lat_range[1],lat_range[2],.01)

SFdata$long_fac<-factor(SFdata$long2,levels=long_seq)
SFdata$lat_fac<-factor(SFdata$lat2,levels=lat_seq)

##### create a dataframe by grouping latitude and longitude with price
price_location <-aggregate(price~lat_fac+long_fac, SFdata,function(x)c(mean(x),length(x)),drop=FALSE)
##### change the dataframe to matrix
house_prices<-matrix(price_location$price[,1],nlevels(SFdata$long_fac),nlevels(SFdata$lat_fac),byrow=TRUE)

##### draw a image and add the lines of boundary
par(mfrow=c(1,1))
image(x=long_seq,y=lat_seq,z=house_prices, xlab = "longitute", ylab = "latitude", 
      main = "avg price distribution of San Francisco's house")
#points(SFdata$long, SFdata$lat)
sfmap<-map("county",plot=FALSE)
lines(sfmap$x-.03,sfmap$y)

##### then, grouping with numbers
number_location <- table(SFdata$long_fac,SFdata$lat_fac)
number_location
number_location[number_location == 0] <- NA
number_location
##### draw a image and add the line of boundary
image(x=long_seq,y=lat_seq,z= number_location, xlab = "longitute", ylab = "latitude", 
      main = "Number distribution of San Francisco's house")
#points(SFdata$long, SFdata$lat)
sfmap<-map("county",plot=FALSE)
lines(sfmap$x-.03,sfmap$y)



