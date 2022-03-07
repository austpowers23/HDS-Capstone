library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggrepel)
library(lubridate)

new <- City_Zhvi_AllHomes_csv

#Data Frame of avg US home price
USAavg1 <- na.omit(City_Zhvi_AllHomes_csv)
USAavg2 <- gather(USAavg1, "1996-01-31" : "2020-03-31", key = "Date" , value = "Price")
USAavg3 <- USAavg2 %>% group_by(Date) %>% summarise(mean(Price))
USAavg4 <- rename(USAavg3, Price = `mean(Price)`) 

#ADD RegionName, State & Delta
State <- "US"
USAavg5 <- cbind(State,USAavg4)
RegionName <- "USA" 
USAavg <- cbind(RegionName,USAavg5)
USAavg$Delta <- round(c(NA, diff(USAavg$Price, lag = 1)),0)
USAavg$Date <- as.Date(USAavg$Date,format = '%Y-%m-%d')
USAavg$Year <- year(ymd(USAavg$Date))
USAavg$Pct_D <- round(USAavg$Delta / (lag(USAavg$Price))*100, 3)

citylist <- vector("list") #initiates a list
region_names <- c("Charlotte", "Miami", "Wilmington", "USA")
state_names <- c("NC", "FL","NC", "US")
start_date <- '1996-01-31'
end_date <- '2020-03-31'


for (i in 1:length(region_names)){
  region_current <- region_names[i]
  state_current <- state_names[i]
  temp_df <- new %>% filter(RegionName == region_current, StateName == state_current)
  temp_df <- gather(temp_df, "1996-01-31":'2020-03-31', key = "Date", value = "Price")
  temp_df <- temp_df %>% select(RegionName, State, Date, Price)
  temp_df$Date <- as.Date(temp_df$Date, format = '%Y-%m-%d')
  temp_df$Year <- year(ymd(temp_df$Date))
  temp_df$Delta <- c(NA, diff(temp_df$Price, lag = 1))
  temp_df$Pct_D <- round(temp_df$Delta / (lag(temp_df$Price))*100, 3)
  citylist[[region_current]] <- temp_df
}

citylist[['USA']] <- USAavg #convert USA to city list


citylist[['Charlotte']]
citylist[['Miami']]
tail(citylist[['Wilmington']]) #No data for Head of data
citylist[['USA']]



for (i in 1:length(region_names)){
  region_current <- region_names[i]
  pricemaxcurrent <- max(citylist[[region_current]]$Price, na.rm = T)
  pricemincurrent <- min(citylist[[region_current]]$Price, na.rm = T)
  print(paste0( 'Lowest price for ', region_current, ' is ', pricemincurrent, ' on the date of ', citylist[[region_current]]$Date[which.min(citylist[[region_current]]$Price)]))
  print(paste0( 'Highest price for ', region_current, ' is ', pricemaxcurrent, ' on the date of ', citylist[[region_current]]$Date[which.max(citylist[[region_current]]$Price)]))
        
}


for (i in 1:length(region_names)){
  region_current <- region_names[i]
  print(ggplot(citylist[[region_current]], aes(x = Date, y= Price, group = 1)) + geom_line()+ scale_y_continuous(labels = scales::dollar_format())+
          geom_point(size = .8) + ggtitle(paste0("Avg ", region_current, " Home Prices"))) 
}


#COMBINE MIA, CLT, ILM and USAavg
AUST <- citylist[['Charlotte']]

for (i in 2: length(region_names)){
  region_current <- region_names[i]
  AUST <- union(AUST, citylist[[region_current]])
}


#PLOT AUST
ggplot(AUST, aes(x = Date, y= Price, color = RegionName))+ 
  geom_point(size = .8) + 
  scale_y_continuous(labels = scales::dollar_format(),breaks = seq(120000, 500000, 50000)) + 
  ylab("House Price") + ggtitle("Avg US Home Prices Over Time") 



for (i in 1:length(region_names)){
  region_current <- region_names[i]
  print(ggplot(citylist[[region_current]],aes(x = Date, y = Pct_D)) + geom_point(size =.6)) + 
  ggtitle(paste0("% Change M-o-Month for ", region_current, " Home Prices"))
}


yoycitylist <- vector("list")

for (i in 1:length(region_names)){
  region_current <- region_names[i]
  yoydf <- citylist[[region_current]] %>% group_by(Year,RegionName) %>% summarize("Price" = mean(Price))
  yoycitylist[[region_current]]<- yoydf
}


yoycitylist[["Charlotte"]]
yoycitylist[["Miami"]]
yoycitylist[["Wilmington"]]
yoycitylist[["USA"]]


for (i in 1:length(region_names)){
  region_current <- region_names[i]
  print(ggplot(yoycitylist[[region_current]], aes(x = Year, y= Price)) + geom_line()+ scale_y_continuous(labels = scales::dollar_format())+
    geom_point() + ggtitle(paste0("Yearly Avg ",region_current, " Home Prices")))
}

yoyAUST <- yoycitylist[['Charlotte']]

for (i in 2: length(region_names)){
  region_current <- region_names[i]
  yoyAUST <- union(yoyAUST, yoycitylist[[region_current]])
}


#PLOTAUSTYOY
ggplot(yoyAUST, aes(x = Year, y= Price, color = RegionName))+ geom_line()+ 
  geom_point(size = .8) + 
  scale_y_continuous(labels = scales::dollar_format(),breaks = seq(120000, 500000, 50000)) + 
  ylab("House Price") + ggtitle("Avg US Home Prices Over Time") 


for (i in 1:length(region_names)){
  region_current <- region_names[i]
  yoydf <- citylist[[region_current]] %>% group_by(Year,RegionName) %>% summarize("Price" = mean(Price))
  yoycitylist[[region_current]]<- yoydf
}

yoycitylist[["Charlotte"]]

crash07 <- vector("list")
crash2_ <- vector("list")

for (i in 1:length(region_names)){
  region_current <- region_names[i]
  crash1 <- yoycitylist[[region_current]] %>% filter(Year %in% c(2001:2007))
  crash2 <- yoycitylist[[region_current]] %>% filter(Year %in% c(2014:2020))
  crash1$Delta <- c(NA, diff(crash1$Price, lag = 1))
  crash2$Delta <- c(NA, diff(crash2$Price, lag = 1))
  crash1$Pct_D <- round(crash1$Delta / (lag(crash1$Price))*100, 3)
  crash2$Pct_D <- round(crash2$Delta / (lag(crash2$Price))*100, 3)
  crash07[[region_current]] <- crash1
  crash2_[[region_current]] <- crash2
}

crash07[["Charlotte"]]
crash07[["Miami"]]
crash07[["Wilmington"]]
crash07[["USA"]]

crash2_[["Charlotte"]]
crash2_[["Miami"]]
crash2_[["Wilmington"]]
crash2_[["USA"]]


#SLOPE OF CRASH DATA

for (i in 1:length(region_names)){
  region_current <- region_names[i]
  print(lm(crash07[[region_current]]$Price ~ crash07[[region_current]]$Year))
  print(lm(crash2_[[region_current]]$Price ~ crash2_[[region_current]]$Year))
  }


    
