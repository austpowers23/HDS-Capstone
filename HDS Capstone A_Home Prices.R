library(dplyr)
library(tidyverse)
library(ggplot2)
library(stringr)
library(ggrepel)
library(lubridate)


new <- City_Zhvi_AllHomes_csv
View(new)

#Data Frame of avg US home price
USAavg1 <- na.omit(City_Zhvi_AllHomes_csv)
USAavg2 <- gather(USAavg1, "1996-01-31" : "2020-03-31", key = "Date" , value = "Price")
USAavg3 <- USAavg2 %>% group_by(Date) %>% summarise(mean(Price))
USAavg4 <- rename(USAavg3, Price = `mean(Price)`)


#ADD RegionName & State
State <- "US"
USAavg5 <- cbind(State,USAavg4)
RegionName <- "USA" 
USAavg <- cbind(RegionName,USAavg5)


#Create Data sets by key cities

  
CLT1 <- new %>% filter(RegionName == "Charlotte", StateName == "NC") 
MIA1 <- new %>% filter(RegionName == "Miami", StateName == "FL")
UNCW1 <- new %>% filter(RegionName == "Wilmington", StateName == "NC")




#Recreate table so dates are now rows
CLT2 <- gather(CLT1, "1996-01-31" : "2020-03-31", key = "Date" , value = "Price")
MIA2 <- gather(MIA1, "1996-01-31" : "2020-03-31", key = "Date" , value = "Price")
UNCW2 <- gather(UNCW1, "1999-05-31" : "2020-03-31", key = "Date" , value = "Price")



CLT <-CLT2 %>% select(RegionName, State, Date, Price)
MIA <- MIA2 %>% select(RegionName, State, Date, Price)
UNCW <- UNCW2 %>% select(RegionName, State, Date, Price)

#Modify date formate
CLT$Date <- as.Date(CLT$Date, format = "%Y-%m-%d")
MIA$Date <- as.Date(MIA$Date, format = "%Y-%m-%d") 
UNCW$Date <- as.Date(UNCW$Date, format = "%Y-%m-%d")
USAavg$Date <- as.Date(USAavg$Date, format = "%Y-%m-%d")


#Add Month to Month Difference column
CLT$delta <- c(NA, diff(CLT$Price, lag = 1)) 
MIA$delta <- c(NA, diff(MIA$Price, lag = 1))
UNCW$delta <- c(NA, diff(UNCW$Price, lag = 1))
USAavg$delta <- c(NA, diff(USAavg$Price, lag = 1))

#Add % Change Column              
CLT$pct_change <- round(CLT$delta / (lag(CLT$Price)) *100, 3)
MIA$pct_change <- round(MIA$delta / (lag(MIA$Price)) *100, 3) 
UNCW$pct_change <- round(UNCW$delta / (lag(UNCW$Price)) *100, 3)
USAavg$pct_change <- round(USAavg$delta / (lag(USAavg$Price)) *100, 3)

view(CLT)
view(MIA)
view(UNCW)
view(USAavg)

#LOWEST PRICE
min(CLT$Price)    #$127,476   #1996-01-31
min(MIA$Price)    #$122,003   #1996-01-31
min(UNCW$Price)   #$157,256   #1999-10-31
min(USAavg$Price) #$129,060   #1996-02-29

#HIGHEST PRICE
max(CLT$Price)    #$252,438   #2020-03-31
max(MIA$Price)    #$417,104   #2007-02-28
max(UNCW$Price)   #$254,183   #2020-03-31
max(USAavg$Price) #$305,468.8 #2020-03-31


#DATE FOR MIN/MAX PRICES
  
  ###CLT MIN
datcltmin <- CLT[CLT$Price == 127476, ]
datcltmin$Date   #1996-01-31

  ###MIA MIN
datmiamin <- MIA[MIA$Price == 122003, ]
datmiamin$Date   #1996-01-31


  ###UNCW MIN
datUNCWmin <- UNCW[UNCW$Price == 157256, ]
datUNCWmin$Date  #1999-10-31

  ###USA MIN
datUSAavgmin <- USAavg[USAavg$Price == 129060, ]
datUSAavgmin$Date  #1996-02-29

  ###CLT MAX
datcltmax <- CLT[CLT$Price == 252438, ]
datcltmax$Date  #2020-03-31

  ###MIA MAX
datmiamax <- MIA[MIA$Price == 417104, ]
datmiamax$Date  #2007-02-28
  
  ###UNCW MAX
datUNCWmax <- UNCW[UNCW$Price == 254183, ]
datUNCWmax$Date #2020-03-31

  ###USA MAX
datUSAavgmax <- USAavg[USAavg$Price == 305468.8, ]
datUSAavgmax$Date #2020-03-31


#PLOT CLT PRICE
ggplot(CLT, aes(x = Date, y= Price, group = 1)) + geom_line(color="mediumturquoise")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="purple", size = .8) + ggtitle("Avg CLT Home Prices")  

#PLOT MIA PRICE
ggplot(MIA, aes(x = Date, y= Price, group = 1)) + geom_line(color="orange") + scale_y_continuous(labels = scales::dollar_format(),breaks = seq(100000, 500000, 50000)) + geom_point(size =.8 ,color="forestgreen")+ ggtitle("Avg MIA Home Prices")

#PLOT UNCW PRICE
ggplot(UNCW, aes(x = Date, y= Price, group = 1)) + geom_line(color="deepskyblue4")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="mediumturquoise", size = .8) + ggtitle("Avg ILM Home Prices")

#PLOT USAavg PRICE
ggplot(USAavg, aes(x = Date, y= Price, group = 1)) + geom_line(color="deepskyblue4")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="Red", size = .8) + ggtitle("Avg USA Home Prices")


#COMBINE MIA, CLT, & UNCW
AUST1 <- union(CLT,MIA)
AUST2 <- union(AUST1,UNCW)
AUST <- union(AUST2,USAavg)
view(AUST)


#PLOT AUST
ggplot(AUST, aes(x = Date, y= Price, color = RegionName))+ 
  geom_point(size = .8) + 
  scale_y_continuous(labels = scales::dollar_format(),breaks = seq(120000, 500000, 50000)) + 
  ylab("House Price") + ggtitle("Avg US Home Prices Over Time") 
  
#Plot Key Cities % Change Month over Month
ggplot(CLT, aes(x = Date, y= pct_change)) + geom_point(size = .6, color = "purple", shape = 22)
ggplot(MIA, aes(x = Date, y= pct_change)) + geom_point(size = .6, color = "forestgreen", shape = 22)
ggplot(UNCW, aes(x = Date, y= pct_change)) + geom_point(size = .6, color = "deepskyblue4", shape = 22)
ggplot(USAavg, aes(x = Date, y= pct_change)) + geom_point(size = .6, color = "Red", shape = 22)


#YOY DF
CLT$Year <- year(ymd(CLT$Date))
MIA$Year <- year(ymd(MIA$Date))
UNCW$Year <- year(ymd(UNCW$Date))
USAavg$Year <- year(ymd(USAavg$Date))




CLTYOY1<- CLT %>% group_by(Year, RegionName) %>% summarize(mean = mean(Price))
MIAYOY1<- MIA %>% group_by(Year, RegionName) %>% summarize(mean = mean(Price))
UNCWYOY1<- UNCW %>% group_by(Year, RegionName) %>% summarize(mean = mean(Price))
USAYOY1<- USAavg %>% group_by(Year, RegionName) %>% summarize(mean = mean(Price))



CLTYOY <- rename(CLTYOY1, Price = "mean")
MIAYOY <- rename(MIAYOY1, Price = "mean")
UNCWYOY <- rename(UNCWYOY1, Price = "mean")
USAYOY <-rename(USAYOY1, Price = "mean")


#PLOT CLTYOY PRICE
ggplot(CLTYOY, aes(x = Year, y= Price, group = 1)) + geom_line(color="mediumturquoise")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="purple", size = .8) + ggtitle("Avg CLT Home Prices")  

#PLOT MIAYOY PRICE
ggplot(MIAYOY, aes(x = Year, y= Price, group = 1)) + geom_line(color="orange") + scale_y_continuous(labels = scales::dollar_format(),breaks = seq(100000, 500000, 50000)) + geom_point(size =.8 ,color="forestgreen")+ ggtitle("Avg MIA Home Prices")

#PLOT UNCWYOY PRICE
ggplot(UNCWYOY, aes(x = Year, y= Price, group = 1)) + geom_line(color="deepskyblue4")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="mediumturquoise", size = .8) + ggtitle("Avg ILM Home Prices")

#PLOT USAYOY PRICE
ggplot(USAYOY, aes(x = Year, y= Price, group = 1)) + geom_line(color="deepskyblue4")+ scale_y_continuous(labels = scales::dollar_format())+
  geom_point(color="Red", size = .8) + ggtitle("Avg USA Home Prices")

#PLOT AUSTYOY
AUSTYOY1 <- union(CLTYOY,MIAYOY)
AUSTYOY2 <- union(AUSTYOY1,UNCWYOY)
AUSTYOY <- union(AUSTYOY2,USAYOY)
view(AUSTYOY)

#PLOTAUSTYOY
ggplot(AUSTYOY, aes(x = Year, y= Price, color = RegionName))+ geom_line()+ 
  geom_point(size = .8) + 
  scale_y_continuous(labels = scales::dollar_format(),breaks = seq(120000, 500000, 50000)) + 
  ylab("House Price") + ggtitle("Avg US Home Prices Over Time") 


#CRASH NUMBERS 2001-2007 & 2014-2020      

CLTCRASH07  <- CLTYOY %>% filter(Year %in% c(2001:2007))
MIACRASH07  <- MIAYOY %>% filter(Year %in% c(2001:2007)) 
UNCWCRASH07<- UNCWYOY %>% filter(Year %in% c(2001:2007)) 
USACRASH07  <- USAYOY %>% filter(Year %in% c(2001:2007)) 
AUSTCRASH07 <- AUSTYOY %>% filter(Year %in% c(2001:2007))

CLTCRASH2_  <- CLTYOY %>% filter(Year %in% c(2014:2020))
MIACRASH2_  <- MIAYOY %>% filter(Year %in% c(2014:2020))
UNCWCRASH2_<- UNCWYOY %>% filter(Year %in% c(2014:2020))
USACRASH2_  <- USAYOY %>% filter(Year %in% c(2014:2020))
AUSTCRASH2_ <- AUSTYOY %>% filter(Year %in% c(2014:2020))

#SLOPE OF CRASH DATA
lm(CLTCRASH07$Price ~ CLTCRASH07$Year)   #3,801  a year
lm(MIACRASH07$Price ~ MIACRASH07$Year)   #45,510 a year
lm(UNCWCRASH07$Price ~ UNCWCRASH07$Year) #13,797 a year
lm(USACRASH07$Price ~ USACRASH07$Year)   #18,338 a year               

lm(CLTCRASH2_$Price ~ CLTCRASH2_$Year)   #15,668 a year
lm(MIACRASH2_$Price ~ MIACRASH2_$Year)   #11,461 a year
lm(UNCWCRASH2_$Price ~ UNCWCRASH2_$Year) #10,501 a year
lm(USACRASH2_$Price ~ USACRASH2_$Year)   #11,655 a year 

#CRASH DELTA  
CLTCRASH07$delta <- c(NA, diff(CLTCRASH07$Price, lag = 1)) 
MIACRASH07$delta <- c(NA, diff(MIACRASH07$Price, lag = 1))
UNCWCRASH07$delta <- c(NA, diff(UNCWCRASH07$Price, lag = 1))
USACRASH07$delta <- c(NA, diff(USACRASH07$Price, lag = 1))

CLTCRASH2_$delta <- c(NA, diff(CLTCRASH2_$Price, lag = 1)) 
MIACRASH2_$delta <- c(NA, diff(MIACRASH2_$Price, lag = 1))
UNCWCRASH2_$delta <- c(NA, diff(UNCWCRASH2_$Price, lag = 1))
USACRASH2_$delta <- c(NA, diff(USACRASH2_$Price, lag = 1))


#CRASH % CHANGE
CLTCRASH07$pct_change <- round(CLTCRASH07$delta / (lag(CLTCRASH07$Price)) *100, 2)
MIACRASH07$pct_change <- round(MIACRASH07$delta / (lag(MIACRASH07$Price)) *100, 2) 
UNCWCRASH07$pct_change <- round(UNCWCRASH07$delta / (lag(UNCWCRASH07$Price)) *100, 2)
USACRASH07$pct_change <- round(USACRASH07$delta / (lag(USACRASH07$Price)) *100, 2)

CLTCRASH2_$pct_change <- round(CLTCRASH2_$delta / (lag(CLTCRASH2_$Price)) *100, 2)
MIACRASH2_$pct_change <- round(MIACRASH2_$delta / (lag(MIACRASH2_$Price)) *100, 2) 
UNCWCRASH2_$pct_change <- round(UNCWCRASH2_$delta / (lag(UNCWCRASH2_$Price)) *100, 2)
USACRASH2_$pct_change <- round(USACRASH2_$delta / (lag(USACRASH2_$Price)) *100, 2)

view(CLTCRASH07)
view(CLTCRASH2_)
View(MIACRASH07)
View(MIACRASH2_)

CrashCompare <- data.frame(
    RegionName = c("Charlotte", "Miami", "Wilmington", "USA"),
    "avg 2001 to 07" = c(sum(na.omit(CLTCRASH07$pct_change))/7, sum(na.omit(MIACRASH07$pct_change))/7,sum(na.omit(UNCWCRASH07$pct_change))/7, sum(na.omit(USACRASH07$pct_change))/7),
    "avg 2014 to 20" = c(sum(na.omit(CLTCRASH2_$pct_change))/7, sum(na.omit(MIACRASH2_$pct_change))/7,sum(na.omit(UNCWCRASH2_$pct_change))/7, sum(na.omit(USACRASH2_$pct_change))/7))



    
