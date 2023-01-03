library(wbstats)
library(dplyr)
library(magrittr)
library(cluster)
library(gridExtra)
library(tidyverse)
library(factoextra)
library(rpart)
library(rpart.plot)
library(GGally)
library(caret)
library(CGPfunctions)
library(nloptr)

#Import the list of countries from World Bank Database and selecting only the desired columns
countries <- wb_cachelist$countries %>% select(country,income_level, iso2c)

#Removing rows which are not countries and also countries whose income level is high as they wont be requiring aid
countries <- subset(countries, income_level != "Aggregates" & income_level != "High income")

#Fetching data for countries based on the indicators from 2001-2020 for only countries that were filtered above
dfmain <- wb_data(indicator = c("Internet_Users_Population" = "IT.NET.USER.ZS","totalPopulation" = "SP.POP.TOTL", "gdpPerCapita" = "NY.GDP.PCAP.KD", "literacyRate" = "SE.ADT.LITR.ZS", "mobileCellularSubs" = "IT.CEL.SETS", "secureInternetServers" = "IT.NET.SECR", "fixedBroadbandSubs" = "IT.NET.BBND" ), country = countries$country, start_date = 2001, end_date = 2020) %>% select(iso2c, country,date, totalPopulation, gdpPerCapita, literacyRate, Internet_Users_Population, mobileCellularSubs,secureInternetServers, fixedBroadbandSubs) %>% mutate(date = as.numeric(date))

summary(dfmain)


#Since we want to prioritise the current data with more significance in our variable, we assigned different weights to different 5 year period. (2020-2016) = 8, (2015-2011) = 6, (2010-2006) = 4, (2005-2000) = 2
dfmain <- dfmain %>% mutate(Weights = case_when(
  date <= 2020 & date > 2015 ~ 8,
  date <= 2015 & date > 2010 ~ 6,
  date <= 2010 & date > 2005 ~ 4,
  date <= 2005 & date > 2000 ~ 2,
))

#Calculating the scores to create new variables
dfmain <- dfmain %>% mutate ( 
  scores_totalPop = totalPopulation * Weights /100,
  scores_gdpPerCapita = gdpPerCapita * Weights/100,
  scores_literacyRate = literacyRate * Weights,              #Data is in percentage
  scores_Inter = Internet_Users_Population * Weights,        #Data is in percentage
  scores_mobileCell = mobileCellularSubs * Weights/100,
  scores_secureInternetServers =secureInternetServers * Weights/100,
  scores_fixedBroadband = fixedBroadbandSubs * Weights/100
)

summary(dfmain)

#Creating an empty data frame where the new variables will be stored
df <- data.frame(matrix(ncol=8,nrow=0, dimnames=list(NULL, c("country", "totalPopulation", "gdpPerCapita", "literacyRate","internetUsersByPopulation","mobileCellularSubs", "secureInternetServer", "fixedBroadbandSubs"))))

for(i in 1:nrow(countries))
{
  
  #Sum Variables of all indicators
  sum_totalPop = 0
  sum_gdpPerCap = 0
  sum_literacyRate = 0
  sum_internetUsers = 0
  sum_mobileCellSubs = 0
  sum_secureInternetServers = 0
  sum_fixedBroadBand = 0
  
  #Mean Variables of all indicators
  mean_literacyRate = 0
  mean_internetUsers = 0
  
  #Creating a temporary dataframe to store the data of a country
  df_temp <- dfmain %>% filter(country == countries$country[i])
  
  #Calculation for total Population
  sum_totalPop <- sum(df_temp$scores_totalPop, na.rm = TRUE)
  
  #Calculation for gdpPer capita
  sum_gdpPerCap <- sum(df_temp$scores_gdpPerCapita, na.rm = TRUE)
  
  #For Literacy Rate
  sum_literacyRate <- sum(df_temp$scores_literacyRate, na.rm = TRUE)
  mean_literacyRate <- (sum_literacyRate / 100)
  
  #For Internet Users
  sum_internetUsers <- sum(df_temp$scores_Inter, na.rm = TRUE)
  mean_internetUsers <- (sum_internetUsers / 100)
  
  #For Mobile Cellular Subscriptions
  sum_mobileCellSubs <- sum(df_temp$scores_mobileCell, na.rm = TRUE)
  
  #For Secure Internet Servers
  sum_secureInternetServers <- sum(df_temp$scores_secureInternetServers, na.rm = TRUE)
  
  #For Fixed Broadband Subscriptions
  sum_fixedBroadBand <- sum(df_temp$scores_fixedBroadband, na.rm = TRUE)
  
  #Creating our final dataframe where each indicator will have a single value for each country
  df[nrow(df) + 1,] = c(countries$country[i],sum_totalPop,sum_gdpPerCap ,mean_literacyRate, mean_internetUsers, sum_mobileCellSubs, sum_secureInternetServers, sum_fixedBroadBand)
}

#Changing Data Types
df$totalPopulation <- as.numeric(df$totalPopulation)
df$gdpPerCapita <- as.numeric(df$gdpPerCapita)
df$literacyRate <- as.numeric(df$literacyRate)
df$internetUsersByPopulation <- as.numeric(df$internetUsersByPopulation)
df$mobileCellularSubs <- as.numeric(df$mobileCellularSubs)
df$secureInternetServer <- as.numeric(df$secureInternetServer)
df$fixedBroadbandSubs <- as.numeric(df$fixedBroadbandSubs)

#We eliminate countries whose literacy rate is below the first quartile
df <- df %>% filter(literacyRate >= 5.5)

summary(df)


# Elbow method
set.seed(123)
fviz_nbclust(df[,-1], kmeans, method = "wss")

# Compute k-means clustering with k = 4
set.seed(123)
final <- kmeans(df[,-1], 3, nstart = 30)
print(final)
fviz_cluster(final, data = df[,-1])

#Cluster assignments
o = order(final$cluster)
clusters_by_internet <- data.frame(df$country[o],final$cluster[o])

#Creating test set for linear regression
df1 <- clusters_by_internet %>% filter(final.cluster.o. == 3)
df1 <- df %>% filter(country %in% df1$df.country.o.)

#Creating training set for linear regression
df2 <- clusters_by_internet %>% filter(final.cluster.o. != 3)
df2 <- df %>% filter(country %in% df2$df.country.o.)


#Linear Regression Model
model1 <- lm(internetUsersByPopulation ~., data = df2[,-1])
summary(model1)
plot(model1)

#Since the p-value for the model is significant, we can continue with model

predictions <- model1 %>% predict(df1)
print(predictions)

ggpairs(df1, columns = 2:8)

RMSE(predictions, df1$internetUsersByPopulation)
R2(predictions, df1$internetUsersByPopulation)


#Visualization for the selected countries
df_1 <-  wb_data(indicator = c("Internet_Users_Population" = "IT.NET.USER.ZS","totalPopulation" = "SP.POP.TOTL", "gdpPerCapita" = "NY.GDP.PCAP.KD", "literacyRate" = "SE.ADT.LITR.ZS", "mobileCellularSubs" = "IT.CEL.SETS", "secureInternetServers" = "IT.NET.SECR", "fixedBroadbandSubs" = "IT.NET.BBND" ), country = c("BD","BR","EG","ID","IR","MX","NG","PK","PH","RU","TH","TR","VN"), start_date = 2001, end_date = 2020) 


df_1 %>% ggplot(aes(x = date, y = Internet_Users_Population, color = as.factor(country)))+ geom_line(show.legend = T)

ggplot(data = df_1, 
       mapping = aes(x = date, y = gdpPerCapita)) +
  geom_line(size = 1) +
  facet_wrap(vars(country))

