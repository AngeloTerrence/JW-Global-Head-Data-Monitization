setwd("C:/Users/Angelo Anthony/OneDrive/Learning R/Week 7 Project")
Cold_Storage_Temp_Data = read.csv("Cold_Storage_Temp_Data.csv")
library(dplyr)

## Observe data characteristics
head(Cold_Storage_Temp_Data)
tail(Cold_Storage_Temp_Data)
str(Cold_Storage_Temp_Data)
summary(Cold_Storage_Temp_Data)
summary(Cold_Storage_Temp_Data$Month)
attach(Cold_Storage_Temp_Data)
hist(Temperature)

Summer_Temp = Cold_Storage_Temp_Data[Season == "Summer", ]                                 
Hist(Summer_Temp$Temperature)
boxplot(Summer_Temp$Temperature)

Rainy_Temp = Cold_Storage_Temp_Data[Season == "Rainy", ]   
Hist(Rainy_Temp$Temperature)
boxplot(Rainy_Temp$Temperature)
Rainy_Temp[Rainy_Temp$Temperature > 4.45, ]

Winter_Temp = Cold_Storage_Temp_Data[Season == "Winter", ]   
Hist(Winter_Temp$Temperature)
boxplot(Winter_Temp$Temperature)
Winter_Temp[Winter_Temp$Temperature > 3.825, ]

Cold_Storage_Temp_Data[Temperature > 4, ]
Cold_Storage_Temp_Data[Temperature < 2, ]

## 1. Mean Temp by Season

by(Cold_Storage_Temp_Data, INDICES = Season, FUN = summary)
Cold_Storage_Temp_Data %>% group_by (Season) %>% summarise (mean_temp = mean(Temperature))

## 2 & 3. Overall mean and standard deviation for the year

Temp_Observations = Temperature
Average_Temp = mean(Temp_Observations)
mean(Cold_Storage_Temp_Data$Temperature)
Standard_Dev = sd(Temp_Observations)   

## 4. Prob of temp < 2 C

pnorm (2, mean = Average_Temp, sd = Standard_Dev)

## 5. Prob of temp > 4 C

1 - pnorm(4, mean = Average_Temp, sd = Standard_Dev)
pnorm(4, mean = Average_Temp, sd = Standard_Dev, lower.tail = FALSE)

## 7. ANOVA

boxplot(Temperature~Season, horizontal = TRUE, col = c("blue", "yellow", "red"))     
boxplot(Temperature~Month, horizontal = TRUE)

## Normality testing : Shapiro - Wilk's

shapiro.test(Temperature)

## Homogeneity of variance : Levene test

install.packages("Rcmdr")
library(Rcmdr)
leveneTest(Temperature~Season, Cold_Storage_Temp_Data)

## Alternate testing where Ho: All means are equal and Ha: All means are not equal
kruskal.test(Temperature, Season)

install.packages("dunn.test")
library("dunn.test")
install.packages("FSA")
library("FSA")

dunn.test(Temperature, Season, method = "bonferroni", alpha = 0.05)

Temp_by_Season = aov(Temperature ~ Season, data = Cold_Storage_Temp_Data)
Temp_Comp_by_Season = TukeyHSD(Temp_by_Season)
print(Temp_Comp_by_Season)
par(oma=c(0,3,0,0))
plot(Temp_Comp_by_Season, las = 1, col = "red")


