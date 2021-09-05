setwd("C:/Users/Angelo Anthony/OneDrive/Learning R/Week 7 Project")
Cold_Storage_Mar2018 = read.csv("Cold_Storage_Mar2018.csv")
head(Cold_Storage_Mar2018)
summary(Cold_Storage_Mar2018)
hist(Cold_Storage_Mar2018$Temperature)
boxplot(Cold_Storage_Mar2018$Temperature, horizontal = TRUE, col = "Blue")

Mar2018_mean = mean(Cold_Storage_Mar2018$Temperature)
Mar2018_sd = sd(Cold_Storage_Mar2018$Temperature)

Mar2018_High_Temp = Cold_Storage_Mar2018[Cold_Storage_Mar2018$Temperature > 3.9, ]
length(Mar2018_High_Temp$Temperature)

t.test(Cold_Storage_Mar2018$Temperature, mu = 3.9, alternative = "greater", conf.level = 0.9)

## NUll hypothesis is rejected

