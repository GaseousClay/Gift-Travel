library(rio)
library(dplyr)
library(ggplot2)

df <- import("110-4GiftTravelFilings2.csv")

sponsors <- count(df, TravelSponsor)
summary(sponsors$n)

mean(sponsors$n)
sd(sponsors$n)

sponsors <- mutate(sponsors, mean = (n / 5))

summary(sponsors$mean)
sd(sponsors$mean)

g <- ggplot(sponsors, aes(mean))
g + geom_histogram(binwidth = 2)

bigs <- subset(sponsors, mean > 4.9)

g <- ggplot(bigs, aes(mean))
g + geom_histogram(binwidth = 2)

df <- import("110-114Congresses.csv")

### FOREIGN AND DOMESTIC ###
df <- import("110-4GiftTravelFilings2.csv")

countries <- c("Cote d'Ivoire", "Arab", "Nigeria", "Ghana", "Ethiopia", "Australia", 
               "Canada", "Algeria", "Jordan", "Netherlands", "Turkey", "Germany","Guatemala", 
               "West Indies", "Colombia", "Tanzania", "Greece", "Azerbaijan", "India",
               "Bosnia", "Spain", "China", "Mozambique", "Ireland", "Belgium", "Serbia",
               "Sudan", "Bermuda", "Israel", "Palestinian", "Kyrgyzstan", "Malawi",
               "Italy", "Montenegro", "Romania", "Hungary", "Argentina", "Burundi",
               "Congo", "Egypt", "Mexico", "Africa", "Haiti", "Venezuela", "Costa Rica",
               "Switzerland", "Senegal", "Zambia", "Sri Lanka", "Denmark", "Cyprus",
               "Peru", "Kenya", "Philippines", "Bangaldesh", "Timor", "Ukraine",
               "Qatar", "Croatia", "Honduras", "Uganda", "Iraq", "Botswana",
               "Japan", "Gori", "Cuba", "Finland", "Vietnam", "Pakistan",
               "Indonesia", "Afghanistan", "Nepal", "Rwanda", "Malaysia", 
               "Kuwait", "Bolivia", "Portugal", "England", "France", "UK",
               "United Kingdom", "Nicaragua", "Morocco", "Somalia", "Liberia",
               "Russia", "Oman", "Myanmar", "Bahamas", "Niger", "Norway",
               "Panama", "Cambodia", "Papua", "Czech", "Kosovo", "Republic",
               "Brazil", "Salvador", "Korea", "Antigua", "Maarten",
               "Sweden", "Taiwan", "Tbilisi", "Thailand", "Algeria",
               "Mongolia", "Austria", "Poland", "Cameroon", "Tunisia")







dems <- subset(df, party == 100)
gop <- subset(df, party == 200)

sd(gop$ideology)
sd(dems$ideology)

dems <- mutate(dems, extreme = ideology - (.6446525))
gop <- mutate(gop, extreme = ideology - (-0.3650046))

mean(gop$extreme)
mean(dems$extreme)

hist(dems$extreme)
hist(gop$extreme)

var(dems$extreme)
var(gop$extreme)

dff <- bind_rows(gop, dems)

dff <- mutate(dff, fourteenth = ifelse(cong == 114,1,0))
dff <- mutate(dff, thirteenth = ifelse(cong == 113,1,0))
dff <- mutate(dff, twelfth = ifelse(cong == 112,1,0))
dff <- mutate(dff, eleventh = ifelse(cong == 111,1,0))

export(dff, "110-114Congresses.csv")
