library(MASS)
library(dplyr)
library(stargazer)
library(ggplot2)
library(RColorBrewer)

setwd("/Users/zacharymcgee/Dropbox/Gift_Travel")

giftdata <- read.csv(file = "110-114Congresses.csv", header = TRUE, stringsAsFactors = FALSE)

#giftdata$committee.leader <- ifelse(giftdata$chairman == 1 | giftdata$committee == 1,1,0)

#fully interacted model
#fullinteract <- glm.nb(trips ~ extreme*democrat + winpct*democrat + seniority*democrat + chairman*democrat + committee*democrat + party.leader*democrat + fourteenth*democrat + thirteenth*democrat + twelfth*democrat + eleventh*democrat, data = giftdata)
#summary(fullinteract)

#Final model
finalmodel <- glm.nb(trips ~ extreme*democrat + winpct + committee*democrat + seniority + chairman + party.leader + fourteenth + thirteenth + twelfth + eleventh, data = giftdata)
summary(finalmodel)

#predicted values
extremitydata <- data.frame(extreme = giftdata$extreme, democrat = 0, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

extremitydataD1 <- data.frame(extreme = giftdata$extreme, democrat = 1, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

democratdata <- data.frame(extreme = mean(giftdata$extreme), democrat = giftdata$democrat, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

winpctdata <- data.frame(extreme = mean(giftdata$extreme), democrat = 0, winpct = giftdata$winpct, seniority = mean(giftdata$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

senioritydata <- data.frame(extreme = mean(giftdata$extreme), democrat = 0, winpct = mean(giftdata$winpct), seniority = giftdata$seniority, chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

chairdata <- data.frame(extreme = mean(giftdata$extreme), democrat = 0, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = giftdata$chairman, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rankingmemdata <- data.frame(extreme = mean(giftdata$extreme), democrat = 0, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = giftdata$committee, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rankingmemdataD1 <- data.frame(extreme = mean(giftdata$extreme), democrat = 1, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = giftdata$committee, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

partyleaderdata <- data.frame(extreme = mean(giftdata$extreme), democrat = 0, winpct = mean(giftdata$winpct), seniority = mean(giftdata$seniority), chairman = 0, committee = 0, party.leader = giftdata$party.leader, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

extremitydata$phat <- predict(finalmodel, extremitydata, type = "response")
extremitydataD1$phat <- predict(finalmodel, extremitydataD1, type = "response")
democratdata$phat <- predict(finalmodel, democratdata, type = "response")
winpctdata$phat <- predict(finalmodel, winpctdata, type = "response")
senioritydata$phat <- predict(finalmodel, senioritydata, type = "response")
chairdata$phat <- predict(finalmodel, chairdata, type = "response")
rankingmemdata$phat <- predict(finalmodel, rankingmemdata, type = "response")
rankingmemdataD1$phat <- predict(finalmodel, rankingmemdataD1, type = "response")
partyleaderdata$phat <- predict(finalmodel, partyleaderdata, type = "response")

summary(extremitydata$extreme)
summary(extremitydata$phat)

summary(extremitydataD1$extreme)
summary(extremitydataD1$phat)

summary(senioritydata$seniority)
summary(senioritydata$phat)

summary(winpctdata$winpct)
summary(winpctdata$phat)

#Hold everything at mean (or 0s) except for extremity; Democrat at 0 (predict) Democrat at 1 (predict) and then plot the two lines

display.brewer.all()
mypalette <- brewer.pal(3,"Purples")
par(mfrow = c(2,2))

extremitydataCI <- data.frame(extremitydata)
extremitydataCI <- cbind(extremitydataCI, predict(finalmodel, extremitydata, type = "link", se.fit = T))
extremitydataCI <- within(extremitydataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})
ggplot(extremitydataCI, aes(extreme, Trips)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = democrat), alpha = .25) +
  geom_line(aes(colour = committee), size = 1) +
  labs(x = "Ideological Extremity", y = "Predicted Trips (Democrat = 0)") + guides(colour=FALSE, fill=FALSE) + scale_fill_distiller(palette = mypalette, type = "seq") + scale_color_distiller(palette = mypalette, type = "seq") + theme_classic()

extremitydataD1CI <- data.frame(extremitydataD1)
extremitydataD1CI <- cbind(extremitydataD1CI, predict(finalmodel, extremitydataD1, type = "link", se.fit = T))
extremitydataD1CI <- within(extremitydataD1CI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})
ggplot(extremitydataD1CI, aes(extreme, Trips)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = chairman), alpha = .25) +
  geom_line(aes(colour = party.leader), size = 1) +
  labs(x = "Ideological Extremity", y = "Predicted Trips (Democrat = 1)") + guides(colour=FALSE, fill=FALSE) + scale_fill_distiller(palette = mypalette, type = "seq") + scale_color_distiller(palette = mypalette, type = "seq") + theme_classic()

democratdataCI <- data.frame(democratdata)
democratdataCI <- cbind(democratdataCI, predict(finalmodel, democratdata, type = "link", se.fit = T))
democratdataCI <- within(democratdataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})

winpctdataCI <- data.frame(winpctdata)
winpctdataCI <- cbind(winpctdataCI, predict(finalmodel, winpctdata, type = "link", se.fit = T))
winpctdataCI <- within(winpctdataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})
ggplot(winpctdataCI, aes(winpct, Trips)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = committee), alpha = .25) +
  geom_line(aes(colour = committee), size = 1) +
  labs(x = "Win Percentage", y = "Predicted Trips") + guides(colour=FALSE, fill=FALSE) + scale_fill_distiller(palette = mypalette, type = "seq") + scale_color_distiller(palette = mypalette, type = "seq") + theme_classic()

senioritydataCI <- data.frame(senioritydata)
senioritydataCI <- cbind(senioritydataCI, predict(finalmodel, senioritydata, type = "link", se.fit = T))
senioritydataCI <- within(senioritydataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})
ggplot(senioritydataCI, aes(seniority, Trips)) +
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = fourteenth), alpha = .25) +
  geom_line(aes(colour = fourteenth), size = 1) +
  labs(x = "Terms Served in the House", y = "Predicted Trips") + guides(colour=FALSE, fill=FALSE) + scale_fill_distiller(palette = mypalette, type = "seq") + scale_color_distiller(palette = mypalette, type = "seq") + theme_classic()

chairdataCI <- data.frame(chairdata)
chairdataCI <- cbind(chairdataCI, predict(finalmodel, chairdata, type = "link", se.fit = T))
chairdataCI <- within(chairdataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})

rankingmemdataCI <- data.frame(rankingmemdata)
rankingmemdataCI <- cbind(rankingmemdataCI, predict(finalmodel, rankingmemdata, type = "link", se.fit = T))
rankingmemdataCI <- within(rankingmemdataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})

rankingmemdataD1CI <- data.frame(rankingmemdataD1)
rankingmemdataD1CI <- cbind(rankingmemdataD1CI, predict(finalmodel, rankingmemdataD1, type = "link", se.fit = T))
rankingmemdataD1CI <- within(rankingmemdataD1CI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})

partyleaderdataCI <- data.frame(partyleaderdata)
partyleaderdataCI <- cbind(partyleaderdataCI, predict(finalmodel, partyleaderdata, type = "link", se.fit = T))
partyleaderdataCI <- within(partyleaderdataCI, {
  Trips <- exp(fit)
  LL <- exp(fit - 1.96 * se.fit)
  UL <- exp(fit + 1.96 * se.fit)})

#descriptives for the paper
summary(giftdata$seniority)
sd(giftdata$seniority)

summary(giftdata$winpct)
sd(giftdata$winpct)

summary(giftdata$trips)
sd(giftdata$trips)

summary(dems$trips)
sd(dems$trips)

summary(gop$trips)
sd(gop$trips)

#Fig 1 - hist of trips
ggplot(data = giftdata, aes(x=trips)) + geom_histogram(color="black", fill="purple", alpha=0.4) + theme_classic() + labs(x="Numer of Trips", y="Number of Members")

#Fig 2 - distribution of trips, colored by party
ggplot(data = giftdata, aes(x = trips, fill = as.factor(party))) + geom_histogram(alpha=0.8, position="dodge") + theme_classic() + scale_fill_manual(name =c("Legend"), values=c("blue", "red"), labels=c("100"="Democrats", "200"="Republicans")) + labs(x="Numer of Trips", y="Number of Members")

#produce regression table - manually edited notes and full mode/gop/dem formatting in LaTeX
stargazer(finalmodel, title = "Effects on Total Number of Gifted Trips - 110th-114th Congresses", style = "default", covariate.labels = c("Ideological Extremity", "Democrat", "Win Percentage", "Ranking Member", "Seniority", "Committee Chair", "Party Leader", "114th Congress", "113th Congress", "112th Congress", "111th Congress", "Ideological Extremity x Democrat", "Ranking Member x Democrat"), dep.var.caption = "", dep.var.labels = "Count of Total Gifted Trips", decimal.mark = ".", star.cutoffs = c(0.05, 0.01, 0.001), notes = "Standard errors reported in parentheses.", omit.stat = "aic")

########## Previous Empirics ##########

#full model - both parties
fullmodel <- glm.nb(trips ~ democrat + extreme + winpct + seniority + chairman + committee + party.leader + fourteenth + thirteenth + twelfth + eleventh, data = giftdata)
summary(fullmodel)

#predicted values - full model no interactions
extremitydata$phatfm <- predict(fullmodel, extremitydata, type = "response")
democratdata$phatfm <- predict(fullmodel, democratdata, type = "response")
winpctdata$phatfm <- predict(fullmodel, winpctdata, type = "response")
senioritydata$phatfm <- predict(fullmodel, senioritydata, type = "response")
chairdata$phatfm <- predict(fullmodel, chairdata, type = "response")
rankingmemdata$phatfm <- predict(fullmodel, rankingmemdata, type = "response")
partyleaderdata$phatfm <- predict(fullmodel, partyleaderdata, type = "response")

summary(extremitydata$phatfm)
summary(senioritydata$phatfm)
summary(winpctdata$phatfm)

#model with Democrats only
dems <- giftdata %>% filter(party != 200)
demmodel <- glm.nb(trips ~ house.majority + extreme + winpct + seniority + chairman + committee + party.leader + fourteenth + thirteenth + twelfth + eleventh, data = dems)
summary(demmodel)

#predicted values - Democrats only
dhousemajdata <- data.frame(house.majority = dems$house.majority, extreme = mean(dems$extreme), winpct = mean(dems$winpct), seniority = mean(dems$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dextremedata <- data.frame(house.majority = 0, extreme = dems$extreme, winpct = mean(dems$winpct), seniority = mean(dems$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dwinpctdata <- data.frame(house.majority = 0, extreme = mean(dems$extreme), winpct = dems$winpct, seniority = mean(dems$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dsenioritydata <- data.frame(house.majority = 0, extreme = mean(dems$extreme), winpct = mean(dems$winpct), seniority = dems$seniority, chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dchairdata <- data.frame(house.majority = 0, extreme = mean(dems$extreme), winpct = mean(dems$winpct), seniority = mean(dems$seniority), chairman = dems$chairman, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

drankingmemdata <- data.frame(house.majority = 0, extreme = mean(dems$extreme), winpct = mean(dems$winpct), seniority = mean(dems$seniority), chairman = 0, committee = dems$committee, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dpartyleaderdata <- data.frame(house.majority = 0, extreme = mean(dems$extreme), winpct = mean(dems$winpct), seniority = mean(dems$seniority), chairman = 0, committee = 0, party.leader = dems$party.leader, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

dextremedata$phat <- predict(demmodel, dextremedata, type = "response")
dhousemajdata$phat <- predict(demmodel, dhousemajdata, type = "response")
dwinpctdata$phat <- predict(demmodel, dwinpctdata, type = "response")
dsenioritydata$phat <- predict(demmodel, dsenioritydata, type = "response")
dchairdata$phat <- predict(demmodel, dchairdata, type = "response")
drankingmemdata$phat <- predict(demmodel, drankingmemdata, type = "response")
dpartyleaderdata$phat <- predict(demmodel, dpartyleaderdata, type = "response")

summary(dextremedata$phat)
summary(dsenioritydata$phat)
summary(dwinpctdata$phat)

#model with Republicans only
gop <- giftdata %>% filter(party != 100)
gopmodel <- glm.nb(trips ~ house.majority + extreme + winpct + seniority + chairman + committee + party.leader + fourteenth + thirteenth + twelfth + eleventh, data = gop)
summary(gopmodel)

#predicted values - Republicans only
rhousemajdata <- data.frame(house.majority = gop$house.majority, extreme = mean(gop$extreme), winpct = mean(gop$winpct), seniority = mean(gop$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rextremedata <- data.frame(house.majority = 0, extreme = gop$extreme, winpct = mean(gop$winpct), seniority = mean(gop$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rwinpctdata <- data.frame(house.majority = 0, extreme = mean(gop$extreme), winpct = gop$winpct, seniority = mean(gop$seniority), chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rsenioritydata <- data.frame(house.majority = 0, extreme = mean(gop$extreme), winpct = mean(gop$winpct), seniority = gop$seniority, chairman = 0, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rchairdata <- data.frame(house.majority = 0, extreme = mean(gop$extreme), winpct = mean(gop$winpct), seniority = mean(gop$seniority), chairman = gop$chairman, committee = 0, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rrankingmemdata <- data.frame(house.majority = 0, extreme = mean(gop$extreme), winpct = mean(gop$winpct), seniority = mean(gop$seniority), chairman = 0, committee = gop$committee, party.leader = 0, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rpartyleaderdata <- data.frame(house.majority = 0, extreme = mean(gop$extreme), winpct = mean(gop$winpct), seniority = mean(gop$seniority), chairman = 0, committee = 0, party.leader = gop$party.leader, fourteenth = 0, thirteenth = 0, twelfth = 0, eleventh = 0)

rextremedata$phat <- predict(gopmodel, rextremedata, type = "response")
rhousemajdata$phat <- predict(gopmodel, rhousemajdata, type = "response")
rwinpctdata$phat <- predict(gopmodel, rwinpctdata, type = "response")
rsenioritydata$phat <- predict(gopmodel, rsenioritydata, type = "response")
rchairdata$phat <- predict(gopmodel, rchairdata, type = "response")
rrankingmemdata$phat <- predict(gopmodel, rrankingmemdata, type = "response")
rpartyleaderdata$phat <- predict(gopmodel, rpartyleaderdata, type = "response")

summary(rextremedata$phat)
summary(rsenioritydata$phat)
summary(rwinpctdata$phat)

#old table results
stargazer(fullmodel, gopmodel, demmodel, title = "Negative Binomial Regression - Gift Travel (110th-114th Congresses)", style = "default", column.labels = c("Full Model", "Republicans","Democrats"), column.separate = c(1,2,3), covariate.labels = c("Democrat", "Majority Status", "Ideological Extremity", "Win Percentage", "Seniority", "Chairman", "Committee (Ranking Member)", "Party Leader", "114th Congress", "113th Congress", "112th Congress", "111th Congress"), dep.var.caption = "", dep.var.labels = "Count of Total Gifted Trips", decimal.mark = ".", star.cutoffs = c(0.05, 0.01, 0.001), notes = "Standard errors reported in parentheses.", omit.stat = "aic")
