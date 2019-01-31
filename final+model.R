giftdata <- import("110-115Congresses.csv")
giftdata$democrat <- ifelse(giftdata$party == 100, 1, 0)
finalmodel <- glm.nb(trips ~ extreme*democrat + winpct + seniority + chairman + committee + party.leader + fifteenth + fourteenth + thirteenth + twelfth + eleventh, data = giftdata)
summary(finalmodel)


