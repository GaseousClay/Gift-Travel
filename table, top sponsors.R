library(readr)
library(tidyverse)
library(stargazer)

X110_4gt_foreign_staff <- read_csv("~/Documents/Scholastic Scribblings/Ph.D./2016-17 First Year/Spring 2017/Congress/Gift Travel paper/Gift-Travel/110-4gt_foreign_staff.csv")


r <- X110_4gt_foreign_staff
spon <- r %>% group_by(TravelSponsor) %>% count()
topspon <- arrange(spon, desc(n))
topspon <- filter(topspon, n >= 30)
topspon <- mutate(topspon, avg = (n/5))


stargazer(topspon, summary = F, 
          title = "List of Top Travel Sponsors by Total Trips Given per Congress (2007-2017)"
          ,keep = c(2,4))
