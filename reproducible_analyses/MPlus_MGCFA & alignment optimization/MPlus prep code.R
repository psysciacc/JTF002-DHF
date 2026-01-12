install.packages("MplusAutomation")
library(MplusAutomation)
setwd("C:/Users/lg1556/OneDrive - University of York/Documents/Research projects 2023+ (York)/PSA_Dignity, honor, face/Main analysis")
dt4$cluster <- recode_factor(dt4$country_res_full, 
                             "Argentina" = "Latin America",
                             "Brazil" = "Latin America",
                             "Chile" = "Latin America",
                             "Mexico" = "Latin America",
                             "Armenia" = "Eastern Europe",
                             "Bosnia-Herzegovina" = "Eastern Europe",
                             "Bulgaria" = "Eastern Europe",
                             "Croatia" = "Eastern Europe",
                             "Czech Republic" = "Eastern Europe",
                             "Georgia" = "Eastern Europe",
                             "Greece" = "Eastern Europe",
                             "Hungary" = "Eastern Europe",
                             "Kosovo" = "Eastern Europe",
                             "North Macedonia" = "Eastern Europe",
                             "Poland" = "Eastern Europe",
                             "Russia" = "Eastern Europe",
                             "Serbia" = "Eastern Europe",
                             "Slovakia" = "Eastern Europe",
                             "Kazakhstan" = "Eastern Europe",
                             "Ukraine" = "Eastern Europe",
                             "Australia" = "Anglo-Saxon",
                             "Canada" = "Anglo-Saxon",
                             "New Zealand" = "Anglo-Saxon",
                             "Ireland" = "Anglo-Saxon",
                             "United Kingdom" = "Anglo-Saxon",
                             "United States" = "Anglo-Saxon",
                             "Austria" = "Germanic",
                             "Germany" = "Germanic",
                             "Switzerland" = "Germanic",
                             "The Netherlands" = "Germanic",
                             "Bangladesh" = "South-East Asia",
                             "India" = "South-East Asia",
                             "Indonesia" = "South-East Asia",
                             "Iran" = "South-East Asia",
                             "Malaysia" = "South-East Asia",
                             "Pakistan" = "South-East Asia",
                             "Philippines" = "South-East Asia",
                             "Thailand" = "South-East Asia",
                             "Cameroon" = "Africa",
                             "Ethiopia" = "Africa",
                             "Kenya" = "Africa",
                             "Namibia" = "Africa",
                             "Nigeria" = "Africa",
                             "South Africa" = "Africa",
                             "Ghana" = "Africa",
                             "China" = "Confucian",
                             "Japan" = "Confucian",
                             "Singapore" = "Confucian",
                             "Hong Kong" = "Confucian",
                             "Taiwan" = "Confucian",
                             "Vietnam" = "Confucian",
                             "France" = "Latin Europe",
                             "Israel" = "Latin Europe",
                             "Italy" = "Latin Europe",
                             "Portugal" = "Latin Europe",
                             "Romania" = "Latin Europe",
                             "Spain" = "Latin Europe",
                             "Kuwait" = "Middle East",
                             "Morocco" = "Middle East",
                             "Saudi Arabia" = "Middle East",
                             "Lebanon" = "Middle East",
                             "Turkey" = "Middle East",
                             "Uzbekistan" = "Middle East",
                             "United Arab Emirates" = "Middle East")
table(dt4$cluster)
dtm <- dt4[ ,c(13:72,1110,1117)]
dtm[dtm == -99] <- NA
table(dtm$dhf_endors_1,useNA = "ifany")

dt4[dt4 == -99] <- NA

prepareMplusData(dtm, filename="dt.mplus.dat")

cameroon <- subset(dt4, dt4$country_res_full=="Cameroon")
cor.plot(cameroon[ ,c(13:42)])
describe(cameroon[ ,c(13:42)])
cor.plot(cameroon[ ,c(43:72)])
describe(cameroon[ ,c(43:72)])

nigeria <- subset(dt4, dt4$country_res_full=="Nigeria")
cor.plot(nigeria[ ,c(13:42)])
describe(nigeria[ ,c(13:42)])
cor.plot(nigeria[ ,c(43:72)])
describe(nigeria[ ,c(43:72)])

africa <- subset(dt4, dt4$cluster=="Africa")
cor.plot(africa[ ,c(13:42)])
describe(africa[ ,c(13:42)])
cor.plot(africa[ ,c(43:72)])
describe(africa[ ,c(43:72)])