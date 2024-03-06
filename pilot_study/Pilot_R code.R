rm(list = ls())
# setwd("")

library(readr)
library(dplyr)
library(psych)
library(tidyr)
library(stringr)
library(lavaan)

dt <- read_csv("DHF_PSA_Pilot_May 10, 2023_12.10.csv")


#######################################################
#####################DATA CLEANING#####################
#######################################################

###socio-demographics###
dt[,c(9,11:14)] <- lapply(dt[,c(9,11:14)], as.factor)
dt[,c(10)] <- lapply(dt[,c(10)], as.numeric)

table(dt$Gender) #Nman=149, Nwoman=146, Nnb=5, Nna=1
describe(dt$Age) #18-80, M=38.7, SD=13.9
table(dt$Education) #55% have a university degree (Bachelor's or higher)
table(dt$`US state`) #146 from New York, 147 from Texas
table(dt$`Race/ethnicity`) #160 (53%) White,  56 (19%) Hispanic or Latino, 
                          #38 (13%) Asian or Pacific Islander, 33 (11%) Black or AA
                          #14 (5%) other or prefer not to say
table(dt$Migration) #76% grew up in the USA, 24% were first or second generation immigrants

###DHF###
table(dt$DHF_endorsement_3)

dt <- dt %>%
  mutate_at(vars(15:74),~dplyr::recode(., "Strongly disagree"=1, 
                                       "Disagree"=2,"Somewhat disagree"=3,
                                       "Somewhat agree"=4,"Agree"=5,
                                       "Strongly agree"=6))
describe(dt[,c(15:74)])

###Reciprocity###
table(dt$Reciprocity_1)

dt <- dt %>%
  mutate_at(vars(75:80),~dplyr::recode(., "Does not apply to me at all\n1\n"=1, 
                                       "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,
                                       "Applies to me perfectly\n7\n"=7))
describe(dt[,c(75:80)])

psych::alpha(dt[c("Reciprocity_1","Reciprocity_2","Reciprocity_3")]) #a=.82
psych::alpha(dt[c("Reciprocity_4","Reciprocity_5","Reciprocity_6")]) #a=.89

dt$PosRec <- rowMeans(dt[c("Reciprocity_1","Reciprocity_2","Reciprocity_3")],na.rm=TRUE)
describe(dt$PosRec)
dt$NegRec <- rowMeans(dt[c("Reciprocity_4","Reciprocity_5","Reciprocity_6")],na.rm=TRUE)
describe(dt$NegRec)

###Self-construal###
table(dt$`Self-construal_1`)

dt <- dt %>%
  mutate_at(vars(81:104),~dplyr::recode(., "Doesn’t describe me at all\n1\n"=1, 
                                       "1½"=2,"Describes me a little\n2\n"=3,
                                       "2½"=4,"Describes me moderately\n3\n"=5,
                                       "3½"=6,"Describes me very well\n4\n"=7,
                                       "4½"=8,"Describes me exactly\n5\n"=9))
describe(dt[,c(81:104)])

#all variables are recoded in the direction of independence
#Difference versus similarity: 5, 19, 22 	
dt$`Self-construal_19R`<-10-dt$`Self-construal_19`
psych::alpha(dt[c("Self-construal_5","Self-construal_19R","Self-construal_22")]) #a=.66
dt$SC_simdif <- rowMeans(dt[c("Self-construal_5","Self-construal_19R","Self-construal_22")],na.rm=TRUE)
describe(dt$SC_simdif)

#Self-containment versus connection to others: 8, 11, 14
dt$`Self-construal_11R`<-10-dt$`Self-construal_11`
dt$`Self-construal_14R`<-10-dt$`Self-construal_14`
psych::alpha(dt[c("Self-construal_8","Self-construal_11R","Self-construal_14R")]) #a=.47
dt$SC_scont <- rowMeans(dt[c("Self-construal_8","Self-construal_11R","Self-construal_14R")],na.rm=TRUE)
describe(dt$SC_scont)

#Self-direction versus receptiveness to influence: 12, 16, 21
dt$`Self-construal_12R`<-10-dt$`Self-construal_12`
dt$`Self-construal_16R`<-10-dt$`Self-construal_16`
psych::alpha(dt[c("Self-construal_12R","Self-construal_16R","Self-construal_21")]) #a=.51
dt$SC_sdir <- rowMeans(dt[c("Self-construal_12R","Self-construal_16R","Self-construal_21")],na.rm=TRUE)
describe(dt$SC_sdir)

#Self-reliance versus dependence on others: 2, 15, 23 
dt$`Self-construal_2R`<-10-dt$`Self-construal_2`
psych::alpha(dt[c("Self-construal_2R","Self-construal_15","Self-construal_23")]) #a=.70
dt$SC_srel <- rowMeans(dt[c("Self-construal_2R","Self-construal_15","Self-construal_23")],na.rm=TRUE)
describe(dt$SC_srel)

#Consistency versus variability: 1, 4, 13			
dt$`Self-construal_4R`<-10-dt$`Self-construal_4`
dt$`Self-construal_13R`<-10-dt$`Self-construal_13`
psych::alpha(dt[c("Self-construal_1","Self-construal_4R","Self-construal_13R")]) #a=.79
dt$SC_cons <- rowMeans(dt[c("Self-construal_1","Self-construal_4R","Self-construal_13R")],na.rm=TRUE)
describe(dt$SC_cons)

#Self-expression versus harmony: 7, 17, 20								
dt$`Self-construal_20R`<-10-dt$`Self-construal_20`
psych::alpha(dt[c("Self-construal_7","Self-construal_17","Self-construal_20R")]) #a=.58
dt$SC_sexp <- rowMeans(dt[c("Self-construal_7","Self-construal_17","Self-construal_20R")],na.rm=TRUE)
describe(dt$SC_sexp)

#Self-Interest versus Commitment to Others: 3, 6, 24 
dt$`Self-construal_3R`<-10-dt$`Self-construal_3`
psych::alpha(dt[c("Self-construal_3R","Self-construal_6","Self-construal_24")]) #a=.51
dt$SC_sint <- rowMeans(dt[c("Self-construal_3R","Self-construal_6","Self-construal_24")],na.rm=TRUE)
describe(dt$SC_sint)

#Contextualized vs. decontextualized self: 9, 10, 18
dt$`Self-construal_10R`<-10-dt$`Self-construal_10`
dt$`Self-construal_18R`<-10-dt$`Self-construal_18`
psych::alpha(dt[c("Self-construal_9","Self-construal_10R","Self-construal_18R")]) #a=.58
dt$SC_dec <- rowMeans(dt[c("Self-construal_9","Self-construal_10R","Self-construal_18R")],na.rm=TRUE)
describe(dt$SC_dec)

###Behavior###

#dictator=universal prosociality
dt$prosocial <- as.numeric(dt$Dictator_decision)
describe(dt$prosocial) #m=4.39 (4.39/11*100=39.6%)


#ultimatum_receiver=negative reciprocity
a <- dt[c(8,118:128)]
b <- a %>%
  pivot_longer(!ResponseId, names_to = "proposed", values_to = "decision")
  
b$change <- ifelse(b$decision=="I accept" & 
                     (((b$decision != dplyr::lag(b$decision))==TRUE)|
                        (b$proposed=="Ultimatum Receiver_1")),1,0)
b$proposed <- str_remove(b$proposed, "Ultimatum Receiver_")
b$UG_negrec <- ifelse(b$change==1,b$proposed,0)
c <- subset(b, b$UG_negrec!=0)
c$dupl <- ifelse(duplicated(c$ResponseId)==TRUE,1,0)
c <- subset(c, c$dupl==0)
c <- c[,c(1,5)]
dtg <- merge(dt,c,by="ResponseId")
dtg$UG_negrec <- as.numeric(dtg$UG_negrec)

100*table(dtg$`Ultimatum Receiver_1`)%>% prop.table() #88% reject 0 
100*table(dtg$`Ultimatum Receiver_2`)%>% prop.table() #75% reject 1
100*table(dtg$`Ultimatum Receiver_3`)%>% prop.table() #70% reject 2
100*table(dtg$`Ultimatum Receiver_4`)%>% prop.table() #61% reject 4
100*table(dtg$`Ultimatum Receiver_5`)%>% prop.table() #44% reject 5

#ultimatum_proposer=positive reciprocity
a <- dt[c(8,129:139)]
b <- a %>%
  pivot_longer(!ResponseId, names_to = "proposed", values_to = "decision")
b$proposed <- str_remove(b$proposed, "Ultimatum Proposer_")
b[,c(2,3)]<-lapply(b[,c(2,3)],as.numeric)
b$proposed <- b$proposed-1
cor.test(b$proposed,b$decision) #r between proposal to and from = .54***  
describe(b$decision) #m=4.78 (4.78/11*100=43%)


b$pr <- ifelse(b$decision>=b$proposed,1,0)
c <- aggregate(b$pr,by=list(b$ResponseId), FUN=sum)
c$UG_posrec <- (c$x/11)*100
c$ResponseId <- c$Group.1
c <- c[3:4]

dtg <- merge(dtg,c,by="ResponseId")

cor.test(dtg$PosRec,dtg$UG_posrec) #.14, p=.015
cor.test(dtg$NegRec,dtg$UG_negrec) #.21, p<.001
cor.test(dtg$PosRec,dtg$UG_negrec) #-.04, p=.522
cor.test(dtg$NegRec,dtg$UG_posrec) #-.05, p=.404

#Expectations
a <- dt[c(8,140:150)]
b <- a %>%
  pivot_longer(!ResponseId, names_to = "proposed", values_to = "decision")

b$change <- ifelse(b$decision=="The other person would accept" & 
                     (((b$decision != dplyr::lag(b$decision))==TRUE)|
                        (b$proposed=="Ultimatum expect_1")),1,0)
b$proposed <- str_remove(b$proposed, "Ultimatum expect_")
b$UG_expect <- ifelse(b$change==1,b$proposed,0)
c <- subset(b, b$UG_expect!=0)
c$dupl <- ifelse(duplicated(c$ResponseId)==TRUE,1,0)
c <- subset(c, c$dupl==0)
c <- c[,c(1,5)]
dtg <- merge(dtg,c,by="ResponseId")
dtg$UG_expect <- as.numeric(dtg$UG_expect)

#######################################################
#################DHF FACTOR STRUCTURE##################
#######################################################

###DHF_endorsement

dtg <- dtg %>% rename("D_self1"="DHF_endorsement_1", "D_self2"="DHF_endorsement_17", 
       "D_self3"="DHF_endorsement_20", "D_self4"="DHF_endorsement_25", 
       "D_self5"="DHF_endorsement_26",
       "D_other1"="DHF_endorsement_8", "D_other2"="DHF_endorsement_10",
       "D_other3"="DHF_endorsement_16", "D_other4"="DHF_endorsement_22",
       "D_other5"="DHF_endorsement_28",
       "H_self1"="DHF_endorsement_4", "H_self2"="DHF_endorsement_18",
       "H_self3"="DHF_endorsement_21", "H_self4"="DHF_endorsement_23",
       "H_self5"="DHF_endorsement_29",
       "H_other1"="DHF_endorsement_5", "H_other2"="DHF_endorsement_7",
       "H_other3"="DHF_endorsement_14", "H_other4"="DHF_endorsement_15",
       "H_other5"="DHF_endorsement_24",
       "F_self1"="DHF_endorsement_2", "F_self2"="DHF_endorsement_3",
       "F_self3"="DHF_endorsement_9", "F_self4"="DHF_endorsement_19",
       "F_self5"="DHF_endorsement_27",
       "F_other1"="DHF_endorsement_6", "F_other2"="DHF_endorsement_11",
       "F_other3"="DHF_endorsement_12", "F_other4"="DHF_endorsement_13",
       "F_other5"="DHF_endorsement_30")

col_order <- c("D_self1", "D_self2", "D_self3","D_self4","D_self5",
               "D_other1", "D_other2","D_other3","D_other4","D_other5",
               "H_self1","H_self2","H_self3","H_self4","H_self5",
               "H_other1","H_other2","H_other3","H_other4","H_other5",
               "F_self1","F_self2","F_self3","F_self4","F_self5",
               "F_other1","F_other2","F_other3","F_other4","F_other5")
dtc <- dtg[15:44]
dtc <- dtc[, col_order]

KMO(r=cor(dtc))
# dev.off()
parallel <- fa.parallel(dtc)
fa(dtc, nfactors=6, rotate="oblimin")
fa(dtc, nfactors=3, rotate="oblimin")

#CFA: 6 factors
dhfe <- 'D_self =~ D_self1+D_self2+D_self3+D_self4+D_self5
         D_other =~ D_other1+D_other2+D_other3+D_other4+D_other5
         H_self =~ H_self1+H_self2+H_self3+H_self4+H_self5
         H_other =~ H_other1+H_other2+H_other3+H_other4+H_other5
         F_self =~ F_self1+F_self2+F_self3+F_self4+F_self5
         F_other =~ F_other1+F_other2+F_other3+F_other4+F_other5
'
fit <- cfa(dhfe, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.833, TLI=.813, RMSEA=.070, SRMR=.075, x2=965.859, df=390, p<.001, 
#BIC=24607.016, AIC=24607.016

#CFA: 3 factors
dhfe <- 'D =~ D_self1+D_self2+D_self3+D_self4+D_self5+D_other1+D_other2+D_other3+D_other4+D_other5
         H =~ H_self1+H_self2+H_self3+H_self4+H_self5+H_other1+H_other2+H_other3+H_other4+H_other5
         F =~ F_self1+F_self2+F_self3+F_self4+F_self5+F_other1+F_other2+F_other3+F_other4+F_other5
'
fit <- cfa(dhfe, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.769, TLI=.750, RMSEA=.081, SRMR=.085, x2=1196.6, df=402, p<.001, 
#BIC=25047.259, AIC=24813.711

#CFA: 3 higher-order factors 
dhfe <- 'D_self =~ D_self1+D_self2+D_self3+D_self4+D_self5
         D_other =~ D_other1+D_other2+D_other3+D_other4+D_other5
         H_self =~ H_self1+H_self2+H_self3+H_self4+H_self5
         H_other =~ H_other1+H_other2+H_other3+H_other4+H_other5
         F_self =~ F_self1+F_self2+F_self3+F_self4+F_self5
         F_other =~ F_other1+F_other2+F_other3+F_other4+F_other5
         D =~ NA*D_self+D_other
         H =~ NA*H_self+H_other
         F =~ NA*F_self+F_other
         D ~~ 1*D
         H ~~ 1*H
         F ~~ 1*F
         D_other~~0*D_other
'
fit <- cfa(dhfe, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#negative ns variance is produced for D_other -> variance fixed to 0 
#CFI=.820, TLI=.803, RMSEA=.072, SRMR=.079, x2=1015.3, df=397, p<.001, 
#BIC=24894.543, AIC=24642.459


###DHF_norms
dtg <- dtg %>% rename("D_selfN1"="DHF_Norms_1", "D_selfN2"="DHF_Norms_17", 
                      "D_selfN3"="DHF_Norms_20", "D_selfN4"="DHF_Norms_25", 
                      "D_selfN5"="DHF_Norms_26",
                      "D_otherN1"="DHF_Norms_8", "D_otherN2"="DHF_Norms_10",
                      "D_otherN3"="DHF_Norms_16", "D_otherN4"="DHF_Norms_22",
                      "D_otherN5"="DHF_Norms_28",
                      "H_selfN1"="DHF_Norms_4", "H_selfN2"="DHF_Norms_18",
                      "H_selfN3"="DHF_Norms_21", "H_selfN4"="DHF_Norms_23",
                      "H_selfN5"="DHF_Norms_29",
                      "H_otherN1"="DHF_Norms_5", "H_otherN2"="DHF_Norms_7",
                      "H_otherN3"="DHF_Norms_14", "H_otherN4"="DHF_Norms_15",
                      "H_otherN5"="DHF_Norms_24",
                      "F_selfN1"="DHF_Norms_2", "F_selfN2"="DHF_Norms_3",
                      "F_selfN3"="DHF_Norms_9", "F_selfN4"="DHF_Norms_19",
                      "F_selfN5"="DHF_Norms_27",
                      "F_otherN1"="DHF_Norms_6", "F_otherN2"="DHF_Norms_11",
                      "F_otherN3"="DHF_Norms_12", "F_otherN4"="DHF_Norms_13",
                      "F_otherN5"="DHF_Norms_30")

col_order <- c("D_selfN1", "D_selfN2", "D_selfN3","D_selfN4","D_selfN5",
               "D_otherN1", "D_otherN2","D_otherN3","D_otherN4","D_otherN5",
               "H_selfN1","H_selfN2","H_selfN3","H_selfN4","H_selfN5",
               "H_otherN1","H_otherN2","H_otherN3","H_otherN4","H_otherN5",
               "F_selfN1","F_selfN2","F_selfN3","F_selfN4","F_selfN5",
               "F_otherN1","F_otherN2","F_otherN3","F_otherN4","F_otherN5")
dtcn <- dtg[45:74]
dtcn <- dtcn[, col_order]

KMO(r=cor(dtcn))
# dev.off()
parallel <- fa.parallel(dtcn)
fa(dtcn, nfactors=6, rotate="oblimin")
fa(dtcn, nfactors=3, rotate="oblimin")

#CFA: 6 factors
dhfn <- 'D_self =~ D_selfN1+D_selfN2+D_selfN3+D_selfN4+D_selfN5
         D_other =~ D_otherN1+D_otherN2+D_otherN3+D_otherN4+D_otherN5
         H_self =~ H_selfN1+H_selfN2+H_selfN3+H_selfN4+H_selfN5
         H_other =~ H_otherN1+H_otherN2+H_otherN3+H_otherN4+H_otherN5
         F_self =~ F_selfN1+F_selfN2+F_selfN3+F_selfN4+F_selfN5
         F_other =~ F_otherN1+F_otherN2+F_otherN3+F_otherN4+F_otherN5
'
fit <- cfa(dhfn, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#CFI=.790, TLI=.765, RMSEA=.085, SRMR=.098, x2=1235.850, df=390, p<.001, 
#BIC=25832.911, AIC=25554.878

#CFA: 3 factors
dhfn <- 'D =~ D_selfN1+D_selfN2+D_selfN3+D_selfN4+D_selfN5+D_otherN1+D_otherN2+D_otherN3+D_otherN4+D_otherN5
         H =~ H_selfN1+H_selfN2+H_selfN3+H_selfN4+H_selfN5+H_otherN1+H_otherN2+H_otherN3+H_otherN4+H_otherN5
         F =~ F_selfN1+F_selfN2+F_selfN3+F_selfN4+F_selfN5+F_otherN1+F_otherN2+F_otherN3+F_otherN4+F_otherN5
'
fit <- cfa(dhfn, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.647, TLI=.618, RMSEA=.108, SRMR=.123, x2=1821.874, df=402, p<.001, 
#BIC=26350.450, AIC=26116.902

#CFA: 3 higher-order factors 

dhfn <- 'D_self =~ D_selfN1+D_selfN2+D_selfN3+D_selfN4+D_selfN5
         D_other =~ D_otherN1+D_otherN2+D_otherN3+D_otherN4+D_otherN5
         H_self =~ H_selfN1+H_selfN2+H_selfN3+H_selfN4+H_selfN5
         H_other =~ H_otherN1+H_otherN2+H_otherN3+H_otherN4+H_otherN5
         F_self =~ F_selfN1+F_selfN2+F_selfN3+F_selfN4+F_selfN5
         F_other =~ F_otherN1+F_otherN2+F_otherN3+F_otherN4+F_otherN5
         D =~ NA*D_self+D_other
         H =~ NA*H_self+H_other
         F =~ NA*F_self+F_other
         D ~~ 1*D
         H ~~ 1*H
         F ~~ 1*F
'
fit <- cfa(dhfn, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.770, TLI=.747, RMSEA=.088, SRMR=.111, x2=1320.309, df=396, p<.001, 
#BIC=25883.128, AIC=25627.337

###Revised models for calculating DHF mean scores (items with cross-loadings removed)

dhfe <- 'D_self =~ D_self1+D_self2+D_self3+D_self4+D_self5
         D_other =~ D_other1+D_other2+D_other3+D_other4+D_other5
         H_self =~ H_self2+H_self3+H_self4
         H_other =~ H_other1+H_other2+H_other3+H_other4
         F_self =~ F_self1+F_self2+F_self4
         F_other =~ F_other1+F_other2+F_other3+F_other4
'
fit <- cfa(dhfe, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.891, RMSEA=.061, SRMR=.067 

dhfn <- 'D_self =~ D_selfN1+D_selfN2+D_selfN3+D_selfN4+D_selfN5
         D_other =~ D_otherN1+D_otherN2+D_otherN3+D_otherN4+D_otherN5
         H_self =~ H_selfN2+H_selfN3+H_selfN4
         H_other =~ H_otherN1+H_otherN2+H_otherN3+H_otherN4
         F_self =~ F_selfN1+F_selfN2+F_selfN4
         F_other =~ F_otherN1+F_otherN2+F_otherN3+F_otherN4
'
fit <- cfa(dhfn, data=dtg)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.878, RMSEA=.071, SRMR=.075 

dtg$DSe <- rowMeans(dtg[c("D_self1","D_self2","D_self3","D_self4","D_self5")],na.rm = TRUE)
dtg$DOe <- rowMeans(dtg[c("D_other1","D_other2","D_other3","D_other4","D_other5")],na.rm = TRUE)
dtg$HSe <- rowMeans(dtg[c("H_self2","H_self3","H_self4")],na.rm = TRUE)
dtg$HOe <- rowMeans(dtg[c("H_other1","H_other2","H_other3","H_other4")],na.rm = TRUE)
dtg$FSe <- rowMeans(dtg[c("F_self1","F_self2","F_self4")],na.rm = TRUE)
dtg$FOe <- rowMeans(dtg[c("F_other1","F_other2","F_other3","F_other4")],na.rm = TRUE)
describe(dtg$FOe)

dtg$DSn <- rowMeans(dtg[c("D_selfN1","D_selfN2","D_selfN3","D_selfN4","D_selfN5")],na.rm = TRUE)
dtg$DOn <- rowMeans(dtg[c("D_otherN1","D_otherN2","D_otherN3","D_otherN4","D_otherN5")],na.rm = TRUE)
dtg$HSn <- rowMeans(dtg[c("H_selfN2","H_selfN3","H_selfN4")],na.rm = TRUE)
dtg$HOn <- rowMeans(dtg[c("H_otherN1","H_otherN2","H_otherN3","H_otherN4")],na.rm = TRUE)
dtg$FSn <- rowMeans(dtg[c("F_selfN1","F_selfN2","F_selfN4")],na.rm = TRUE)
dtg$FOn <- rowMeans(dtg[c("F_otherN1","F_otherN2","F_otherN3","F_otherN4")],na.rm = TRUE)
describe(dtg$DSn)

#correlations between personal endorsement and norm scores
cor.test(dtg$DSe, dtg$DSn) #r=.33, p<.001
cor.test(dtg$DOe, dtg$DOn) #r=.46, p<.001
cor.test(dtg$HSe, dtg$HSn) #r=.02, p=.692
cor.test(dtg$HOe, dtg$HOn) #r=.29, p<.001
cor.test(dtg$FSe, dtg$FSn) #r=.29, p<.001
cor.test(dtg$FOe, dtg$FOn) #r=.34, p<.001


#differences in honor between Texas and New York
a <- subset(dtg,dtg$`US state`=="Texas"|dtg$`US state`=="New York")
describeBy(a[183:188],group=a$`US state`)
b<-aov(a$HOe ~ a$`US state`)
b<-aov(a$HSe ~ a$`US state`)
b<-aov(a$HSn ~ a$`US state`)
b<-aov(a$HOn ~ a$`US state`)
summary(b)

cohen.d(a$HOe, a$`US state`)
cohen.d(a$HSe, a$`US state`)
cohen.d(a$HOn, a$`US state`)
cohen.d(a$HSn, a$`US state`)