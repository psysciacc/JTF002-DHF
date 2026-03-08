rm(list = ls())
setwd("C:/Users/lg1556/OneDrive - University of York/Documents/Research projects 2023- (York probation)/PSA_Dignity, honor, face/Pilot 2")

library(readr)
library(dplyr)
library(psych)
library(tidyr)
library(stringr)
library(lavaan)
library(ggplot2)

data <- read_csv("DHF_PSA_Pilot+2_June+6,+2024_18.41.csv")
dt <- data[-c(1:2),-c(4,10:17,19,111)]

dt <- subset(dt,dt$Country=="United Kingdom"|dt$Country=="USA")
dt <- subset(dt,dt$Ethnicity!="Not listed/Do not want to answer" & dt$Finished=="True") #total N=450

#######################################################
#####################DATA CLEANING#####################
#######################################################

###socio-demographics###
dt[,c(10:13)] <- lapply(dt[,c(10:13)], as.factor)
dt[,c(14)] <- lapply(dt[,c(14)], as.numeric)

table(dt$Country)#n_USA=147, n_UK=303
table(dt$ResidenceLength) #n_born=298, >10years=93, 5-10y=31, 1-5y=26, <1y=2
table(dt$Ethnicity) #n_WE=189, n_EA=153, n_ME=108
table(dt$Gender) #Nman=185, Nwoman=254, Nnb=10, Nna=1
describe(dt$Age) #18-83, M=36.3, SD=11.7


###DHF###
table(dt$DHF_endorsement_1)

dt <- dt %>%
  mutate_at(vars(15:94),~dplyr::recode(., "Strongly disagree"=1, 
                                       "Disagree"=2,"Somewhat disagree"=3,
                                       "Somewhat agree"=4,"Agree"=5,
                                       "Strongly agree"=6))
describe(dt[,c(15:94)])

###Reciprocity###
table(dt$Reciprocity_1)

dt <- dt %>%
  mutate_at(vars(95:100),~dplyr::recode(., "Does not apply to me at all\n1\n"=1, 
                                       "2"=2,"3"=3,"4"=4,"5"=5,"6"=6,
                                       "Applies to me perfectly\n7\n"=7))
describe(dt[,c(95:100)])

psych::alpha(dt[c("Reciprocity_1","Reciprocity_2","Reciprocity_3")]) #a=.81
psych::alpha(dt[c("Reciprocity_4","Reciprocity_5","Reciprocity_6")]) #a=.90

dt$PosRec <- rowMeans(dt[c("Reciprocity_1","Reciprocity_2","Reciprocity_3")],na.rm=TRUE)
describe(dt$PosRec) #m=5.93, sd=0.87
dt$NegRec <- rowMeans(dt[c("Reciprocity_4","Reciprocity_5","Reciprocity_6")],na.rm=TRUE)
describe(dt$NegRec) #m=3.03, sd=1.41


#######################################################
#################DHF FACTOR STRUCTURE##################
#######################################################


###DHF_endorsement

##D_self =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29
##D_other =~ DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
##H_self =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30
##H_other =~ DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
##F_self =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37
##F_other =~ DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38

#CFI: original bifactor model

dhfbio <- 'D_self =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29
D_other =~ DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H_self =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30
H_other =~ DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F_self =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37
F_other =~ DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38

D =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29+DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30+DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37+DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38

D_self~~0*D_other+0*H_other+0*F_other+0*H_self+0*F_self+0*D+0*H+0*F
D_other~~+0*H_other+0*F_other+0*H_self+0*F_self+0*D+0*H+0*F
H_self~~0*F_self+0*H_other+0*F_other+0*D+0*H+0*F
H_other~~0*F_self+0*F_other+0*D+0*H+0*F
F_self~~0*F_other+0*D+0*H+0*F
F_other~~0*D+0*H+0*F
D~~0*H+0*F
H~~0*F'

fit <- cfa(dhfbio, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#CFI: alternative bifactor model

dhfbia <- 'Self =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29+
DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30+
DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37

Other =~ DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40+
DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34+
DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38

D =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29+DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30+DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37+DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38

Self ~~ 0*D+0*H+0*F
Other ~~ 0*D+0*H+0*F
'

fit <- cfa(dhfbia, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#CFA: 6 factors
dhf6 <- 'D_self =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29
D_other =~ DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H_self =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30
H_other =~ DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F_self =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37
F_other =~ DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38
'
fit <- cfa(dhf6, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.822, TLI=.802, RMSEA=.063, SRMR=.078, x2=1091.67, df=390, p<.001, 
#BIC=36094.056, AIC=35786.029

#CFA: 3 factors
dhf3 <- 'D =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29+
    DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30+
    DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37+
    DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38
'
fit <- cfa(dhf3, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.774, TLI=.756, RMSEA=.070, SRMR=.083, x2=1292.856, df=402, p<.001, 
#BIC=36221.956, AIC=35963.214

#CFA: 3 higher-order factors 
dhf3h <- 'D_self =~ DHF_endorsement_1+DHF_endorsement_10+DHF_endorsement_19+DHF_endorsement_23+DHF_endorsement_29
D_other =~ DHF_endorsement_9+DHF_endorsement_31+DHF_endorsement_35+DHF_endorsement_39+DHF_endorsement_40
H_self =~ DHF_endorsement_6+DHF_endorsement_12+DHF_endorsement_25+DHF_endorsement_27+DHF_endorsement_30
H_other =~ DHF_endorsement_13+DHF_endorsement_14+DHF_endorsement_18+DHF_endorsement_26+DHF_endorsement_34
F_self =~ DHF_endorsement_3+DHF_endorsement_7+DHF_endorsement_11+DHF_endorsement_32+DHF_endorsement_37
F_other =~ DHF_endorsement_8+DHF_endorsement_21+DHF_endorsement_28+DHF_endorsement_33+DHF_endorsement_38
         D =~ NA*D_self+D_other
         H =~ NA*H_self+H_other
         F =~ NA*F_self+F_other
         D ~~ 1*D
         H ~~ 1*H
         F ~~ 1*F
         D_self~~0*D_self
'
fit <- cfa(dhf3h, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)


###DHF_norms


#CFI: original bifactor model

dhfbio <- 'D_self =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29
D_other =~ DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H_self =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30
H_other =~ DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F_self =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37
F_other =~ DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38

D =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29+DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30+DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37+DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38

D_self~~0*D_other+0*H_other+0*F_other+0*H_self+0*F_self+0*D+0*H+0*F
D_other~~+0*H_other+0*F_other+0*H_self+0*F_self+0*D+0*H+0*F
H_self~~0*F_self+0*H_other+0*F_other+0*D+0*H+0*F
H_other~~0*F_self+0*F_other+0*D+0*H+0*F
F_self~~0*F_other+0*D+0*H+0*F
F_other~~0*D+0*H+0*F
D~~0*H+0*F
H~~0*F'

fit <- cfa(dhfbio, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#CFI: alternative bifactor model

dhfbia <- 'Self =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29+
DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30+
DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37

Other =~ DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40+
DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34+
DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38

D =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29+DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30+DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37+DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38

Self ~~ 0*D+0*H+0*F
Other ~~ 0*D+0*H+0*F
'

fit <- cfa(dhfbia, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)

#CFA: 6 factors
dhf6 <- 'D_self =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29
D_other =~ DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H_self =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30
H_other =~ DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F_self =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37
F_other =~ DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38
'
fit <- cfa(dhf6, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.875, TLI=.861, RMSEA=.070, SRMR=.083

#CFA: 3 factors
dhf3 <- 'D =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29+
    DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30+
    DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37+
    DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38
'
fit <- cfa(dhf3, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.811, TLI=.796, RMSEA=.085, SRMR=.089

#CFA: 3 higher-order factors 
dhf3h <- 'D_self =~ DHF_Norms_1+DHF_Norms_10+DHF_Norms_19+DHF_Norms_23+DHF_Norms_29
D_other =~ DHF_Norms_9+DHF_Norms_31+DHF_Norms_35+DHF_Norms_39+DHF_Norms_40
H_self =~ DHF_Norms_6+DHF_Norms_12+DHF_Norms_25+DHF_Norms_27+DHF_Norms_30
H_other =~ DHF_Norms_13+DHF_Norms_14+DHF_Norms_18+DHF_Norms_26+DHF_Norms_34
F_self =~ DHF_Norms_3+DHF_Norms_7+DHF_Norms_11+DHF_Norms_32+DHF_Norms_37
F_other =~ DHF_Norms_8+DHF_Norms_21+DHF_Norms_28+DHF_Norms_33+DHF_Norms_38
         D =~ NA*D_self+D_other
         H =~ NA*H_self+H_other
         F =~ NA*F_self+F_other
         D ~~ 1*D
         H ~~ 1*H
         F ~~ 1*F
         D_self~~0*D_self
         H_other~~0*H_other
'
fit <- cfa(dhf3h, data=dt)
summary(fit,fit.measures=TRUE, standardized=TRUE)
#CFI=.863, TLI=.850, RMSEA=.072, SRMR=.086

###DHF norms mean comparison
dt$d_self <- rowMeans(dt[c("DHF_Norms_1","DHF_Norms_10","DHF_Norms_19","DHF_Norms_23","DHF_Norms_29")], na.rm=T)
describe(dt$d_self)
dt$d_other <- rowMeans(dt[c("DHF_Norms_9","DHF_Norms_31","DHF_Norms_35","DHF_Norms_39","DHF_Norms_40")], na.rm=T)
describe(dt$d_other)
dt$h_self <- rowMeans(dt[c("DHF_Norms_6","DHF_Norms_12","DHF_Norms_25","DHF_Norms_27","DHF_Norms_30")], na.rm=T)
describe(dt$h_self)
dt$h_other <- rowMeans(dt[c("DHF_Norms_13","DHF_Norms_14","DHF_Norms_18","DHF_Norms_26","DHF_Norms_34")], na.rm=T)
describe(dt$h_other)
dt$f_self <- rowMeans(dt[c("DHF_Norms_3","DHF_Norms_7","DHF_Norms_11","DHF_Norms_32","DHF_Norms_37")], na.rm=T)
describe(dt$f_self)
dt$f_other <- rowMeans(dt[c("DHF_Norms_8","DHF_Norms_21","DHF_Norms_28","DHF_Norms_33","DHF_Norms_38")], na.rm=T)
describe(dt$f_other)

summary(aov(dt$d_self ~ dt$Ethnicity)) #p<.001
summary(aov(dt$d_other ~ dt$Ethnicity)) #p<.001
summary(aov(dt$h_self ~ dt$Ethnicity)) #p<.001
summary(aov(dt$h_other ~ dt$Ethnicity)) #p<.001
summary(aov(dt$f_self ~ dt$Ethnicity)) #p<.001
summary(aov(dt$f_other ~ dt$Ethnicity)) #p<.001

describeBy(dt$d_self,dt$Ethnicity) #WE=4.59(.77), ME=3.64(1.16), EA=3.61(.92) - as predicted
describeBy(dt$d_other,dt$Ethnicity) #WE=4.45(1.01), ME=3.88(1.21), EA=4(.96) - as predicted
describeBy(dt$h_self,dt$Ethnicity) #WE=4.29(.88), ME=4.87(.79), EA=4.31(.72) - as predicted
describeBy(dt$h_other,dt$Ethnicity) #WE=4.48(.88), ME=5.36(.64), EA=4.87(.67) - as predicted
describeBy(dt$f_self,dt$Ethnicity) #WE=4.4(.68), ME=4.96(.68), EA=4.92(.66) - equally strong in EA and ME
describeBy(dt$f_other,dt$Ethnicity) #WE=3.97(.81), ME=4.61(.74), EA=4.77(.72) - as predicted

###links with reciprocity 
dt$d_selfe <- rowMeans(dt[c("DHF_endorsement_1","DHF_endorsement_10","DHF_endorsement_19","DHF_endorsement_23","DHF_endorsement_29")], na.rm=T)
describe(dt$d_selfe)
dt$d_othere <- rowMeans(dt[c("DHF_endorsement_9","DHF_endorsement_31","DHF_endorsement_35","DHF_endorsement_39","DHF_endorsement_40")], na.rm=T)
describe(dt$d_othere)
dt$h_selfe <- rowMeans(dt[c("DHF_endorsement_6","DHF_endorsement_12","DHF_endorsement_25","DHF_endorsement_27","DHF_endorsement_30")], na.rm=T)
describe(dt$h_selfe)
dt$h_othere <- rowMeans(dt[c("DHF_endorsement_13","DHF_endorsement_14","DHF_endorsement_18","DHF_endorsement_26","DHF_endorsement_34")], na.rm=T)
describe(dt$h_othere)
dt$f_selfe <- rowMeans(dt[c("DHF_endorsement_3","DHF_endorsement_7","DHF_endorsement_11","DHF_endorsement_32","DHF_endorsement_37")], na.rm=T)
describe(dt$f_selfe)
dt$f_othere <- rowMeans(dt[c("DHF_endorsement_8","DHF_endorsement_21","DHF_endorsement_28","DHF_endorsement_33","DHF_endorsement_38")], na.rm=T)
describe(dt$f_othere)


cor(dt[c("d_selfe","d_othere","h_selfe","h_othere","f_selfe","f_othere","PosRec","NegRec")])
apaTables::apa.cor.table(dt[c("d_selfe","d_othere","h_selfe","h_othere","f_selfe","f_othere","PosRec","NegRec")])
#dignity and honor-other correlate with positive reciprocity, honor-self and -other and face-self correlate with negative reciprocity