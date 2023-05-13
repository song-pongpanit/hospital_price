# ------- import data
data.df <- read.csv("C:/Users/song-/Documents/STAT-CHULA/ana2/clean.csv",header=TRUE,sep=",")
dataSp.lm <- lm(TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT, data= data.df)
plot(dataSp.lm)
summary(dataSp.lm)

# find OUTLIER
#1
boxplot(data.df$BODY.WEIGHT)
#2
fitted(dataSp.lm)
e_star <- dataSp.lm$residuals/115100
plot(e_star, pch=19)
text(e_star, as.character(1:239),pos=3)
abline(0,0, col="red")
#3
rstandard(dataSp.lm)
which(abs(rstandard(dataSp.lm)) > 3) # 1 2 4 7 13 36
cutout.df <- data.df[-c(1,2,4,7,11,12,13,15,17,36),]
cutingdata.lm<-lm(TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT, data=cutout.df)
summary(cutingdata.lm)
plot(cutingdata.lm)

# CHECKING PLOT
library(olsrr)
ols_pure_error_anova(cutingdata.lm)
qf(0.95,71,166)
ols_test_normality(cutingdata.lm)
plot(cutingdata.lm)
38

## Simple Linear Regression

# TRANSFER lnY
cutout.df$ln.TOTAL.COST.TO.HOSPITAL <- c(log(cutout.df$TOTAL.COST.TO.HOSPITAL))
cutingdata.lnylm <- lm(ln.TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT, data=cutout.df)
# CHECKING PLOT
plot(cutingdata.lnylm)
ols_pure_error_anova(cutingdata.lnylm) # REJECT H0
qf(0.95, 70,157)
ols_test_normality(cutingdata.lnylm)
## 1.
summary(cutingdata.lnylm)
min(cutout.df$BODY.WEIGHT)
## 2. & 3.
predict.lm(cutingdata.lnylm, newdata = data.frame(BODY.WEIGHT=51),
           interval = "confidence",level = 0.95)
predict.lm(cutingdata.lnylm, newdata = data.frame(BODY.WEIGHT=50),
           interval = "confidence",level = 0.95)
exp(12.05579)
exp(12.18104)
exp(12.04978)
exp(12.17271)
## 4.
predict.lm(cutingdata.lnylm, newdata = data.frame(BODY.WEIGHT=50),
           interval = "predict",level = 0.999)
exp(13.49261)


#---------- Other transferring model that Fail
# TRANSFER X^2
cutout.df$sq.BODY.WEIGHT <- cutout.df$BODY.WEIGHT^2
cutingdata.sqlm<-lm(TOTAL.COST.TO.HOSPITAL~sq.BODY.WEIGHT, data=cutout.df)
plot(cutingdata.sqlm)
ols_pure_error_anova(cutingdata.sqlm)
ols_test_normality(cutingdata.sqlm) # REJECT H0: NOT NORMAL
# TRANSFER X^1/2
cutout.df$sqr.BODY.WEIGHT <- cutout.df$BODY.WEIGHT^(1/2)
cutingdata.sqrlm<-lm(TOTAL.COST.TO.HOSPITAL~sqr.BODY.WEIGHT, data=cutout.df)
plot(cutingdata.sqrlm)
ols_pure_error_anova(cutingdata.sqrlm)
ols_test_normality(cutingdata.sqrlm) # REJECT H0: NOT NORMAL
# TRANSFER X^3
cutout.df$cub.BODY.WEIGHT <- cutout.df$BODY.WEIGHT^3
cutingdata.cublm<-lm(TOTAL.COST.TO.HOSPITAL~cub.BODY.WEIGHT, data=cutout.df)
39
plot(cutingdata.cublm)
ols_pure_error_anova(cutingdata.cublm)
ols_test_normality(cutingdata.cublm) # REJECT H0: NOT NORMAL
# TRANSFER lnX
cutout.df$ln.BODY.WEIGHT <- c(log(cutout.df$BODY.WEIGHT))
cutingdata.lnlm<-lm(TOTAL.COST.TO.HOSPITAL~ln.BODY.WEIGHT, data=cutout.df)
plot(cutingdata.lnlm)
ols_pure_error_anova(cutingdata.lnlm)
ols_test_normality(cutingdata.lnlm) # REJECT H0: NOT NORMAL
# TRANSFER 1/X
cutout.df$x.BODY.WEIGHT <- 1/cutout.df$BODY.WEIGHT
cutingdata.xlm<-lm(TOTAL.COST.TO.HOSPITAL~x.BODY.WEIGHT, data=cutout.df)
plot(cutingdata.xlm)
ols_pure_error_anova(cutingdata.xlm)
ols_test_normality(cutingdata.xlm) # REJECT H0: NOT NORMAL
# TRANSFER Y^2
cutout.df$Sq.TOTAL.COST.TO.HOSPITAL <- c(cutout.df$TOTAL.COST.TO.HOSPITAL^2)
cutingdata.sqylm <- lm(Sq.TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT, data=cutout.df)
plot(cutingdata.sqylm)
ols_pure_error_anova(cutingdata.sqylm)
ols_test_normality(cutingdata.sqylm) # REJECT H0: NOT NORMAL
# TRANSFER lnY
cutout.df$ln.TOTAL.COST.TO.HOSPITAL <- c(log(cutout.df$TOTAL.COST.TO.HOSPITAL))
cutingdata.lnylm <- lm(ln.TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT, data=cutout.df)
plot(cutingdata.lnylm)
ols_pure_error_anova(cutingdata.lnylm)
qf(0.05, 70,157)
ols_test_normality(cutingdata.lnylm) # FAIL TO REJECT H0: NORMAL

#-----------------------------------------------------------------------------------------------------


## Multiple Linear Regression

library(olsrr)
library(car)

data.df <- read.csv("C:/Users/song-/Documents/STAT-CHULA/ana2/clean.csv",header=TRUE,sep=",")
data.df <- data.frame(c(data.df[,1:16],data.df[,18:24]))
names(data.df)[5] <-"COMPLAINTS.CODE"
names(data.df)[9] <-"BP.HIGH"
names(data.df)[12] <-"PAST.CODE"
#names(data.df)[16] <-"STATE.ARRIVAL"
names(data.df)[20] <-"ICU.LENGHT"
names(data.df)[21] <-"WARD.LENGHT"
names(data.df)[22] <-"IMPLANTATION"
data.df$PAST.CODE[data.df$PAST.CODE == ""] <- "NONE"
data.lm <- lm(TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT+BODY.HEIGHT+
                relevel(factor(COMPLAINTS.CODE),ref = "other-general")+
                AGE+relevel(factor(GENDER),ref = "F")+
                relevel(factor(MARITAL.STATUS),ref = "MARRIED")+ HR.PULSE + BP.HIGH +
                BP.LOW + RR + relevel(factor(PAST.CODE),ref = "NONE")+HB + UREA + CREATININE
              + relevel(factor(MODE.OF.ARRIVAL),ref = "AMBULANCE")
              + relevel(factor(TYPE.OF.ADMSN),ref = "ELECTIVE") + ICU.LENGHT + WARD.LENGHT
              + relevel(factor(IMPLANTATION),ref = "N") ,data= data.df)
summary(data.lm)
vif(data.lm)
sapply(lapply(data.df, unique),length)
lapply(data.df[c('MARITAL.STATUS','GENDER','STATE.ARRIVAL')],unique)
#----------------------------------SELECTION MODEL--------------------------#
library(olsrr)
ols_step_backward_p(data.lm, prem=0.1)
ols_step_forward_p(data.lm, penter = 0.05,details = TRUE)
ols_step_both_p(data.lm,prem= 0.1,pent=0.05 ,details = TRUE)
#---------------------new model----------------------------#
data.Rlm <- lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
                 relevel(factor(PAST.CODE),ref = "NONE")+
                 ICU.LENGHT + WARD.LENGHT +
                 relevel(factor(IMPLANTATION),ref = "N")
               ,data= data.df)
summary(data.Rlm)

#--------------------------------OUTLIERS-----------------------------------------#
# rstandard
which(abs(rstandard(data.Rlm)) > 3)
# 7 13 36
#Studen deleted Residuals
rstudent(data.Rlm)
which(abs(rstudent(data.Rlm)) > qt(1-(0.05/(2*239)),(239-11-1)))
# 7 13 36
#hatvalue
which(hatvalues(data.Rlm) > (2*12)/239)
# 2 3 4 5 7 9 12 15 17 21 22 23 24 25 28 36 37 38 42 49 55
# 131 132 150 160 171 179 185 196 206 224 236
which(hatvalues(data.Rlm) > (3*12)/239)
# 2 12 15 25 36 38 42 49 55
# DFFITS
dffits(data.Rlm)
which(abs(dffits(data.Rlm)) > 2*sqrt(12/239))
# 1 2 4 7 10 12 13 15 17 25 28 36 42 78 84 150 165 179
# Cook's Distance
cooks.distance(data.lm)
which(cooks.distance(data.lm) > qf(0.95,12,(239-11-1)))
which(cooks.distance(data.lm) > 4/(239-11-1))
# 1 2 4 7 10 12 13 15 24 30 37 58 63 65 83 131
#-----------------------Studen deleted Residuals--------------------------------------------------#
# CUT 7 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-7,]))
# CUT 13 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-13,]))
# CUT 36 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           42
           ,data= data.df[-36,]))
# CUT 7,13 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-c(7,13),]))
# CUT 7,36 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-c(7,36),]))
# CUT 13,36 obs
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-c(13,36),]))
# CUT 7,13,36 obs!!!!!!
summary(lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
             relevel(factor(PAST.CODE),ref = "NONE")+
             ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
           ,data= data.df[-c(7,13,36),]))
#-----------------------Studen deleted Residuals--------------------------------------------------#
data.lm2 <- lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
                 relevel(factor(PAST.CODE),ref = "NONE")+
                 ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
               ,data= data.df[-c(7,13,36),])
plot(data.lm2)
summary(data.lm2)
data.lm3 <- lm(TOTAL.COST.TO.HOSPITAL ~ BODY.WEIGHT+HR.PULSE +
                 relevel(factor(PAST.CODE),ref = "NONE")+
                 ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
               ,data= data.df[-c(1,2,4,7,10,12,13,15,24,30,36,37,58,63,65,83,131),])
plot(data.lm3)
summary(data.lm3)
ols_test_normality(data.lm3)
vif(data.lm3)
##---------------------Assumption--------------------------------#
# TRANSFER Y^1/2 ->> YES
data.df$Sqr.TOTAL.COST.TO.HOSPITAL <- c(data.df$TOTAL.COST.TO.HOSPITAL^(1/2))
43
data.sqrylm <- lm(Sqr.TOTAL.COST.TO.HOSPITAL~BODY.WEIGHT+HR.PULSE +
                    relevel(factor(PAST.CODE),ref = "NONE")+
                    ICU.LENGHT + WARD.LENGHT + relevel(factor(IMPLANTATION),ref = "N")
                  ,data= data.df[-c(1,2,4,7,10,12,13,15,24,30,36,37,58,63,65,83,131),])
summary(data.sqrylm)
plot(data.sqrylm)
ols_test_normality(data.sqrylm) # REJECT H0: NOT NORMAL
vif(data.sqrylm)
# Q 7
library(fastDummies)
data.dum.df1 <- dummy_cols(data.df,select_columns = "PAST.CODE")
data.dum.df2 <- dummy_cols(data.dum.df1,select_columns = "IMPLANTATION")
attach(data.dum.df2)
head(data.dum.df2)
str(data.dum.df2)
multi.lm3 <- lm(sqrt(TOTAL.COST.TO.HOSPITAL) ~ (BODY.WEIGHT+HR.PULSE) +
                  (PAST.CODE_Diabetes1)+(PAST.CODE_Diabetes2)+(PAST.CODE_hypertension1)+(PAST.CODE
                                                                                         _hypertension2)+
                  (PAST.CODE_hypertension3)+(PAST.CODE_other)+
                  (ICU.LENGHT)+(WARD.LENGHT)+
                  (IMPLANTATION_Y)
                ,data= data.dum.df2[-c(1,2,4,7,10,12,13,15,24,30,36,37,58,63,65,83,131),])
summary(multi.lm3)

library("QuantPsyc")
lm.beta(multi.lm3)

#-------- Standardized regression coefficient ------------------#
library(QuantPsyc)
lm.beta(data.sqrylm) # error
install.packages("MuMIn")
round(MuMIn::std.coef(data.sqrylm, partial.sd = TRUE),4)
