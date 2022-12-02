{library(readxl)
  library(car)
  library(nlme)
  library(MASS)
  library(rcompanion)
  library(lmerTest)
  library(lme4)
  library(moments)
  library(ggplot2)
  library(ggpubr)
  library(bestNormalize)
  library(multcomp)
  library(tidyverse)}

##################### OBTAINING HMAX ######################################
RW<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/General.xlsx")
RW
FA = RW%>%
  group_by(CODIGO)%>%
  summarize(Gender=max(Genero),
            Species=max(Especie),
            HMAX=max(Altura),
            date=max(Fecha),
            Treatment=unique(Tratamiento),
            Zone=unique(Zona),
            Island=unique(Isla))

##################### M ER G I N G       D A T A ##########################
#Merging tables

DA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Summary.xlsx")

DF= DA%>% add_column(HMAX=NA)
DF
DF$HMAX= FA$HMAX[match(DA$CODIGO, FA$CODIGO)]
DF
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/")
write.csv(DF, "Hydrogel.csv")


############################################################################
#
#                        B   R  A  S   S   I   C  A 
#
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON BRASSICA PRODUCTIVITY OF WET SEASON IN SANTA CRUZ?
#############################

BA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/Brassica.xlsx")
BA
#PRODUCTIVITY
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=BA,las=2)
#No transformation
BA$Treatment=as.factor(BA$Treatment)
modelSO<- lm(Productivity~Treatment,data=BA)
modelSO
Anova(modelSO, type="II")
xSO = (residuals(modelSO))
xSO

# NON SIGNIFICANT RESULTS ##

##IFW###
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(IFW~Treatment, data=BA,las=2)
#No transformation
BA$Treatment=as.factor(BA$Treatment)
modelSO<- lm(IFW~Treatment,data=BA)
modelSO
Anova(modelSO, type="II")
xSO = (residuals(modelSO))
xSO

###NON SIGNIFICANT RESULTS###########

##############Correlation Matrix-Brassica########################
##Hydrogel##
BH<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/BrassicaH.xlsx")
BH
df <- (BA[,c(8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/")
write.csv(res2$P,"BRACORH.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

##############Correlation Matrix-Brassica########################
##Control##
BC<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/BrassicaC.xlsx")
BC
df <- (BC[,c(8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/")
write.csv(res2$P,"BRACORC.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")



############################################################################
#
#       c       A      P        S       I      C      U          M
#
############################################################################
##############################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM PRODUCTIVITY IN  SANTA CRUZ?
##############################################################################################

CA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/Capsicum.xlsx")
CA
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(Productivity~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
modelCA2=lme(Productivity~Treatment, random = ~ 1 | Zona,data=CA)
modelCA2
Anova(modelCA2, type="II")
AIC(modelCA1)
AIC(modelCA2)
xCA = (residuals(modelCA2))
xCA
#######################################################################################
 #NON SIGNIFICANT EFFECTS
#######################################################################################

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM NUMBER OF FRUITS IN  SANTA CRUZ?
#####################################################################################

CA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/Capsicum.xlsx")
CA
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(Fruts~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
modelCA2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CA)
modelCA2
Anova(modelCA2, type="II")
AIC(modelCA1)
AIC(modelCA2)
#######################################################################################
#NON SIGNIFICANT RESULTS
#######################################################################################

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM NUMBER OF FRUITS IN DRY SEASON IN SANTA CRUZ?
#####################################################################################

CA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/Capsicum.xlsx")
CA
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(Fruts~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
modelCA2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CA)
modelCA2
Anova(modelCA2, type="II")
AIC(modelCA1)
AIC(modelCA2)
###### NON SIGNIFICANT RESULTS

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM INDIVIDUAL FRUIT WEIGHT IN WET SEASON IN SANTA CRUZ?
#####################################################################################

CA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/Capsicum.xlsx")
CA
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(IFW~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(IFW~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
modelCA2=lme(IFW~Treatment, random = ~ 1 | Zona,data=CA)
modelCA2
Anova(modelCA2, type="II")
AIC(modelCA1)
AIC(modelCA2)
#NON SIGNIFICANT RESULTS 

##############Correlation Matrix-Capsicum########################
##Hydrogel##
CH<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/CapsicumH.xlsx")
CH
df <- (CH[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/")
write.csv(res2$P,"CAPCORH.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")


##Control##
CC<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/CapsicumC.xlsx")
CC
df <- (CC[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/")
write.csv(res2$P,"CAPCORC.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")



############################################################################
#
#       C     I     T     R     U     L     L   U   S
#
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON CITRULUS PRODUCTIVITY  IN SANTA CRUZ?
#############################

CI<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/Citrullus.xlsx")
CI
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(IFW~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
modelCI2=lme(IFW~Treatment, random = ~ 1 | Zona,data=CI)
modelCI2
Anova(modelCI2, type="II")
AIC(modelCI1)
AIC(modelCI2)


#######################################################################################
# NON SIGNIFICANT RESULTS  
#######################################################################################
#################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CITRULUS NUMBER OF FRUITS  IN SANTA CRUZ?
#################################################################################
CI<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/Citrullus.xlsx")
CI
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(Fruts~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
modelCI2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CI)
modelCI2
Anova(modelCI2, type="II")
AIC(modelCI1)
AIC(modelCI2)
####NON SIGNIFICANT RESULTS####
#################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CITRULUS IFW  IN SANTA CRUZ?
#################################################################################


CI<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/Citrullus.xlsx")
CI
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(IFW~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
modelCI2=lme(IFW~Treatment, random = ~ 1 | Zona,data=CI)
modelCI2
Anova(modelCI2, type="II")
AIC(modelCI1)
AIC(modelCI2)

##############Correlation Matrix - Citrullus - Hydrogel########################
##Hydrogel##
CH<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/CitrullusH.xlsx")
CH
df <- (CH[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/")
write.csv(res2$P,"CIPCORH.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

#############Correlation Matrix - Citrullus - Control########################
##Control##
CC<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/CitrullusC.xlsx")
CC
df <- (CC[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/")
write.csv(res2$P,"CITCORC.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")










############################################################################
#
#                 C   U   C  U   M   I  S          S  A  T  I  V  U  S
#
############################################################################

#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON CUCUMIS SATIVUS PRODUCTIVITY IN WET SEASON IN SANTA CRUZ?
#############################

CU<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/Cucumis.xlsx")
CU
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1<- lm(Productivity~Treatment,data=CU) 
modelCU1
Anova(modelCU1, type="II")
modelCU2=lme(Productivity~Treatment, random = ~ 1 | Zona,data=CU)
modelCU2
Anova(modelCU2, type="II")
AIC(modelCU1)
AIC(modelCU2)

############################################################################
#NON SIGNIFICANT RESULTS 
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON CUCUMIS SATIVUS NUMBER OF FRUITS IN SANTA CRUZ?
#############################

CU<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/Cucumis.xlsx")
CU
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1= lm(Fruts~Treatment, data=CU)
modelCU1
Anova(modelCU1, type="II")
modelCU2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=CU)
modelCU2
summary(modelCU2)
Anova(modelCU2, type="II")
AIC(modelCU1)
AIC(modelCU2)
############################################################################
#NON SIGNIFICANT RESULTS 
############################################################################

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CUCUMIS INDIVIDUAL FRUIT WEIGHT IN WET SEASON IN SANTA CRUZ?
#####################################################################################
CU<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/Cucumis.xlsx")
CU
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(IFW~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1= lm(IFW~Treatment, data=CU)
modelCU1
summary(modelCU1)
Anova(modelCU1, type="II")
modelCU2<- lme(IFW~Treatment, random = ~ 1 | Zona,data=CU)
modelCU2
summary(modelCU2)
Anova(modelCU2, type="II")
AIC(modelCU1)
AIC(modelCU2)
#######################################################################################
#NON SIGNIFICANT RESULTS

##############Correlation Matrix - Cucumis - Hydrogel########################
##Hydrogel##
CH<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/CucumisH.xlsx")
CH
df <- (CH[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/")
write.csv(res2$P,"CUPCORH.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

#############Correlation Matrix - Citrullus - Control########################
##Control##
CC<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/CitrullusC.xlsx")
CC
df <- (CC[,c(7,8,9,15)])#select columns for pcoa
df
res <- cor(df)

round(res, 2)
cor(df, use = "complete.obs")
library("Hmisc")
res2 <- rcorr(as.matrix(df))
res2
# Extract the correlation coefficients
res2$r
# Extract p-values
res2$P
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/")
write.csv(res2$P,"CUMCORC.csv", row.names = TRUE)
library(corrplot)
corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
# Insignificant correlation are crossed
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")
# Insignificant correlations are leaved blank
corrplot(res2$r, type="upper", order="hclust", 
         p.mat = res2$P, sig.level = 0.05, insig = "blank")

############################################################################
#
#                                    M  E   L  O
#
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON C. MELO PRODUCTIVITY OF DRY SEASON IN SANTA CRUZ?
#############################

CU<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Produtivity_summary__SC_Melo.xlsx")
CU
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelSO<- lm(Productivity~Treatment,data=CU)
modelSO
Anova(modelSO, type="II")
xSO = (residuals(modelSO))
xSO
#######################################################################################
# NON SIGNIFICANT RESULTS AND NOT DRY SEASON
#######################################################################################
############################################################################
#
#       S      O     L     A      N     U     M
#
############################################################################

#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM PRODUCTIVITY IN WET SEASON IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Wet_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Productivity~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")

#######################################################################################
# NON SIGNIFICANT RESULTS
#######################################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM PRODUCTIVITY OF DRY SEASON IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Dry_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Productivity~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")
#######################################################################################
TUKEYSO<- summary(glht(modelSO2, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO
summary(TUKEYSO)
tSO<- glht(modelSO2, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                   ifelse(levels(SO$Treatment)=="Groasis", rgb(0.6,0.6,0.5,1),
                   ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Productivity~Treatment,main=" V)",cex.main=3, 
        ylab="Solanum productivity (Kg) - Dry season", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,9),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Waterboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:2,9,lab=c("a",  "b", "a"),
     cex=2, las=3)  

#################################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM NUMBER OF FRUITS OF WET SEASON IN SANTA CRUZ?
#################################################################################################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Wet_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Fruts~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(Fruts~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")
#######################################################################################
TUKEYSO<- summary(glht(modelSO2, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO
summary(TUKEYSO)
tSO<- glht(modelSO2, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(SO$Treatment)=="Groasis", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Fruts~Treatment,main=" VI)",cex.main=3, 
        ylab="Solanum number of fruits - Wet season", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,55),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Waterboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:2,55,lab=c("a",  "b", "a"),
     cex=2, las=3)  
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM NUMBER OF FRUITS OF DRY SEASON IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Dry_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Fruts~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Fruts~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(Fruts~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")
#######################################################################################
#NON-SIGNIFICANT RESULTS
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM INDIVIDUAL FRUIT WEIGHT OF WET SEASON IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Wet_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(IFW~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(IFW~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(IFW~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")
#######################################################################################
#NON SIGNIFICANT RESULTS
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM INDIVIDUAL FRUIT WEIGHT OF DRY SEASON IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/cinco-cultivos-analysis//resubmission1/Summary_SC_Dry_Solanum.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(IFW~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(IFW~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
modelSO2<- lme(IFW~Treatment, random = ~ 1 | Zona,data=SO)
modelSO2
summary(modelSO2)
Anova(modelSO2, type="II")
#######################################################################################
TUKEYSO<- summary(glht(modelSO2, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO
summary(TUKEYSO)
tSO<- glht(modelSO2, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                   ifelse(levels(SO$Treatment)=="Groasis", rgb(0.6,0.6,0.5,1),
                  ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(IFW~Treatment,main=" VII)",cex.main=3, 
        ylab="Solanum IFW - Dry season", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,0.30),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Waterboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,0.30,lab=c("a",  "b", "b"),
     cex=2, las=3)  

