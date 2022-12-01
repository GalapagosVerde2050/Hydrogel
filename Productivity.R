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
RW<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Productividad.xlsx")
RW
FA = RW%>%
  group_by(CODIGO)%>%
  summarize(Frutos=sum(FRUTOS),
            Productividad=sum(PESO),
            IFW=mean(PESO),
            date=max(FECHA),
            Treatment=unique(TRATAMIENTO),
            Island=max(ISLA),
            Species=max(ESPECIE))
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/")
write.csv(FA, "ProduSUM.csv")


##################### OBTAINING HMAX ######################################
SD<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/General.xlsx")
SD
XA = SD%>%
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
DF= FA%>% add_column(HMAX=NA)
DF
DF$HMAX= XA$HMAX[match(FA$CODIGO, XA$CODIGO)]
DF
setwd("C:/Users/DETPC/Documents/R/github/gv2050party/")
write.csv(DF, "SUM3.csv")


############################################################################
#
#                        B   R  A  S   S   I   C  A 
#
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON BRASSICA PRODUCTIVITY OF WET SEASON IN SANTA CRUZ?
#############################

BA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Brassica/BRAS.xlsx")
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


##HMAX###
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(HMAX~Treatment, data=BA,las=2)
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
df=BA%>% filter(Treatment=="Hidrogel")%>% select(Productivity, IFW, HMAX)
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
df=BA%>% filter(Treatment=="Control")%>% select(Productivity, IFW, HMAX)
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

CA<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Capsicum/CAPS.xlsx")
CA
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(Productivity~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
#modelCA2=lme(Productivity~Treatment, random = ~ 1 | Zona,data=CA)
#modelCA2
#Anova(modelCA2, type="II")
#AIC(modelCA1)
#AIC(modelCA2)
#xCA = (residuals(modelCA2))
#xCA
TUKEYCA1<- summary(glht(modelCA1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYCA1
summary(TUKEYCA1)
tCA1<- glht(modelCA1, linfct = mcp(Treatment= "Tukey"))
t.cldCA1 <- cld(tCA1)   # letter-based display
t.cldCA1
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(CA$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(CA$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(CA$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldCA1, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Productivity~Treatment,main=" I)",cex.main=3, 
        ylab="Capsicum Maximum plant height (cm) -  Santa Cruz", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,7),frame=T, data=CA, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,7,lab=c("ab",  "a", "b"),
     cex=2, las=3)

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM NUMBER OF FRUITS IN  SANTA CRUZ?
#####################################################################################

boxplot(Fruits~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(Fruits~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
#modelCA2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CA)
#modelCA2
#Anova(modelCA2, type="II")
#AIC(modelCA1)
#AIC(modelCA2)
TUKEYCA1<- summary(glht(modelCA1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYCA1
summary(TUKEYCA1)
tCA1<- glht(modelCA1, linfct = mcp(Treatment= "Tukey"))
t.cldCA1 <- cld(tCA1)   # letter-based display
t.cldCA1
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(CA$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(CA$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(CA$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldCA1, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Fruits~Treatment,main=" II)",cex.main=3, 
        ylab="Capsicum Maximum plant height (cm) -  Santa Cruz", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,70),frame=T, data=CA, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,70,lab=c("ab",  "a", "b"),
     cex=2, las=3)

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM IFW  IN SANTA CRUZ?
#####################################################################################

boxplot(IFW~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(IFW~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
#modelCA2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CA)
#modelCA2
#Anova(modelCA2, type="II")
#AIC(modelCA1)
#AIC(modelCA2)
TUKEYCA1<- summary(glht(modelCA1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYCA1
summary(TUKEYCA1)
tCA1<- glht(modelCA1, linfct = mcp(Treatment= "Tukey"))
t.cldCA1 <- cld(tCA1)   # letter-based display
t.cldCA1
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(CA$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(CA$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(CA$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldCA1, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(IFW~Treatment,main=" III)",cex.main=3, 
        ylab="Capsicum Maximum plant height (cm) -  Santa Cruz", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,0.20),frame=T, data=CA, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,0.20,lab=c("b",  "a", "b"),
     cex=2, las=3)

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CAPSICUM HMAX  IN SANTA CRUZ?
#####################################################################################

boxplot(HMAX~Treatment, data=CA,las=2)
#No transformation
CA$Treatment=as.factor(CA$Treatment)
modelCA1<- lm(HMAX~Treatment,data=CA) 
modelCA1
Anova(modelCA1, type="II")
#modelCA2=lme(Fruts~Treatment, random = ~ 1 | Zona,data=CA)
#modelCA2
#Anova(modelCA2, type="II")
#AIC(modelCA1)
#AIC(modelCA2)
###### NON SIGNIFICANT RESULTS
TUKEYCA1<- summary(glht(modelCA1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYCA1
summary(TUKEYCA1)
tCA1<- glht(modelCA1, linfct = mcp(Treatment= "Tukey"))
t.cldCA1 <- cld(tCA1)   # letter-based display
t.cldCA1
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(CA$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(CA$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(CA$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldCA1, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(HMAX~Treatment,main=" III)",cex.main=3, 
        ylab="Capsicum Maximum plant height (cm) -  Santa Cruz", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,270),frame=T, data=CA, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,270,lab=c("b",  "a", "b"),
     cex=2, las=3)

##############Correlation Matrix-Capsicum########################
##Hydrogel##
df=CA%>% filter(Treatment=="Hidrogel")%>% select(Productivity,Fruits, IFW, HMAX)
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
##Growboxx##
df=CA%>% filter(Treatment=="Growboxx")%>% select(Productivity,Fruits, IFW, HMAX)
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
write.csv(res2$P,"CAPCORG.csv", row.names = TRUE)
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
df=CA%>% filter(Treatment=="Control")%>% select(Productivity, Fruits, IFW, HMAX)
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

CI<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Citrullus/CITR.xlsx")
CI
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(IFW~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
#modelCI2=lme(IFW~Treatment, random = ~ 1 | Zona,data=CI)
#modelCI2
#Anova(modelCI2, type="II")
#AIC(modelCI1)
#AIC(modelCI2)
#######################################################################################
# NON SIGNIFICANT RESULTS  
#######################################################################################
#################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CITRULUS NUMBER OF FRUITS  IN SANTA CRUZ?
#################################################################################
boxplot(Fruits~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(Fruits~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
#modelCI2=lme(Fruits~Treatment, random = ~ 1 | Zona,data=CI)
#modelCI2
#Anova(modelCI2, type="II")
#AIC(modelCI1)
#AIC(modelCI2)
####NON SIGNIFICANT RESULTS####
#################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CITRULUS IFW  IN SANTA CRUZ?
#################################################################################
boxplot(IFW~Treatment, data=CI,las=2)
#No transformation
CI$Treatment=as.factor(CI$Treatment)
modelCI1<- lm(IFW~Treatment,data=CI) 
modelCI1
Anova(modelCI1, type="II")
#modelCI2=lme(IFW~Treatment, random = ~ 1 | Zona,data=CI)
#modelCI2
#Anova(modelCI2, type="II")
#AIC(modelCI1)
#AIC(modelCI2)

##############Correlation Matrix - Citrullus - Hydrogel########################
##Hydrogel##
df=CI%>% filter(Treatment=="Hidrogel")%>% select(Productivity, IFW, HMAX)
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
df=CI%>% filter(Treatment=="Control")%>% select(Productivity, IFW, HMAX)
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

CU<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Cucumis/CUCU.xlsx")
CU
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1<- lm(Productivity~Treatment,data=CU) 
modelCU1
Anova(modelCU1, type="II")
#modelCU2=lme(Productivity~Treatment, random = ~ 1 | Zona,data=CU)
#modelCU2
#Anova(modelCU2, type="II")
#AIC(modelCU1)
#AIC(modelCU2)

############################################################################
#NON SIGNIFICANT RESULTS 
############################################################################
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON CUCUMIS SATIVUS NUMBER OF FRUITS IN SANTA CRUZ?
#############################
boxplot(Fruits~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1= lm(Fruts~Treatment, data=CU)
modelCU1
Anova(modelCU1, type="II")
#modelCU2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=CU)
#modelCU2
#summary(modelCU2)
#Anova(modelCU2, type="II")
#AIC(modelCU1)
#AIC(modelCU2)
############################################################################
#NON SIGNIFICANT RESULTS 
############################################################################

#####################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON CUCUMIS INDIVIDUAL FRUIT WEIGHT IN WET SEASON IN SANTA CRUZ?
#####################################################################################
boxplot(IFW~Treatment, data=CU,las=2)
#No transformation
CU$Treatment=as.factor(CU$Treatment)
modelCU1= lm(IFW~Treatment, data=CU)
modelCU1
summary(modelCU1)
Anova(modelCU1, type="II")
#modelCU2<- lme(IFW~Treatment, random = ~ 1 | Zona,data=CU)
#modelCU2
#summary(modelCU2)
#Anova(modelCU2, type="II")
#AIC(modelCU1)
#AIC(modelCU2)
#######################################################################################
#NON SIGNIFICANT RESULTS

##############Correlation Matrix - Cucumis - Hydrogel########################
##Hydrogel##
df=CU%>% filter(Treatment=="Hidrogel")%>% select(Productivity, IFW, HMAX)
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
df=CU%>% filter(Treatment=="Control")%>% select(Productivity, IFW, HMAX)
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


#######################################################################################
# NON SIGNIFICANT RESULTS AND NOT DRY SEASON
#######################################################################################
############################################################################
#
#       S      O     L     A      N     U     M
#
############################################################################

#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM PRODUCTIVITY  IN SANTA CRUZ?
#############################

SO<-read_xlsx("C:/Users/DETPC/Documents/R/github/gv2050party/Solanum/SOLA.xlsx")
SO
oldpar=par(mai=c(2,0.5,0.5,0.5))
boxplot(Productivity~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Productivity~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
#modelSO2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=SO)
#modelSO2
#summary(modelSO2)
#Anova(modelSO2, type="II")
TUKEYSO1<- summary(glht(modelSO1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO1
summary(TUKEYSO1)
tSO1<- glht(modelSO1, linfct = mcp(Treatment= "Tukey"))
t.cldSO1 <- cld(tSO1)   # letter-based display
t.cldSO1
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(SO$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO1, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Productivity~Treatment,main=" I)",cex.main=3, 
        ylab="Solanum Productivity (Kg) -  Santa Cruz", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,7),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:4,7,lab=c("b",  "a", "b"),
     cex=2, las=3)


#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM NUMBER O FRUITS  IN SANTA CRUZ?
#############################
boxplot(Fruits~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(Fruits~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
#modelSO2<- lme(Productivity~Treatment, random = ~ 1 | Zona,data=SO)
#modelSO2
#summary(modelSO2)
#Anova(modelSO2, type="II")
#######################################################################################
TUKEYSO<- summary(glht(modelSO1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO
summary(TUKEYSO)
tSO<- glht(modelSO1, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(SO$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(Fruits~Treatment,main=" V)",cex.main=3, 
        ylab="Solanum number of fruits", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,50),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Waterboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:3,50,lab=c("b",  "a", "b"),
     cex=2, las=3)  

#################################################################################################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM IFW IN SANTA CRUZ?
#################################################################################################

boxplot(IFW~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(IFW~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
#modelSO2<- lme(Fruts~Treatment, random = ~ 1 | Zona,data=SO)
#modelSO2
#summary(modelSO2)
#Anova(modelSO2, type="II")


TUKEYSO2<- summary(glht(modelSO1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO2
summary(TUKEYSO2)
tSO<- glht(modelSO1, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(SO$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(IFW~Treatment,main=" VI)",cex.main=3, 
        ylab="Solanum IFW", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,0.5),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:3,0.5,lab=c("b",  "a", "b"),
     cex=2, las=3)  
#############################
#       WHAT IS THE EFFECT OF TREATMENTS ON SOLANUM HMAX IN SANTA CRUZ?
#############################
boxplot(HMAX~Treatment, data=SO,las=2)
#No transformation
SO$Treatment=as.factor(SO$Treatment)
modelSO1= lm(HMAX~Treatment, data=SO)
modelSO1
summary(modelSO1)
Anova(modelSO1, type="II")
#modelSO2<- lme(~Treatment, random = ~ 1 | Zona,data=SO)
#modelSO2
#summary(modelSO2)
#Anova(modelSO2, type="II")
TUKEYSO3<- summary(glht(modelSO1, linfct = mcp(Treatment= "Tukey")), test = adjusted("holm"))
TUKEYSO3
summary(TUKEYSO3)
tSO<- glht(modelSO1, linfct = mcp(Treatment= "Tukey"))
t.cldSO <- cld(tSO)   # letter-based display
t.cldSO
oldpar=par(mai=c(1.4,1,1,1),no.readonly = T)
myColors <-        ifelse(levels(SO$Treatment)=="Control" , rgb(0.8,0.8,0.8,1) , 
                          ifelse(levels(SO$Treatment)=="Growboxx", rgb(0.6,0.6,0.5,1),
                                 ifelse(levels(SO$Treatment)=="Hidrogel", rgb(1,1,1,1),     
                                        "grey90" )))


plot(t.cldSO, cex.main=0.5, cex.lab=0.75, cex.axis=0.75, las=2, col=myColors)

boxplot(HMAX~Treatment,main=" VI)",cex.main=3, 
        ylab="Solanum HMAX (cm)", xlab = "" ,
        cex.lab=1.5,axes=F, ylim=c(0,400),frame=T, data=SO, col=myColors)
axis(2, cex.axis=1.25);
axis(1,at=1:3,labels=c("Control",   
                       "Growboxx",
                       "Hydrogel"
),cex.axis=1.5, las=2)
text(1:3,400,lab=c("b",  "a", "b"),
     cex=2, las=3)  
