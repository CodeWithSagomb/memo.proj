
######################        ##################

# Chargement des packages
library(demography)
library(forecast)
library(tidyverse)
library(StMoMo)
library(lifecontingencies)

# Chargement des données
# Supposons que les données de mortalité sont dans un fichier CSV nommé "mortality_data.csv"
# avec les âges en lignes et les années en colonnes
#mortality_data <- read.csv("mortality_data.csv", row.names = 1)

df<-Mxdata[,-c(1,45)]
class(df)
dim(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
mx<-Mat01
view(mx)
mortality_data<-mx
dim(mortality_data)
# Conversion des données en objet demogdata
# Création d'un objet demogdata pour les taux de mortalité
colnames(mortality_data)<-1977:2019
class(colnames(mortality_data))
rownames(mortality_data)<-0:84
age <- as.numeric(rownames(mortality_data))
year <- as.numeric(colnames(mortality_data))
class(year)
mx <- as.matrix(mortality_data)
class(mx)
nrow(mx)

######################## QX ########################

dv<-as.matrix(QX_MALEDATA[,-c(1,45)])
dim(dv)
class(dv)
#tb1(dv[,2])

A3<-0:84
for (i in 1:ncol(dv) ) {
  A<-tb1(dv[,i])
  A2<-as.matrix(A[,3])
  A3<-cbind(A3,A2)
}
dim(A3)
mat_pop<-A3[,-1]
dim(mat_pop)
dim(mx)
###################################################################

# Création de l'objet demogdata
data_demog <- demogdata(mx, pop=mat_pop, ages=age, years=year, 
                        type="mortality", label="Mortality Rates",name="male")

data_demog
NZStMoMo <- StMoMoData(data_demog, series = "male")
summary(NZStMoMo)
dxt<-NZStMoMo$Dxt
ext<-NZStMoMo$Ext
LC<-lc(link ="log")
CBD<- cbd(link = "log")
APC<- apc(link="log")
M7<- m7(link = "log")

LCfit<-fit(LC,data=NZStMoMo)
plot(LCfit,lwd=3,col="red")
tp<-LCfit$kt
class(tp)
dim(tp)
tp
class(k.t)
LCfit$kt<-t(k.t)
colnames(LCfit$kt)<-1977:2019


LCres<-residuals(LCfit)
plot(LCres,type="scatter")
AIC(LCfit)
BIC(LCfit)
LCfor<-forecast(LCfit,h=31)
plot(LCfor)
rt<-LCfor$rates
dim(LCfor$rates)
lc_qx<- mx2qx(rt)
length(lc_qx[,1])
dim(lc_qx)
lc_lifetable<- probs2lifetable(lc_qx[,31],type = "qx")
lc_lifetable
length(k)
class(k)
t(k)

APCfit<-fit(APC,data=NZStMoMo)
plot(APCfit,lwd=3,col="red")
APCres<-residuals(APCfit)
plot(APCres,type="scatter")
AIC(APCfit)
BIC(APCfit)
APCfor<-forecast(APCfit,h=30)
plot(APCfor)
rt<-LCfor$rates
dim(LCfor$rates)
lc_qx<- mx2qx(rt)
length(lc_qx[,1])
dim(lc_qx)
lc_lifetable<- probs2lifetable(lc_qx[,1],type = "qx")
lc_lifetable


###########################################################