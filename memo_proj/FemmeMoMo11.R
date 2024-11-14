
######################        ##################

# Chargement des packages
library(demography)
library(forecast)
library(tidyverse)
library(StMoMo)
library(lifecontingencies)

## Chargement des données 

femme<-as.matrix(data_femme[,-1]) #mx_femme
class(femme)
dim(femme)
summary(femme)
mode(femme)
length(as.vector(femme))
mortality_data<-femme
dim(mortality_data)

# Conversion des données en objet demogdata
# Création d'un objet demogdata pour les taux de mortalité
colnames(mortality_data)<-1977:2019
class(colnames(mortality_data))
rownames(mortality_data)<-0:119
age <- as.numeric(rownames(mortality_data))
year <- as.numeric(colnames(mortality_data))
class(year)
mx <- as.matrix(mortality_data)
class(mx)
nrow(mx)

######################## QX ########################

dv<-as.matrix(data_femme[,-1])
dim(dv)
class(dv)
#tb1(dv[,2])

A3<-0:119
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
LC<-lc(link ="logit")
CBD<- cbd(link = "log")
APC<- apc(link="log")
M7<- m7(link = "log")

LCfit<-fit(LC,data=NZStMoMo)
plot(LCfit,lwd=3,col="red")
Kt2
Ktt<-matrix(km,ncol = 43)
Ktt
LCfit$kt<-Ktt
colnames(LCfit$kt)<-1977:2019

LCres<-residuals(LCfit)
plot(LCres,type="colourmap")
AIC(LCfit)
BIC(LCfit)
LCfor<-forecast(LCfit,h=31)
#######################
class(LCfor$fitted)
M<-LCfor$fitted
adj<-mx2qx(M)
plot(log(femme[,43]),type="l",col=4,lwd=3.5)
lines(log(adj[,43]),type="l",col=6,lwd=3.5)
esp1<- probs2lifetable(femme[,43],type = "qx")
esp1

exp1<- numeric(120)
for (i in 0:119) {
  exp1[i+1] <-exn(esp1,x=i)
}

exp1


esp2<- probs2lifetable(adj[,43],type = "qx")
esp2

exp2<- numeric(120)
for (i in 0:119) {
  exp2[i+1] <-exn(esp2,x=i)
}

exp2

plot(exp1,type="l",col=4,lwd=3.5)
lines(exp2,type="l",col=6,lwd=3.5)

#####
LCfor$kt.f$upper
L1<-as.list(LCfor$kt.f$upper)
L2<-as.vector(L1)

k=0
for (i in 32:62) {
 k[i] <- L2[[i]]
}
km<-k[32:62]
km
class(LCfor$kt.f$upper)

s<-LCfor$kt.f$upper
view(s)


as.matrix(LCfor$kt.f$upper)
#######################
plot(LCfor)
rt<-LCfor$rates
view(rt)
dim(LCfor$rates)
lc_qx<- mx2qx(rt)
length(lc_qx[,1])
dim(lc_qx)
lc_lifetable<- probs2lifetable(lc_qx[,1],type = "qx")
lc_lifetable

