
################################ Chargement des packages  ##############################
library(demography)
library(forecast)
library(tidyverse)
library(StMoMo)
library(lifecontingencies)
library(rgl)

##############################  Importation de taux de morta Mx  ##################

df<-dataONS02[,-1] #(mx)
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
rownames(mortality_data)<-0:119
age <- as.numeric(rownames(mortality_data))
length(age)
year <- as.numeric(colnames(mortality_data))
class(year)
mx <- as.matrix(mortality_data)
class(mx)
nrow(mx)

######################## Importation de Quotien QX ########################

dv<-as.matrix(dataONS02[,-1]) #qx
dim(dv)
class(dv)
view(dv)
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

###################################################

# Création de l'objet demogdata
data_demog <- demogdata(mx, pop=mat_pop, ages=age, years=year, 
                        type="mortality", label="Mortality Rates",name="male")

data_demog
NZStMoMo <- StMoMoData(data_demog, series = "male")
summary(NZStMoMo)
dxt<-NZStMoMo$Dxt
dim(dxt)
ext<-NZStMoMo$Ext
dim(ext)
LC<-lc(link ="log")
CBD<- cbd(link = "log")
APC<- apc(link="log")
M7<- m7(link = "log")

########  Modelisation ,Projection et Estimation de l'Esperance H=30 ########## 

LCfit<-fit(LC,data=NZStMoMo)
class(LCfit$kt)
length(LCfit$ax)
plot(LCfit)
Ax<-LCfit$ax
Bx<-LCfit$bx
Kt<-LCfit$kt
# create data
age_x <- 0:119
year_t<-1977:2019
kappat_t<-as.vector(Kt)
Valeur_ax <- Ax
Valeur_bx <-Bx
data1 <- data.frame(xValue,Valeur_ax,Valeur_bx)
data2<-data.frame(year_t,kappat_t)
length(year_t)
# Plot
ggplot(data1, aes(x=age_x, y=Valeur_ax)) +
  geom_line( color="brown", size=2, alpha=1.5, linetype=1)+
  ggtitle("Comportement moyen de Ln(mxt)")

ggplot(data1, aes(x=age_x, y=Valeur_bx)) +
  geom_line( color="brown", size=2, alpha=1, linetype=1)+
  ggtitle("Sensibilité a l'age x par rapport a la Mortalité")

ggplot(data2, aes(x=year_t, y=kappat_t)) +
  geom_line( color="blue", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")
class(k)
dim(t(k))

LCfit$kt<-(tp)
k
k2
###################### kappa.t as ts#######################

tpp<-as.vector(tp)
serie <- ts(tpp,start = c(1977,1))
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

ar(diff(serie))
ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")
adf.test(serie, alternative="stationary")

dinf <- diff(serie)
adf.test(dinf, alternative="stationary")
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)
plot(armasubsets(dinf,nar=3,nma=3))

final_model <- auto.arima(serie)
final_model
summary(final_model)
#################################################
colnames(LCfit$kt)<-1977:2019
length(1977:2019)
dim(tp)
summary(LCfit)
class(LCfit)
#plot(LCfit,lwd=3,col="blue")
LCres<-residuals(LCfit)
plot(LCres,type="colourmap")
plot(LCres,type="scatter",col="brown")
AIC(LCfit)
BIC(LCfit)
LCfor<-forecast(LCfit,h=31)
plot(LCfor,lwd=3,col="brown")
rt<-LCfor$rates
dim(LCfor$rates)

ln_mxt_for<-log(rt)
ln_mx<-log(mortality_data)
dim(ln_mx)
ENS_ln<-cbind(ln_mx,ln_mxt_for)
dim(ENS_ln)

persp3d(0:119,2020:2050,ln_mxt_for,theta = 40, phi = 10, col = "#85e085",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

persp3d(0:119,1977:2050,ENS_ln,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~ENS_ln) %>% add_surface(
  contours = list(
    z = list(
      show=TRUE,
      usecolormap=TRUE,
      highlightcolor="#ff0000",
      project=list(z=TRUE)
    )
  )
)
fig <- fig %>% layout(
  scene = list(
    camera=list(
      eye = list(x=1.87, y=0.88, z=-0.64)
    )
  )
)

fig

lc_qx<- mx2qx(rt)
length(lc_qx[,1])
dim(lc_qx)

lc_lifetable<- probs2lifetable(lc_qx[,31],type = "qx")
lc_lifetable

exn(lc_lifetable,x=0)

extract_exn<- function(lcqx){
  
  v1<-numeric(length(lcqx))
  
  for (i in 0:(length(lcqx)-1)) {
    v1[i+1] <-exn(lc_lifetable,x=i)
  }
  
  return(data.frame(v1))
  
}
extract_exn(lc_qx[,1])

##########################################################

