
FemmeONS %>%
as.matrix()%>%
write.csv(file = "FEM_ONS.csv")


################################ IMPORTER LES PACKAGES ######

library(lifecontingencies)
library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
library(TSA)
library(caret)
library(rgl)

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE

femme<-as.matrix(data_femme[,-1])
class(femme)
dim(femme)
summary(femme)
mode(femme)
length(as.vector(femme))

#### Representation graphique

ages_femme <- 0:(nrow(femme)-1)
length(ages_femme)
years <- 1977:2019
length(years)
dfemme <- expand.grid(Age = ages_femme, Year = years)
tail(dfemme)
dfemme$MortalityRate <- as.vector(femme)
head(dfemme)
ggplot(dfemme, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "Taux de Mortalité Femme") +
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")

persp3d(ages_femme,years, femme,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################

ln_mxt_fem <- femme
dim(ln_mxt_fem)
## Estimation de ax 
ax_femme2 <- apply(ln_mxt_fem, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
length(ax_femme2)
datax_femme <- data.frame(age.x=0:119,ax=ax_femme2)
ggplot(datax_femme, aes(x=age.x, y=ax)) +
  geom_line( color="purple", size=2, alpha=1.5, linetype=1)+
  ggtitle("Comportement moyen de Ln(mxt)")


# Calcul de matrice  A(x,t)
M <- ln_mxt_fem - ax_femme
dim(M)

################## Decomposition en valeur singuliere de A(x,t)

SVD_2 <- svd(M,1,1)
v2 <- SVD_2$v
u2 <- SVD_2$u
d2 <- SVD_2$d

BBx2 <- u2/sum(u2)
Kt <- d[1]*sum(u2)*v2
Kt
length(BBx2)
dat.bx <- data.frame(age.x=0:119,bx=BBx2)
ggplot(dat.bx, aes(x=age.x, y=bx)) +
  geom_line( color="blue", size=2, alpha=1.5, linetype=1)+
  ggtitle("Sensibilité a l'age x par rapport a la mortalité")

dat.Kt<-data.frame(year.t=1977:2019,kt=Kt)
ggplot(dat.Kt, aes(x=year.t, y=Kt)) +
  geom_line( color="red", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")

sum(Kt)
sum(Bx)

########## Analyse de Kappa t ########

ts_fem <- ts(Kt,start = c(1977,1))
length(ts_fem)
ts_info(ts_fem)
ts_plot(ts_fem,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

#Décomposition de la série
summary(ts_fem)
ts_lags(ts_fem)
ar(diff(ts_fem))
ggAcf(ts_fem) + ggtitle("ACF of kappa.t")
ggPacf(ts_fem) + ggtitle("PACF of kappa.t")
adf.test(ts_fem, alternative="stationary")

fem_model <- auto.arima(ts_fem)
fem_model

fit.fem <- Arima(ts_fem,order = c(0,1,0),
                 include.drift=TRUE,include.mean = TRUE) #bon model
predict.fem <- forecast((fit.fem), h = 31)
predict.fem %>% autoplot()
accuracy(fit.fem) #RMSE 2.797706
pred.fem<- as.data.frame(predict.fem)
view(pred.fem)

k_pred.fem<-pred.fem[,1]
length(k_pred.fem)
B_tk<-BBx%*%t(k_pred.fem)
dim(B_tk)
ln_mxt_pred.fem<-ax_femme+B_tk
dim(ln_mxt_pred.fem)

persp3d(0:119,2020:2050,ln_mxt_pred.fem ,theta = 40, phi = 10, col = 10,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

mxt_proj.fem<-exp(ax_femme+B_tk)
dim(mxt_proj.fem)

qx_proj_fem<- mx2qx(mxt_proj.fem)
dim(qx_proj_fem)
view(qx_forcasted)

tab_esp.fem<-probs2lifetable(qx_proj_fem[,31] ,radix = 100000,type = "qx")
tab_esp.fem
