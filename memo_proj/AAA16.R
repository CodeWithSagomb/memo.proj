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

dt<-table[-(80:84),-c(1,45)]
class(dt)
dim(dt)
summary(dt)
dt<-as.matrix(dt)
class(dt)
mode(dt)
head(as.vector(dt))
length(as.vector(dt))

##### Representation graphique

ages <- 0:(nrow(dt)-1)
length(ages)
years <- 1977:2019
length(years)
dfm <- expand.grid(Age = ages, Year = years)
tail(dfm)
dfm$MortalityRate <- as.vector(dt)
head(dfm)
ggplot(dfm, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "male_Mortality Rates Over Time") +
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")

#### 3D plot ###########

persp3d(ages,years, dt,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################

ln_m <- dt
dim(ln_m)
## Estimation de ax 
ax <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
length(ax)
datax <- data.frame(age.x=0:79,ax=ax)
ggplot(datax, aes(x=age.x, y=ax)) +
  geom_line( color="purple", size=2, alpha=1.5, linetype=1)+
  ggtitle("Comportement moyen de Ln(mxt)")

# Calcul de matrice  A(x,t)
A <- ln_m - ax
dim(A)

################## Decomposition en valeur singuliere de A(x,t)

SVD_1 <- svd(A,1,1)
v <- SVD_1$v
u <- SVD_1$u
d <- SVD_1$d
d

b.x <- u/sum(u)
k.t <- d[1]*sum(u)*v
k.t

data.bx <- data.frame(age.x=0:79,bx=b.x)
ggplot(data.bx, aes(x=age.x, y=bx)) +
  geom_line( color="blue", size=2, alpha=1.5, linetype=1)+
  ggtitle("Sensibilité a l'age x par rapport a la mortalité")

data.kt<-data.frame(year.t=1977:2019,kt=k.t)
ggplot(data.kt, aes(x=year.t, y=kt)) +
  geom_line( color="red", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")

sum(k.t)
sum(b.x)
class(k.t)
t(k.t)

########## Analyse de Kappa t ########

serie <- ts(k.t,start = c(1977,1))
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

#Décomposition de la série
summary(serie)
ts_lags(serie)
ar(diff(serie))
ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")
adf.test(serie, alternative="stationary")

dinf <- diff(serie)
adf.test(dinf, alternative="stationary")
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)
plot(armasubsets(dinf,nar=5,nma=5))

full_model <- auto.arima(dinf)
full_model

# model optimal:arima(0,1,0),arima(1,1,0),arima(1,1,1),arima(0,1,1)

fit.1 <- dinf %>% Arima(order = c(1,0,0))
v1<-summary(fit.1)
v1
fit.2 <- dinf %>% Arima(order = c(0,0,0))
v2<-summary(fit.2)
v2
fit.3 <- dinf %>% Arima(order = c(1,0,1))
v3<-summary(fit.3)
v3
fit.4 <- dinf %>% Arima(order = c(0,1,1))
v4<-summary(fit.4)
v4
veec1<-c(v1=v1$aic,v2=v2$aic,v3=v3$aic,v4=v4$aic)
veec1
dat<-list(v1=v1$aic,v2=v2$aic,v3=v3$aic,v4=v4$aic)
dat
checkresiduals(fit.1)
shapiro.test(fit.1$residuals)
qqnorm(fit.1$residuals)
qqline(fit.1$residuals)
Box.test(fit.1$residuals, lag = 10, type = "Ljung-Box")
runs(fit$residuals)

fit.S <- Arima(serie,order = c(1,0,0),
               include.drift=TRUE,include.mean = TRUE) #bon model
predictt <- forecast((fit.S), h = 30)
predictt %>% autoplot()
accuracy(fit.S) #RMSE 1.769699
pred<- as.data.frame(predictt)
view(pred)
k_forecasted<-pred[,1]
length(k_forecasted)
b_tk<-b1%*%t(k_forecasted)
dim(b_tk)
ln_mxt_forecasted<-ax1+b_tk
dim(ln_mxt_forecasted)
persp(0:119,2020:2049, ln_mxt_forecasted,theta = 0, phi = 30,col = "red",ltheta = 50) 
persp3d(0:119,2020:2049, ln_mxt_forecasted,theta = 40, phi = 10, col = 10,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

LN_Mxt.past<-LN_Mxt
global_ln_mxt<-cbind(LN_Mxt.past,ln_mxt_forecasted)
dim(global_ln_mxt)
global_ln_mxt<-as.matrix(global_ln_mxt)
class(global_ln_mxt)
persp3d(0:119,1977:2049, global_ln_mxt,theta = 40, phi = 10, col = c("#85e085"),ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~global_ln_mxt) %>% add_surface(
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


mxt_forecasted<-exp(ax1+b_tk)
dim(mxt_forecasted)
#view(mxt_forecasted)
#mxt_forecasted %>%
#as.matrix()%>%
#write.csv(file = "MXT_forecasted.csv")

qx_forcasted<- mx2qx(mxt_forecasted)
dim(qx_forcasted)
view(qx_forcasted)

tab_esp<-probs2lifetable(qx_forcasted[,30] ,radix = 100000,type = "qx")
tab_esp
A<-tabbx(qx_forcasted[,30])
A
