################################ IMPORTER LES PACKAGES ##################
library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
library(TSA)
library(rgl)

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE

view(table)
df<-table[,-45]
class(df)
dim(df)
summary(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
mx<-Mat01[,-1]
#view(mx)
head(as.vector(mx))
length(as.vector(mx))

#### Representation graphique

ages <- 0:(nrow(mx)-1)
length(ages)
years <- 1977:2019
length(years)
df <- expand.grid(Age = ages, Year = years)
tail(df)
df$MortalityRate <- as.vector(mx)
head(df)
ggplot(df, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "male_Mortality Rates Over Time") +
  theme_minimal()+
  scale_color_viridis_c("mortality EVOLUTION", option = "plasma")

#### 3D plot ###########

persp(ages,years, mx,theta = 40, phi = 10,col = 1:5,ltheta = 100) 

persp3d(ages,1977:2019, mx,theta = 40, phi = 10, col = 3:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################
length(1977:2019)
Mxt<-mx
dim(Mxt)
as.numeric()
ln_m <- Mxt
## Estimation de ax 
ax <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
head(ax)
length(ax)
plot(ax,col="blue",lwd=3,type = "l",main = "Estimation de ax_homme",
     xlab = "age")
# Calcul de matrice  A(x,t)
A <- ln_m - ax
dim(A)
################## Decomposition en valeur singuliere de A(x,t)

SVD <- svd(A,nu=min(85,85),nv = min(44, 44))
SVD_1 <- svd(A,1,1)
v <- SVD_1$v
u <- SVD_1$u
d <- SVD_1$d

b <- u/sum(u)
b
k <- v * sum(u) * d[1]

plot(b,lwd=3,type = "l",main = "b_x Homme",col='blue',
     xlab = "age")
plot(k,x=1977:2019,lwd=3,type = "l",main = "k_x Homme",col='red',
     xlab = "age")

sum(k)
sum(b)

########## Analyse de Kappa t ########

serie <- ts(k,start = c(1977,1),frequency = 1)
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",width = 4)
#Décomposition de la série
summary(serie)
class(serie)
ts_lags(serie)
#ar(diff(AirPassengers))
ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")
adf.test(serie, alternative="stationary")
plot(AirPassengers)
decompose(serie)
?filter

adf.test(AirPassengers)
BoxCox.lambda(AirPassengers)
plot(log(AirPassengers))
dinf <- diff(serie)
adf.test(dinf, alternative="stationary",k=1)
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)
plot(armasubsets(dinf,nar=5,nma=5))
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
fit.S <- Arima(serie,order = c(1,1,0),
               include.drift=FALSE,include.mean = TRUE) #bon model
predictt <- forecast((fit.S), h = 30)
plot(fit.S)
predictt %>% autoplot()
accuracy(fit.S) #RMSE 1.769699
pred<- as.data.frame(predictt)
view(pred)
k_forecasted<-pred[,1]
length(k_forecasted)
b_tk<-b%*%t(k_forecasted)
dim(b_tk)
ln_mxt_forecasted<-ax+b_tk
dim(ln_mxt_forecasted)
persp(ages,2020:2049, ln_mxt_forecasted,theta = 0, phi = 30,col = "blue",ltheta = 50) 
persp3d(ages,2021:2050, ln_mxt_forecasted,theta = 40, phi = 10, col = 2:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 1)

mxt_forecasted<-exp(ax+b_tk)
dim(mxt_forecasted)
view(mxt_forecasted)
mxt_forecasted %>%
  as.matrix()%>%
  write.csv(file = "mxt_forecasted.csv")

############### ANALYSE DES RESIDUS

#ext=log(mx)-ax-b*kt
length(ax)
I = matrix(rep(1,43), nrow=1)
#view(I)
Z1<-ax%*%I
dim(Z1)
dim(b_tk)
Z2<-b%*%t(k)
dim(Z2)
dim(Z1)

ext<-mx-Z1-Z2
dim(ext)

persp(ages,years, ext,theta = 200, phi = 10,col = 1:5,ltheta = 50)
library(plotly)
fig <- plot_ly(z = t(ext), type = "heatmap")
fig


# moyenne par age et par annee

moy_age<-rowMeans(ext)
length(moy_age)
moy_tmps<-colMeans(ext)
length(moy_tmps)
variance<- sd(ext[1:85,])
plot(moy_age,xlim = c(0,120),type = "l",lwd=3,col=3)
plot(moy_tmps,ylim=c(-0.15,0.15),xlim = c(0,50),type = "l",lwd=3,col=4)
checkresiduals(moy_tmps)
checkresiduals(moy_age)

# variance par age et par annee

G1<-as.numeric()
G1
j1<-0
for (i in 1:43) {
  G1[j1<-j1+1]<-var(ext[,i])
}
length(G1)
plot(G1,type = "l",col=6,lwd=3,ylim = c(0,0.03),
     main = "variance par anne_Homme")

G2<-as.numeric()
G2
j2<-0
for (i in 1:85) {
  G2[j2<-j2+1]<-var(ext[i,])
}
length(G2)
plot(G2,type = "l",col=5,lwd=3,ylim = c(0,0.05),
     main = "variance par age_Homme")

MUxt<-cbind(mx,W2)
dim(MUxt)

persp3d(ages,1977:2049, MUxt,theta = 40, phi = 10, col = c("blue","red","white"),ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(z = ~MUxt) %>% add_surface(
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




length(ax)
I10 = matrix(rep(1,30), nrow=1)
length(I10)
Y1<-ax%*%I10
dim(Y1)
dim(b_tk)

m2_forecasted<-exp(Y1+b_tk)
dim(m2_forecasted)
view(m2_forecasted)
m_forecasted %>%
  as.matrix()%>%
  write.csv(file = "morta_forecasted.csv")
