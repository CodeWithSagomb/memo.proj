


#####################################

view(Mat01)
df<-QX_FEMMES[,-1]
class(df)
dim(df)
summary(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
view(MAT01)
head(as.vector(Mat01))
length(as.vector(Mat01))

#### Representation graphique

ages <- 0:(nrow(Mat01)-1)
length(ages)
years <- 1977:2019
length(years)
df <- expand.grid(Age = ages, Year = years)
tail(df)
df$MortalityRate <- as.vector(Mat01)
head(df)
ggplot(df, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "male_Mortality Rates Over Time") +
  theme_minimal()+
  scale_color_viridis_c("mortality EVOLUTION", option = "plasma")

#### 3D plot ###########

persp(ages,years, Mat01,theta = 40, phi = 10,col = 1:5,ltheta = 100) 

persp3d(ages,1977:2019, Mat01,theta = 40, phi = 10, col = 3:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################
length(1977:2019)
Mxt<-Mat01
dim(Mxt)
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

ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")
adf.test(serie, alternative="stationary")

ar(diff(serie)) #Order selected 1
BoxCox.lambda(serie)
dinf <- diff(serie)
adf.test(dinf, alternative="stationary")
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(dinf, ar.max = 4, ma.max = 4)
plot(armasubsets(Dinf,nar=4,nma=4))
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

#### ANALYSE DE RESIDUS 

checkresiduals(fit.1)
checkresiduals(fit.2)
checkresiduals(fit.3)
checkresiduals(fit.4)
shapiro.test(fit.1$residuals)
shapiro.test(fit.2$residuals)
shapiro.test(fit.3$residuals)
shapiro.test(fit.4$residuals)

qqnorm(fit.1$residuals)
qqline(fit.1$residuals)

qqnorm(fit.2$residuals)
qqline(fit.2$residuals)

qqnorm(fit.3$residuals)
qqline(fit.3$residuals)

qqnorm(fit.4$residuals)
qqline(fit.4$residuals)

Box.test(fit.1$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.2$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.3$residuals, lag = 10, type = "Ljung-Box")
Box.test(fit.4$residuals, lag = 10, type = "Ljung-Box")
runs(fit.1$residuals)

fit.S <- Arima(serie,order = c(1,1,0),
               include.drift=TRUE,include.mean = TRUE) # bon model
predictt <- forecast((fit.S), h = 30)
plot(fit.S)
predictt %>% autoplot()
accuracy(fit.S) #RMSE 1.769699
pred<- as.data.frame(predictt)
view(pred)

