# Example data
age <- 0:3
Nx <- c(100000, 99000, 98500, 98205) # Number of people alive at each age
Dx <- c(1000, 500, 300, 200)         # Number of deaths at each age
qx <- Dx / Nx                        # Probability of death at each age
px <- 1 - qx                         # Probability of survival at each age

# Calculate number of survivors (lx)
lx <- numeric(length(Nx))
lx[1] <- 100000 # Starting point
for (i in 2:length(Nx)) {
  lx[i] <- lx[i-1] * px[i-1]
}
lx
2:length(Nx)
# Calculate person-years lived (Lx)
Lx <- numeric(length(Nx))
for (i in 1:(length(Nx)-1)) {
  Lx[i] <- (lx[i] + lx[i+1]) / 2
}
Lx[length(Nx)] <- lx[length(Nx)]

# Calculate total person-years lived above age x (Tx)
Tx <- numeric(length(Nx))
Tx[length(Nx)] <- Lx[length(Nx)]
for (i in (length(Nx)-1):1) {
  Tx[i] <- Tx[i+1] + Lx[i]
}

# Calculate life expectancy (ex)
ex <- Tx / lx

# Combine into a data frame
life_table <- data.frame(
  Age = age,
  Nx = Nx,
  Dx = Dx,
  qx = qx,
  px = px,
  lx = lx,
  Lx = Lx,
  Tx = Tx,
  ex = ex
)

# Print the data frame
print(life_table)

log(10)
################################ IMPORTER LES PACKAGES ##################
library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
library(TSA)
library(rgl)

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE

df<-table
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
ages
length(ages)
years <- 1977:2020
years
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

persp(ages,years, mx,theta = 200, phi = 10,col = 1:5,ltheta = 50) 

persp3d(ages,years, mx,theta = 40, phi = 10, col = 3:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)



################## Estimation des Parametres ############################

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
k <- v * sum(u) * d[1]

plot(b,lwd=3,type = "l",main = "b_x Homme",col='blue',
     xlab = "age")
plot(k,lwd=3,type = "l",main = "b_x Homme",col='red',
     xlab = "age")

sum(k)
sum(b)

ext=log(mx)-ax-b*kt
ext
length(ax)
I = matrix(rep(1,44), nrow=1)
Z1<-ax%*%I
Z2<-b%*%t(k)
dim(Z2)
dim(Z1)
head(Z)
Z3<-ax-Z2
Z4<-mx-Z3
dim(Z4)

ext<-mx-Z1-Z2
  dim(ext)
  
  persp(ages,years, ext,theta = 200, phi = 10,col = 1:5,ltheta = 50)   
heatmap(ext,col = , scale = "column",
        Colv = NA, Rowv = NA)
?heatmap
########## Analyse de Kappa t ########

serie <- ts(k,start = c(1977,1))
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time")
#Décomposition de la série
class(serie)
ts_lags(serie)

summary(serie)

ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")

"train <- window(serie, start=1977, end = 2014)
train
ts_info(train)
autoplot(train)
test <- window(serie, start = 2015, end = 2020)
test
autoplot(test)
fit <- auto.arima(train)
summary(fit)
fit.train <- Arima(train,order = c(1,1,0))
eacf(serie, ar.max = 7, ma.max = 6)"

dinf <- diff(serie)
adf.test(dinf, alternative="stationary")
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)

# model optimal:arima(0,1,0),arima(1,1,0),arima(1,1,1),arima(0,1,1)

fit.1 <- dinf %>% Arima(order = c(1,0,0))
v1<-summary(fit.1)
v1
fit.2 <- dinf %>% Arima(order = c(0,0,0))
v2<-summary(fit.2)
fit.3 <- dinf %>% Arima(order = c(1,0,1))
v3<-summary(fit.3)
fit.4 <- dinf %>% Arima(order = c(0,1,1))
v4<-summary(fit.4)
veec1<-c(v1=v1$aic,v2=v2$aic,v3=v3$aic,v4=v4$aic)
veec1
dat<-list(v1=v1$aic,v2=v2$aic,v3=v3$aic,v4=v4$aic)
dat
checkresiduals(fit.1)
shapiro.test(fit.1$residuals)
qqnorm(fit.1$residuals)
qqline(fit.1$residuals)
lambda = model$lambda

fit.S <- Arima(serie,order = c(1,1,0),
               include.drift=TRUE,include.mean = TRUE) #bon model
predictt <- forecast((fit.S), h = 30)
#predictt %>%
 # as.data.frame() %>%
  #write.csv(file = "prediction-30B.csv")
predictt %>% autoplot()
accuracy(fit.S) #RMSE1.791893
k_forecasted<-prediction.30B$Point.Forecast
length(k_forecasted)

W1<-b%*%t(k_forecasted)
dim(W1)
W2<-ax+W1
dim(W2)
persp(ages, 1:30, W2,theta = 40, phi = 10,col = "red",ltheta = 50) 
view(W2)

persp3d(ages,1:30, W2,theta = 40, phi = 10, col = "red",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

forecast_mortality <- function(a, s1, k_forecasted, b) {
  m_forecasted <- exp(a + s1 * k_forecasted %*% t(b))
  return(m_forecasted)
}

v<-forecast_mortality(ax,d[1],k_forecasted,u[,1])
dim(v)
t_v<-t(v)
dim(t_v)
persp3d(1:85,1:30,t_v ,theta = 40, phi = 10, col = 3:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

persp(1:85, 1:30, t_v,theta = 10, phi = 5,col = 1:5,ltheta = 50)

lambda <- BoxCox.lambda(fit.1)
lambda
estimate <- InvBoxCox((predictt$mean), lambda = 0.6052142)
estimate
#error<- test - estimate
error
autoplot(estimate)
SSE <- sum(error^2)
SSE
SSE1 <- mean(error^2)
SSE1

"projection de Mxt
erreur du model de lee-carter
projection aux grands ages"