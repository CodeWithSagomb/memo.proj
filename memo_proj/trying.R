library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
remove
###################################################

df<-table
class(df)
dim(df)
head(df)
tail(df)
summary(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
view(Mat01)
mx<-Mat01[,-1]
head(as.vector(mx))
length(as.vector(mx))
### Manipulation de la matrice mx ###########
ages <- 0:(nrow(mx)-1)
ages
years <- 1977:2020
years
df <- expand.grid(Age = ages, Year = years)
tail(df)
df$MortalityRate <- as.vector(mx)
head(df)
############# Representation graphique ###################
ggplot(df, aes(x = Age, y = MortalityRate, group = Year, color = as.factor(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "male_Mortality Rates Over Time") +
  theme_minimal()
################## Estimation des Parametres ############################

Mxt<-mx
head(Mxt)
dim(Mxt)
ln_m <- Mxt
## Estimation de ax 
ax <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
head(ax)
ax
length(ax)
plot(ax,col="blue",lwd=3,type = "l",main = "Estimation de ax_homme",
     xlab = "age")

# Calcul de matrice  A(x,t)
A <- ln_m - ax
dim(A)
################## Decomposition en valeur singuliere de A(x,t)

SVD <- svd(A,nu=min(85,85),nv = min(44, 44))
#####################

SVD_1 <- svd(A,1,1)
v <- SVD_1$v
u <- SVD_1$u
d <- SVD_1$d
d

b <- u/sum(u)
k <- v * sum(u) * d[1]

plot(b,lwd=3,type = "l",main = "b_x Homme",col='blue',
     xlab = "age")
plot(k,lwd=3,type = "l",main = "b_x Homme",col='red',
     xlab = "age")

sum(k)
sum(b)
#ts_info(k)
serie <- ts(k,start = c(1977,1))
length(serie)
plot(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time")
#Décomposition de la série
ts_decompose(serie,type = "both")
?ts_decompose
class(serie)
ts_lags(serie)

summary(serie)


ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")

adf.test(serie, alternative="stationary")
dinf <- diff(serie)
adf.test(dinf, alternative="stationary")
?diff
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
acf(dinf)
pacf(dinf,xlim=c(0,30))

?ARIMA
k
forecast_k <- function(k, n) {
  forecasted_k <- k[length(k)] + seq(1, n)
  return(forecasted_k)
}


### arima(0,1,0),arima(1,1,0),arima(1,1,1),arima(0,1,1)

f1<-arima(dinf, order = c(0,0,0))
summary(f1)
f2<-arima(dinf, order = c(1,0,0))
summary(f2)
f3<-arima(dinf, order = c(1,0,1))
summary(f3)
f4<-arima(serie, order = c(0,0,1))
summary(f4)


t = system.time({
  fit = auto.arima(serie, method = "CSS")  # Autres choix : "ML", "CSS-ML"
})
summary(fit)
forecast = forecast(fit, h = 30) 
class(forecast)
plot(forecast)
forecast %>%
  as.data.frame() %>%
  write.csv(file = "prediction-30.csv")
autoplot(forecast)
fit
  p_data = read.csv('prediction-30.csv') %>%
  mutate(date = (as.Date('2023-11-30') + 1:365)) %>%
  rename(consommation = Point.Forecast)
  
  w1<-prediction.30$Point.Forecast
  length(w1)
  
  
  
  
  forecast_mortality <- function(a, s1, k_forecasted, b) {
    m_forecasted <- exp(a + s1 * k_forecasted %*% t(b))
    return(m_forecasted)
  }
v<-forecast_mortality(ax,d[1],forecast,b)
