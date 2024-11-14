################################ IMPORTER LES PACKAGES ######
#####
library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
library(TSA)
library(caret)
library(rgl)

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE
df<-Extra_data_Homme[,-1]
class(df)
dim(df)
summary(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
mx<-Mat01
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
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")

#### 3D plot ###########

persp(ages,years, mx,theta = 40, phi = 10,col = 6:7,ltheta = 100) 

persp3d(ages,1977:2019, mx,theta = 40, phi = 10, col = 6:7,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)


################## Estimation des Parametres ############################

Mxt<-mx
dim(Mxt)
as.numeric()
ln_m <- Mxt
## Estimation de ax 
ax1 <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
head(ax1)
length(ax1)
plot(ax1,col="blue",lwd=3,type = "l",main = "Estimation de ax_homme",
     xlab = "age")
# Calcul de matrice  A(x,t)
A <- ln_m - ax1
dim(A)
################## Decomposition en valeur singuliere de A(x,t)

SVD <- svd(A,nu=min(85,85),nv = min(44, 44))
SVD_1 <- svd(A,1,1)
v <- SVD_1$v
u <- SVD_1$u
d <- SVD_1$d

b1 <- u/sum(u)
k1 <- v * sum(u) * d[1]

plot(b1,lwd=3,type = "l",main = "b_x Homme",col='blue',
     xlab = "age")
plot(y=k1,x=1977:2019,lwd=3,type = "l",main = "b_x Homme",col='red',
     xlab = "age")

sum(k1)
sum(b1)
k1
k2<-matrix(k1,ncol = 43)
dim(k2)
########## Analyse de Kappa t ########

serie <- ts(k,start = c(1977,1))
length(serie)
ts_info(serie)
summary(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

############### TIME SERIES ANALYSIS 2 #########

#1. Charger et explorer les données

serie <- ts(k,start = c(1977,1))
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

# Diviser les données en un jeu d'entraînement (1977-2015) et un jeu de test (2016-2020)
train <- window(serie, end=c(2014))
length(train)
test <- window(serie, start=c(2015))
length(test)

#3. Appliquer des techniques de validation croisée pour évaluer les modèles


# Fonction pour calculer l'erreur quadratique moyenne (RMSE)
rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2))
}

# Initialiser une liste pour stocker les RMSE des modèles
model_rmse <- list()

# Créer des rééchantillonnages pour la validation croisée
resamples <- createTimeSlices(y = train, initialWindow = 30, horizon = 1, fixedWindow = TRUE)
length(resamples$train)
resamples$train
# Parcourir chaque échantillon pour ajuster et évaluer les modèles
for(i in 1:length(resamples$train)) {
  train_slice <- train[resamples$train[[i]]]
  test_slice <- train[resamples$test[[i]]]
  
  # Ajuster un modèle ARIMA
  fit <- auto.arima(train_slice)
  forecasted <- forecast(fit, h=length(test_slice))$mean
  
  # Calculer le RMSE pour cet échantillon
  model_rmse[[i]] <- rmse(test_slice, forecasted)
}

# Afficher la performance moyenne du modèle
mean(unlist(model_rmse))

######4. Ajuster les modèles

# Ajuster un modèle ARIMA sur l'ensemble des données d'entraînement
#Décomposition de la série
summary(train)
class(train)
ts_lags(train)
ar(diff(train))
ggAcf(train) + ggtitle("ACF of kappa.t")
ggPacf(train) + ggtitle("PACF of kappa.t")
adf.test(train, alternative="stationary")

dinf <- diff(train)
adf.test(dinf, alternative="stationary")
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)
plot(armasubsets(dinf,nar=3,nma=3))


final_model <- auto.arima(train)
final_model
summary(final_model)
# Faire des prévisions pour les années 2015-2019
final_forecast <- forecast(final_model, h=length(test))
final_forecast
test
# Afficher les prévisions
plot(final_forecast, main="Prévisions du modèle ARIMA")
lines(test, col="red", type="o")

### 5. Évaluer les performances des modèles

# Calculer le RMSE sur le jeu de test
final_rmse <- rmse(test, final_forecast$mean)
print(paste("RMSE sur le jeu de test:", final_rmse))


### 6. Faire des prévisions futures

# Ajuster le modèle sur toutes les données (1977-2020)
full_model <- auto.arima(serie)
full_model
# Faire des prévisions pour les 5 prochaines années (2021-2025)
future_forecast <- forecast(full_model, h=30)

# Afficher les prévisions futures
plot(future_forecast, main="Prévisions futures du modèle ARIMA")
future_forecast %>% autoplot()

################################# Schro ##########
# Normaliser les données
data_scaled <- scale(serie)

# Diviser en ensembles d'entraînement et de test
train_size <- round(length(data_scaled) * 1)
train_size
train <- data_scaled[1:train_size]
length(train)
test <- data_scaled[(train_size + 1):length(data_scaled)]
length(test)

train1 <- ts(train,start = c(1977,1))
test1<- ts(test,start = c(2011,1))
ts_plot(train1,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)
ts_info(train1)

ts_plot(test1,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)
ts_info(test1)

ggAcf(train1) + ggtitle("ACF of kappa.t")
ggPacf(train1) + ggtitle("PACF of kappa.t")

# Ajuster un modèle ARIMA
fit_arima <- auto.arima(train1)

# Résumé du modèle
summary(fit_arima)

# Visualiser les diagnostics du modèle
checkresiduals(fit_arima)
qnorm(fit_arima$residuals)
qqline(fit_arima$residuals)
shapiro.test(fit_arima$residuals)
residuals(fit_arima)
# Faire des prévisions
forecasts <- forecast(fit_arima, h = 30)

# Visualiser les prévisions
autoplot(forecasts) +
  ggtitle("Prévisions de la Série Temporelle Annuelle (2020-2049)") +
  xlab("Année") + ylab("Valeur")

# 2. Métriques de performance
# Obtenir les valeurs ajustées (fitted values)
fitted_values <- fitted(fit_arima)
plot(fitted_values)
# Calculer les résidus
residuals <- residuals(fit_arima)

# Calculer les métriques de performance
MAE <- mean(abs(residuals))
MSE <- mean(residuals^2)
RMSE <- sqrt(MSE)
MAPE <- mean(abs(residuals / serie)) * 100

cat("Mean Absolute Error (MAE):", MAE, "\n")
cat("Mean Squared Error (MSE):", MSE, "\n")
cat("Root Mean Squared Error (RMSE):", RMSE, "\n")
cat("Mean Absolute Percentage Error (MAPE):", MAPE, "%\n")

# 3. Test de Ljung-Box
# Effectuer le test de Ljung-Box pour vérifier l'autocorrélation des résidus
Box.test(residuals, lag = 10, type = "Ljung-Box")
" p-value = 0.5086 ,les résidus n'ont pas d'autocorrélation significative et ressemblent à un bruit blanc."

