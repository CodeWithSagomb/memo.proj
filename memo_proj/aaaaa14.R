

# Créer les données avec une variable 'type' pour distinguer les deux séries
data <- data.frame(
  time = 1977:2019,
  serie.kappa.t = k.t_morta,
  tendance_estimé = moving_avg
)

# Convertir les données au format long pour ggplot
library(tidyr)
data_long <- data %>%
  pivot_longer(cols = c(serie.kappa.t, tendance_estimé), names_to = "type", values_to = "measurement")

# Visualiser les données originales et la tendance estimée avec une légende
ggplot(data_long, aes(x = time, y = measurement, color = type)) +
  geom_line(size = 2, na.rm = TRUE) +
  scale_color_manual(values = c("serie.kappa.t" = "blue","tendance_estimé" = "red")) +
  labs(
    title = "Série Temporelle kt et Tendance Estimée par Moyenne Mobile",
    x = "Temps",
    y = "Valeur",
    color = "Légende des Graphes"
  )+
  theme(legend.position = "bottom")


fit.morta1 <- St %>% Arima(order = c(1,0,1),
                                 include.mean = TRUE,include.drift = TRUE,
                                 include.constant=TRUE)
m1<-summary(fit.morta1)#3.471744 2.693567
m1


#############################################################################

# Installer les packages nécessaires si ce n'est pas déjà fait
# install.packages("ggplot2")
# install.packages("gridExtra")

library(ggplot2)
library(gridExtra)

# Créer les données pour les deux graphes
data1 <- data.frame(x = 1:10, y = rnorm(10, mean = 10, sd = 2), group = "Graphe 1")
data2 <- data.frame(x = 1:10, y = rnorm(10, mean = 5, sd = 2), group = "Graphe 2")

# Combiner les deux jeux de données
data <- rbind(data1, data2)

# Créer le graphique combiné
plot <- ggplot(data, aes(x = x, y = y, color = group)) +
  geom_line() +
  geom_point() +
  scale_color_manual(values = c("Graphe 1" = "blue", "Graphe 2" = "red")) +
  labs(title = "Titre du Graphique Combiné",
       x = "Axe X",
       y = "Axe Y",
       color = "Légende des Graphes") +
  theme_minimal()

# Afficher le graphique
print(plot)




############################################"
tb<-LEf[,2]
tb<-as.vector(tb$LE)
tb
serie <- ts(tb,start = c(1977,1),frequency = 1)
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
# Charger les bibliothèques nécessaires

fit.S <- Arima(serie,order = c(3,1,1),
               include.drift=TRUE,include.mean = TRUE,include.constant=TRUE) #bon model
predictt <- forecast(fit.S, h = 30)
predictt %>% autoplot()
accuracy(fit.S) #RMSE 1.769699
pred<- as.data.frame(predictt)
view(pred)


library(ggplot2)

# Les valeurs de la série temporelle
k <- c(78.005363, 74.153547, 69.945357, 66.191014, 60.336653, 55.484992,
       41.822329, 41.496120, 37.281176, 30.877602, 26.698216, 20.009266,
       10.593001, 11.347092, 9.670047, 7.419993, 17.525351, 22.422939,
       21.398612, 15.139136, 10.740677, -1.000637, -8.167666, -12.041795,
       -16.804016, -21.102066, -19.632472, -28.838695, -27.756777, -30.263634,
       -32.143875, -34.112904, -34.216532, -41.217687, -40.837971, -38.975080,
       -44.074127, -44.202906,-44.259522, -52.250156, -51.247977, -51.444022, -53.967969)
##############################################################################


ts_plot(ts_morta,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)



# Créer un objet de série temporelle
k_ts <- ts(k.t_morta, frequency = 1)
length(k_ts)
# Définir la fenêtre de la moyenne mobile
window_size <- 3
rep(1/window_size, window_size)
# Calculer la moyenne mobile
moving_avg <- stats::filter(k_ts, rep(1/window_size, window_size), 
                     sides = 2,method= "convolution",circular = FALSE)
length(moving_avg )
S<-k.t_morta-moving_avg
class(S)
ggAcf(S) + ggtitle("ACF of kappa.t")
ggPacf(S) + ggtitle("PACF of kappa.t")
adf.test(S, alternative="stationary")

auto.arima(S)
# Créer un dataframe pour ggplot2
data <- data.frame(time = 1:length(k.t_morta), value = k.t_morta, trend = moving_avg)
# Visualiser les données originales et la tendance estimée
ggplot(data, aes(x = time)) +
  geom_line(aes(y = value), color = "blue", na.rm = TRUE,size=1) +
  geom_line(aes(y = trend), color = "red", na.rm = TRUE,size=1) +
  labs(title = "Série Temporelle et Tendance Estimée par Moyenne Mobile",
       x = "Temps",
       y = "Valeur") +
  theme_minimal()

S

final_model1 <- auto.arima(S)
final_model1
summary(final_model)
# Faire des prévisions pour les années 2015-2019
final_forecast <- forecast(final_model, h=length(test))
final_forecast
tes


c(78.005363  ,74.153547 , 69.945357  ,66.191014  ,60.336653  ,55.484992,
41.822329 , 41.496120 , 37.281176 , 30.877602 , 26.698216  ,20.009266,
10.593001  ,11.347092   ,9.670047  , 7.419993 , 17.525351 , 22.422939,
21.398612  ,15.139136 , 10.740677 , -1.000637 , -8.167666 -12.041795,
-16.804016 ,-21.102066 ,-19.632472, -28.838695, -27.756777 ,-30.263634,
-32.143875, -34.112904, -34.216532 ,-41.217687 ,-40.837971 ,-38.975080,
-44.074127 ,-44.202906 ,-44.259522, -52.250156, -51.247977 ,-51.444022,-53.967969)


mxt_forecasted<-exp(ax%+%b_tk)
dim(mxt_forecasted)
view(mxt_forecasted)
ax
ax%+%b_tk

v1<-c(2,1,5,4)
w1<-matrix(1:12,4)
w1
v1+w1
t(v1)+w1
l= matrix(rep(1,3), nrow=1)
l
n<-v1%*%l
n+w1
# Installation et chargement des bibliothèques nécessaires
install.packages("forecast")
install.packages("keras")
install.packages("tibbletime")

library(forecast)
library(keras)
library(tibbletime)

#############  Étape 1 : Préparation des Données

# Chargement des données
# Suppose we have the data in a CSV file named 'time_series.csv'
data <- read.csv('path/to/time_series.csv')
head(data)

# Convertir en série temporelle
data_ts <- ts(data$value, start = c(1977), frequency = 1)
plot(data_ts)

##########   Étape 2 : Construction du Modèle


# Normaliser les données
data_scaled <- scale(data_ts)

# Diviser en ensembles d'entraînement et de test
train_size <- round(length(data_scaled) * 0.8)
train <- data_scaled[1:train_size]
test <- data_scaled[(train_size + 1):length(data_scaled)]

# Préparer les données pour le réseau de neurones
create_dataset <- function(dataset, look_back = 1) {
  dataX <- dataY <- list()
  for (i in 1:(length(dataset) - look_back - 1)) {
    dataX[[i]] <- dataset[i:(i + look_back - 1)]
    dataY[[i]] <- dataset[i + look_back]
  }
  return(list(X = array(unlist(dataX), dim = c(length(dataX), look_back, 1)),
              Y = array(unlist(dataY), dim = c(length(dataY), 1))))
}

look_back <- 3
train_data <- create_dataset(train, look_back)
test_data <- create_dataset(test, look_back)

# Définir le modèle LSTM
model <- keras_model_sequential() %>%
  layer_lstm(units = 50, input_shape = c(look_back, 1)) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam'
)

# Entraîner le modèle
history <- model %>% fit(
  train_data$X, train_data$Y,
  epochs = 100,
  batch_size = 1,
  validation_split = 0.2,
  verbose = 2
)

