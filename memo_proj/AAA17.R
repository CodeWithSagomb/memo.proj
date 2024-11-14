
################################ IMPORTER LES PACKAGES ######
library(lifecontingencies)
library(TSstudio)
library(tidyverse)
library(forecast)
library(tseries)
library(TSA)
library(caret)
library(rgl)
library(plotly)

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE

morta<-as.matrix(dataONS02[,-1]) ##-(61:120)
class(morta)
mode(morta)
dim(morta)
summary(morta)

##### Representation graphique

ages_morta <- 0:(nrow(morta)-1)
length(ages_morta)
years <- 1977:2019
length(years)
dfmorta <- expand.grid(Age = ages_morta, Year = years)
tail(dfmorta)
dfmorta$MortalityRate <- as.vector(morta)
head(dfmorta)
ggplot(dfmorta, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "male_Mortality Rates Over Time") +
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")

#### 3D plot ###########

persp3d(ages_morta,years, morta,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################

ln_morta <- morta
dim(ln_morta)
## Estimation de ax 
ax_morta <- apply(ln_morta, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
length(ax_morta)
datax_morta <- data.frame(age.x=0:119,ax=ax_morta)
ggplot(datax_morta, aes(x=age.x, y=ax)) +
  geom_line( color="purple", size=2, alpha=1.5, linewidth=3)+
  ggtitle("Comportement moyen de Ln(mxt)")

# Calcul de matrice  A(x,t)
Mt <- ln_morta - ax_morta
dim(Mt)

################## Decomposition en valeur singuliere de A(x,t)

SVD_morta <- svd(Mt,1,1)
vm <- SVD_morta$v
um <- SVD_morta$u
dm <- SVD_morta$d

b.x_morta <- um/sum(um)
k.t_m <- d[1]*sum(um)*vm
k.t_morta
kk
kk2
kk3
### Representation graphique de la composante Bx

data.bx_morta <- data.frame(age.x=0:119,bx=b.x_morta)
ggplot(data.bx_morta, aes(x=age.x, y=bx)) +
  geom_line( color="blue", size=2, alpha=1.5, linewidth=3)+
  ggtitle("Sensibilité a l'age x par rapport a la mortalité")

### Representation graphique de la composante Kappa_t


data.kt_morta<-data.frame(year.t=1977:2019,kt=k.t_morta)
ggplot(data.kt_morta, aes(x=year.t, y=kt)) +
  geom_line( color="red", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")

sum(k.t_morta)
sum(b.x_morta)

##### ANALYSE DES RESIDUS
"ext=log(mxt)-ax-bx*kt"

length(ax_morta)
I_morta = matrix(rep(1,43), nrow=1)
ZZ1<-ax_morta%*%I_morta
ZZ2<-b.x_morta%*%t(k.t_morta)
dim(ZZ1)
dim(ZZ2)

ext_morta<-morta-ZZ1-ZZ2
dim(ext_morta)

figm <- plot_ly(y=0:69,x=1977:2019,z = ext_morta, type = "heatmap")
figm

########## ANALYSE DE LA SERIE Kappa_t ########

ts_morta <- ts(k.t_morta,start = c(1977,1))

### Analyse Descriptive De la Serie Brute

summary(ts_morta)
ts_info(ts_morta)

#### Representation Graphique de la Serie brute

ts_plot(ts_morta,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

#### Analyse du Correlogramme de la serie brute

ggAcf(ts_morta) + ggtitle("ACF of kappa.t")
ggPacf(ts_morta) + ggtitle("PACF of kappa.t")
ts_lags(ts_morta)

#### Test du Dickey-Fuller Augmenté

adf.test(ts_morta, alternative="stationary") # p-value =0.05843
kpss.test(ts_morta)

### Test de la Racine Unitaire

ar(diff(ts_morta))

### Differenciation d'ordre 1 de la Serie brute

diff_morta <- diff(ts_morta)


### Analyse du Correlogramme simple et partiel de la serie differentié

ggAcf(diff_morta) + ggtitle("ACF of kappa.t")
ggPacf(diff_morta) + ggtitle("PACF of kappa.t")
ts_lags(diff_morta)

### Test du Dickey-Fuller Augmenté

adf.test(diff_morta, alternative="stationary")
kpss.test(diff_morta)

### Determination des Ordres P,d et Q du processus ARIMA

eacf(ts_morta, ar.max = 5, ma.max = 5)
plot(armasubsets(ts_morta,nar=5,nma=5))

full_model_morta <- auto.arima(ts_morta)
full_model_morta



### Estimation des Parametres des Modeles candidats

# model optimal:arima(0,1,0),arima(1,1,0),arima(1,1,1),(2,1,1)

fit.morta1 <- ts_morta %>% Arima(order = c(1,0,1),
                                   include.mean = TRUE,include.drift = TRUE,
                                   include.constant=TRUE)
m1<-summary(fit.morta1)#3.471744 2.693567
m1
fit.morta2 <- diff_morta %>% Arima(order = c(0,0,0))
m2<-summary(fit.morta2)
m2
fit.morta3 <- diff_morta %>% Arima(order = c(1,0,1))
m3<-summary(fit.morta3)
m3
fit.morta4 <- diff_morta %>% Arima(order = c(0,1,1))
m4<-summary(fit.morta4)
m4

##### Diagnostique des residus des modeles

#### arima(1,0,1)

### Test de Nullité de la Moyenne des Residus

checkresiduals(fit.morta1$residuals) # p-value = 0.2126 , on accepte H0: E(et)=0

### Test d'absence d'autocorrelation

Box.test(fit.morta1$residuals, type = "Ljung-Box") # p-value = 0.961, on accepte H0: rok=0 

#### Test de Normalité

shapiro.test(fit.morta1$residuals) # p-value = 0.1382
jarque.bera.test(fit.morta1$residuals)

qqnorm(fit.morta1$residuals)
qqline(fit.morta1$residuals,col="red", lwd=2)
runs(fit.morta1$residuals)

###########


### Prevision de la serie kappa_t



fit.morta <- Arima(ts_morta,order = c(1,0,1),
               include.drift=TRUE,include.mean = TRUE,include.constant=TRUE) #bon model
predict_morta <- forecast((fit.morta), h = 31)
predict_morta %>% autoplot()
accuracy(fit.morta) #RMSE 4.026234,#MAE 2.894114
pred_morta<- as.data.frame(predict_morta)
view(pred_morta)

##############################################################

k_ts <- ts(k.t_morta, frequency = 1)
length(k_ts)

# Définir la fenêtre de la moyenne mobile
window_size <- 6
rep(1/window_size, window_size)
# Calculer la moyenne mobile
moving_avg <- stats::filter(k_ts, rep(1/window_size, window_size), 
                            sides = 2,method= "convolution",circular = FALSE)
length(moving_avg )

St<-k_ts-moving_avg
ggAcf(St) + ggtitle("ACF of kappa.t")
ggPacf(St) + ggtitle("PACF of kappa.t")

auto.arima(St)

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

class(St)
ggAcf(St) + ggtitle("ACF of kappa.t")
ggPacf(St) + ggtitle("PACF of kappa.t")

fit.morta1 <- ts_morta %>% Arima(order = c(1,0,1),
                           include.mean = TRUE,include.drift = TRUE,
                           include.constant=TRUE)
m1<-summary(fit.morta1)#3.471744 2.693567
m1

checkresiduals(fit.morta1$residuals) # p-value = 0.2126 , on accepte H0: E(et)=0

### Test d'absence d'autocorrelation

Box.test(fit.morta1$residuals, type = "Ljung-Box") # p-value = 0.961, on accepte H0: rok=0 

#### Test de Normalité

shapiro.test(fit.morta1$residuals) # p-value = 0.1382
jarque.bera.test(fit.morta1$residuals)

qqnorm(fit.morta1$residuals)
qqline(fit.morta1$residuals,col="red", lwd=2)


fit.morta <- Arima(ts_morta,order = c(1,0,1),
                   include.drift=TRUE,include.mean = TRUE,include.constant=TRUE) #bon model
predict_morta <- forecast((fit.morta), h = 31)
predict_morta %>% autoplot()
accuracy(fit.morta) #RMSE 4.026234,#MAE 2.894114
pred_morta<- as.data.frame(predict_morta)
view(pred_morta)


###############################

### Projection des taux de mortalité

k_for_morta<-pred_morta[,5]
length(k_for_morta)
b_tk_morta<-b.x_morta%*%t(k_for_morta)
dim(b_tk_morta)
ln_mxt_for_morta<-ax_morta+b_tk_morta
dim(ln_mxt_for_morta)

### Representation Graphique en 3D des taux Projetés

Ln.Mxt.projete<-ln_mxt_for_morta
persp3d(0:119,2020:2050, Ln.Mxt.projete,theta = 40, phi = 10, col = 10,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

### Representation Graphique de surface logarithmique 
### de taux Historiques et Projectés

LN_Mxt.past_morta<-as.matrix(dataONS02[,-1])
global_ln_mxt_morta<-cbind(LN_Mxt.past_morta,ln_mxt_for_morta)
dim(global_ln_mxt_morta)
global_ln_mxt<-as.matrix(global_ln_mxt_morta)
class(global_ln_mxt_morta)

persp3d(0:119,1977:2050,global_ln_mxt_morta ,theta = 40, phi = 10, col = c("#85e085"),ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

surface_H=global_ln_mxt_morta

fig <- plot_ly(y=0:119,x=1977:2050,z = ~surface_H) %>% add_surface(
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

mxt_for_morta<-exp(ax_morta+b_tk_morta)
dim(mxt_for_morta)

qx_for_morta<- mx2qx(mxt_for_morta)
dim(qx_for_morta)

tab_esp_morta<-probs2lifetable(qx_for_morta[,1],radix = 100000,type = "qx")
tab_esp_morta

for (i in 1:31) {
  tab_esp_morta<-probs2lifetable(qx_for_morta[,i] ,radix = 100000,type = "qx")
  a[i]<-exn(tab_esp_morta,x=0)
}
a
a=as.numeric(a)
a


extract_exn<- function(lcqx){
  
  v1<-numeric(length(lcqx))
  
  for (i in 0:(length(lcqx)-1)) {
    v1[i+1] <-exn(tab_esp_morta,x=i)
  }
  
  return(data.frame(v1))
  
}

HH=0:119

vect<-numeric(31)
vect
exn(tab_esp_morta)



extract_exn(qx_for_morta[,31])
H
HH<-cbind(HH,H)
HH

H2<-0:119
for (j in 1:31) {
  tab_esp_morta<-probs2lifetable(qx_for_morta[,j],radix = 100000,type = "qx")
  H1<-extract_exn(qx_for_morta[,j])
  H2<-cbind(H2,H1)
  
}

dim(H2)
class(H2)
mode(H2)
view(H2)
H3<-as.matrix(H2)
class(H3)
view(H3)


ages_morta <- 0:(nrow(morta)-1)
length(ages_morta)
years <- 2020:2050
length(years)
dfmorta <- expand.grid(Age = ages_morta, Year = years)
tail(dfmorta)
dfmorta$MortalityRate <- as.vector(H3[,-1])
head(dfmorta)
ggplot(dfmorta, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line(size=0) +
  labs(x = "Age", y = "Esperance residuelle", title = "Esperance de Vie a la Naissance par age unitaire") +
  scale_color_viridis_c("Année", option = "plasma")



fig <- plot_ly(z = ~H3[,-1]) %>% add_surface(
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

H2 %>%
  as.matrix()%>%
  write.csv(file = "Esp_de_vie_projeté2.csv")


