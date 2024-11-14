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

femme<-as.matrix(data_femme[,-1])
class(femme)
dim(femme)
summary(femme)
mode(femme)
view(femme)

###############

#### Representation graphique de Mxt

ages_femme <- 0:(nrow(femme)-1)
years <- 1977:2019
dfemme <- expand.grid(Age = ages_femme, Year = years)
dfemme$MortalityRate <- as.vector(femme)
ggplot(dfemme, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "Taux de Mortalité Femme") +
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")



#### Representation graphique logarithmique de Mxt

ages_femme <- 0:(nrow(femme)-1)
years <- 1977:2019
dfemme <- expand.grid(Age = ages_femme, Year = years)
dfemme$MortalityRate <- as.vector(femme)
ggplot(dfemme, aes(x = Age, y = MortalityRate, group = Year, color = as.numeric(Year))) +
  geom_line() +
  labs(x = "Age", y = "Mortality Rate", title = "Taux de Mortalité Femme") +
  scale_color_viridis_c("EVOLUTION TIME", option = "plasma")

#### 3D plot 

persp3d(ages_femme,years, femme,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)

################## Estimation des Parametres ############################

ln_mxt_fem <- femme
dim(ln_mxt_fem)
## Estimation de ax 
ax_femme <- apply(ln_mxt_fem, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
datax_femme <- data.frame(age.x=0:119,ax=ax_femme)
ggplot(datax_femme, aes(x=age.x, y=ax)) +
  geom_line( color="purple", size=2, alpha=1.5, linewidth=2)+
  ggtitle("Comportement moyen de Ln(mxt)")


# Calcul de matrice  M(x,t)
M <- ln_mxt_fem - ax_femme
dim(M)

################## Decomposition en valeur singuliere de A(x,t)

SVD_2 <- svd(M,1,1)
v2 <- SVD_2$v
u2 <- SVD_2$u
d2 <- SVD_2$d

BBx <- u2/sum(u2)
Kt_femme<- d[1]*sum(u2)*v2
Kt2
k22
kt3
### Representation graphique de la composante Bx

dat.bx <- data.frame(age.x=0:119,bx=BBx)
ggplot(dat.bx, aes(x=age.x, y=bx)) +
  geom_line( color="blue", size=2, alpha=1.5, linetype=1)+
  ggtitle("Sensibilité a l'age x par rapport a la mortalité")

### Representation graphique de la composante Kappa_t

dat.Kt<-data.frame(year.t=1977:2019,kt=Kt2)
ggplot(dat.Kt, aes(x=year.t, y=Kt2)) +
  geom_line( color="red", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")

sum(Kt2)
sum(BBx)
Kt2

##### ANALYSE DES RESIDUS
"ext=log(mxt)-ax-bx*kt"

length(ax_femme)
I_femme = matrix(rep(1,43), nrow=1)
ZZ1_femme<-ax_femme%*%I_femme
ZZ2_femme<-BBx%*%t(Kt2)
dim(ZZ1_femme)
dim(ZZ2_femme)

ext_femme<-femme-ZZ1_femme-ZZ2_femme
dim(ext_femme)

figm <- plot_ly(y=0:59,x=1977:2019,z = ext_femme, type = "heatmap")
figm


########## Analyse de Kappa t ########

ts_fem <- ts(Kt2,start = c(1977,1))

### Analyse Descriptive De la Serie Brute

summary(ts_fem)
ts_info(ts_fem)

#### Representation Graphique de la Serie brute

ts_plot(ts_fem,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

#### Analyse du Correlogramme de la serie brute

ggAcf(ts_fem) + ggtitle("ACF of kappa.t")
ggPacf(ts_fem) + ggtitle("PACF of kappa.t")

## Representation graphique de la relation lineaire entre la serie kappa_t et ses Lags

ts_lags(ts_fem)

#### Test du Dickey-Fuller Augmenté

adf.test(ts_fem, alternative="stationary")
kpss.test(ts_fem)

### Test de la Racine Unitaire

ar(diff(ts_morta))

### Differenciation d'ordre 1 de la Serie brute

dinf_fem <- diff(ts_fem)

ts_plot(dinf_fem,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

### Analyse du Correlogramme simple et partiel de la serie differentié

ggAcf(dinf_fem) + ggtitle("ACF of kappa.t")
ggPacf(dinf_fem) + ggtitle("PACF of kappa.t")
ts_lags(dinf_fem)

### Test du Dickey-Fuller Augmenté

adf.test(dinf_fem, alternative="stationary")


### Determination des Ordres P,d et Q du processus ARIMA

eacf(dinf_fem, ar.max = 4, ma.max = 5)
plot(armasubsets(dinf_fem,nar=5,nma=5))

fem_model <- auto.arima(ts_fem)
fem_model

### Estimation des Parametres des Modeles candidats

# model optimal:arima(0,1,0),arima(0,1,2),arima(1,1,1),arima(0,1,1), (3,0,1)

fit.fem1 <- dinf_fem%>% Arima(order = c(3,0,1),include.mean = TRUE,include.drift = TRUE,
                              include.constant=TRUE)
fm1<-summary(fit.fem1)
fm1
fit.fem2 <- dinf_fem%>% Arima(order = c(0,0,0))
fm2<-summary(fit.fem2)
fm2
fit.fem3 <- dinf_fem %>% Arima(order = c(0,0,1))
fm3<-summary(fit.fem3)
fm3
fit.fem4 <- dinf_fem %>% Arima(order = c(1,1,1))
fm4<-summary(fit.fem4)
fm4

##### Diagnostique des residus des modeles

#### arima(0,1,0)

### Test de Nullité de la Moyenne des Residus

checkresiduals(fit.fem1) # p-value = 0.2126 , on accepte H0: E(et)=0

### Test d'absence d'autocorrelation

Box.test(fit.fem1$residuals, type = "Ljung-Box") # p-value = 0.961, on accepte H0: rok=0 

#### Test de Normalité

shapiro.test(fit.fem1$residuals) # p-value = 0.1382
jarque.bera.test(fit.fem1$residuals)

qqnorm(fit.fem1$residuals)
qqline(fit.fem1$residuals,col="red", lwd=2)

### Prevision de la serie kappa_t

fit.fem <- Arima(ts_fem,order = c(3,1,1),
               include.drift=TRUE,include.mean = TRUE,include.constant=TRUE) #bon model
predict.fem <- forecast((fit.fem), h = 31)
predict.fem %>% autoplot()
accuracy(fit.fem) #RMSE 2.735404
pred.fem<- as.data.frame(predict.fem)
view(pred.fem)

### Projection des taux de mortalité
#km
k_pred.fem<-pred.fem[,5]
#k_pred.fem
length(k_pred.fem)
B_tk<-BBx%*%t(k_pred.fem)
dim(B_tk)
ln_mxt_pred.fem<-ax_femme+B_tk
dim(ln_mxt_pred.fem)

### Representation Graphique en 3D des taux Projetés
#Ln.Mxt.projete.femme<-ln_mxt_pred.fem
#persp3d(0:119,2020:2050,Ln.Mxt.projete.femme ,theta = 40, phi = 10, col = 10,ltheta = 50)
#play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

### Representation Graphique de surface logarithmique de taux Historiques et Projectés

LN_Mxt.past_morta<-as.matrix(dataONS02[,-1])
global_ln_mxt_morta<-cbind(LN_Mxt.past_morta,ln_mxt_for_morta)
dim(global_ln_mxt_morta)
global_ln_mxt<-as.matrix(global_ln_mxt_morta)
class(global_ln_mxt_morta)

persp3d(0:119,1977:2050,global_ln_mxt_morta ,theta = 40, phi = 10, col = c("#85e085"),ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

surface_f<-global_ln_mxt_morta
library(plotly)
# volcano is a numeric matrix that ships with R
fig <- plot_ly(y=0:119,x=1977:2050,z = ~surface_f) %>% add_surface(
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


mxt_proj.fem<-exp(ax_femme+B_tk)
dim(mxt_proj.fem)

qx_proj_fem<- mx2qx(mxt_proj.fem)
dim(qx_proj_fem)
class(qx_proj_fem)
view(qx_proj_fem)
plot(log(qx_proj_fem[,21]),type="l",col=4,lwd=3.5)
lines(log(femme[,43]),type="l",col=6,lwd=3.5)


#####################################################
fem<-probs2lifetable(femme[,43] ,radix = 100000,type = "qx")
fem
?mx2qx

###################################################
tab_esp.fem<-probs2lifetable(qx_proj_fem[,31] ,radix = 100000,type = "qx")
tab_esp.fem



for (i in 1:31) {
  tab_esp.fem<-probs2lifetable(qx_proj_fem[,i] ,radix = 100000,type = "qx")
  a[i]<-exn(tab_esp.fem,x=0)
}
aa<-as.numeric(a)
aa
plotMortalityTables(makeQxDataFrame(tab_esp.fem,YOB = 2050,Period = NA, reference = NULL))


#########################################
#76.67251274

extract_exn<- function(lcqx){
  
  v1<-numeric(length(lcqx))
  
  for (i in 0:(length(lcqx)-1)) {
    v1[i+1] <-exn(tab_esp.fem,x=i)
  }
  
  return(data.frame(v1))
  
}



v<-numeric(31)
v
exn(tab_esp.fem,x=0)

G<-0
for (i in 1:31) {
  G<-tab_esp_morta<-probs2lifetable(qx_proj_fem[,i],radix = 100000,type = "qx")
  v<-exn(tab_esp.fem,x=0)
}

H2

H2<-0:119
for (j in 1:31) {
  tab_esp_morta<-probs2lifetable(qx_proj_fem[,j],radix = 100000,type = "qx")
  H1<-extract_exn(qx_proj_fem[,j])
  H2<-cbind(H2,H1)
  
}
dim(H2)
view(H2)

H2 %>%
  as.matrix()%>%
  write.csv(file = "Esp_scenario_inf.csv")




##############################################


# Installation et chargement du package MortalityTables
install.packages("MortalityTables")
library(MortalityTables)

# Chargement d'une table de mortalité (par exemple, table de mortalité allemande pour hommes de 2012)
data("Germany_2012_male")

# Conversion de la table de mortalité en intervalles de 5 ans
convert_to_5_year_intervals <- function(mortality_table) {
  # Extraire les âges et les taux de mortalité
  ages <- as.numeric(names(mortality_table@qx))
  mortality_rates <- mortality_table@qx
  
  # Créer un data frame
  mortality_data <- data.frame(Age = ages, MortalityRate = mortality_rates)
  
  # Ajouter une colonne 'Interval' pour identifier les groupes de 5 ans
  mortality_data <- mortality_data %>%
    mutate(Interval = cut(Age, breaks = seq(0, max(Age) + 5, by = 5), right = FALSE, labels = FALSE) - 1)
  
  # Calculer le taux de mortalité moyen par intervalle de 5 ans
  mortality_5_year_intervals <- mortality_data %>%
    group_by(Interval) %>%
    summarise(MortalityRate = mean(MortalityRate, na.rm = TRUE)) %>%
    mutate(AgeInterval = paste(Interval * 5, (Interval + 1) * 5 - 1, sep = "-")) %>%
    select(AgeInterval, MortalityRate)
  
  return(mortality_5_year_intervals)
}

# Utilisation de la fonction
mortality_5_year_intervals <- convert_to_5_year_intervals(Germany_2012_male)

# Affichage de la table de mortalité par intervalle de 5 ans
print(mortality_5_year_intervals)
