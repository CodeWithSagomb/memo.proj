

#### ANALYSE EXPLORATOIRE ET DESCRIPTIVE
df<-dataONS02[,-1]
class(df)
dim(df)
summary(df)
Mat01<-as.matrix(df)
class(Mat01)
mode(Mat01)
mx<-Mat01
view(mx)
head(as.vector(mx))
length(as.vector(mx))

#### Representation graphique

ages <- 0:(nrow(mx)-1)
length(ages)
years <- 1977:2014
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


persp3d(ages,1977:2014, mx,theta = 40, phi = 10, col = "#ff8080",ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 30)


################## Estimation des Parametres ############################

Mxt<-mx
dim(Mxt)
ln_m <- Mxt
## Estimation de ax 
ax <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
length(ax)
datax <- data.frame(age.x=0:79,ax=ax)
ggplot(datax, aes(x=age.x, y=ax)) +
  geom_line( color="blue", size=2, alpha=1.5, linetype=1)+
  ggtitle("Comportement moyen de Ln(mxt)")

# Calcul de matrice  A(x,t)
A <- ln_m - ax
dim(A)

################## Decomposition en valeur singuliere de A(x,t)

SVD_1 <- svd(A,1,1)
v <- SVD_1$v
u <- SVD_1$u
d <- SVD_1$d

b <- u/sum(u)
k <- v * sum(u)*d[1]
b
k

kkk
bbb

data.bx <- data.frame(age.x=0:79,bx=b)
ggplot(data.bx, aes(x=age.x, y=bx)) +
  geom_line( color="blue", size=2, alpha=1.5, linetype=1)+
  ggtitle("Sensibilité a l'age x par rapport a la mortalité")

data.kt<-data.frame(year.t=1977:2014,kt=k)
ggplot(data.kt, aes(x=year.t, y=kt)) +
  geom_line( color="red", size=2, alpha=1, linetype=1)+
  ggtitle("Evolution Générale de la Mortalité")

sum(k)
sum(b)

########## Analyse de Kappa t ########

serie <- ts(k,start = c(1977,1))
length(serie)
ts_info(serie)
ts_plot(serie,
        title = "Evolution generale du taux de mortalité",
        Ytitle = "Serie",
        Xtitle = "Time",slider = FALSE,width = 4)

#Décomposition de la série
summary(serie)
class(serie)
ts_lags(serie)
ar(diff(serie))
ggAcf(serie) + ggtitle("ACF of kappa.t")
ggPacf(serie) + ggtitle("PACF of kappa.t")
adf.test(serie, alternative="stationary")

dinf <- diff(serie,differences = 1)
adf.test(dinf, alternative="stationary")
ts_lags(dinf)
ggAcf(dinf) + ggtitle("ACF of kappa.t")
ggPacf(dinf) + ggtitle("PACF of kappa.t")
eacf(serie, ar.max = 7, ma.max = 7)
plot(armasubsets(dinf,nar=5,nma=5))

full_model <- auto.arima(dinf)
full_model


fit.S <- Arima(serie,order = c(1,0,0),
               include.drift=TRUE,include.mean = TRUE) #bon model
predictt <- forecast((fit.S), h = 36)
predictt %>% autoplot()
accuracy(fit.S) #RMSE 1.776857
pred<- as.data.frame(predictt)
view(pred)
k_forecasted<-pred[,1]
k_forecasted
length(k_forecasted)
b_tk<-bbb%*%t(k_forecasted)
dim(b_tk)
ln_mxt_forecasted<-ax+b_tk
dim(ln_mxt_forecasted)

persp(0:119,2015:2050, ln_mxt_forecasted,theta = 0, phi = 30,col = "red",ltheta = 50) 
persp3d(0:119,2015:2050, ln_mxt_forecasted,theta = 40, phi = 10, col = 2:5,ltheta = 50)
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

LN_Mxt.past<-dataONS02[,-1]
dim(LN_Mxt.past)
global_ln_mxt<-cbind(LN_Mxt.past,ln_mxt_forecasted)
dim(global_ln_mxt)
global_ln_mxt<-as.matrix(global_ln_mxt)
class(global_ln_mxt)
persp3d(0:119,1977:2050, global_ln_mxt,theta = 40, phi = 10, col = c("#85e085"),ltheta = 50)
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

mxt_forecasted<-exp(ax+b_tk)
dim(mxt_forecasted)

qx_forcasted<- mx2qx(mxt_forecasted)
dim(qx_forcasted)

tab_esp<-probs2lifetable(qx_forcasted[,3] ,radix = 100000,type = "qx")
tab_esp



