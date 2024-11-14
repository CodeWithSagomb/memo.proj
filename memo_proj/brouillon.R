
################### Chargement library ###################
library(forecast)
library(tidyverse)

############# Importation de Donnees QX_FEMALEDATA ##########
QX_MALEDATA<-QX_MALE[1:85,]
##### Analyse descriptive ##################################
df<-QX_MALEDATA
view(df)
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
  labs(x = "Age", y = "Mortality Rate", title = "Female_Mortality Rates Over Time") +
  theme_minimal()
################## Estimation des Parametres ############################

Mxt<-mx
ln_m<-log(Mxt)
head(ln_m)
dim(ln_m)
## Estimation de ax 
ax <- apply(ln_m, 1, mean)  # Assuming ln_m is the matrix of ln(mx,t) values
head(ax)
length(ax)
plot(ax,col="blue",lwd=3,type = "l",main = "Estimation de ax_FEMME",
     xlab = "age")

# Calcul de matrice  A(x,t)
A <- ln_m - ax
dim(A)
################## Decomposition en valeur singuliere de A(x,t) 
SVD <- svd(A,nu=min(85,85),nv = min(44, 44))
#SVD
x1<-SVD$u
dim(x1)
x2<-SVD$v
dim(x2)
x3<-SVD$d
length(x3)
#### Determination de kt, s1, et bx
bx <- SVD$u[,1]  # First column of U
s1 <- SVD$d[1]    # First singular value
kt <- SVD$v[,1]  # First column of V

plot(bx,type = "l",lwd=3,col=6,main = "Estimation de bx_FEMME")
plot(kt,type = "l",lwd=3,col=2,main = "Estimation de kt_FEMME")
