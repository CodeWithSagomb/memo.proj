
################### Chargement library ###################
library(forecast)
library(tidyverse)
library(TSA)

############# Importation de Donnees QX_FEMALEDATA ##########
##### Analyse descriptive ##################################


df<- table
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

sum(k)


serie <- ts(k,start = c(1977,1))
length(serie)
plot(serie)



acf(serie)
pacf(serie)


auto.arima(serie)


















##################################################################################
#SVD
x1<-SVD$u

x1
dim(x1)
x2<-SVD$v
dim(x2)
x3<-SVD$d
length(x3)



#### Determination de kt, s1, et bx
 bx<- SVD$u[,1]  # First column of U
s1 <- SVD$d[1]    # First singular value
kt<- SVD$v[,1]  # First column of V

plot(bx,type = "l",main = "Estimation de bx_homme")
plot(kt,type = "l",lwd=3,col=2,main = "Estimation de kt_homme")


sum(kt)


###############################################################"

Years <- 1977:2020
Age <- 0:84

m <- Mxt
n.x = nrow(m) # 85
n.y = ncol(m) # 44

m_mat <- matrix(m, nrow = n.x, ncol = n.y) # 111 X 94
plot(m_mat[,1], type = "l") #mortality of all ages in year 1933
# Mean log mortality for all age groups under consideration (Age)
a_age <- rowMeans(m_mat)
lines(x = Age, y = a_age, col = 2)#average mortality for all age groups

# plotting mortality for Age = 65 as a trial run to see if code is working
# as exxpected
plot(x = Years, y = m_mat[65,], pch = 18, xlab = "Years", ylab = "log m", col = 2) #working!!!

# Average mortality across each year
a_year <- colMeans(m_mat)

#We need to estimate parameters ax, bx and kt of the Lee-Carter model
# ax is the avg of log mortality rates over time. ie sum(log m)/n.y
# avg of log rates -> a_hat <- rowSums(m)/94
# Subtracting a from all years in m_mat
m_hat <- matrix(1, nrow = n.y, ncol = n.x) #Dummy matrix with dim 94 X 111
m_hat <- t(m_mat)
m_mat <- t(m_mat)
for(i in 1:n.y) m_hat[,i] <- m_mat[,i] - a_age[i]
m_hat <- m_hat[,0:84]
age_mod <- 0:84
#SVD
svd_ <- svd(m_hat,1,1)
v <- svd_$v
u <- svd_$u
d <- svd_$d

b <- v/sum(v)
k <- u * sum(v) * d[1]
fit_1 = a_age[age_mod] + (b * k[1])
plot(x = age_mod, y = m_mat[0,age_mod], pch = 19, xlab = "Age", ylab = "m")
lines(x = age_mod, y = fit_1, col = 2, lwd = 2, xlab = "Age", ylab = "m")

fit_end <- a_age[age_mod] + (b * k[84])
plot(x = age_mod, y = m_mat[84,age_mod], pch = 18)
lines(x = age_mod, y = fit_end, col = 2, lwd = 2)

#Plotting parameter
par(mfrow = c(2,2))
plot(x = Years, y = k, type = "l")
plot(x = age_mod, y = a[age_mod], type = "l", xlab = "Ages", ylab = "ax")
plot(x = age_mod, y = b[age_mod], type = "l", xlab = "Ages", ylab = "bx" )
#plotting m over "Years"
plot(x = Years, y = m_mat[,1], pch = 19, ylab = "m", xlab = "Years", type = "l")
par(mfrow = c(1,1))

########################################################################################

##Performing SVD over ages 40:90
Age_new <- 40:90
m_mat_new <- m_mat[,Age_new]
m_hat_new <- m_hat[,Age_new]
n_x <- nrow(m_hat_new)
n_y <- ncol(m_hat_new)

for(i in 1:n_y) m_hat_new[,i] <- m_mat[,i] - a_age[i]

#SVD
svd_ <- svd(m_hat_new,1,1)
v_1 <- svd_$v
u_1 <- svd_$u
d_1 <- svd_$d

b_1 <- v_1/sum(v_1)
k_1 <- u_1 * sum(v_1) * d_1[1]
fit_1_new = a_age[Age_new] + (b_1 * k_1[1])
plot(x = Age_new, y = m_mat_new[1,], pch = 19, xlab = "Age", ylab = "m")
lines(x = Age_new, y = fit_1_new, col = 2, lwd = 2)

fit_end_new <- a_age[Age_new] + (b_1 * k_1[84])
plot(x = Age_new, y = m_mat_new[84,], pch = 18, xlab = "Age", ylab = "m")
lines(x = Age_new, y = fit_end_new, col = 2, lwd = 2)

#Plotting parameter
par(mfrow = c(2,2))
plot(x = Years, y = k_1, type = "l",  type = "l", xlab = "Years", ylab = "k")
plot(x = Age_new, y = a_age[Age_new], type = "l", type = "l", xlab = "Ages", ylab = "ax")
plot(x = Age_new, y = b_1, type = "l",  type = "l", xlab = "Ages", ylab = "bx")
par(mfrow = c(1,1))


