# Example data for Age-Specific Mortality Rates (replace with your own data)
# This data represents mortality rates at different ages (rows) over time (columns)
m <- matrix(c(
  0.001, 0.002, 0.003, 0.004, 0.005,
  0.002, 0.003, 0.004, 0.005, 0.006,
  0.003, 0.004, 0.005, 0.006, 0.007,
  0.004, 0.005, 0.006, 0.007, 0.008
), nrow = 4, ncol = 5)

# Create a meshgrid for x and t
x <- 1:nrow(m) # Age
t <- 1:ncol(m) # Time

# Create 3D plot
persp(x, t, m, 
      main = "Age-Specific Mortality Rates", 
      xlab = "Age", 
      ylab = "Time", 
      zlab = "Mortality Rate",
      theta = 50, phi = 35,col = 2,expand = 0.5,
      border = NA,shade = 0.2)
?persp

# import and load rgl package 
install.packages("rgl") 
library(rgl) 

# Generate some sample data 
x <- seq(-5, 6, by = 0.1) 
y <- seq(-5, 7, by = 0.1) 
z <- outer(x, y, function(x, y) dnorm(sqrt(x^2 + y^2))) 

# Create a 3D surface plot 
persp3d(x, y, z, col = "blue") 

# add animation 
play3d(spin3d(axis = c(0, 0, 1)), duration = 10)

###################TRYING MORTALITY TABLE ############################################################

library(lifecontingencies)
library(tidyverse)

x1<-seq(from=0, to=19, by =1)
length(x1)
lx1<-c(1000,965,950,914,850,768,740,705,680,
       634,600,586,550,487,400,367,310,256,139,50)
length(lx1)
data.frame(x1,lx1)

Lt<-new("lifetable",x=x1,lx=lx1,name="Example of life table")
print(Lt)

b1<-0:84
length(b1)
M<-W2[,1]
length(M)

Lt<-new("lifetable",x=b1,lx=M,name="Example of life table")
print(Lt)
?probs2lifetable

qx2mx(mx2qx(.2))
M1<-mx2qx(W2[,30])
plot(M1)

probsup<-seq(0.9,0,by=-0.01)
tab<-probs2lifetable(probsup,type = "px")
class(tab)
view(tab)
tab
?probs2lifetable
exn(object=tab, x=87)
plot(tab,y="ANY")
summary(tab)
show(tab)
tab1<-coerce(from = "tab",to="data.frame")
?coerce
tab1<-as.data.frame(tab,from = "lifetable", to = "data.frame")
tab1
class(tab1)
tab1<-as.data.frame(tab,from = "tab", to = "data.frame")
print(tab)
fix(tab)
summary(tab)
a<-getOmega(tab)
?getOmega
exn(object=tab, x=0:10)
class(tab)
j<-0
for (i in 0:89) {
  a[j<-j+1] <-exn(object=tab, x=i)
}
a
class(a)
length(a)

getexpval<- function(t){
  j<-0
  for (i in 0:89) {
    a[j<-j+1] <-exn(object=t, x=i)
  }
  return(a)
}
a1<-getexpval(tab)
length(a1)
a==a1


df<-as.matrix(Classeur1)
class(df)
qx1<-df[-19,6]
qx1
probsup<-seq(0.9,0,by=-0.01)
tab<-probs2lifetable(qx,radix = 100000,type = "qx")
tab
data("demoUsa", package="lifecontingencies")
dim(demoUsa)
view(demoUsa)
?probs2lifetable
US07Male<-demoUsa[,c("age","USSS2007M")]
names(US07Male)<-c("x","lx")
view(US07Male)0
US07MaleLt<-as(US07Male,"lifetable")
print(US07MaleLt)
Tx(tab, 1)
qx
class(qx)
length(qx)


data("demoUsa", package="lifecontingencies")
is.data.frame(demoUsa)
## [1] TRUE
US07Male<-demoUsa[,c("age","USSS2007M")]
names(US07Male)<-c("x" ,"lx")
names(US07Male)
US07MaleLt<-as(US07Male,"lifetable") 
class(US07Male)
print(US07MaleLt)






################################################### fonction pour la table #######

df<-as.matrix(Classeur1)
class(df)

Ix=100000
Ix
dx<-qx1[5]*Ix
Ix<-Ix-dx
dx
Ix

qx<-df[-19,6]
qx
qx1<-df[,6]
length(qx1)


Ix=100000
vect<-0
Dx<-0
k<-0
l<-0
for (i in 1:length(qx)) {
  dx<-qx[i]*Ix
  Ix<-Ix-dx
  vect[k<-k+1] <-Ix
  Dx[l<-l+1] <-dx
}
vect
Dx
view(vect)

gettabx<- function(qx){
  
  Ix=100000
  vect<-0
  l<-0
  k<-0
  for (i in 1:length(qx)) {
    dx<-qx[i]*Ix
    Ix<-Ix-dx
    vect[k<-k+1] <-Ix
    Dx[l<-l+1] <-dx
  }
  vect1<-c(100000,vect)
  Dx1<-c(Dx,vect[length(qx)])
  DD<-data.frame(X=0:length(qx), I_x=vect1,d_x=Dx1)
  return(cbind(DD[-(length(qx)+1),],q_x=qx1,p_x=1-qx1,l_x=(1-qx1)*vect1[-20]))
}

gettabx(qx1)
cbind(gettabx(qx1),qx1,px=1-qx1)



# Exemple de données fictives
ages <- 0:3
Nx <- c(100000, 99000, 98500, 98205) # Nombre de vivants
Dx <- c(1000, 500, 300, 200)         # Nombre de décès

# Étape 1 : Calculer les probabilités de décès (qx)
qx <- Dx / Nx

# Étape 2 : Calculer les probabilités de survie (px)
px <- 1 - qx

# Initialisation de la table
lx <- numeric(length(Nx))
Lx <- numeric(length(Nx))
Tx <- numeric(length(Nx))

# Étape 3 : Calculer le nombre de survivants (lx)
lx[1] <- 100000 # Hypothèse initiale, souvent 100000
for (i in 2:length(Nx)) {
  lx[i] <- lx[i-1] * px[i-1]
}

# Étape 4 : Calculer les personnes-vies (Lx)
for (i in 1:(length(Nx)-1)) {
  Lx[i] <- (lx[i] + lx[i+1]) / 2
}
Lx[length(Nx)] <- lx[length(Nx)]

# Étape 5 : Calculer les personnes-vies cumulées (Tx)
Tx[length(Nx)] <- Lx[length(Nx)]
for (i in (length(Nx)-1):1) {
  Tx[i] <- Tx[i+1] + Lx[i]
}

# Étape 6 : Calculer l'espérance de vie (ex)
ex <- Tx / lx

# L'espérance de vie à la naissance (e0) est simplement e_x à l'âge 0
e0 <- ex[1]

# Résultats
table_mortalite <- data.frame(
  Age = ages,
  Nx = Nx,
  Dx = Dx,
  qx = qx,
  px = px,
  lx = lx,
  Lx = Lx,
  Tx = Tx,
  ex = ex
)

# Afficher la table de mortalité
print(table_mortalite)

# Afficher l'espérance de vie à la naissance
cat("Espérance de vie à la naissance (e0):", e0, "années\n")





##################### CODE Table de mortalité abrégée   #######################################

########### lifecontengencies
dg<-as.matrix(QX_MALEDATA)
class(dg)
tab<-probs2lifetable(dg[,45] ,radix = 100000,type = "qx")
class(tab)
tab

########## 

# Étape 1 : Calculer le nombre de survivants (lx)
lx[1] <- 100000 # Hypothèse initiale, souvent 100000
for (i in 2:length(Nx)) {
  lx[i] <- lx[i-1] * px[i-1]
}

cl<-as.matrix(Classeur1)
cl[,1]
dqx<-cl[,6]
class(dqx)

Ix=100000
Ix
dx<-qx1[2]*Ix
Ix<-Ix-dx
dx
Ix

# Initialisation de la table
lx <- numeric(length(Nx))
dx <- numeric(length(Nx))
Tx <- numeric(length(Nx))


Ix <- numeric(length(dqx))
dx <- numeric(length(dqx))
Ix
dx
Ix[1] =100000

for (i in 2:c(length(dqx)+1)) {
  dx[i-1]<- Ix[i-1]*dqx[i-1]
  if(i==20){break}
  Ix[i] <-Ix [i-1] - dx[i-1]
}
Ix[3]
dx
length(Ix)
Lx<-numeric(length(dqx))
Lx[1] <-0.2*dx[1]+Ix[2]
Lx[2]<- 2*dx[2]+4*Ix[3]
Lx[19]<-5*dx[19]
for (i in 3:c(length(dqx)-1)) {
  Lx[i] <-2.5*dx[i]+5*Ix[i+1]
}
Lx
length(Lx)
sum(Lx[3:19] )/97658

data.frame(X=0:c(length(dqx)-1),Q_x=dqx,P_x=1-dqx,
           I_x=Ix[-c(length(dqx)+1)] ,dx)

#Calculer les personnes-vies cumulées (Tx)

Tx <- numeric(length(Lx))
for (i in 1:length(Lx)) {
  Tx[i]<-sum(Lx[i:length(Lx)])
}
Tx
Lx
length(Lx)

#Étape 6 : Calculer l'espérance de vie (ex)

ex <- Tx / lx

ex<-numeric(length(Tx))
for (i in 1:length(Tx)) {
  ex[i]<-Tx[i]/ Ix[i]
}
ex
c(length(dqx)+1)

get_tab_morta<-function(dqx){
  Ix <- numeric(length(dqx))
  dx <- numeric(length(dqx))
  Ix[1] =100000
  for (i in 2:c(length(dqx)+1)) {
    dx[i-1]<- Ix[i-1]*dqx[i-1]
    if(i==c(length(dqx)+1)){break}
    Ix[i] <-Ix [i-1] - dx[i-1]
  }
  #Calculer les personnes-vies (Lx)
  Lx<-numeric(length(dqx))
  Lx[1] <-0.2*dx[1]+Ix[2]
  Lx[2]<- 2*dx[2]+4*Ix[3] # a=2;n=4;5
  Lx[19]<-6*dx[19]
  for (i in 3:c(length(dqx)-1)) {
    Lx[i] <-2.5*dx[i]+5*Ix[i+1]
  }
  #Calculer les personnes-vies cumulées (Tx)
  Tx <-numeric(length(Lx))
  for (i in 1:length(Lx)) {
    Tx[i]<-sum(Lx[i:length(Lx)])
  }
  #Calculer l'espérance de vie (ex)
  ex<-numeric(length(Tx))
  for (i in 1:length(Tx)) {
    ex[i]<-Tx[i]/ Ix[i]
  }
  tab<-data.frame(X=0:c(length(dqx)-1),Q_x=dqx,
                  P_x=1-dqx,I_x=Ix,d_x=dx,L_x=Lx,T_x=Tx,E_x=ex)
  return(tab)
}


cl<-as.matrix(Classeur1)
cl[,1]
dqx<-cl[,6]
class(dqx)
A<-get_tab_morta(dqx)
A

df<- as.matrix(QX_MALEDATA)  
class(df)
dim(df)
dfx1<-df[,45]
length(dfx1)
A2<-get_tab_morta(dfx1)
A2

###########################################################

lx<-100000
Dx<-lx*dfx1[4]
lx<-lx-Dx
Dx
lx

# calcul de nombre de survivant
lx<-numeric(length(dfx1))
Dx<-numeric(length(dfx1))
lx[1]<-100000
for (i in 2:c(length(dfx1)+1)) {
  Dx[i-1] <-lx[i-1] *dfx1[i-1]
  if(i==c(length(dfx1)+1)){break}
  lx[i] <-lx[i-1]-Dx[i-1]
}
length(lx)
length(Dx)
lx
Dx

tabbx<-function(dfx1){
  # calcul de nombre de survivant
  lx<-numeric(length(dfx1))
  Dx<-numeric(length(dfx1))
  lx[1]<-100000
  for (i in 2:c(length(dfx1)+1)) {
    Dx[i-1]<-lx[i-1] *dfx1[i-1]
    if(i==c(length(dfx1)+1)){break}
    lx[i] <-lx[i-1]-Dx[i-1]
  }
  lx<-ceiling(lx)
  Dx<-ceiling(Dx)
  #Calculer les personnes-vies (Lx)
  Lx<-numeric(length(dfx1))
  Lx[1] <-0.2*Dx[1]+lx[2]
  #Lx[length(dfx1)]<-6*Dx[length(dfx1)]
  for (i in 2:c(length(dfx1))) {
    ifelse(i < length(dfx1),
    Lx[i] <-0.5*Dx[i]+ lx[i+1],
    Lx[i] <- 0.5* lx[i])
  }
  
  Lx<-ceiling(Lx)
  #Calculer les personnes-vies cumulées (Tx)
  Tx <-numeric(length(Lx))
  for (i in 1:length(Lx)) {
    Tx[i]<-sum(Lx[i:length(Lx)])
  }
  Tx<-ceiling(Tx)
  #Calculer l'espérance de vie (ex)
  ex<-numeric(length(Tx))
  for (i in 1:length(Tx)) {
    ex[i]<-Tx[i]/ lx[i]
  }
  #ex<-ceiling(ex)
  Dt<- data.frame(X=0:c(length(dfx1)-1),qx=dfx1,
                  px=1-dfx1,lx=lx,dx=Dx,Lx=Lx,Tx=Tx,Ex=ex)
  return(Dt)
  
}

dff<- as.matrix(QX_MALEDATA)  
class(dff)
dim(dff)
dfx1<-dff[,44]
length(dfx1)
A1<-tabbx(dfx1)
dim(A1)
A1

##### table de morta avec les Qx projetes

Df<-as.matrix(Qx10Proj)
class(Df)
dim(Df)
dfx2<-Df[,1]
length(dfx2)
A2<-tabbx(dfx2)
dim(A2)
A2
0.17722461*100000
?mx2qx

d<-mmx_forecasted[,2]
d
length(d)
dmqx<-mx2qx(dm)
view(dmqx)
tb<-probs2lifetable(dmqx,type = "qx")
tb
A3<-tabbx(dmqx)
dim(A3)
A3

########### lifecontengencies
dg<-as.matrix(QX_MALEDATA)
class(dg)
tab<-probs2lifetable(dg[,45] ,radix = 100000,type = "qx")
class(tab)

tab

s<-mx2qx(mxt_forecasted[,2])
s
ab<-probs2lifetable(s$V1,radix = 100000,type = "qx")
ab

###############################



tabbx<-function(dfx1){
  # calcul de nombre de survivant
  lx<-numeric(length(dfx1))
  Dx<-numeric(length(dfx1))
  lx[1]<-100000
  for (i in 2:c(length(dfx1)+1)) {
    Dx[i-1]<-lx[i-1] *dfx1[i-1]
    if(i==c(length(dfx1)+1)){break}
    lx[i] <-lx[i-1]-Dx[i-1]
  }
  lx<-ceiling(lx)
  Dx<-ceiling(Dx)
  #Calculer les personnes-vies (Lx)
  Lx<-numeric(length(dfx1))
  Lx[1] <-0.2*Dx[1]+lx[2]
  #Lx[length(dfx1)]<-6*Dx[length(dfx1)]
  for (i in 2:c(length(dfx1))) {
    ifelse(i < length(dfx1),
           Lx[i] <-0.5*Dx[i]+ lx[i+1],
           Lx[i] <- 0.5* lx[i])
  }
  
  Lx<-ceiling(Lx)
  #Calculer les personnes-vies cumulées (Tx)
  Tx <-numeric(length(Lx))
  for (i in 1:length(Lx)) {
    Tx[i]<-sum(Lx[i:length(Lx)])
  }
  Tx<-ceiling(Tx)
  #Calculer l'espérance de vie (ex)
  ex<-numeric(length(Tx))
  for (i in 1:length(Tx)) {
    ex[i]<-Tx[i]/ lx[i]
  }
  #ex<-ceiling(ex)
  Dt<- data.frame(X=0:c(length(dfx1)-1),qx=dfx1,
                  px=1-dfx1,lx=lx,dx=Dx,Lx=Lx,Tx=Tx,Ex=ex)
  return(Dt)
  
}

Df<-as.matrix(Qx_HOMMES[-121,-1])
class(Df)
dim(Df)
dfx2<-Df[,1]
length(dfx2)
A2<-tabbx(dfx2)
dim(A2)
A2
floor(78.530994)
round(78.530994,2)

tab<-probs2lifetable(dfx2 ,radix = 100000,type = "qx")
class(tab)
tab
view(EWMaleData$Dxt)
?EWMaleData


####################################



tb1<-function(datt){
  # calcul de nombre de survivant
  lx<-numeric(length(datt))
  Dx<-numeric(length(datt))
  lx[1]<-100000
  for (i in 2:c(length(datt)+1)) {
    Dx[i-1]<-lx[i-1] *datt[i-1]
    if(i==c(length(datt)+1)){break}
    lx[i] <-lx[i-1]-Dx[i-1]
  }
  lx<-ceiling(lx)
  Dx<-ceiling(Dx)
  #Calculer les personnes-vies (Lx)
  Lx<-numeric(length(datt))
  Lx[1] <-0.5*Dx[1]+lx[2]
  #Lx[length(dfx1)]<-6*Dx[length(dfx1)]
  for (i in 2:c(length(datt))) {
    ifelse(i < length(datt),
           Lx[i] <-0.5*Dx[i]+ lx[i+1],
           Lx[i] <- 0.5* lx[i])
  }
  
  Lx<-ceiling(Lx)
  
  qq<-data.frame(x=0:(length(datt)-1),datt,lx,Dx,Lx)

  return(qq)
}

A3<-0:119
for (i in 1:ncol(Df) ) {
  A<-tb1(Df[,i])
  A2<-as.matrix(A[,5])
  A3<-cbind(A3,A2)
}
dim(A3)
A3
A4<-0
A4<-A3[,-1]
A5<-A3[,-1]
view(A4)
view(A5)
ncol(Df)

A<-tb1(dfx2)
A
dim(A)
as.matrix(A[,4])

############################# Stmomo 

LC<-lc(link = "logit")
Dxt<-A4
Ext<-A5
age<-0:119
year<-1977:2019

LCfit<-fit(LC,Dxt=Dxt,Ext=Ext,ages=age,years=year)
