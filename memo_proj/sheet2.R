library(Q2q)
library(tidyverse)
dg<-Classeur1
dg
mode(dg)
class(dg)
dim(dg)
dg1<-as.matrix(dg)
dg1<-dg1[-19,]
v
Fem<- getqxt(dg1,18,6)
QXF<-Fem$qxt
fix(QXF)
dim(QXF)
write.csv(QXF,"QX_females.csv",row.names = TRUE)
##################
library(readxl)
Classeur1 <- read_excel("C:/Users/DELL/Desktop/memoire doc/Classeur1.xlsx", 
                        sheet = "Femme", col_names = FALSE, col_types = c("numeric", 
                                                                          "numeric", "numeric", "numeric", 
                                                                          "numeric", "numeric"))
View(Classeur1)



dh<-Classeur1
dh
mode(dh)
class(dh)
dim(dh)
dh1<-as.matrix(dh)
dh1
Hom<- getqxt(dh1,19,6)
QXH<-Hom$qxt
fix(QXH)
dim(QXH)
write.csv(QXH,"QX_males.csv",row.names = TRUE)
###########################
dv<-Classeur1
dv
mode(dv)
class(dv)
dim(dv)
dv1<-as.matrix(dv)
dv1
Ens<- getqxt(dv1,19,6)
QXE<-Ens$qxt
fix(QXE)
dim(QXE)
write.csv(QXH,"QX_both.csv",row.names = TRUE)

#################################################

wx<-Classeur1
View(wx)
mode(wx)
class(wx)
dim(wx)
wx1<-as.matrix(wx)
wx1
wxx1<-wx1[-19,]
wxx1
homme<- getqxt(wxx1,18,6)
QXH2<-homme$qxt
fix(QXH2)
dim(QXH2)
write.csv(QXH,"QX_homme.csv",row.names = TRUE)

############## DATA ONS VERS DATA DE TRAITEMENT #####

D1<-onsdatamale
head(D1)
D2<-D1[1:90,]
View(D2)
DD<-cbind(D2,QX_homme)
DD<-DD[,-40]
DD<-DD[-90,]
View(DD)
write.csv(DD,"QX_MALE.csv",row.names = TRUE)

F1<-ONSdatafemales
head(F1)
F2<-F1[1:90,]
View(F2)
FF<-cbind(F2,QX_females)
FF<-FF[,-40]
FF<-FF[-90,]
View(FF)
write.csv(FF,"QX_FEMALE.csv",row.names = TRUE)

G1<-ONSdataboth1
head(G1)
G2<-G1[1:90,]
View(G2)
GG<-cbind(G2,QX_both)
GG<-GG[,-40]
GG<-GG[-90,]
View(GG)
write.csv(GG,"QX_BOTH1.csv",row.names = TRUE)

######################################
dv<-Classeur1
dv
mode(dv)
class(dv)
dim(dv)
dv1<-as.matrix(dv)
dv1
dv1<-dv1[-19,]
dv1
Ens<- getqxt(dv1,18,6)
QXE<-Ens$qxt
fix(QXE)
dim(QXE)
write.csv(QXE,"QX_both11.csv",row.names = TRUE)

#################################################

D1<-ONSdataboth1
head(D1)
D2<-D1[1:85,]
View(D2)
DD<-cbind(D2,QX_both11)
DD<-DD[,-40]
View(DD)
write.csv(DD,"QX_BOTHDATA.csv",row.names = FALSE)
