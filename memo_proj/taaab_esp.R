
f<-as.matrix(data_femme[,-1])
class(f)
dim(f)
summary(f)
mode(f)



fem<-morta[,43]
length(fem)
class(fem)
ff<-f[,43]
f3<-qx_proj_fem[,31]
f3<-qx_for_morta[,31]
f3<-qx_proj_fem[,31]

esp_tab(f3)

DT2=0:119
DT1=0
for (j in 1:31) {

  DT1<-esp_tab(qx_proj_fem[,j])
  DT2<-cbind(DT2,DT1[,8])
}

view(DT2)

DT2 %>%
  as.matrix()%>%
  write.csv(file = "ESPERANCE_FEM_Bas.csv")


dim(mortality_table)

esp_tab<-function(Qx){
  
  # Vectofem# Vector of mortality quotients qx from age 0 to 119
  qx <- c(Qx)
  # Number of ages
  n <- length(qx)
  n
  # Step 1: Calculate px
  px <- 1 - qx
  
  # Step 2: Initialize lx
  lx <- numeric(n)
  lx[1] <- 100000  # Initial cohort size
  
  # Calculate lx for each age
  for (i in 2:n) {
    lx[i] <- lx[i - 1] * px[i - 1]
  }
  
  # Step 3: Calculate dx
  dx <- lx * qx
  
  # Step 4: Calculate Tx and Lx
  Tx <- numeric(n)
  Lx <- numeric(n)
  
  # Tx[n] is zero because there are no people alive after the last age
  Tx[n] <- 0
  
  # Calculate Lx
  for (i in 1:(n - 1)) {
    Lx[i] <- (lx[i] + lx[i + 1]) / 2
  }
  
  # Lx[n] is simply lx[n] because it's the last age
  Lx[n] <- lx[n]
  
  # Calculate Tx from bottom to top
  for (i in (n - 1):1) {
    Tx[i] <- Tx[i + 1] + Lx[i]
  }
  
  # Step 5: Calculate ex
  ex <- Tx / lx
  
  # Create a data frame for the mortality table
  mortality_table <- data.frame(
    Age = 0:(n - 1),
    qx = qx,
    px = px,
    lx = lx,
    dx = dx,
    Lx = Lx,
    Tx = Tx,
    ex = ex
  )
  
  # Print the mortality table
  #print(mortality_table)
 
  return(as.data.frame(mortality_table))
   
}



# Vectofem# Vector of mortality quotients qx from age 0 to 119
qx <- c(f3)
# Number of ages
n <- length(qx)
n
# Step 1: Calculate px
px <- 1 - qx

# Step 2: Initialize lx
lx <- numeric(n)
lx[1] <- 100000  # Initial cohort size

# Calculate lx for each age
for (i in 2:n) {
  lx[i] <- lx[i - 1] * px[i - 1]
}

# Step 3: Calculate dx
dx <- lx * qx

# Step 4: Calculate Tx and Lx
Tx <- numeric(n)
Lx <- numeric(n)

# Tx[n] is zero because there are no people alive after the last age
Tx[n] <- 0

# Calculate Lx
for (i in 1:(n - 1)) {
  Lx[i] <- (lx[i] + lx[i + 1]) / 2
}

# Lx[n] is simply lx[n] because it's the last age
Lx[n] <- lx[n]

# Calculate Tx from bottom to top
for (i in (n - 1):1) {
  Tx[i] <- Tx[i + 1] + Lx[i]
}

# Step 5: Calculate ex
ex <- Tx / lx

# Create a data frame for the mortality table
mortality_table <- data.frame(
  Age = 0:(n - 1),
  qx = qx,
  px = px,
  lx = lx,
  dx = dx,
  Lx = Lx,
  Tx = Tx,
  ex = ex
)

# Print the mortality table
print(mortality_table)

# Optionally, save the table to a CSV file
#write.csv(mortality_table, "mortality_table_2019.csv", row.names = FALSE)
