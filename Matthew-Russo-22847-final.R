# clear
rm(list = ls())

# install.packages("pacman", dependencies = TRUE)
pacman::p_load(lars, caret)

#load the data set
data <- read.csv("~/Desktop/_AMS/data/AirQualityUCI/AirQualityUCI.csv", header = TRUE, sep = ";")

# calculate the vector of Air Quality Idex with regards to NO2
calculate_AQI_NO2 <- function(data) {
  #empty vector as an accumulator
  AQI_vec <- c()
  
  # loop through the NO2 column
  for (i in 1:length(data[,10])) {
    # value at the index
    dat <- data[i,10]
    
    # make sure the value is not NA and also above 0
    if (!is.na(dat) & dat >= 0) {
      
      # get the NO2 max and min c
      vec <- getNO2Limits(dat)
      Imax <- vec[4]
      Imin <- vec[3]
      Cmax <- vec[2]
      Cmin <- vec[1]
  
      # get the Air Quality Index value
      I <- calculate_I(Imax, Imin, Cmax, Cmin, dat)
      # add the value to the AQI vector
      AQI_vec <- c(AQI_vec, I)
  
    } else { # if the value does not meet the criteria,
             # append -200 to the AQI vector
      AQI_vec <- c(AQI_vec, -200)
    }
  }
  # return the AQI vector
  return(AQI_vec)
}

# take in the NO2 value and return the bracket limits
# for the AQI and polutant
getNO2Limits <- function(value) {
  if (value >= 0 & value <= 53) {
    return(c(0,53,0,50))
  } else if (value >= 54 & value <= 100) {
    return(c(54,100,51,100))
  } else if (value >= 101 & value <= 360) {
    return(c(101,3600,101,150))
  } else if (value >= 361 & value <= 649) {
    return(c(361,649,151,200))
  } else if (value >= 650 & value <= 1249) {
    return(c(650,1249,201,300))
  } else if (value >= 1250 & value <= 1649) {
    return(c(1250,1649,301,400))
  } else if (value >= 1650 & value <= 2049) {
    return(c(1650,2049,401,500))
  } else {
    stop("ERROR: NO2 value is outside the AQI limits")
  }
}

# Calculates the AQI, needs the AQI (Imax, Imin)  and polutant bracket (Cmax, Cmin)
# values and the polutant value (C)
calculate_I <- function(Imax, Imin, Cmax, Cmin, C) {
  I <- ((Imax - Imin)/(Cmax - Cmin))*(C - Cmin) + Imin
  return(I)
}

# calculate the AQI vecvtor for the data
data2 <- calculate_AQI_NO2(data)

# append vector to a new column on the data called AQI_NO2
data$AQI_NO2 <- data2

# subset the data to remove all -200 values
data3 <- subset(data, AQI_NO2 != -200 & NO2.GT. != -200 & RH != -200  & CO.GT. != -200 & T != -200  
                & C6H6.GT. != -200 & NOx.GT. != -200)

# vector of column names to keep
cols <- c("AQI_NO2", "NO2.GT.", "RH", "CO.GT.", "T", "C6H6.GT.", "NOx.GT.")
# create a data frame with only the predefined columns
data4 <- data3[cols]

# write the new data to a csv without NA
write.csv(data4, file = "~/Desktop/_AMS/data/newAQI.csv",row.names=FALSE, na="")

data5 <- read.csv("~/Desktop/_AMS/data/newAQI.csv")

# Variable groups
x <- data5[-1] # remove AQI column
x2 <- as.matrix(x[-1]) # remove NO2 column and convert to a matrix
y <- data5[, 1] # AQI column only

# set the RFE control
ctrl <- rfeControl(method = "repeatedcv", repeats = 10, verbose = TRUE, functions = lmFuncs)
# Recursive Feature Elimination
rfe<- rfe(x2, y, sizes = c(1:5), rfeControl = ctrl)

# Plot the Root Mean Square Error graph
plot(rfe, type = c("o", "g"), lwd = 1, main = " Root Mean Square Error for predicted Air Quality Index (NO2)")

# print the RFE
rfe
