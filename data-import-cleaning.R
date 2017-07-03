library(dplyr)
library(ggplot2)
library(xts)
library(zoo)
library(caTools)
setwd("C://Users//Bangda//Desktop//project-power")

### Data import and inspect ###
raw_data = read.csv("powerdata.csv", skip = 1)
raw_data[1:10, 1:12]

#   To store conveniently, split into 4 parts. To combine them together:
subdata1 = powerdata[, 1:5]
subdata2 = powerdata[, 6:10]
subdata3 = powerdata[, 11:15]
subdata4 = powerdata[, 16:20]
write.csv(subdata1, "subdata1.csv")
write.csv(subdata1, "subdata2.csv")
write.csv(subdata1, "subdata3.csv")
write.csv(subdata1, "subdata4.csv")
# read into environment ->
powerdata = cbind(subdata1, subdata2, subdata3, subdata4)

### Data Cleaning ###
# We try to convert the data into tidy data form

View(raw_data[-(1:2), ])
# Remove titles
powerdata = as.matrix(raw_data[-c(1:2), ], byrow = FALSE, ncol = 1)
dim(powerdata)
# Reshape
powervec = as.vector(powerdata, mode = "numeric")
powervec[1:10]

powermat = matrix(powervec, ncol = 20)
dim(powermat)

# Generate label of machines
machinelabel = sapply(1:20, rep, dim(powerdata)[1])
machinelabel = as.vector(machinelabel, mode = "numeric")

# Tidy data
powerdf = data.frame(
  power = powervec,
  machine = machinelabel
)
head(powerdf)
str(powerdf)

# Check missing data
sum(is.na(powerdf$power))
countNA = function(vec) {
  return(sum(is.na(vec)))
}

nadf = ddply(powerdf, .(machine), countNA)
arrange(nadf, V1)

### Pick four machine without missing values
subpower = subset(powerdf, machine %in% c(1, 5, 11, 15))
