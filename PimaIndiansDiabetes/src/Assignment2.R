setwd("E:/8th semester/Lie Detector/Lab 2/Assignment")
install.packages("mlbench")

library(mlbench)

data("PimaIndiansDiabetes")

data <- PimaIndiansDiabetes
data
##Question 1
#number of variables
length(variable.names(data))

#classes of variables
sapply(data, class)

#number of observations
nrow(data)

##Question 2
#Dataframe 1
dataframe1 <- data[(data$glucose > 150) & (data$pregnant > 5),]
dataframe1

#Dataframe 2
dataframe2 <- data[(data$mass < 30) | (data$mass > 40),]
dataframe2

#Dataframe 3
dataframe3 <- data[(data$age < 50) & (data$insulin < 400),]
dataframe3

#Dataframe 4
dataframe4 <- data[(data$pedigree > 1) & (data$diabetes == "neg" ) & (data$pressure > 80),]
dataframe4

##Question 3
#converting dataframe to matrix
mat <- data.matrix(data, rownames.force = NA)

#extracting first 100 rows
mat <- mat[0:100,]

#extracting specific columns
res_mat <- matrix(c("insulin", "glucose", "pressure", "pregnant", "mass", "age", "diabetes" ), ncol = 1)
fin_mat <- mat[, res_mat[, 1]]

fin_mat
#removing rows 
x2 <- fin_mat[!fin_mat %in% 10]
x2 <- x2[!x2 %in% 25]
x2 <- x2[!x2 %in% 60]
x2 <- x2[!x2 %in% 45]
x2
##Question 4
sum <- 0
count <- 0
len <- length(data$glucose)
typeof(len)
for (i in 1:len){
  if (data$age[i] > 20 & data$age[i] < 30) {
    sum <- sum + data$glucose[i]
    count <- count+1
  }
}
sum
#mean glucose level
average <- sum/count
average


#normal blood sugar level is between 4.0 to 5.4 mmol/L (72 to 99 mg/dL) when fasting
#Up to 7.8 mmol/L (140 mg/dL) 2 hours after eating
#The mean glucose level is 113.7449 mg/dL
#Reference: https://www.diabetes.co.uk/diabetes_care/blood-sugar-level-ranges.html

##Question 5
pregnancy <- c()
for (i in 1:len){
  if (data$pregnant[i] > 0){
    pregnancy[i] <- "Y"
  }
  else{
    pregnancy[i] <- "N"
  }
}
length(pregnancy)

#shows number of Yes and No values. There are 111 women who hae never been pregnant. While 657 women have been pregnant even once. 
table(pregnancy)

#including the variable in our dataset
data$pregnancy = pregnancy
data

##Question 6
pressuretype <- c()
for (i in 1:len){
  if (data$pressure[i] > 80){
    pressuretype[i] <- "High"
  }
  else if (data$pressure[i] > 40){
    pressuretype[i] <- "Average"
  }
  else{
    pressuretype[i] <- "Low"
  }
}

#including the vairable in our dataset
data$pressuretype = pressuretype
data

#shows number of Yes and No values. There are 563 women with average blood pressure. 
#While 165 women have high blood pressure and 40 women have low blood pressure 
table(pressuretype)

#high blood pressure women with diabetes
value <- data[(data$pressuretype == "High") & (data$diabetes == "pos" ),]
#number of women with high blood pressure and diabetes
nrow(value)

##Question 7
minage <- min(data$age)
minage
maxage <- max(data$age)
maxage

gap <- 5 - (max%%5)

gap

agecategory <- cut(data$age, breaks = seq(min-1, max+gap, by = 5), labels = 1:13)
## table of the resulting factor           
table(agecategory)
data$agecategory = agecategory
data
len
levels(agecategory)

means= c()
maxs = c()
for (i in 1:length(levels(agecategory))){
  sum <- 0
  count <- 0
  max <- 0
  category <- levels(agecategory)[i]
  category
  for (j in 1:len){
    if (data$agecategory[j] == as.numeric(category)){
      sum <- sum + data$pregnant[j]
      count <- count+1
      if (data$pregnant[j] > max ){
        max <- data$pregnant[j]
      }
    }
  }
  means[i] <- sum/count
  maxs[i] <- max
}

means
maxs

##Question 8

agecategory <- cut(data$age, breaks = seq(minage-1, maxage+gap, by = 10), labels = 1:6)
## table of the resulting factor           
table(agecategory)
data$agecategory = agecategory
data
len
levels(agecategory)


means= c()
maxs = c()
for (i in 1:length(levels(agecategory))){
  sum <- 0
  count <- 0
  max <- 0
  category <- levels(agecategory)[i]
  category
  for (j in 1:len){
    if ((data$agecategory[j] == as.numeric(category)) & (data$diabetes[j] == "pos") ){
      sum <- sum + data$pregnant[j]
      count <- count+1
      if (data$pregnant[j] > max ){
        max <- data$pregnant[j]
      }
    }
  }
  means[i] <- sum/count
  maxs[i] <- max
}

means
maxs

##Question 9
install.packages("plyr")
library(plyr)

#part i
ddply(
  .data = data,
  .variables = "pressuretype",
  .fun = c(mean = function(z) mean(z$glucose),
  median = function(z) median(z$glucose),
  sd = function(z) sd(z$glucose),
  max = function(z) max(z$glucose),
  min = function(z) min(z$glucose))
)

#part ii
ddply(
  .data = data,
  .variables = c("pressuretype", "pregnancy"),
  .fun = c(mean = function(z) mean(z$mass),
           median = function(z) median(z$mass),
           sd = function(z) sd(z$mass),
           max = function(z) max(z$mass),
           min = function(z) min(z$mass))
)

#part iii
ddply(
  .data = data,
  .variables = c("pressuretype", "pregnancy", "diabetes"),
  .fun = c(glucosemean = function(z) mean(z$glucose),
           glucosemedian = function(z) median(z$glucose),
           glucosesd = function(z) sd(z$glucose),
           glucosemax = function(z) max(z$glucose),
           glucosemin = function(z) min(z$glucose),
           insulinmean = function(z) mean(z$insulin),
           insulinmedian = function(z) median(z$insulin),
           insulinsd = function(z) sd(z$insulin),
           insulinmax = function(z) max(z$insulin),
           insulinmin = function(z) min(z$insulin))
)

##Question 10

#part i
plot(glucose ~ insulin, data = data, type='p',pch=19,ylab="Glucose Level (mg/dL)", xlab="Insulin Level (mu U/ml)", col=c("red", "yellow")[data$diabetes], main="Glucose Level versus Insulin Level")
#adding legend
levels(data$diabetes)
legend(550,80, legend = c("Not diabetic", "Diabetic"), col = c("red", "yellow"), pch = c(16,16,16), cex = 0.75) #providing coordinates on a plot (1 on the x axis, 8 on the y axis)

#part ii
plot(insulin ~ pedigree, data = data, type='p',pch=19,ylab="Insulin Level (mu U/ml)", xlab="Pedigree Factor", col=c("red", "yellow")[data$diabetes], main="Insulin Level versus Pidegree Factor")
#adding legend
levels(data$diabetes)
legend(1.0,800, legend = c("Not diabetic", "Diabetic"), col = c("red", "yellow"), pch = c(16,16,16), cex = 0.75) #providing coordinates on a plot (1 on the x axis, 8 on the y axis)

#part iii
shapes = c(16, 17) 
shapes <- shapes[as.numeric(iris$Species)]

plot(mass ~ pressure, data = data, type='p', pch=shapes ,ylab="Body mass index (kg/m^2)", xlab="Blood Pressure (mm Hg) ", col=c("red", "black")[data$diabetes], main="Body mass index Vs Blood pressure")
#adding legend
levels(data$diabetes)
data$pregnancy
#Legend is incomplete
legend(10,70, legend = c("Not Diabetic and pregnant", "Not Diabetic and not pregnant","Diabetic and pregnant", "Diabetic and not pregnant"), col = c("red", "red","black", "black"), pch = c(16,17,16,17), cex = 0.75) 

