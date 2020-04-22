setwd("E:/8th semester/Lie Detector/Lab 1/Assignment")

##Question 1
#Reading gapminder.csv into gapminder variable seperated by commas
gapminder<-read.table(file = "data/gapminder.csv", header = TRUE, sep = ",")

#prints the names of variables in gapminder
names(gapminder)

#prints the number of variables in gapminder
length(names(gapminder))

# changing the class of variables into factors
b<-sapply(gapminder, class)
b<-as.factor(b) 
class(b)
#different levels means different classes 
levels(b)

##Question 2
#different countries
countries<-gapminder$country
countries<-as.factor(countries) 
class(countries)
#different levels means different classes 
levels(countries)
#number of countries
length(levels(countries))

#different continents
b<-gapminder$continent
b<-as.factor(b) 
class(b)
#different levels means different classes 
levels(b)
#number of continents
length(levels(b))

#Antartica not included due to no population or few population 

##Question 3
#minimum value of year column determines oldest time
min(gapminder$year)

##Question 4
#prints the information regarding maximum population
gapminder[which.max(gapminder$pop),]

#prints the country with maximum population
val <- gapminder[which.max(gapminder$pop),]
val$country

#prints the information regarding maximum population
gapminder[which.min(gapminder$pop),]

#prints the country with maximum population
val <- gapminder[which.min(gapminder$pop),]
val$country

##Question 5
#this funtion aggregates all the rows of each country and takes the mean of life expentencies for each country 
meancountry <- aggregate(gapminder$lifeExp , by=list(gapminder$country), FUN=mean)
meancountry

#this funtion aggregates all the rows of each country and takes the median of life expentencies for each country 
aggregate(gapminder$lifeExp , by=list(gapminder$country), FUN=median)

#this funtion aggregates all the rows of each continents and takes the mean of life expentencies for each country 
meancontinent <- aggregate(gapminder$lifeExp , by=list(gapminder$continent ), FUN=mean)
meancontinent

#this funtion aggregates all the rows of each continents and takes the median of life expentencies for each country 
aggregate(gapminder$lifeExp , by=list(gapminder$continent ), FUN=median)

#country with greatest mean life expectancy
meancountry[which.max(meancountry$x),]

#continent with greatest mean life expectancy
meancontinent[which.max(meancontinent$x),]

#country with least mean life expectancy
meancountry[which.min(meancountry$x),]

#continent with least mean life expectancy
meancontinent[which.min(meancontinent$x),]

#this function calculates variation of each country
variation <- aggregate(gapminder$lifeExp , by=list(gapminder$country), FUN=sd)
variation

#country with minimum variation
variation[which.min(variation$x),]

#country with maximum variation
variation[which.max(variation$x),]

##Question 6
#this funtion aggregates all the rows of each country and takes the mean of gdp per capita for each country 
meancountry <- aggregate(gapminder$gdpPercap , by=list(gapminder$country), FUN=mean)
meancountry

#this funtion aggregates all the rows of each country and takes the median of gdp per capita for each country 
aggregate(gapminder$gdpPercap , by=list(gapminder$country), FUN=median)

#this funtion aggregates all the rows of each continents and takes the mean of gdp per capita for each country 
meancontinent <- aggregate(gapminder$gdpPercap , by=list(gapminder$continent ), FUN=mean)
meancontinent

#this funtion aggregates all the rows of each continents and takes the median of gdp per capita for each country 
aggregate(gapminder$gdpPercap , by=list(gapminder$continent ), FUN=median)

#country with greatest mean gdp per capita
meancountry[which.max(meancountry$x),]

#continent with greatest mean gdp per capita
meancontinent[which.max(meancontinent$x),]

#country with least mean gdp per capita
meancountry[which.min(meancountry$x),]

#continent with least mean gdp per capita
meancontinent[which.min(meancontinent$x),]

#this function calculates variation of gdp per capita for each country
variation <- aggregate(gapminder$gdpPercap , by=list(gapminder$country), FUN=sd)
variation

#country with minimum variation
variation[which.min(variation$x),]

#country with maximum variation
variation[which.max(variation$x),]

#scatterplot
plot(lifeExp ~ gdpPercap, data = gapminder, type='p',pch=19,ylab="Life Expectancy", xlab="Gdp per Capita", col=c("red", "blue", "green", "black", "yellow")[gapminder$continent], main="Sepal Width versus Petal Width")
#adding legend
legend(8e+04,80, legend = c("Africa", "Americas", "Asia", "Europe", "Oceania"), col = c("red", "blue", "green", "black", "yellow"), pch = c(16,16,16), cex = 0.75) #providing coordinates on a plot (1 on the x axis, 8 on the y axis)

