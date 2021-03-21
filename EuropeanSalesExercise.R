#################################################################
## BDA5002 - Marketing Analytics Course                        ##
## Lecture-2 Exercise in Market Response Models				         ##
## EuropeanSales DataSet Exercise                              ##
##                                                             ## 
#################################################################

# free memory if need be
#rm(list = ls())
#gc()

# Get the working directory. If needed, you can set the working directory to another folder.
getwd()
#setwd("C:/")


# Read the Data files from a directory  
EuropeanSalesDataFrame <-read.csv("C:/Users/Bora/Desktop/EuropeanSales.csv", header=T)

EuropeanSalesDataFrame

#Showing the attributes  
attributes(EuropeanSalesDataFrame)

# Drawing the Yearly sales
plot(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$ComputerSales, ylab="GDPperHead", xlab="SalesPerCapita")
plot(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$SalesPerCapita, ylab="GDPperHead", xlab="ComputerSales")
plot(EuropeanSalesDataFrame) 

#Correlation between all the attributes
cor(EuropeanSalesDataFrame$Population, EuropeanSalesDataFrame$SalesPerCapita)
cor(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$SalesPerCapita) 
cor(EuropeanSalesDataFrame$UnemploymentRate, EuropeanSalesDataFrame$SalesPerCapita)
cor(EuropeanSalesDataFrame$EducationSpending, EuropeanSalesDataFrame$SalesPerCapita)

#GDPperHead is the highest one. Education Spending that is the closest one as a second highest one for the corr. 
#And, also I can see in the graphical representation.(SalesPerCapita)

cor(EuropeanSalesDataFrame$Population, EuropeanSalesDataFrame$ComputerSales)
cor(EuropeanSalesDataFrame$GDPperHead, EuropeanSalesDataFrame$ComputerSales) 
cor(EuropeanSalesDataFrame$UnemploymentRate, EuropeanSalesDataFrame$ComputerSales)
cor(EuropeanSalesDataFrame$EducationSpending, EuropeanSalesDataFrame$ComputerSales)

#Population is the highest one. UnemploymentRate that is the closest one as a second highest one for the corr.(ComputerSales)
#And, also I can see in the graphical representation.(ComputerSales)

#Correlation table between the all
cor(EuropeanSalesDataFrame[,])

# Fitting the Data into a model
mymodel <- lm(SalesPerCapita ~ GDPperHead + EducationSpending, data=EuropeanSalesDataFrame)
summary(mymodel)

#######################################################################
## It is actually a normal model, not very good or not very bad      ##
## but it is in the middle level. R-squared and 			               ##
## Adjusted R-squared seems good, they are closed                    ##
## p-values seems bad except for GDPperHead attribute, and also      ##
## t-values seems low, and also std.errors and residual err.         ##
## seems low. Normally, it is a good model for the best performing.  ##
#######################################################################


#The Model attributes & coefficients  
attributes(mymodel)
mymodel$coefficients

#Find the SalesPerCapita for GDPperHead 50 and EducationSpending 8 
SalespcapitaGDP50ED8 <- mymodel$coefficients[[1]]+mymodel$coefficients[[2]]*50+mymodel$coefficients[[3]]*8


#SalespcapitaGDP50ED8 = 200.4201
SalespcapitaGDP50ED8

#More simpler Model without EducationSpending
mymodel2 <- lm(SalesPerCapita ~ GDPperHead, data=EuropeanSalesDataFrame)
summary(mymodel2)


#######################################################################
## It is actually a normal model, not good or not bad                ##
## but it is something like in middle level. R-squared and 		       ##
## Adj. R-squared seems normal, they are closed it is very important ##
## p-value seem good, very good,std. error and residual err. seems   ##
## good, not bad, t-values low,and it has 2-stars. And, all in all,  ##
## it is actually a mid-level model when evaluating model quality.   ##
#######################################################################


# The differences between observed values and fitted values
residuals(mymodel2) 


# Show the plot of the model
#layout(matrix(1)) # one graph per page 
plot(mymodel2)

# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(mymodel2)

# Comparing the Different Modelings for SalesPerCapita
modela <- lm(SalesPerCapita ~ GDPperHead + EducationSpending, data=EuropeanSalesDataFrame)
summary(modela)


modelb <- lm(SalesPerCapita ~ GDPperHead , data=EuropeanSalesDataFrame)
summary(modelb)

# Fitting the Data into a model
model1 <- lm(ComputerSales ~ Population + UnemploymentRate, data=EuropeanSalesDataFrame)
summary(model1)

#################################################################################
## It is actually a good model, not very good or not bad                       ##
## something like good model in model quality when evaluated.                  ##
## Adj. R-squared & R-squared seems good, they are closed it is very important ##
## p-value seem good for Population except for Unemployment Rate.              ##
## t-values low and errors seems high and general p-value seems very good.     ##
## All in all, it is good, not very good but seems good model when evaluated.  ##
#################################################################################


#The Model attributes and coefficients  
attributes(model1)
model1$coefficients

#Find the ComputerSales for Population 50 and UnemploymentRate 10 
ComputerSalesPop50UR10 <- model1$coefficients[[1]] + model1$coefficients[[2]]*50 + model1$coefficients[[3]]*10

#ComputerSalesPop50UR10 = 4213.406
ComputerSalesPop50UR10

#More simpler Model without UnemploymentRate
model2 <- lm(ComputerSales ~ Population, data=EuropeanSalesDataFrame)
summary(model2)

#################################################################################
## It is actually a good model, not very good or not bad                       ##
## something like good model in model quality when evaluated.                  ##
## Adj. R-squared & R-squared seems good, they are closed it is very important ##
## p-value seems very good (0.05<), very below for the critical value          ##
## Then, I can clearly and strongly state that is statistically significant.   ##
## All in all, general p-value seems also good, then it is a good model        ##
## when evaluating the general model quality, (it has 3-star also.)            ##
#################################################################################

# The differences between observed values and fitted values
residuals(model2) 

# Showing the Plot of the Modeling Section
#layout(matrix(1)) # one graph per page (exactly 4)
plot(model2)

# Change layout to show 4 graphs per page 
layout(matrix(c(1,3,2,4),2,2)) 
plot(model2)


#Find the ComputerSales for Population 40  
ComputerSalesPop40 <- model2$coefficients[[1]]+model2$coefficients[[2]]*40


#ComputerSalesPop40= 3519.062
ComputerSalesPop40


# Comparing the Different Modelings for ComputerSales
modelc <- lm(ComputerSales ~ Population + UnemploymentRate, data=EuropeanSalesDataFrame)
summary(modelc)


modeld <- lm(ComputerSales ~ Population , data=EuropeanSalesDataFrame)
summary(modeld)


