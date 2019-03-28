
#==================================================================
## Title: Impact of salesperson training on performance.
#==================================================================

#==================================================================
## SET UP R MARKDOWN
#==================================================================

#Clearing the working space
rm(list = ls())

#Setting the working directory
setwd("/Users/SP/Desktop/Econometrics/Case Study2")


#Install packages
install.packages("stargazer")
install.packages("ggplot2")
install.packages("gdata")
install.packages("ggeffects")
install.packages("QuantPsyc")
install.packages("VIF")
install.packages("multiwayvcov")
install.packages("lmtest")
install.packages("AER")

#Load libraries
library(stargazer)
library(gdata)
library(ggplot2)
library(psych) 
library(ggeffects)
library(QuantPsyc)
library(VIF)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(AER)


# turning off scientific notation except for big numbers
options(scipen = 9)

#================================================================
## LOAD AND EXPLORE DATA
#================================================================
``{r}
mydata = read.csv("Salesperson_training.csv")

# Summary statistics
stargazer(mydata, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Plotting annual_sales with self_training_score to see a correlation
ggplot(mydata, aes(y=annual_sales, x=self_training_score)) + geom_point(size = 2.5, colour = "black") + xlab("Self Training Score") + ylab("Annual Sales($K)")

#Since annual sales is a dollar value we take the log and see if the relationship is linear
ggplot(mydata, aes(y=log(annual_sales), x=self_training_score)) + geom_point(size = 2.5, colour = "black") + stat_smooth(method = lm, colour = 'red') + xlab("Self Training Score") + ylab("Log(Annual Sales)")

#We also plot the distribution of annual sales to check
ggplot(mydata, aes(x=annual_sales)) + geom_histogram(colour="green")

#Using log(annual_sales) the histogram is more normal. So we will use log(annual_sales) for our model
ggplot(mydata, aes(x=log(annual_sales))) + geom_histogram(colour="green") 



#==========================================================
## BUILD UP MODEL
#==========================================================

#Creating a new variable log_annual_sales
mydata$log_annual_sales <- log(mydata$annual_sales)

## CHECKING FOR MULTICOLLINEARITY
#Creating a new dataframe for storing all variables except the annual_sales
df<- mydata[c("male", "married","child","self_training_score","age","school_years","experience_years","service_years","year_2010","year_2011","year_2012","year_2013","year_2014","year_2015")]

cor(df) # Generates the correlation matrix
vif(df)# Calculates VIF scores
vifstep(df, th=10000)


#Creating the first Model
model1<- lm(log_annual_sales~self_training_score+male+married+age+school_years+experience_years+service_years+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015, data=mydata)
stargazer(model1,  
          title="Regression Results", type="text", 
          column.labels=c("Model-1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

#Adding variable child and rerunning the model

## Test heteroscedasticity
pred<-predict(model1) #obtain fitted values
residual=resid(model1) # obtain residuals

#Let's check the heteroscadasticity from the fitted values and residuals
df1 <- data.frame(pred,residual)
ggplot(df1, aes(y=residual, x=pred)) + geom_point(size=2.5)


gqtest(model1) # Goldfeld-Quandt test indicated heteroscadasticity
bptest(model1) # Breusch-Pagan test 

is.factor(mydata$year)
as.factor(mydata$year)

#Checking the data for clusters.The data is clustered on the variable year
ggplot(mydata, aes(x=factor(year), y=log_annual_sales, fill=factor(year))) + geom_boxplot() + xlab("Year") + ylab("Annual Sales")
#Since the data is clustered using Clustered Robust Standard Errors
clusrobstder <- sqrt(diag(cluster.vcov(model1, mydata$year))) # produces clustered robust standard errors
stargazer(model1,  
          se=list(clusrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Clustered SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 


#==========================================================
## USING 2SLS TO DEAL WITH ENDOGENOUS VARIABLE
#==========================================================
##Since we theoretically know that endogeneity exists in the model, we use 2SLS estimator. We use mother_education and score_other_test as the IVs.
model2<- ivreg(log_annual_sales~self_training_score+male+child+age+school_years+experience_years+service_years+married+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015 | score_other_test+mother_education+male+child+age+married+school_years+experience_years+service_years+year_2010+year_2011+year_2012+year_2013+year_2014+year_2015, data=mydata)  # gives 2SLS estimator
#Regressing Results
stargazer(model2, 
          se=list(clusrobstder),
          title="Regression Results", type="text", 
          column.labels=c("Model-2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 
#Performing Wald Test for checking the model fit and checking model diagnostics
summary(model2,diagnostics = TRUE)
#The Model-2 generates significant values according to the Weak Instuments statistics. It signifies that with every 1 point increase in the self_training_score the annual_sales increase by 1.13%.
#Since the model is overidentified, we check the Sargan statistic which is not significant, that implies that both the instruments used in the model are good instruments.
#The Wu-Hausman statistic is significant, that indicates that we should use 2SLS model instead of OLS. 

#The results show that, every unit increase in the self_training_score increases the annual sales of a salesperson by 1.13%.

#================================================================
## CHANGE IN SALES DUE TO THE IMPACT
#================================================================

#To quantify the impact of training on the people who did not take the training we first calculated predicted sales using the final model
predicted_sales <- predict(model2)
self_training_score = mydata$self_training_score

#The only independent variable we know about the untrained employees is the self_training_score so we regress the predicted sales on only self training score using Simple OLS
model3 <- lm(predicted_sales~self_training_score)
stargazer(model3,  
          title="Regression Results", type="text", 
          column.labels=c("Model-5"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

#Visualization of predicted sales wrt self training score
df2 <- data.frame(predicted_sales,self_training_score)

ggplot(df2, aes(y=predicted_sales, x=self_training_score)) +geom_point(size = 2.5) + stat_smooth(method = lm, colour = "red") + xlab("Self Training Score") + ylab(" Predicted Annual Sales($K)") #As we expected they show a linear relationship
#Using the Model-6 we calculate the predicted sales corresponding to average test scores of trained and untrained salesperson
predict(model3, data.frame(self_training_score = c(101, mean(mydata$self_training_score))))
exp(5.686739)-exp(5.636512) # This is the impact of completing training on annual sales. On average, we can conclude that training increases annual sales per salesperson by about $14.4K.
 #================================================================
## END
#================================================================

