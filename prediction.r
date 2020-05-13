#importing dataset
dataset=read.csv('Predicting_salaries.csv')  

#splitting data into trainin set
library(caTools)
set.seed(123)
split = sample.split(dataset$AnnualSalary,SplitRatio=3/4) #this means 75% of dataset is used for training and 25% for testing 
training_set = subset(dataset,split==TRUE)                # is used for training
testing_set = subset(dataset,split==FALSE)                # is used for testing

#Fitting linear regression
linearRegressor= lm(formula = AnnualSalary ~ YearsOfExperience,
                    data=training_set)
#*** means that data has high impact/significance

#predict test results
Y_pred = predict(linearRegressor,newdata = testing_set)

#visualizing training_set
library(ggplot2)
library(scales)

ggplot()+
  geom_point(aes(x=training_set$YearsOfExperience,y=training_set$AnnualSalary),
       colour='red')+
  geom_line(aes(x=training_set$YearsOfExperience,
                y=predict(linearRegressor,newdata=training_set)),colour='Navy')+
  ggtitle("Annual Salary vs Experience")+
  xlab('Years of Experience')+
  ylab('Annual Salary')+
  scale_x_continuous(limits = c(0,12))+   
  scale_y_continuous(limits = c(0,150000))

#visualizing test_set
ggplot()+
  geom_point(aes(x=testing_set$YearsOfExperience,y=testing_set$AnnualSalary),
             colour='red')+
  geom_line(aes(x=training_set$YearsOfExperience,
                y=predict(linearRegressor,newdata=training_set)),colour='Navy')+
  ggtitle("Annual Salary vs Experience")+
  xlab('Years of Experience')+
  ylab('Annual Salary')+
  scale_x_continuous(limits = c(0,12))+   
  scale_y_continuous(limits = c(0,150000))