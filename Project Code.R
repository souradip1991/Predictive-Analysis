library(ggplot2)
library(dplyr)
library(tidyr)
library(GGally)
# Importing the KC House dataset 
house_train=read.csv(file.choose(), header = TRUE)
house_train$price1=house_train$price/1000
# Extracting the City from Address
final1<-separate(data = house_train, col = House.Address, into = c("Street", "City","State","Country"), sep = "\\,")
View(final1)
# Exploratory Data Analysis
# 1. Summary of House Price
summary(final1$price)
# Plotting of price distribution
par(mfrow=c(1,2))
boxplot(final1$price1, ylab="Price", main="Boxplot of Price distribution") 
hist(final1$price1,freq = F, xlab="Price", main="Histogram-Density plot of Price") 
lines(density(final1$price1), lwd=2, col="blue")
# Interpretation of Sales Variation wrt Categorical Variables
# converting variables to categorical variables
columns<-c("bedrooms","bathrooms","floors","waterfront","view","condition","grade","City") 
final1[,columns]<-lapply(final1[,columns], as.factor)

View(final1)
print("# of Bedrooms:")
summary(final1$bedrooms)
print("# of Bathrooms:")
summary(final1$bathrooms)
print("# of Floors:")
summary(final1$bathrooms)
print("Waterfront:")
summary(final1$waterfront)
print("View:")
summary(final1$view)
print("Condition:")
summary(final1$condition)
print("Grade:")
summary(final1$grade)
print("City:")
summary(final1$City)
# Variation of price wrt categorical variables
by(final1$price,final1$bedrooms,summary)
by(final1$price,final1$bathrooms,summary)
by(final1$price,final1$floors,summary)
by(final1$price,final1$waterfront,summary)
by(final1$price,final1$view,summary)
by(final1$price,final1$condition,summary)
by(final1$price,final1$grade,summary)
by(final1$price,final1$City,summary)
# barplot of categorical variables 
par(mar=c(04,04,04,4)) # increase y-axis margin
count1 <- table(final1$bedrooms)
barplot1<-barplot(count1, main="Number of Bedrooms",
                  xlab="Number of Bedrooms", col=c("darkblue","red"),
                  legend=rownames(count1) ,beside = TRUE,width = c(10,5))
#boxplot of the price wrt categorical variables
#boxplot(final1$price1~final1$bedrooms, xlab="Number of Beds",ylab="Price",
#       main="Boxplot of Price distribution w.r.t # of beds")
p1 <- ggplot(final1, aes(x=final1$bedrooms, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p1
p2 <- ggplot(final1, aes(x=final1$bathrooms, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p2
p3 <- ggplot(final1, aes(x=final1$floors, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p4 <- ggplot(final1, aes(x=final1$waterfront, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p5 <- ggplot(final1, aes(x=final1$view, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p6 <- ggplot(final1, aes(x=final1$condition, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p7 <- ggplot(final1, aes(x=final1$grade, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
p9 <- ggplot(final1, aes(x=final1$City, y=final1$price1)) + 
  geom_boxplot(varwidth = T)
#price variation wrt numeric variables
summary(final1$sqft_living)
summary(final1$sqft_lot)
summary(final1$yr_built)
#pairs plot of each numeric variable 
pair_plot<-ggpairs(final1[,c(3,6,7)], 
                   columnLabels=c("price","sqft_living","sqft_lot")) 
pair_plot
# Correlation coefficient for numeric predictor variables with price
cor.test(final1$price,final1$sqft_living,method = "pearson")
cor.test(final1$price,final1$sqft_lot,method = "pearson")
cor.test(final1$price,final1$sqft_above,method = "pearson")
cor.test(final1$price,final1$sqft_basement,method = "pearson")
cor.test(final1$price,final1$sqft_living15,method ="pearson")
cor.test(final1$price,final1$sqft_lot15,method = "pearson")
cor.test(final1$price,final1$yr_built,method = "pearson")
cor.test(final1$sqft_living,final1$sqft_lot,method="pearson")
cor.test(final1$sqft_living,final1$yr_built,method="pearson")
cor.test(final1$yr_built,final1$sqft_lot,method="pearson")
#Regression Model
linear_model<-lm(price ~ bedrooms,data=final1)
summary(linear_model)
linear_model1<- lm(price ~ bathrooms, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ sqft_living, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ sqft_lot, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ floors, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ view, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ condition, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ grade, data = final1)
summary(linear_model1)
linear_model1<- lm(price ~ City, data = final1)
summary(linear_model1)
linear_model1<- lm(price1 ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+grade+condition+City+yr_built, data = final1)
summary(linear_model1)
linear_model1$coefficients[2]
library(car)
Anova(linear_model1)
library(olsrr)
abc<- ols_step_all_possible(linear_model1)
View(abc)
fitnull<- lm(price1~1,data=final1)
fitfull<- lm(price1 ~ bedrooms+bathrooms+sqft_living+sqft_lot+floors+waterfront+view+grade+condition+City, data = final1)
step(fitfull,scope = list(lower=fitnull,upper=fitfull),direction = "backward")
house_test=read.csv(file.choose(), header = TRUE)
house_test$price1=house_test$price/1000
View(house_test)
final2<-separate(data = house_test, col = House.Address, into = c("Street", "City","State","Country"), sep = "\\,")
columns<-c("bedrooms","bathrooms","floors","waterfront","view","condition","grade","yr_built","City") 
final2[,columns]<-lapply(final2[,columns], as.factor)
View(final2)
pred<-predict(linear_model1,final2)
View(pred)
actuals_preds <- data.frame(cbind(actuals=final2$price1, predicteds=pred))  # make actuals_predicteds dataframe.
correlation_accuracy <- cor(actuals_preds)  
View(actuals_preds)
View(correlation_accuracy)
prediction_interval=predict(linear_model1,level=0.95,interval = "predict")
View(prediction_interval)
plot(linear_model1)

confidence_interval=predict(linear_model1,level=0.95,interval = "confidence") 
prediction_interval=predict(linear_model1,level=0.95,interval = "predict")
#Plotting the expected value and actual value 
plotting<-ggplot(final1,aes(x=fitted(linear_model1),y=final1$price1))+geom_point()+ 
  labs(title="Fitted line, CI and PI bands for Linear model", 
       x="Actual Price",y="Expected Price")+ 
  geom_line(aes(y=confidence_interval[,"fit"]), col="green")+ 
  stat_smooth(aes(y=prediction_interval[,"upr"]),method=lm,se=F,col="blue")+ 
  stat_smooth(aes(y=prediction_interval[,"lwr"]),method=lm,se=F,col="blue")+ 
  stat_smooth(aes(y=confidence_interval[,"upr"]),method=lm,se=F,col="red")+ 
  stat_smooth(aes(y=confidence_interval[,"lwr"]),method=lm,se=F,col="red")
plotting
avPlots(linear_model1)
dwt(linear_model1)
library(corrplot)
M<-cor(final1[,c(6:7,15)]) 
corrplot.mixed(M)
vif(linear_model1)
plot(fitted(linear_model1),rstudent(linear_model1))
hist(rstudent(linear_model1),xlab="Studentized Residuals", 
     ylab="Frequency", main="Histogram of Studentized Residuals")
qqnorm(rstudent(linear_model1), main="QQ plot of studentized residuals") 
qqline(rstudent(linear_model1),col=2)
cooks.distance(linear_model1)
with(final1,plot(bathrooms,cooks.distance(linear_model1)))
leverage_points<-as.numeric(which(hatvalues(linear_model1)> (2*9)/length(final1$price1))) 
leverage_points
leveragePlots(linear_model1)
influencePlot(linear_model1, main="Influence Plot", 
              sub="Circle size if proportional to Cook's Distance")
outlierTest(linear_model1)
ols_plot_resid_lev(linear_model1)
min_max_accuracy <- mean(apply(actuals_preds, 1, min) / apply(actuals_preds, 1, max))
mape <- mean(abs((actuals_preds$predicteds - actuals_preds$actuals))/actuals_preds$actuals)
