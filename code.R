# Project 1
# André Zazzera (alz9cb), Colleen Callahan (cc5dh), David Vann (dv6bq), Nikki Aaron (na5zn)

## ---- Data Analysis----------------------------------------------------------------
# Load data set
diamonds <- read.csv('project1_clean_diamond_data.csv', header=TRUE)

# Randomly shuffle rows
diamonds <- diamonds[sample(nrow(diamonds)),]

names(diamonds)
head(diamonds)
summary(diamonds)

# Bin colors
diamonds$color_cat [diamonds$color %in% c("D", "E", "F")] <- 'Colorless'
diamonds$color_cat [diamonds$color %in% c("G", "H", "I", "J")] <- 'Near Colorless'
diamonds$color_cat <- factor(diamonds$color_cat)
levels(diamonds$color_cat) <- c("Colorless", "Near Colorless")
contrasts(diamonds$color_cat)

# Bin clarity
diamonds$clarity_cat [diamonds$clarity %in% c("FL")] <- 'Flawless'
diamonds$clarity_cat [diamonds$clarity %in% c("IF")] <- 'Internally Flawless'
diamonds$clarity_cat [diamonds$clarity %in% c("VVS1", "VVS2")] <- 'Very Very Slightly Included'
diamonds$clarity_cat [diamonds$clarity %in% c("VS1", "VS2")] <- 'Very Slightly Included'
diamonds$clarity_cat [diamonds$clarity %in% c("SI1", "SI2")] <- 'Slightly Included'
diamonds$clarity_cat <- factor(diamonds$clarity_cat)
levels(diamonds$clarity_cat) <- c("Flawless", "Internally Flawless", "Very Very Slightly Included", "Very Slightly Included", "Slightly Included")


# Dummy code cut
diamonds$cut <- factor(diamonds$cut)
levels(diamonds$cut) <- c('Astor Ideal', 'Ideal', 'Very Good','Good')
relevel(diamonds$cut, ref = "Good")
contrasts(diamonds$cut)


## ---- Diamond Price Stats----------------------------------------------------------------
summary(diamonds$price)
sd(diamonds$price)


## ------Diamond Price Histogram----------------------------------------------------------------------------
par(mfrow=c(1,2))
hist(diamonds$price, xlab = "", main="Histogram of Price ($)")
h <- hist(log(diamonds$price), xaxt = "n", xlab = "", main="Histogram of Price ($), ln scale")
axis(1, at = log(c(250, 2500, 25000, 250000, 2000000)), labels = format(c(250, 2500, 25000, 250000, 2000000), digits=0, scientific=FALSE), las=2)


## ------Predictor Plots----------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(diamonds$price~diamonds$carat, main="Price vs. Carat", xlab="Weight (carat)", ylab="Price (USD)")
boxplot(diamonds$price~diamonds$color, main="Price vs. Color Boxplot", xlab="Color", ylab="Price (USD)", log = "y")
boxplot(diamonds$price~diamonds$clarity, main="Price vs. Clarity Boxplot", xlab="Clarity", ylab="Price (USD)", log = "y")
boxplot(diamonds$price~diamonds$cut, main="Price vs. Cut Boxplot", xlab="Cut", ylab="Price (USD)", log = "y")


## ---- Price Carat Correlation ----------------------------------------------------------------
round(cor(diamonds[,c(5,1)]),3)


## -----Scatterplot and Verify Assumptions-----------------------------------------------------------------------------
par(mfrow=c(2,2))

result.car <- lm(price~carat, data=diamonds)
#summary(result.car)
#anova(result.car)

# Scatterplot
plot(diamonds$carat,diamonds$price,xlab="Carat", ylab="Price", main="Plot of Price against Carat")
abline(result.car, col='red')

# residual plot of model
plot(result.car$fitted.values,result.car$residuals,main="Residual plot")
abline(h=0,col="red")

# ACF plot of residuals
acf(result.car$residuals)

# QQ plot of residuals
qqnorm(result.car$residuals)
qqline(result.car$residuals, col="red")


## ---Transformation 1-------------------------------------------------------------------------------
library(MASS)
boxcox(result.car, lambda = seq(0.3,0.4,0.01))

# Cube Root Price
diamonds$cube_root_price <- diamonds$price^(1/3) # Transform responses
result.transformed <- lm(cube_root_price~carat, data=diamonds) # Fit new linear regression
#summary(result.transformed) # Summarize results

# Check assumptions
par(mfrow=c(2,2))
plot(diamonds$carat, diamonds$cube_root_price, xlab='Carat', ylab='Cube Root of Price', main='Cube Root of Price vs Carat') # Scatterplot of transformed data and new model
abline(result.transformed) # Linear regression line
plot(result.transformed$fitted.values, result.transformed$residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot') # Residual plot of model
abline(h=0)
acf(result.transformed$residuals) # ACF plot of residuals
qqnorm(result.transformed$residuals) # QQ plot of residuals
qqline(result.transformed$residuals)


## -----Transformation 2-----------------------------------------------------------------------------
# Sqrt Carat
diamonds$sqrt_carat <- diamonds$carat^(1/2) # Transform predictor
result.transformed2 <- lm(cube_root_price~sqrt_carat, data=diamonds) # Fit new linear regression
#summary(result.transformed2)

# Check assumptions
par(mfrow=c(2,2))

# Scattarplot
plot(diamonds$sqrt_carat, diamonds$cube_root_price, xlab='Sqrt Carat', ylab='Cube Root of Price', main='Cube Root of Price vs Sqrt of Carat') # Scatterplot of transformed data and new model
abline(result.transformed2) # Linear regression line
plot(result.transformed2$fitted.values, result.transformed2$residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot') # Residual plot of model
abline(h=0)

# ACF plot of residuals
acf(result.transformed2$residuals)

# QQ plot of residuals
qqnorm(result.transformed2$residuals) 
qqline(result.transformed2$residuals)


## -----Fransformation 3 Log-Log-----------------------------------------------------------------------------
# log price
result.car2 <- lm(log(price)~log(carat), data=diamonds)
#summary(result.car2)

# Check assumptions
par(mfrow=c(2,2))
plot(log(diamonds$carat),log(diamonds$price),xlab="log(Carat)", ylab="log(Price)", main="Plot of Price against Carat")
abline(result.car2, col='red')
plot(result.car2$fitted.values,result.car2$residuals,main="Residual plot")
abline(h=0,col="red")
##ACF plot of residuals
acf(result.car2$residuals)
##QQ plot of residuals
qqnorm(result.car2$residuals)
qqline(result.car2$residuals, col="red")


## -----Categorical Predictor Plots-----------------------------------------------------------------------------

par(mfrow=c(2,2))

# ---------- Cut

lr.cut <- lm(log(price) ~ log(carat) + cut, data=diamonds)
summary(lr.cut)

# Divide cut classes into subsets of data
a1<-subset(diamonds,cut=="Astor Ideal") 
a2<-subset(diamonds,cut=="Ideal") 
a3<-subset(diamonds,cut=="Very Good")
a4<-subset(diamonds,cut=="Good")

# fit 4 separate regressions, one for each cut
reg1<-lm(log(price)~log(carat),data=a1)
reg2<-lm(log(price)~log(carat),data=a2)
reg3<-lm(log(price)~log(carat),data=a3)
reg4<-lm(log(price)~log(carat),data=a4)

# create a scatterplot with different colors and symbols for each cut
plot(log(diamonds$carat),log(diamonds$price), main="log(Price) against log(Carat), by Cut")
# points(log(a2$carat),log(a2$price), pch=2, col="red") 
# points(log(a3$carat),log(a3$price), pch=12, col="cyan")
# points(log(a4$carat),log(a4$price), pch=8, col="green")
abline(reg1,lty=1, col="orange")
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="cyan")
abline(reg4,lty=4, col="green")
legend("topleft", c("Astor Ideal", "Ideal", "Very Good", "Good"), 
       lty=c(1,2,3,4), 
       # pch=c(1,2,12,8), 
       col=c("orange","red","cyan","green")) 

# confint(lr.cut, level=1-0.05/5) # calculating 5 simultaneous intervals at 95% level

# --------- Clarity

lr.clarity <- lm(log(price)~log(carat)+clarity, data=diamonds)
summary(lr.clarity)

# Clarity Scale
# FL (flawless) > IF (internally flawless) > VVS (very very slightly included) > VS (very slightly included) > SI (slightly included)
# 1 better than 2 for subcategories (e.g., VVS1 > VVS2)

# Divide clarity classes into subsets of data
a1<-subset(diamonds,clarity=="FL") 
a2<-subset(diamonds,clarity=="IF") 
a3<-subset(diamonds,clarity=="VVS1")
a4<-subset(diamonds,clarity=="VVS2")
a5<-subset(diamonds,clarity=="VS1")
a6<-subset(diamonds,clarity=="VS2")
a7<-subset(diamonds,clarity=="SI1")
a8<-subset(diamonds,clarity=="SI2")

# fit 4 separate regressions, one for each cut
reg1<-lm(log(price)~log(carat),data=a1)
reg2<-lm(log(price)~log(carat),data=a2)
reg3<-lm(log(price)~log(carat),data=a3)
reg4<-lm(log(price)~log(carat),data=a4)
reg5<-lm(log(price)~log(carat),data=a5)
reg6<-lm(log(price)~log(carat),data=a6)
reg7<-lm(log(price)~log(carat),data=a7)
reg8<-lm(log(price)~log(carat),data=a8)

# create a scatterplot with different colors and symbols for each clarity class
plot(log(diamonds$carat),log(diamonds$price), main="log(Price) against log(Carat), by Clarity")


abline(reg1,lty=1, col="orange")
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="cyan")
abline(reg4,lty=4, col="green")
abline(reg5,lty=5, col="pink")
abline(reg6,lty=6, col="purple")
abline(reg7,lty=1, col="aquamarine")
abline(reg8,lty=2, col="coral")
legend("topleft", c("FL","IF","VVS1","VVS2","VS1","VS2","SI1","SSI2"), 
       lty=c(1,2,3,4,5, 6, 1, 2), 
       # pch=c(1,2,12,8,9,10,11), 
       col=c("orange","red","cyan","green", "pink", "purple", "aquamarine", "coral")) 

# ------------ Color

lr.color <- lm(log(price)~log(carat) + color, data=diamonds)
summary(lr.color)

# Color Scale
# D (colorless) is best, gets "worse" as you go down the alphabet
# DEF -> "colorless"
# GHIJ -> "near colorless"

# Divide color classes into subsets of data
a1<-subset(diamonds,color=="D") 
a2<-subset(diamonds,color=="E") 
a3<-subset(diamonds,color=="F")
a4<-subset(diamonds,color=="G")
a5<-subset(diamonds,color=="H")
a6<-subset(diamonds,color=="I")
a7<-subset(diamonds,color=="J")

# fit 4 separate regressions, one for each cut
reg1<-lm(log(price)~log(carat),data=a1)
reg2<-lm(log(price)~log(carat),data=a2)
reg3<-lm(log(price)~log(carat),data=a3)
reg4<-lm(log(price)~log(carat),data=a4)
reg5<-lm(log(price)~log(carat),data=a5)
reg6<-lm(log(price)~log(carat),data=a6)
reg7<-lm(log(price)~log(carat),data=a7)

# create a scatterplot with different colors and symbols for each cut
plot(log(diamonds$carat),log(diamonds$price), main="log(Price) against log(Carat), by Color")

# decided that having all the different colors was too visually distracting, just plotting different color lines instead
# points(log(a2$carat),log(a2$price), pch=2, col="red") 
# points(log(a3$carat),log(a3$price), pch=12, col="blue")
# points(log(a4$carat),log(a4$price), pch=8, col="green")
# points(log(a5$carat),log(a5$price), pch=9, col="pink")
# points(log(a6$carat),log(a6$price), pch=10, col="purple")
# points(log(a7$carat),log(a7$price), pch=11, col="aquamarine")

abline(reg1,lty=1, col="orange")
abline(reg2,lty=2, col="red") 
abline(reg3,lty=3, col="cyan")
abline(reg4,lty=4, col="green")
abline(reg5,lty=5, col="pink")
abline(reg6,lty=6, col="purple")
abline(reg7,lty=1, col="aquamarine")
legend("topleft", c("D","E","F", "G", "H", "I", "J"), 
       lty=c(1,2,3,4,5, 6, 1), 
       # pch=c(1,2,12,8,9,10,11), 
       col=c("orange","red","cyan","green", "pink", "purple", "aquamarine")) 


## ----Summary of Categorical Predictor Plots-----------------------------------------------------------------------
df.cut <- data.frame(
        Price.Reduction.Factor.Cut=c("Reference", 0.91, 0.77, 0.71),
        row.names=c("Astor Ideal", "Ideal", "Very Good", "Good")
)

df.clarity <- data.frame(
        Price.Reduction.Factor.Clarity=c("Reference", 0.66, 0.59, 0.55, 0.53, 0.50, 0.45, 0.38),
        row.names=c("FL", "IF", "VVS1", "VVS2", "VS1", "VS2", "SI1", "SI2")
)

df.color <- data.frame(
        Price.Reduction.Factor.Color=c("Reference", 0.91, 0.88, 0.84, 0.79, 0.71, 0.63),
        row.names=c("D", "E", "F", "G", "H", "I", "J")
)

df.cut
df.clarity
df.color


## ------Full MLR Model----------------------------------------------------------------------------
result_full<-lm(log(price)~log(carat)+clarity_cat+cut+color_cat, data=diamonds)
summary(result_full)


## ------Check for Multicollinearity----------------------------------------------------------------------------
library(car)
vif(result_full)


## ------Verify Normality Assumptions----------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(result_full$fitted.values, result_full$residuals, xlab='Fitted Values', ylab='Residuals', main='Residual Plot')
abline(h=0)
acf(result_full$residuals)
qqnorm(result_full$residuals)
qqline(result_full$residuals)


## ------Reduce Model----------------------------------------------------------------------------
# Remove color
reduced<-lm(log(price)~log(carat) + clarity_cat + cut, data=diamonds)
anova(reduced,result_full)

# Remove cut
reduced<-lm(log(price)~log(carat) + clarity_cat, data=diamonds)
anova(reduced,result_full)

# Remove clarity
reduced<-lm(log(price)~log(carat), data=diamonds)
anova(reduced,result_full)


## -------Pairwise Interaction Models---------------------------------------------------------------------------
par(mfrow=c(2,2))

result_carat_cut <- lm(log(price)~log(carat)*cut, data=diamonds)
#summary(result_carat_cut)
plot(result_carat_cut$residuals~result_carat_cut$fitted.values,
     main = 'Plot of residuals against fits')
abline(h=0, col='red')


result_carat_col <- lm(log(price)~log(carat)*color_cat, data=diamonds)
#summary(result_carat_col)
plot(result_carat_col$residuals~result_carat_col$fitted.values,
     main = 'Plot of residuals against fits')
abline(h=0, col='red')


result_carat_clar <- lm(log(price)~log(carat)*clarity_cat, data=diamonds)
#summary(result_carat_clar)
plot(result_carat_clar$residuals~result_carat_clar$fitted.values,
     main = 'Plot of residuals against fits')
abline(h=0, col='red')


## ---- Total Interaction Model----------------------------------------------------------------

result_tot <- lm(log(price)~log(carat)*cut*color_cat*clarity_cat, data=diamonds)
summary(result_tot)
anova(result_full, result_tot)


## -----Verify Normality Assumptions---------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(result_tot$residuals~result_tot$fitted.values,
     main = 'Plot of residuals against fits')
abline(h=0, col='red')
acf(result_tot$residuals, main="ACF of Residuals")
qqnorm(result_tot$residuals)
qqline(result_tot$residuals, col="red")


## ------Levene Test----------------------------------------------------------------------------
leveneTest(diamonds$price, diamonds$cut)  
leveneTest(diamonds$price, diamonds$color_cat)  
leveneTest(diamonds$price, diamonds$clarity_cat)  


## ---- Confidence and Prediction Intervals for Each Model----------------------------------------------------------------
# SLR
newdata<-data.frame(carat=1.1)
int.c <- predict.lm(result.car2,newdata,level=0.95, interval="confidence")
int.p<- predict.lm(result.car2,newdata,level=0.95, interval="prediction")
exp(int.c)
exp(int.p)

newdata<-data.frame(carat=9.09)
int.c <- predict.lm(result.car2,newdata,level=0.95, interval="confidence")
int.p <- predict.lm(result.car2,newdata,level=0.95, interval="prediction")
exp(int.c)
exp(int.p)

# Full MLR
newdata<-data.frame(carat=1.1, clarity_cat='Very Slightly Included', color_cat='Near Colorless', cut='Very Good')
int.c <- predict.lm(result_full,newdata,level=0.95, interval="confidence")
int.p<- predict.lm(result_full,newdata,level=0.95, interval="prediction")
exp(int.c)
exp(int.p)

newdata<-data.frame(carat=9.09, clarity_cat='Very Slightly Included', color_cat='Near Colorless', cut='Ideal')
int.c <- predict.lm(result_full,newdata,level=0.95, interval="confidence")
int.p <- predict.lm(result_full,newdata,level=0.95, interval="prediction")
exp(int.c)

# Total Interaction MRL
newdata<-data.frame(carat=1.1, clarity_cat='Very Slightly Included', color_cat='Near Colorless', cut='Very Good')
int.c <- predict.lm(result_tot,newdata,level=0.95, interval="confidence")
int.p<- predict.lm(result_tot,newdata,level=0.95, interval="prediction")
exp(int.c)
exp(int.p)

newdata<-data.frame(carat=9.09, clarity_cat='Very Slightly Included', color_cat='Near Colorless', cut='Ideal')
int.c <- predict.lm(result_tot,newdata,level=0.95, interval="confidence")
int.p <- predict.lm(result_tot,newdata,level=0.95, interval="prediction")
exp(int.c)
exp(int.p)

