d2<-read_xls("C://DATA(E)//ManipalChapter//Sem_2//Linear_Regression//LungCap.xls")
names(d2)

shapiro.test(d2$LungCap) # not normal
shapiro.test(d2$Age) #not normal
shapiro.test(d2$Height) #not normal

d2$Smoke<-as.factor(d2$Smoke)
d2$Gender<-as.factor(d2$Gender)
d2$Caesarean<-as.factor(d2$Caesarean)

m1<-lm(LungCap~Age+Height+Smoke+Gender+Caesarean,data=d2)
summary(m1)

# interaction checking (Smoke, Gender, Caesarean)

library(ggplot2)
ggplot(data=d2,aes(x=Height,y=LungCap,col=Caesarean))+
  geom_point()+
  geom_smooth(method = "lm",se=F)+
  theme_minimal()


anova(m_base, m_interaction)

m_base<-lm(LungCap~Age+Height+Smoke+Gender,data=d2)
m_interaction<-lm(LungCap~Caesarean*Height+Age+Height+Smoke+Gender,data=d2)
anova(m_base,m_interaction)

# how do I say I checked the interaction of Age*Smoke, Age*Gender, Height*Smoke, Height*Gender
# on LungCap and no significant interaction was found.


# no significant interaction found

m2<-lm(LungCap~Age+Height+Smoke+Gender,data=d2)
summary(m1)

#Backward elimination
backward = step(m2,direction = "backward")
formula(backward) #LungCap ~ Age + Height + Smoke + Gender + Caesarean

#stepwise selection
step_model = step(m2, direction = "both")
formula(step_model) #LungCap ~ Age + Height + Smoke + Gender + Caesarean

#Linearity
pairs(LungCap~Age+Height+Smoke+Gender+Caesarean,data=d2,
      lower.panel=function(x,y) text(mean(x),mean(y),round(cor(x,y),2)),
      upper.panel=function(x,y){points(x,y); abline(lm(y~x),col="red")}
)

plot(d2$Age,d2$LungCap,xlab="Age",ylab="Lung Capacity",main="Age vs Lung Capacity")
abline(lm(LungCap~Age, data=d1),col="red")
#cor(age,Lungcap) = 0.82

plot(d1$Height,d1$LungCap,xlab="Height",ylab="Lung Capacity",main="Height vs Lung Capacity")
abline(lm(LungCap~Height, data=d1),col="red")
#cor(Height,Lungcap) = 0.84 

#Normality of Residuals
m2<-lm(LungCap ~ Age + Height + Smoke + Gender,data=d2)
shapiro.test(residuals(m2))  #pVal=0.4862 We fail to reject Null Hypothesis. There is not enough evidence to conclude that the data is not normally distributed.

#homooscedasticity
plot(m2)
plot(fitted(m2), residuals(m2))
abline(h = 0, col = "red")

# Breusch-Pagan test
library(lmtest)
bptest(m2)
plot(m2,1)

# no heterscedasticity
# Null Hypothesis (H0): Homoscedasticity is present (the residuals are distributed with equal variance)
# Alternative Hypothesis (HA): Heteroscedasticity is present (the residuals are not distributed with equal variance)
# p-value of the test is 0.8037 which is greater than significance level alpha therefore we fail to reject the null hypothesis and conclude that and conclude that heteroscedasticity is not present in the regression model.


#multicollinearity
vif(m2)

#Outliers and Influential plots
plot(m2, 5) #Residual vs Leverage
plot(m2, 4) #cooks distance

# Step 4: ANOVA for model significance
anova(m2)
summary(m2)
