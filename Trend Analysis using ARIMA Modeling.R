
covid_data=read.csv("covid_vaccine_statewise (1).csv")
covid_data
Total_Doses_Administered=covid_data[covid_data$State == "India",c("Total.Doses.Administered")]
Total_Doses_Administered=Total_Doses_Administered[1:30]
Total_Doses_Administered
Days=seq(1,30,1)
datfra=data.frame(x=Days,Y=Total_Doses_Administered)
head(datfra)
plot(datfra$x, datfra$y, col="gray")
acf(Total_Doses_Administered)


#Main Project
vaccine_weekly_India=read.csv("updated_covid_vacc_weekly.csv")
vaccine_weekly_India=(vaccine_weekly_India[1:23,])

dotchart(vaccine_weekly_India$Total.Individuals.Vaccinated,xlab = "Vaccinated (Weekly)",main="Dot Plot",col="Red")   #Dot Plot on Vaccination
#(Weekly Stationary)
#Check for Stationarity using Auto-Correlation Function
acf(vaccine_weekly_India$Total.Individuals.Vaccinated,main="Correlogram")   #Correlogram

#Check for the presence of Trend
windows(width=8, height=6)
barplot(vaccine_weekly_India$Total.Individuals.Vaccinated~vaccine_weekly_India$ï..Weeks,xlab = "Weeks",ylab = "Vaccinated (Weekly)",main="Determination of Trend by Moing Avg.(Period 2)",col="green")   #Bar Plot on Vaccination vs Weeks 
lines(vaccine_weekly_India$Total.Individuals.Vaccinated,col="blue",lw=3)
legend("topleft", 
       legend = c("Bars","Moving Avg.(Period 2)"), 
       col = c("green","blue"),
       lty = 1, lwd=3)
cy=cycle(DF_vaccine_weekly$y)
windows(width=8,height = 6)
plot(DF_vaccine_weekly$y,type="l")
boxplot(DF_vaccine_weekly$y~DF_vaccine_weekly$x)
windows(width=8,height = 6)
plot(log(DF_vaccine_weekly$y),type="l")   #Smoothing(Mean Const.)
plot(diff(log(DF_vaccine_weekly$y)),type="l")   #Control Deviation(Sd. Const.)
plot(diff(diff(DF_vaccine_weekly$y)),type="l")
d.vacc=diff(diff(DF_vaccine_weekly$y))   #d=2
acf(d.vacc,col="red") #q=0
pacf(d.vacc,col="red")#p=1
plot(d.vacc,type="l")
#ARIMA(p,d,q)
fitmod=arima(log(DF_vaccine_weekly$y),c(1,2,0)) #seasonal = list(order=c(0,2,1)) == 0
predmod=predict(fitmod,n.ahead = 7)
predmodel=exp(predmod$pred)
predseas=exp(predmod$se)

library(forecast)
auto.arima(DF_vaccine_weekly$y,trace=TRUE)
arimamod=arima(DF_vaccine_weekly$y,c(1,2,0))
arimapred=predict(arimamod,n.ahead=7)
futval=arimapred$pred+arimapred$se
plot(futval)
plot(arimapred$se)
library(forecast)
plot(forecast(arimamod,h=7))
plot(arimapred$pred)

windows(width=8,height=12)
par(mfrow=c(1,2))
plot(predmodel,ylab="Values",main="Secular Trend",col="green",lwd=2,lty=5)
plot(predseas,ylab="Values",main="Seasonal Variation",col="blue",lwd=2,lty=5)

windows(width=8,height=6)
plot(c(DF_vaccine_weekly$y,predmodel),ylab="Vaccination Weekly (in Millions)",main="Observed and Predicted",type="l",col="green",lty=3)
points(DF_vaccine_weekly$y,col="blue")
abline(v=23.5,col="red",lty=5)

library(smooth)
smaval=sma(DF_vaccine_weekly$y,h=23)










plot(predmodel)
#Analysis of the data
DF_vaccine_weekly = data.frame(x=vaccine_weekly_India$ï..Weeks,y=(vaccine_weekly_India$Total.Individuals.Vaccinated)/1000000)   #Minimized digits by /1000000(Millions)
summary(DF_vaccine_weekly)
cor.test(DF_vaccine_weekly$y,DF_vaccine_weekly$x,method = "pearson")

#Analysis of Trend
plot(DF_vaccine_weekly$x,xlab = "Weeks",DF_vaccine_weekly$y,ylab = "Total Vaccinated (Weekly) [in Millions]",main="Line Diagram",col="Blue")   #Scatter plot on Vaccination vs Weeks
lines(DF_vaccine_weekly$x,DF_vaccine_weekly$y,col="SkyBlue",lty=5,lw=1)   #Line diagram on Vaccination vs Weeks

#Predicting of Linear & Non-Linear models

model0=lm(y~poly(x,1), data = DF_vaccine_weekly)   #Linear Model(Degree 1)
summary(model0)
confint(model0, level=0.95)
res0=residuals(model0)

model1=lm(y~I(exp(x)), data = DF_vaccine_weekly)   #Exponential Model
summary(model1)
confint(model1, level=0.95)
res1=residuals(model1)

model2=lm(y~poly(x,3), data = DF_vaccine_weekly)   #Quadratic Model(Degree 4)
summary(model2)
confint(model2, level=0.95)
res2=residuals(model2)

model3=lm(y~poly(x,5), data = DF_vaccine_weekly)   #Cubic Model(Degree 5)
summary(model3)
confint(model3, level=0.95)
res3=residuals(model3)


#Fitting Models

#Linear
fit0=predict(model0, data = DF_vaccine_weekly$x,interval="confidence",level=.95)
fit0

windows(width=8, height=6)
plot(DF_vaccine_weekly$y,xlab="Weeks",ylab="Values (in Millions)",main="Observed Values vs Fitted Model",col="blue",lwd=1,)
lines(fit0[,1],col="brown",lwd=2)
lines(fit0[,2],col="red",lty=5,lwd=1)
lines(fit0[,3],col="red",lty=5,lwd=1)
legend("topleft", 
       legend = c("Obs. Data","Fitted Trend (Linear)","95% C.I. Level"), 
       col = c("blue","brown","red"),
       lty = 1, lwd=3)

#Exponential
fit1=predict(model1, data = DF_vaccine_weekly$x,interval="confidence",level=.95)
fit1

windows(width=8, height=6)
plot(DF_vaccine_weekly$y,xlab="Weeks",ylab="Values (in Millions)",main="Observed Values vs Fitted Model",col="blue",lwd=1,)
lines(fit1[,1],col="orange",lwd=2)
lines(fit1[,2],col="red",lty=5,lwd=1)
lines(fit1[,3],col="red",lty=5,lwd=1)
legend("topleft", 
       legend = c("Obs. Data","Fitted Trend (Exponential)","95% C.I. Level"), 
       col = c("blue","yellow","red"),
       lty = 1, lwd=3)

#Quartic~Poly(4)
fit2=predict(model2, data = DF_vaccine_weekly$x,interval="confidence",level=.95)
fit2

windows(width=8, height=6)
plot(DF_vaccine_weekly$y,xlab="Weeks",ylab="Values (in Millions)",main="Observed Values vs Fitted Model",col="blue",lwd=1,)
lines(fit2[,1],col="green",lwd=2)
lines(fit2[,2],col="red",lty=5,lwd=1)
lines(fit2[,3],col="red",lty=5,lwd=1)
legend("topleft", 
       legend = c("Obs. Data","Fitted Trend (Poly(4))","95% C.I. Level"), 
       col = c("blue","orange","red"),
       lty = 1, lwd=3)

#Quintic~Poly(5)
fit3=predict(model3, data = DF_vaccine_weekly$x,interval="confidence",level=.95)
fit3

windows(width=8, height=6)
plot(DF_vaccine_weekly$y,xlab="Weeks",ylab="Values (in Millions)",main="Observed Values vs Fitted Model",col="blue",lwd=1,)
lines(fit3[,1],col="yellow",lwd=2)
lines(DF_vaccine_weekly$x,fit3[,2],col="red",lty=5,lwd=1)
lines(DF_vaccine_weekly$x,fit3[,3],col="red",lty=5,lwd=1)
legend("topleft", 
       legend = c("Obs. Data","Fitted Trend (Poly(5))","95% C.I. Level"), 
       col = c("blue","green","red"),
       lty = 1, lwd=3)


#Graph Plotting
windows(width=8, height=6)

plot(DF_vaccine_weekly$x,xlab = "Weeks",DF_vaccine_weekly$y,ylab = "Values (in Crores.)",main="Observed Values vs Fitted Models",col="Blue",lwd=1)

lines(fit0[,1],col="brown",lwd=2)
lines(fit1[,1],col="yellow",lwd=2)
lines(fit2[,1],col="orange",lwd=2)
lines(fit3[,1],col="green",lwd=4)

legend("topleft", 
       legend = c("Obs. Values","Linear Model","Exponential Model","Poly(4) Model","Poly(5) Model"), 
       col = c("blue","Brown","yellow","Orange","green"),
       lty = 1, lwd=3)




#Predicting future values : y = 0.0461x3 - 0.686x2 + 3.8021x + 0.1659
t = DF_vaccine_weekly$x
t=append(t,seq(16,20))
pred_yt = function(t,a,b,c,d){
        yt = a*t^3 + b*t^2 + c*t + d
        return(yt)
}
predict_y = pred_yt(t,0.0461,-0.686,3.8021,0.1659)

plot(predict_y,main="Obs. & Future prediction on Covid Vaccination",col="green",lwd=2)
points(DF_vaccine_weekly,col="blue",lwd=2)
curve(x-x+66,from=0,to=25,add=TRUE,col="red",ity=5)
abline(v=15.5,col="red",lty=5)

library(ggplot2)
ggplot(data=predict_y,aes(x,y))

predn2
diff_modeln2 = DF_vaccine_weekly$y-predn2
diff_modeln2
mean(diff_modeln2)
sde2=var(diff_modeln2)
sde2

curve(5932.17008*(x^5)-343337.75551*(x^4)+6876621.74538*(x^3)-51534467.73338*(x^2)+159231096.87942*(x)-129323292.01760,from = 1, to = 23)

abline(v=15.5,col="red",lty=5)


i=c(seq(24,28,1))
eqn2=(5932.17008*(i^5)-343337.75551*(i^4)+6876621.74538*(i^3)-51534467.73338*(i^2)+159231096.87942*(i)-129323292.01760)
eqn2

pred2=c(fit2[1:23,2])
pred2=append(pred2,eqn2)
pred2
plot(pred2[1:28],type="l",col="green")

points(pred2[1:23],col="red")

forecast=data.frame(x=seq(1,23),y=DF_vaccine_weekly$y)
forecast

library(ggplot2)
ggplot(data=DF_vaccine_weekly,aes(x,y))+ geom_point(col="blue") + ggplot(data=DF_vaccine_weekly,aes(x,y))+ geom_point(col="blue")

pacf(d.DF)
arima(DF_vaccine_weekly$y,c(1,4,0))
d.DF=diff(DF_vaccine_weekly$y)
d.DF=diff(d.DF)
summary(d.DF)
library(tseries)
adf.test(d.DF)
acf(d.DF)
arima(DF_vaccine_weekly$y,order=c(1,3,0))
plot(d.DF)
lines(d.DF)

plot(diff_modeln2)
lines(diff_modeln2)
library(tseries)
adf.test(d.modeln2)
acf(diff_modeln2)
d.modeln2=diff(d.modeln2)
acf(d.modeln2)
plot(d.modeln2)
lines(d.modeln2)

T=0:300
t_sta=rnorm(length(T),mean=3,sd=1.5)
plot(t_sta,type="l")
lines(t_sta)
t_trend=(cumsum(rnorm(length(T),mean=3,sd=6)))
plot(t_trend,type="l")

plot(rnorm(seq(-5,5,.1)),type="l")

res2=resid(model2)
m2=mean(res2)
sd2=sd(res2)
plot(rnorm(15,0,1),type="l")
plot(fitted(model2),resid(model2))
abline(0,0)
plot(density(resid(model2)))
qqnorm(resid(model2))
qqline(resid(model2))
par(mfrow=c(2,2))
plot(model2)
exp(1)
