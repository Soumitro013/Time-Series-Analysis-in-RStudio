covid_data=read.csv("covid_vaccine_statewise (1).csv")
covid_data1=covid_data[covid_data$State == "India",c("Updated.On","Total.Covaxin.Administered")]
covid_data2=covid_data[covid_data$State == "India",c("Total.Covishield.Administered","Total.Sputnik.V.Administered")]
covid_data3=covid_data[covid_data$State == "India",c("Updated.On","Total.Covaxin.Administered","Total.Covishield.Administered","Total.Sputnik.V.Administered","Total.Doses.Administered")]
covid_india=covid_data[covid_data$State == "India",c("Updated.On","Total.Covaxin.Administered","Total.Covishield.Administered","Total.Sputnik.V.Administered","Total.Doses.Administered")]


covid_data=read.csv("covid_vaccine_statewise (1).csv")
Total_Doses_Administered=covid_data[covid_data$State == "India",c("Total.Doses.Administered")]
Total_Doses_Administered=Total_Doses_Administered[1:30]
Total_Doses_Administered
Days=seq(1,30,1)
datfra=data.frame(x=Days,Y=Total_Doses_Administered)
head(datfra)
plot(datfra$x, datfra$y, col="gray")
acf(Total_Doses_Administered)

#Main Project
vaccine_weekly=read.csv("covid_vacc_weekly.csv")
#Check for Stationarity using Auto-Correlation Function
acf(vaccine_weekly$Total.Doses.Administered,main="Correlogram")
#Scatter_plot and Line_diagram of the  data
plot(vaccine_weekly$Weeks,xlab = "Weeks",vaccine_weekly$Total.Doses.Administered,ylab = "Total Doses Administered",col="Blue")
lines(vaccine_weekly$Weeks,vaccine_weekly$Total.Doses.Administered,col="Green",lw=0.5)
legend("topleft", 
       legend = c("Plots","Line"), 
       col = c("blue","green"),
       lty = 1, lwd=2)

DF_vaccine_weekly = data.frame(x=vaccine_weekly$Weeks,y=(vaccine_weekly$Total.Doses.Administered))
summary(DF_vaccine_weekly)
#Fitting of Linear & Non-Linear models

model0=lm(y~x, data = DF_vaccine_weekly)   #Linear Model(Degree 1)
summary(model0)

model1=lm(y~x+I(x^2)+I(x^3)+I(x^4), data = DF_vaccine_weekly)   #Quartic Model(Degree 4)
summary(model1)

model2=lm(y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5), data = DF_vaccine_weekly)   #Quintic Model(Degree 5)
summary(model2)

model3=lm(y~I(exp(x)), data = vaccine_weekly)   #Exponential Model
summary(model3)

plot(DF_vaccine_weekly$x,xlab = "Weeks",DF_vaccine_weekly$x,ylab = "Total Doses Administered",col="Blue")
plot(vaccine_weekly$Weeks,xlab = "Weeks",vaccine_weekly$Total.Doses.Administered,ylab = "Total Doses Administered",col="Blue")

lines(DF_vaccine_weekly$x,predict(lm(y~x, data = DF_vaccine_weekly)),type="l", col="green", lwd=2)
lines(DF_vaccine_weekly$x,predict(lm(y~poly(x,2)+poly(x,3)+poly(x,4), data = DF_vaccine_weekly)),type="l", col="green", lwd=2)
lines(DF_vaccine_weekly$x,predict(lm(y~poly(x,2)+poly(x,3)+poly(x,4)+poly(x,5), data = DF_vaccine_weekly)),type="l", col="green", lwd=2)
legend("topleft", 
       legend = c("Plots","Line"), 
       col = c("blue","green"),
       lty = 1, lwd=2
       )

pred=predict(model1, data = DF_vaccine_weekly)
pred
plot(model1)
