peq = function(x) x^3+2*x^2+5

x = seq(-0.99, 1, by = .01)
y = peq(x) + runif(200)

df = data.frame(x = x, y = y)
head(df)

plot(df$x, df$y, pch=20, col="gray")

model = lm(y~x+I(x^3)+I(x^2), data = df)
summary(model)

pred = predict(model,data=df) 
pred

windows(width=8, height=6)
plot(x=df$x, y=df$y, pch=20, col="grey")

lines(df$x, predict(lm(y~x, data=df)), type="l", col="orange1", lwd=2)
lines(df$x, predict(lm(y~I(x^2), data=df)), type="l", col="pink1", lwd=2)
lines(df$x, predict(lm(y~I(x^3), data=df)), type="l", col="yellow2", lwd=2)
lines(df$x, predict(lm(y~poly(x,3)+poly(x,2), data=df)), type="l", col="blue", lwd=2)

legend("topleft", 
       legend = c("y~x,  - linear","y~x^2", "y~x^3", "y~x^3+x^2"), 
       col = c("orange","pink","yellow","blue"),
       lty = 1, lwd=3
) 

pred = predict(model,data = df)
lines(df$x, pred, lwd = 3, col = "blue")  

library(ggplot2) 
ggplot(data=df, aes(x,y)) + geom_point() 
+ geom_smooth(method="lm", formula=y~I(x^3)+I(x^2))
