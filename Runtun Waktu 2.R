#Muhammad Abizart_22106010077
setwd("C:/Users/ASUS/Documents")
closales.data <- read.csv("closales.data.CSV", sep=";", header=TRUE)
closales.data
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(closales.data[,2],lag.max=50,type="correlation",main="ACF for
    the Clothing Sales")
acf(closales.data[,2], lag.max=50,type="partial",main="PACF for
    the Clothing Sales")
wt.closales<-diff(diff(closales.data[,2],lag=1),lag=12)
plot(wt.closales,type="o",pch=16,cex=.5,xlab='Date',ylab='w(t)',
     xaxt='n')
axis(1, seq(1,144,24), closales.data[seq(13,144,24),1])
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(wt.closales,lag.max=50,type="correlation",main="ACF for w(t)")
acf(wt.closales, lag.max=50,type="partial",main="PACF for w(t)")
closales.fit.sar<-arima(closales.data[,2],order=c(0,1,1),
                        seasonal=list(order = c(0,1,1),period=12),)
res.closales.sar<-as.vector(residuals(closales.fit.sar))
library(forecast)
fit.closales.sar<-as.vector(fitted(closales.fit.sar))
#ACF and PACF of the Residuals
par(mfrow=c(1,2),oma=c(0,0,0,0))
acf(res.closales.sar,lag.max=50,type="correlation",main="ACF of the Residuals")
acf(res.closales.sar,lag.max=50,type="partial",main="PACF of the Residuals")
#4-in-1 plot of the residuals
par(mfrow=c(2,2),oma=c(0,0,0,0))
qqnorm(res.closales.sar,datax=TRUE,pch=16,xlab='Residual',main='')
qqline(res.closales.sar,datax=TRUE)
plot(fit.closales.sar,res.closales.sar,pch=16, xlab='Fitted
Value',ylab='Residual')
abline(h=0)
hist(res.closales.sar,col="gray",xlab='Residual',main='')
plot(res.closales.sar,type="l",xlab='Observation Order',
     ylab='Residual')
points(res.closales.sar,pch=16,cex=.5)
abline(h=0)
plot(closales.data[,2],type="p",pch=16,cex=.5,xlab='Date',
     ylab='Clothing Sales',xaxt='n')
axis(1, seq(1,144,24), closales.data[seq(1,144,24),1])
lines(1:144, fit.closales.sar)
legend(2,17500,c("US Clothing Sales","Fitted"), pch=c(16, NA),
       lwd=c(NA,.5),cex=.55,col=c("black","black"))
