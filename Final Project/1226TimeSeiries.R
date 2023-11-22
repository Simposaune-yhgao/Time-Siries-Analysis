library(fBasics)
data1<-read.csv("C:/Users/User/Desktop/0063R.csv")

da1 = data1$日報酬率..
tdx = c(1:1007)
plot(tdx[1:1007],da1,type ='l',xlab = 'data period',ylab = 'data1
     numerical')
abline(a = mean(da1), b = 0) #畫平均線
abline(a = 2*stdev(da1), b = 0) #畫 2 倍標準差線
abline(a= -2*stdev(da1), b = 0)

head(da1)
mean(da1)

diffdata1 <- diff(da1) #將原始資料做差分。
tdxx = c(1:1007)
plot(tdx[1:1006],diffdata1,type ='l',xlab = 'data period',ylab = 'data1
     numerical')
abline(a = mean(diffdata1), b = 0) #畫平均線
abline(a = 2*stdev(diffdata1), b = 0) #畫 2 倍標準差線
abline(a = -2*stdev(diffdata1), b = 0)

acf(diffdata1,lag = 32)
pacf(diffdata1,lag =42     )

m1 = arima(diffdata1,order = c(10,0,8))
m1 #call m1
Box.test(m1$residuals,lag = 12 ,type = 'Ljung')
tsdiag(m1, gof=12)

res= m1$residuals
par(mfcol=c(1,1))
hist(res)

d1=density(res)
range(res)
x=seq(-.1,.1,.001)
y1=dnorm(x,mean(res),stdev(res))
plot(d1$x,d1$y,xlab='Residual',ylab='density',type='l')
lines(x,y1,lty=4)
qqnorm(res)

normalTest(res,method='jb')

m1 = arima(diffdata1,order = c(3,0,3), fixed=c(NA,NA,0,NA,NA,NA,0))

m1
Box.test(m1$residuals,lag = 12 ,type = 'Ljung')


tsdiag(m1, gof=12)







pv = 1-pchisq(6.06,10) # X-square = 18.663 自由度 = 12-2
pv #call m2
diff_da1 = diff(da1)
adf.test(diff_da1, alternative = c("stationary", "explosive"), k =
           trunc((length(diff_da1)-1)^(1/3)))


m1 = arima(diffdata1,order = c(5,0,5))
m1 #call m1
Box.test(m1$residuals,lag = 12 ,type = 'Ljung')


pv = 1-pchisq(24.218,10) # X-square = 18.663 自由度 = 12-2
pv #call m2

tsdiag


mm1 = ar(diff_da1,method = 'mle')
print(mm1$aic,digits = 3)
aic = mm1$aic
length(aic)
plot(c(0:12),aic,type='h',xlab='order',ylab='aic')
lines(0:12,aic,lty=2)

library(fBasics)
library(tseries)
library(TSA)


setInternet2(TRUE)



