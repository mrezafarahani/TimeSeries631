data<-read.csv("C:\\Users\\mohammad\\Dropbox\\Courses\\SYDE631\\Project\\windMW.csv",header = TRUE)
str(data)
# Hourly MW production
MW<-data$ClearedMW
str(MW)
plot(MW, type='l')
acf(MW)
pacf(MW)
sd(MW)
mean(MW)
#__________________________________________________________________
#Differentiated hourly calculation dfMW
dfMW<-rep(0,50543)
    for (i in 1:50543)
{	
	dfMW[i] <- MW[i] - MW[i+1]
}
plot(dfMW, type='l')
sd(dfMW)
mean(dfMW)
acf(dfMW)
pacf(dfMW)
#___________________________________________________________________
#Daily mean calculation dlMW
dlMW<-rep(0,2106)
    for (i in 1:2106)
{	
	dlMW[i] <- mean(MW[((i-1)*24+1):(i*24)])/24
}
plot(dlMW,type='l')
acf(dlMW,lag=40)
pacf(dlMW)
#_____________________________________________________________________
#Monthly mean calculation mMW
mMW<-rep(0,70)
    for (i in 1:70)
{	
	mMW[i] <- sum(dlMW[((i-1)*30+1):(i*30)])/30
}
ts(mMW,frequency = 12)
plot(mMW,type='l')
acf(mMW)
pacf(mMW)

#_____________________________________________________________________
#Differentiated Daily mean calculation dfdlMW
dfdlMW<-rep(0,2105)
    for (i in 1:2105)
{	
	dfdlMW[i] <- dlMW[i] - dlMW[i+1]
}
plot(dfdlMW, type='l')
acf(dfdlMW)
pacf(dfdlMW)
fit <- arima(dfdlMW, order = c(0,0,2))
predict(fit, n.ahead = 6)
plot(dfdlMW[2000:2105], type='l',predict(fit, n.ahead = 6))
#______________________________________________________________________
#Differentiated monthly mean calculation dfdlMW

dfmMW<-rep(0,69)
    for (i in 1:69)
{	
	dfmMW[i] <- (mMW[i+1]-mMW[i])
}
ts(mMW,frequency = 12)
plot(dfmMW,type='l')
attach(mtcars)
par(mfrow=c(1,2))
acf(dfmMW, lag = 24)
pacf(dfmMW, lag = 24)
#_____________________________________________________________________

# (Differentiated) seasonal data calculation for each hour in day (dfhMW) hMW
dfhMW<- mat.or.vec(24, 2105)
hMW<- mat.or.vec(24, 2102)

    for (i in 1:2105)
{	for (j in 1:24){
	dfhMW[j,i] <- MW[(((i-1)*24)+j)] - MW[((i*24+j))]
	hMW[j,i] <- MW[(((i-1)*24)+j)]
	}
}

attach(mtcars)
par(mfrow=c(3,2))
for (j in 1:6){
acf(dfhMW[j+6,1:2105])}


plot(dfhMW[1,1:2105], type='l')
acf(dfhMW[1,1:2105])
pacf(dfhMW)
plot(hMW[11,1:1036],type='l')
mean(hMW[1,1:1036])
acf(hMW[11,1:1036])
pacf(hMW[11,1:1036])
#______________________________________________________________________
# Differentiate seasonal data in a 24 hour time window dsMW
dsMW<-rep(0,50515)
ddsMW<-rep(0,50490)
dddsMW<-rep(0,50450)

    for (i in 1:50515)
{	
	dsMW[i] <- dfMW[i+24] - dfMW[i]	
}
plot(dsMW, type='l')
attach(mtcars)
par(mfrow=c(1,2))
acf(dsMW, lag = 80)
pacf(dsMW, lag =80)
    for (i in 1:50490)
{	
	ddsMW[i] <- dsMW[i+24] - dsMW[i]	
}
    for (i in 1:50450)
{	
	dddsMW[i] <- ddsMW[i+24] - ddsMW[i]	
}
attach(mtcars)
par(mfrow=c(1,2))
acf(dddsMW, lag = 80)
pacf(dddsMW, lag =80)
help(sarima)
ans<-sarima(dfMW,1,0,1,0,0,1,24)
summary(ans$fit)
ans
ans201002<-ans
ans200001<-ans
hist((resid(ans2$fit)))
acf(resid(ans2$fit))
residuals<-resid(ans3$fit)
shapiro.test(residuals[1:5000])
#_____________________________________________________________________
#dfsdlMW diff seasonal for every hour
MhMW<- mat.or.vec(24,1)

for (i in 1:24){
	MhMW[i] <- mean (hMW[i,880:1036])
}
plot(MhMW, type='l')

sd(MhMW/40)
mean(MhMW[1:24])
#______________________________________________________________________
#Calculate the estimated values
ans<-FitARMA(dfdlMW, order=c(0,0,3), MeanMLEQ=TRUE)
ans
GetFitARMA(dfdlMW, 1, 2, pApprox = 1, init = 0)
tsdiag(ma)
ma<-FitARMA(dfMW, order=c(0,0,3), MeanMLEQ=TRUE)
ma
fit <- arima(dfMW, c(1,0,0))
tsdiag(fit)
coef(ans)
sarima.for(dfMW,12,0,0,2,0,0,1,24)
summary(ans)

plot(dfdlMW[
hist(resid(ans))
norm<-resid(ma)
acf(resid(ma))
shapiro.test(norm[1:5000])
plot(fitted(ans),type='l')
plot(ans2$racf,type='l')
acf(ans2$racf)
z<-SimulateGaussianARMA(0.694545, 0.274827, 200)
GetFitARMA(z, p=0, q=2)
z



