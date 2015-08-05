data<-read.csv("C:\\Users\\mohammad\\Dropbox\\Courses\\SYDE631\\Project\\wind.csv",header = TRUE)
str(data)
# hourly lmp
MW<-data$ClearedMW
str(MW)
plot(MW, type='l')
acf(MW)
pacf(MW)
sd(MW)
mean(MW)
#__________________________________________________________________
dfMW<-rep(0,24887)
    for (i in 1:24887)
{	
	dfMW[i] <- MW[i] - MW[i+1]
}
plot(dfMW, type='l')
acf(dfMW)
pacf(dfMW)
#___________________________________________________________________

#dlMW or daily MW
dlMW<-rep(0,1037)
    for (i in 1:1037)
{	
	dlMW[i] <- sum(MW[((i-1)*24+1):(i*24)])/24
}
plot(dlMW,type='l')
acf(dlMW)
pacf(dlMW)
#_____________________________________________________________________
#dfdlMW or daily MW
dfdlMW<-rep(0,1036)
    for (i in 1:1036)
{	
	dfdlMW[i] <- dlMW[i] - dlMW[i+1]
}
plot(dfdlMW, type='l')
acf(dfdlMW)
pacf(dfdlMW)
#______________________________________________________________________
#dfhMW diff seasonal for every hour
dfhMW<- mat.or.vec(24, 1036)
hMW<- mat.or.vec(24, 1036)

    for (i in 1:1036)
{	for (j in 1:24){
	dfhMW[j,i] <- MW[(((i-1)*24)+j)] - MW[((i*24+j))]
	hMW[j,i] <- MW[(((i-1)*24)+j)]
	}
}
plot(dfhMW, type='l')
acf(dfhMW[1,1:1036])
pacf(dfhMW)
plot(hMW[11,1:1036],type='l')
mean(hMW[1,1:1036])
acf(hMW[11,1:1036])
pacf(hMW[11,1:1036])
#______________________________________________________________________
#dfsdlMW diff seasonal for every hour
MhMW<- mat.or.vec(24,1)

for (i in 1:24){
	MhMW[i] <- mean (hMW[i,880:1036])
}
plot(MhMW, type='l')
sd(MhMW/40)
mean(MhMW[1:24])
#______________________________________________________________________


ans<-FitARMA(dfdlMW, order=c(0,0,3), MeanMLEQ=TRUE)
ans
GetFitARMA(dfdlMW, 0, 3, pApprox = 1, init = 0)
help(RACF)




#mlmp or monthly lmp
mlmp<-rep(0,103)
    for (i in 1:103)
{	
	mlmp[i] <- sum(dlmp[((i-1)*10+1):(i*10)])
}
plot(mlmp,type='l')
acf(mlmp)
pacf(mlmp)

#dmlmp or diffrential monthly lmp
dmlmp<-rep(0,102)
    for (i in 1:102)
{	
	dmlmp[i] <- mlmp[i] - mlmp[i+1]
}
plot(dmlmp,type='l')
acf(dmlmp)
pacf(dmlmp)
help(acf)

ans<-FitARMA(dmlmp, order=c(0,1,2), MeanMLEQ=TRUE)

#dlmp or diffrential monthly lmp
dlmp<-rep(0,24887)
    for (i in 1:24887)
{	
	dlmp[i] <- lmp[i] - lmp[i+1]
}
plot(dlmp,type='l')
acf(dlmp)
pacf(dlmp)
#end
ddd<-resid(ans$fit)
shapiro.test(log(ddd[1:5000]+100))
#sdmlmp or seasonal diffrential monthly lmp
sdmlmp<-rep(0,143)
    for (i in 1:143)
{	
	sdmlmp[i] <- dmlmp[i] - dmlmp[i+24]
}
plot(sdmlmp,type='l')
acf(sdmlmp)
pacf(sdmlmp)
help(acf)
#ssdmlmp or seasonal diffrential monthly lmp
ssdmlmp<-rep(0,100)
    for (i in 1:100)
{	
	ssdmlmp[i] <- sdmlmp[i] - sdmlmp[i+24]
}
plot(ssdmlmp,type='l')
acf(ssdmlmp[1:80])
pacf(ssdmlmp[1:80])

TS <- ts(mlmp[1:100], frequency = 12)
decompose(TS)


arma(lmp,order=c(1,1,1))
lnmlmp <- log(mlmp)
dmlmp<-rep(0,144)
    for (i in 1:144)
{	
	lndmlmp[i] <- lnmlmp[i] - lnmlmp[i+25]
}
plot(lndmlmp, type='l')
acf(lndmlmp)
pacf(lndmlmp)
BCdlmp <- 2*((dlmp + 10)^0.5 -1)