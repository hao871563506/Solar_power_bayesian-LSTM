load("data2.Rda")
load('data2complete.Rda')
mydata<-read.csv("daily_solar_time_series.csv")
mydata2 <- mydata[mydata$stn =='NEW', ]
mydata3<-mydata2[13515:18262,3]
T3[T3$Year=='2010' & T3$Month =='12' & T3$Day =='31',1:5]
T4<-T3[1:4748,66]





firstday<-c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,5111,5476,5841,6206,6571,6936)

for(i in 0:364){
  for(location in 1:331){
    currentmean<-mean(T3[(firstday+i),(3+location)])
    currentsd<-sd(T3[(firstday+i),(3+location)])
    T3[(firstday+i),(3+location)]<-(T3[(firstday+i),(3+location)]-currentmean)/currentsd
  }}

#save(T3,file="data3.Rda")
library(ggplot2)
load("data3.Rda")
##coeff list of AR(1)
cofflist<-c()
for(i in 4:334){
model <- arima(T3[,i],order=c(1,0,0))
cofflist<-c(cofflist,model$coef[1])
}
boxplot(cofflist)

springdf<-T3[1:91,]
for(i in 1:19){
springdata<-T3[(1+i*365):(91+i*365),]
springdf <- rbind(springdf,springdata)
}
cofflist<-c()
for(i in 4:334){
  model <- arima(springdf[,i],order=c(1,0,0))
  cofflist<-c(cofflist,model$coef[1])
}
coffdf<-as.data.frame(cofflist)
p1<-ggplot(coffdf)+geom_boxplot(aes(y=cofflist))


springdf<-T3[92:183,]
for(i in 1:19){
  springdata<-T3[(1+i*365):(91+i*365),]
  springdf <- rbind(springdf,springdata)
}
cofflist<-c()
for(i in 4:334){
  model <- arima(springdf[,i],order=c(1,0,0))
  cofflist<-c(cofflist,model$coef[1])
}
coffdf<-as.data.frame(cofflist)
p2<-ggplot(coffdf)+geom_boxplot(aes(y=cofflist))

springdf<-T3[184:275,]
for(i in 1:19){
  springdata<-T3[(1+i*365):(91+i*365),]
  springdf <- rbind(springdf,springdata)
}
cofflist<-c()
for(i in 4:334){
  model <- arima(springdf[,i],order=c(1,0,0))
  cofflist<-c(cofflist,model$coef[1])
}
coffdf<-as.data.frame(cofflist)
p3<-ggplot(coffdf)+geom_boxplot(aes(y=cofflist))


springdf<-T3[276:365,]
for(i in 1:19){
  springdata<-T3[(1+i*365):(91+i*365),]
  springdf <- rbind(springdf,springdata)
}
cofflist<-c()
for(i in 4:334){
  model <- arima(springdf[,i],order=c(1,0,0))
  cofflist<-c(cofflist,model$coef[1])
}
coffdf<-as.data.frame(cofflist)
p4<-ggplot(coffdf)+geom_boxplot(aes(y=cofflist))
require(gridExtra)
grid.arrange(p1, p2,p3,p4,nrow=2, ncol=2)





##it does not work with 300 points, I tried 30 it works though
library(NHMSAR)
mydata<- T3[,4:334]
T = 365#dim(mydata)[1]
d = dim(mydata)[2]
N.samples = 20 #dim(mydata)[3]
new.arr <- array( unlist(mydata), dim=c(T, N.samples,d) ) 

#number of regimes
M = 8
#order of AR processes
order = 1
theta.init = init.theta.MSAR(new.arr,M=M,order=order,label="HH")
mod.hh = fit.MSAR(new.arr,theta.init,verbose=TRUE,MaxIter=100)




#variogram
load("data.Rda")#newdf
newdf<-newdf[!(newdf$Month==2 & newdf$Day==29),]
library(gstat)


currentdf<-newdf[(newdf$Year==2000 & newdf$Month==1 & newdf$Day==1),]
DHI.vgm <- variogram(DHI~1, loc= ~lon+lat, data=currentdf)
plot(DHI.vgm)
mean(DHI.vgm$gamma)

##SOM map
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)




##spatial temporal variogram

timesq<-seq(as.Date("1998-01-01"), as.Date("2017-12-31"), by="days")
remove<-c(as.Date("2000-02-29"),as.Date("2004-02-29"),as.Date("2008-02-29"),as.Date("2012-02-29"),as.Date("2016-02-29"))
removeindex<-match(remove,timesq)
timesq2 <- timesq[-removeindex]




##PCA
load("data3.Rda")
mydata<-T3[,4:334]
aa<-as.matrix(mydata)%*%eigen(cor(mydata))$vectors ##as.matrix(mydata)%*%ae1

pca <- prcomp(mydata, scale = FALSE)
#summary(pca)

#pca$rotation
aa2<-pca$x
#which(aa2[,1]< -60)
#aa=aa2
ae<-pca$rotation
library(forecast)
Acf(aa2[,1:5])



load("data2.Rda")
mydata<-T3[,4:334]

library(pear)
site1<-mydata[,1]
#PC1t<-ts(PC1, start = c(1998, 1, 1),frequency =365.25)
site1t<-ts(site1,frequency =73)
#result<-pear(PC1t, m=3,ic="none")
result<-pear(site1t, m=3,ic="bic")

load("data2.Rda")
#mydata<-read.csv("daily_solar_time_series.csv")
#mydata2 <- mydata[mydata$stn =='NEW', ][,3]
#mydata2 <- mydata2[1:18250]
mydata<-T3[,4:334]
aa<-as.matrix(mydata)%*%eigen(cor(mydata))$vectors ##as.matrix(mydata)%*%ae1
aa2<-aa
PC1<-aa[,1]
PC1<-colMeans(matrix(PC1, 5))
#PC1<-colMeans(matrix(mydata2, 5))
PC1t<-ts(PC1,frequency = 73)

result<-pear(PC1t, m=3,ic="none")

result2<-peacf(PC1t)
var<-(result2$standard.deviations)^2
plot(c(1:73),result$resvar/var)
var(result$residuals)/var(PC1)#0.239d

percetagepear<-mean(abs(result$residuals/PC1))
percetagepear#0.195

arfit<-arima(PC1,order=c(1,0,0))
percentagear<-mean(abs(arfit$residuals/PC1))
percentagear#0.226
var(arfit$residuals)/var(PC1)#0.30
armafit<-auto.arima(PC1)
percentagearma<-mean(abs(armafit$residuals/PC1))
percentagearma#0.190
var(armafit$residuals)/var(PC1)#0.23





library(tseries)

PC1garch<-garch(PC1, order = c(1, 1), series = NULL, control = garch.control(maxiter = 200, trace = TRUE, start = NULL,
                                                                   grad = c("analytical","numerical"), abstol = max(1e-20, .Machine$double.eps^2),
                                                                   reltol = max(1e-10, .Machine$double.eps^(2/3)), xtol = sqrt(.Machine$double.eps),
                                                                   falsetol = 1e2 * .Machine$double.eps))
PC1garch<-garch(PC1, order = c(1, 1))
summary(PC1garch)



require(rugarch)
spec = ugarchspec(variance.model = list(model = 'eGARCH', garchOrder = c(2, 1)), distribution = 'std')
fit = ugarchfit(spec, PC1[1:1000], solver = 'hybrid')
forc1 = ugarchforecast(fit, n.ahead = 5)

percentage = 0
for(i in 1100:1120){
  fit = ugarchfit(spec, PC1[1:i], solver = 'hybrid')
  forc1 = ugarchforecast(fit, n.ahead = 1)
  ypred = forc1@forecast$seriesFor[1,1]
  yreal = PC1[i+1]
  percentage = percentage + abs((ypred-yreal)/yreal)
}
percentage/20






RMSEwithoutsum = function(m, o){
  sqrt((m - o)^2)
}
library(forecast)
ahead = 5
j=1
RMSEtotal<-0
dayspredict<-100
for( i in 1:dayspredict){
  #armafit <- auto.arima(PC1[(0+i):(3500+i)])#arima(aa2[,i],order=c(1,0,0))
  armafit<-arima(PC1[(0+i):(1200+i)],order=c(1,0,0))
  b<-forecast(armafit,h=ahead)
  predictb<-as.numeric(b$mean)
  actual<-PC1[(1201+i):(1201+ahead-1+i)]
  #RMSEtotal = RMSEtotal + RMSEwithoutsum(predictb,actual)
  RMSEtotal = RMSEtotal +abs((predictb-actual)/actual)
}
#Variancetotal <- (RMSEtotal/(dayspredict))^2
#Varmean <- var(PC1)
#Rsquare = Variancetotal/Varmean
RMSEtotal/dayspredict






















RMSEwithoutsum = function(m, o){
  sqrt((m - o)^2)
}
library(forecast)
ahead = 5
j=1
RMSEtotal<-0
dayspredict<-100
for( i in 1:dayspredict){
  #armafit <- auto.arima(aa2[(0+i):(2000+i),j])#arima(aa2[,i],order=c(1,0,0))
  armafit<-arima(aa2[(0+i):(6000+i),j],order=c(1,0,0))
  b<-forecast(armafit,h=ahead)
  predictb<-as.numeric(b$mean)
  actual<-aa2[(6001+i):(6001+ahead-1+i),j]
  #RMSEtotal = RMSEtotal + RMSEwithoutsum(predictb,actual)
  RMSEtotal = RMSEtotal +abs((predictb-actual)/actual)
}
RMSEAR <- RMSEtotal/(dayspredict)
RMSEAR

springdf<-PC1[31:300]
for(i in 1:19){
  springdf<-append(springdf,PC1[(31+i*365):(300+i*365)])
}
winterdf<-PC1[c(1:30,301:365)]
for(i in 1:19){
  winterdf<-append(winterdf,PC1[(1+i*365):(30+i*365)])
  winterdf<-append(winterdf,PC1[(301+i*365):(365+i*365)])
}

library(biwavelet)
wt1=wt(cbind(1:1300, winterdf[1:1300]))
plot(wt1, type="power.corr.norm", main="Bias-corrected")










#For hmm
library(depmixS4)
PC1<-as.data.frame(aa2[,1])
AICHMMlist<-c()
LoglikHMMlist<-c()
for(i in 3:8){
  mod <- depmix(list(aa2[, 1]~1), family=list(gaussian()),data=PC1, nstates=i,ntimes=7300)
  HMMfit<-fit(mod)
  #before fitting the mod
  #AIC <- AIC(mod)
  #BIC <- BIC(mod)
  AIC <- AIC(HMMfit)
  AICHMMlist<-c(AICHMMlist,AIC)
  LoglikHMMlist<-c(LoglikHMMlist,logLik(HMMfit))
}




library(NHMSAR)
#mydata<- aa2[,1]
T = 3500
d = 1
N.samples = 1
new.arr <- array( unlist(mydata2), dim=c(T, N.samples,d) ) 


#number of regimes
M = 4
#order of AR processes
order = 2
theta.init = init.theta.MSAR(new.arr,M=M,order=order,label="HH")
mod.hh = fit.MSAR(new.arr,theta.init,verbose=TRUE,MaxIter=100)

Bsim = 100
Ksim = Bsim*N.samples
Y0 = array(new.arr[1:2,1,],c(2,Ksim,d))
T = 3600
Y.sim = simule.nh.MSAR(mod.hh$theta,Y0 = Y0,T,N.samples = Ksim)

Ysimmedian<-apply(Y.sim$Y[,,1],1, median, na.rm = TRUE)

percentageNHMSAR<-mean(abs((PC1[1:3500]-Ysimmedian)/PC1[1:3500]))
#0.544

Ysimmedianpredict<-Ysimmedian[3501:3600]
#Yreal<-aa2[7001:7100,1]
Yreal<-PC1[3501:3600]
RMSENHMSAR<-RMSEwithoutsum(Yreal,Ysimmedianpredict)
Variancetotal <- (RMSENHMSAR/(1))^2
Varmean <- var(PC1)
Rsquare = Variancetotal/Varmean

precentage<-abs((Ysimmedianpredict-Yreal)/Yreal)


####SETAR model
library(tsDyn)

llar.predict(aa2[1:7100,1], m=2, d=1, steps=d, n.ahead=10,eps=1,onvoid="enlarge", r=5)

mod.setar <- setar(aa2[,1], m=2, thDelay=1)

predict(mod.setar, aa2[1:7100,1], n.ahead=10)











load("data2.Rda")
df<-T3[,4:334]
firstday<-c(1,366,731,1096,1461,1826,2191,2556,2921,3286,3651,4016,4381,4746,5111,5476,5841,6206,6571,6936)
df2<-df
for(i in 360:364){
  for(location in 1:331){
    df2[(firstday+i),(location)]<-0
}}
row_sub = apply(df2, 1, function(row) all(row !=0 ))
##Subset as usual
df3<-df2[row_sub,]

n=30
mydata<-aggregate(df3,list(rep(1:(nrow(df3)%/%n+1),each=n,len=nrow(df3))),mean)[-1]


mydata3<-mydata/1000
mydata3<-as.matrix(mydata3)

library(rstan)


schools_dat <- list(t = 240,
                    j=12,
                    k=10,
                    x = mydata3[,1:10])



fit <- stan(file = '8schools.stan', data = schools_dat,iter = 2000,chains = 3, control = list(adapt_delta = 0.99))



list_of_draws <- extract(fit)
names(list_of_draws)
head(list_of_draws$vari)
list_of_draws$vari[3000,,]
list_of_draws$a[3000,,]
list_of_draws$u[3000,,]

list_of_draws$uj[3000,]
list_of_draws$usigmaj[3000,]
list_of_draws$usigma[3000]
#list_of_draws$vsigmaj[3000,]
#list_of_draws$vsigma[3000]
list_of_draws$asigmaj[3000,]
list_of_draws$asigma[3000]
list_of_draws$uv[3000]
list_of_draws$av[3000]
#list_of_draws$vv[3000,]













##make up some data to check
j = 12
k = 10
t = 240
u = matrix(0.2,j,k)
a = matrix(0.7,j,k)
uv = 0.2
av = 0.2
usigma = 0.05
asigma = 0.05
uk = rnorm(k,mean = uv , sd= usigma)
ak = rnorm(k,mean = uv , sd= usigma)
usigmak = rep(0.05,k)
asigmak = rep(0.05,k)
for(n in 1:j){
  for(m in 1:k){
    u[n,m] = rnorm(1, mean = uk[m], sd = usigmak[m]);
    a[n,m] = rnorm(1, mean = ak[m], sd = asigmak[m]);
}}
v = 0.1*(1 - a^2)

mydata4=matrix(0,t,k)
mydata4[1,]<-mydata3[1,1:k]
for(n in 2:t){
  for(m in 1:k){
    currentx = u[n%%12+1,m] + a[n%%12+1,m] * (mydata4[n-1,m] - u[(n-1)%%12+1,m]) + rnorm(1, mean = 0, sd = v[n%%12+1,m])
    mydata4[n,m]<-currentx
}}
schools_dat <- list(
  t = t,
  j=j,
  k=k,
  x = mydata4)

fit <- stan(file = '8schools.stan', data = schools_dat,iter = 2000,chains = 2, control = list(adapt_delta = 0.99))

list_of_draws <- extract(fit)
names(list_of_draws)











##a-0.5,u-0.5, sigma very small, a2 will be incorrect
##a-0.5,u-0.2, sigma very small, u0u1u2 will be incorrect / a1v,uu0 nor correct  / 
##a-0.0 u-0.5, sigma very small, all correct
##a-0.7 u-0.5, sigma very small, all correct
##a-0.7 u-0.2, sigma very small, all correct

##the problem is they are not converging with 20 years

##make up some data to check
j = 12
k = 10
t = 240
a0v = a1v = a2v = 0.5
a0sigma = a1sigma = a2sigma = 0.005
a0 = rnorm(k, mean = a0v, sd = a0sigma)
a1 = rnorm(k, mean = a1v, sd = a1sigma)
a2 = rnorm(k, mean = a2v, sd = a2sigma)
#a0 = rep(0.5,k)
#a1 = rep(0.1,k)
#a2 = rep(0.1,k)
u0v = u1v = u2v = 0.2
u0sigma = u1sigma = u2sigma = 0.005
u0 = rnorm(k, mean = u0v, sd = u0sigma)
u1 = rnorm(k, mean = u1v, sd = u1sigma)
u2 = rnorm(k, mean = u2v, sd = u2sigma)
#u0 = rep(0.5,k)
#u1 = rep(0.1,k)
#u2 = rep(0.1,k)
u = matrix(0,j,k)
a = matrix(0,j,k)
asigmak = rep(0.0005,k)
usigmak = rep(0.0005,k)
for(n in 1:j){
  for(m in 1:k){
    a[n,m] = rnorm(1, mean = 0.5* (a0[m] + a1[m]*sin(n*3.1415926/6)+a2[m]*cos(n*3.1415926/6)), sd = asigmak[m]);    
    u[n,m] = rnorm(1, mean = u0[m] + u1[m]*sin(n*3.1415926/6)+u2[m]*cos(n*3.1415926/6), sd = usigmak[m]);
  }}

v = 1-a^2

mydata4=matrix(0,t,k)
mydata4[1,]<-mydata3[1,1:k]
for(n in 2:t){
for(m in 1:k){
  currentx = u[n%%12+1,m] + a[n%%12+1,m] * (mydata4[n-1,m] - u[(n-1)%%12+1,m]) + rnorm(1, mean = 0, sd = v[n%%12+1,m])
  mydata4[n,m]<-currentx
}}





n=240
jj=1:n
j=jj%%12+0.5
x=matrix(ncol=3,nrow=240)
x[,1]=rep(1,240)
x[,2]=sin(j*pi/6)
x[,3]=cos(j*pi/6)
#check
uc=c(0,1,1)/sqrt(2)
ac=c(0.5,0,0)
u=x%*%uc
a=x%*%ac

v = 1-a^2

mydata4=matrix(0,t,k)
mydata4[1,]<-mydata3[1,1:k]
for(n in 2:t){
  for(m in 1:k){
    currentx = u[n] + a[n] * (mydata4[n-1,m] - u[(n-1)]) + rnorm(1, mean = 0, sd = v[n])
    mydata4[n,m]<-currentx
  }}



schools_dat <- list(
  t = t,
  k=k,
  x = x,
  mydata = mydata4)


fit <- stan(file = '8schools4.stan', data = schools_dat,iter = 2000,chains = 2, control = list(adapt_delta = 0.99))

list_of_draws <- extract(fit)
names(list_of_draws)


library(pear)
site1<-mydata4[,1]
site1t<-ts(site1,frequency = 12)
result<-pear(site1t, m=1,ic="none")
names(result)
(result$phi)
(list_of_draws$a[3000,,1])
# mydata4[c(12,24,36,48,60,72,84,96,108,120),1]

totaldifference = 0 
for(n in 1:j){
  for(m in 1:k){
    totaldifference = totaldifference + abs((list_of_draws$vari[2000,,][n,m] - v[n,m])/v[n,m])
  }
}
totaldifference/(j*k)


totaldifference = 0 
for(n in 1:j){
  for(m in 1:k){
    totaldifference = totaldifference + abs((list_of_draws$u[2000,,][n,m] - u[n,m])/u[n,m])
  }
}
totaldifference/(j*k)

totaldifference = 0 
for(n in 1:j){
  for(m in 1:k){
    totaldifference = totaldifference + abs((list_of_draws$a[2000,,][n,m] - a[n,m])/a[n,m])
  }
}
totaldifference/(j*k)











#####samelocation

uv = 0.5
#av = 0.2
usigma = 0.1
#asigma = 0.3
j = 12
t = 240
#u= rnorm(j, mean = uv, sd = usigma)
#a= rnorm(j, mean = av, sd = asigma)

#v = runif(k,min=0.8,max=1.2)

alpha = 1 
beta = 1
# u = 1/2
# sigma = 1/(12)
#u= rbeta(j, alpha, beta, ncp = 0)
u= rnorm(j, mean = uv, sd = usigma)
a= rbeta(j, alpha, beta, ncp = 0)

v = 1-a^2



mydata4<-rep(0,t)
mydata4[1]<-mydata3[1,1]

for(n in 2:t){
  currentx = u[n%%12+1] + a[n%%12+1] * (mydata4[n-1] - u[n%%12+1]) + rnorm(1, mean = 0, sd = v[n%%12+1])
  mydata4[n]<-currentx
}

schools_dat <- list(
  t = t,
  j=j,
  x = mydata4)

fit <- stan(file = 'samelocation.stan', data = schools_dat,iter = 2000,chains = 3, control = list(adapt_delta = 0.99))

list_of_draws <- extract(fit)
names(list_of_draws)
head(list_of_draws$vari)

list_of_draws$vari[3000,]
list_of_draws$a[3000,]
list_of_draws$u[3000,]


totaldifference = 0 
for(n in 1:j){
    totaldifference = totaldifference + abs((list_of_draws$vari[3000,][n] - v[n])/v[n])
  }

totaldifference/(j)
totaldifference = 0 
for(n in 1:j){
  totaldifference = totaldifference + abs((list_of_draws$a[3000,][n] - a[n])/a[n])
}

totaldifference/(j)
totaldifference = 0 
for(n in 1:j){
  totaldifference = totaldifference + abs((list_of_draws$u[3000,][n] - u[n])/u[n])
}

totaldifference/(j)


library(MASS)
fitdistr(a,"beta",list(shape1=1,shape2=1))
fitdistr(u, "normal")









#####sametime

uv = 0.5#0.2
av = 0.5
usigma = 0.1
asigma = 0.1
t = 2400
k = 10

#u = a/(a+b)
#sigma = ab/((a+b)^2 * (a+b+1))
alpha = 1 
beta = 1
# u = 1/2
# sigma = 1/(12)
#u= rbeta(k, alpha, beta, ncp = 0)
u= rnorm(k, mean = uv, sd = usigma)
#a= rbeta(k, alpha, beta, ncp = 0)
a= rnorm(k, mean = av,sd = asigma)

v = 1-a^2


mydata4=matrix(0,t,k)
mydata4[1,]<-mydata3[1,1:k]
for(n in 2:t){
  for(m in 1:k){
    currentx = u[m] + a[m] * (mydata4[n-1,m] - u[m]) + rnorm(1, mean = 0, sd = v[m])
    mydata4[n,m]<-currentx
}}


schools_dat <- list(
  t = t,
  k = k,
  x = mydata4)


fit <- stan(file = 'sametime.stan', data = schools_dat,iter = 2000,chains = 3, control = list(adapt_delta = 0.99))

list_of_draws <- extract(fit)
names(list_of_draws)
head(list_of_draws$vari)

list_of_draws$vari[3000,]
list_of_draws$a[3000,]
list_of_draws$u[3000,]





totaldifference = 0 
for(n in 1:k){
  totaldifference = totaldifference + abs((list_of_draws$vari[3000,][n] - v[n])/v[n])
}

totaldifference/(k)
totaldifference = 0 
for(n in 1:k){
  totaldifference = totaldifference + abs((list_of_draws$a[3000,][n] - a[n])/a[n])
}

totaldifference/(k)

totaldifference = 0 
for(n in 1:k){
  totaldifference = totaldifference + abs((list_of_draws$u[3000,][n] - u[n])/u[n])
}

totaldifference/(k)











totaldifference = 0 
for(n in 1:k){
  totaldifference = totaldifference + (list_of_draws$u[3000,][n] - u[n])^2
}
totaldifference / var(u)





