# Import packages
library(zoo)
library(fpp2)
library(vars)
library(glmnet)
library(sandwich)
library(lubridate)
library(dplR)
library(tseries)

#Import Data
data <- read.table("dataset.csv", sep=",", header=TRUE)
index <- as.data.frame(data$indpro, start=c(1970,1), frequency=4)
names(index) <- "indpro"

#Plot the Data
index <- ts(data$indpro, start=c(1970,1), frequency=4) # turn data into time-series object
autoplot(window(index, start=1970, frequency=4), series="Index") +
  ggtitle("Figure 1: Industrial Production Index (Seasonally Adjusted)")+ 
  ylab("Index") + 
  xlab("Time") +
  theme(plot.title = element_text(size = 11))

#Transform the Data:
indpro <- 100*(diff(log(data$indpro)))
#Store data in a time-series object
x0 <- ts(cbind(indpro), start=c(1970,1), frequency=4)
#plot original dataset
autoplot(x0, series = 'Growth Rate') + ylab("Log-Difference (%)") + 
  xlab("Time") + 
  ggtitle("Figure 2: Industrial Production Index Growth Rate") +
  theme(plot.title = element_text(size = 11))

#Creates Lagged Variables
data <- read.table("dataset.csv", 
                   sep=",", 
                   header=TRUE)
#Transform the Data:
indpro <- 100*(diff(log(data$indpro)))
#Store data in a time-series object
x0 <- ts(cbind(indpro), 
         start=c(1970,1), 
         frequency=4)
#Set values for lags
n.lag=8 # eight quarterly lags
n.var = dim(x0)[2] # take the number of variables
seq.l = seq(0,-n.lag) # generate a sequence from zero to the allowed number of lags.

# Creating Lagged Data Set
xl <- ts(do.call(cbind,lapply(1:ncol(x0), 
                              function(z) lag(as.zoo(x0[,z]),seq.l))), 
         start=start(x0), 
         end=end(x0), 
         frequency=frequency(x0))
# Name variables in new data set by adding the respective lag
colnames(xl) <- as.vector(sapply(colnames(x0), 
                                 function(z) paste(z,"_l",-seq.l, sep = "")))

#Prepare the data inputs for LASSO
window.start = c(1980,1)
window.end = end(xl)
xt <- window(xl, start=window.start, end=window.end)
# dim(xt)
n.obs <- nrow(xt)
#Set up the variables for LASSO
h=1
yt <- xt[,"indpro_l0"]
y <- yt[(1+h):n.obs]
X <- xt[1:(n.obs-h),]
x.new <- matrix(xt[n.obs,], nrow = 1)
#LASSO Regression
lasso <- glmnet(X,y)
#LASSO Coefficients
plot(lasso, xvar='lambda')
mtext("Figure 3: LASSO trace plot", side=3, line=2.5, cex=0.9)

#Relevant Coefficients
bb <- lasso$beta[,24]
bb[bb!=0] # nonzero beta coefficients
lasso$a0[24] # intercept 
lasso$lambda[24] # lambda value

#Set Variables for Forecast
h=1
yt <- xt[,"indpro_l0"]
y <- yt[(1+h):n.obs]
X <- xt[1:(n.obs-h),]
x.new <- matrix(xt[n.obs,], nrow = 1)
lasso <- glmnet(X,y)
#Restricting Maximum Number of Variables
n.var.max <- 5
ind.lambda <- max(which(lasso$df<=n.var.max))
lambda <- lasso$lambda[ind.lambda]
plot(lasso,xvar='lambda')
abline(v=log(lambda),col='gray',lty=2, lwd=3)
mtext("Figure 4: LASSO trace plot", side=3, line=2.5, cex=0.9)

#Loop for creating a h-step ahead forecast
n.fc=12  #Forecast Horizon
fcast.lasso <- rep(NA,n.fc)
for (h in 1:n.fc){
  y <- yt[(1+h):n.obs]
  X <- xt[1:(n.obs-h),]
  x.new <- matrix(xt[n.obs,],nrow=1)
  lasso <- glmnet(X,y)
  #Stores forecast values
  fcast.lasso[h] <- predict(lasso,x.new,s=lambda)
}

#Plot Forecast
yt.fc.lasso <- ts(c(yt,fcast.lasso), start=window.start, freq=4)
autoplot(window(yt.fc.lasso,start=c(2012,1)), series='LASSO Forecast') +  
  autolayer(window(yt, start=c(2012,1)), series='Index growth rate') + 
  ylab("Log-Difference (%)") + 
  xlab("Time") +
  ggtitle("Figure 5: Industrial Production Index Growth Rate") +
  theme(plot.title = element_text(size = 11))

h=1
yt <- xt[,"indpro_l0"]
y <- yt[(1+h):n.obs]
X <- xt[1:(n.obs-h),]
lasso <- glmnet(X,y)
lambda.seq <- lasso$lambda # store lambdas as another variable
length(lambda.seq) # shows the maximum number of lambda values

forecast.horizon = 12
n.fc <- forecast.horizon
lasso.multi.fcast <- matrix(NA, nrow=n.fc, ncol=length(lambda.seq))

for (h in 1:n.fc){
  y <- yt[(1+h):n.obs]
  X <- xt[1:(n.obs-h),]
  x.new <- matrix(xt[n.obs,],nrow=1) 
  lasso <- glmnet(X,y)
  lasso.multi.fcast[h,] <- predict(lasso, x.new, type='response', s=lambda.seq)
}

lasso.multi.fcast <- ts(lasso.multi.fcast, 
                        start=c(end(yt)[1],end(yt)[2]+1), 
                        freq=4) # start period is one period after the end of the variable
autoplot(window(yt, start=c(2012,1)), series='Index growth rate') +
  autolayer(lasso.multi.fcast) + theme(legend.position="none") + 
  ylab("Log-Difference (%)") + 
  xlab("Time") +
  ggtitle("Figure 6: Multiple Alternative Forecasts") +
  theme(plot.title = element_text(size = 11))

h=1
yt <- xt[,"indpro_l0"]
y <- yt[(1+h):n.obs]
X <- xt[1:(n.obs-h),]
lasso <- glmnet(X,y)
lambda.seq <- lasso$lambda

#Cross Validation
# end of training set, 2005Q4
n.end <- 2006.75
len <- length(yt)-length(window(yt, start=window.start, end=n.end)) #length of test set
# test set: 2007Q1 - 2019Q2
# set matrix for storage, 50 obs in test set; dimension of array based on max number of lambdas
lasso.cv <- array(NA, dim=c(len,length(lambda.seq)))
yt.oos <- matrix(rep(NA),len,1)
# for-loop
for(i in 1:len){
  tmp0 <- 1980
  tmp1 <- n.end+(i-1)*1/4
  #variables for LASSO
  xt <- window(xl,tmp0,tmp1)
  n.obs <- nrow(xt)
  yt <- xt[,"indpro_l0"]
  y <- yt[(1+h):n.obs]
  X <- xt[1:(n.obs-h),]
  x.new <- matrix(xt[n.obs,], nrow = 1)
  #LASSO
  lasso <- glmnet(X,y, lambda=lambda.seq)
  # compute forecasts
  yt.oos[i,1] <- window(x0,tmp1+1/4,tmp1+1/4)
  lasso.cv[i,] <- predict(lasso,x.new,type='response',s=lambda.seq) #LASSO Model
}

#setup variables for plotting
yt.oos <- ts(yt.oos, start = c(2007,1), end=c(2019,2), freq=4)
pred.oos.h1 <- ts(lasso.cv, start = c(2007,1), end=c(2019,2), freq=4)
#Plot
autoplot(pred.oos.h1) + theme(legend.position="none")+
  autolayer(yt.oos) + ylab("Log-Difference") + xlab("Time") +
  ggtitle("Figure 6: Out-of-sample forecasts in the test set") +
  theme(plot.title = element_text(size = 11))

# compute rmse
yt.oos <- window(yt.oos, 
                 start=c(2007,1), 
                 end=c(2019,2), 
                 frequency=4)
rmse.lasso <- rep(NA,length(lambda.seq))
for(m in 1:length(lambda.seq)){rmse.lasso[m] <- sqrt(mean((yt.oos-lasso.cv[,m])^2))}
rmse.min <- min(rmse.lasso)
lambda.cv <- data.frame(cbind(rmse.lasso,lambda.seq))
# Selects column value of lambda that gives min rmse
lambda.cv.min <- which(grepl(rmse.min, 
                             lambda.cv$rmse))
lambda.opt <- lambda.seq[lambda.cv.min] 

# Plot prediction errors
yt.oos <- ts(yt.oos, start = c(2007,1), end=c(2019,2), freq=4) # test sample
Fc.Error <- apply(lasso.cv, 2, function(x) x-yt.oos) # subtract the test sample with the out-of-sample forecasts to get the prediction errors.
Fc.Error.h1 <- ts(Fc.Error, start = c(2007,1), end=c(2019,2), freq=4)
# plot error
autoplot(Fc.Error.h1) + theme(legend.position="none") + ylab("") + xlab("") +
  ggtitle("Figure 7: Out-of-sample forecasts in the test set") +
  theme(plot.title = element_text(size = 11))

# Variance-Bias Tradeoff
lambda.cv <- data.frame(cbind(rmse.lasso, lambda.seq))
ggplot(lambda.cv, 
       aes(x=lambda.seq, y=rmse)) + 
  geom_point(aes(y=rmse.lasso)) + 
  geom_vline(xintercept = lambda.opt, 
             linetype='dashed', 
             color= 'black') + 
  ylab("RMSE") + 
  xlab("Log Lambda") + 
  ggtitle("Figure 8: Variance-Bias Tradeoff") +
  theme(plot.title = element_text(size = 11)) 

#Prepare the data inputs for LASSO
window.start = c(1980,1)
window.end = end(xl)
xt <- window(xl, start=window.start, end=window.end)
#Set Variables for Forecast
h=1
yt <- xt[,"indpro_l0"]
y <- yt[(1+h):n.obs]
X <- xt[1:(n.obs-h),]
x.new <- matrix(xt[n.obs,], nrow = 1)
lasso <- glmnet(X,y)
#Restricting Maximum Number of Variables
n.var.max <- 5
ind.lambda <- max(which(lasso$df<=n.var.max))
lambda <- lambda.opt
plot(lasso,xvar='lambda')
abline(v=log(lambda),col='gray',lty=2, lwd=3)
mtext("Figure 9: LASSO trace plot", side=3, line=2.5, cex=0.9)

n.fc=12  #Forecast Horizon
fcast.lasso <- rep(NA,n.fc)
for (h in 1:n.fc){
  y <- yt[(1+h):n.obs]
  X <- xt[1:(n.obs-h),]
  x.new <- matrix(xt[n.obs,],nrow=1)
  lasso <- glmnet(X,y)
  #Stores forecast values
  fcast.lasso[h] <- predict(lasso,x.new,s=lambda.opt) # predict using optimal lambda 
}

# Plot Forecast
yt.fc.lasso <- ts(c(yt,fcast.lasso), start=window.start, freq=4)
autoplot(window(yt.fc.lasso,start=c(2012,1)), series='LASSO Forecast') +  
  autolayer(window(yt, start=c(2012,1)), series='Index growth rate') + 
  ylab("Log-Difference (%)") + 
  xlab("Time") +
  ggtitle("Figure 10: Validated Forecast of the Industrial Production Index Growth Rate") +
  theme(plot.title = element_text(size = 11))
