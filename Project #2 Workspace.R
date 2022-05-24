#Part I: Historical Analysis#
# ------------------------------------------------------------------------------------------------------------#
#packages
library('quantmod')
library('fBasics')
library('dplyr')
library('forecast')
library('tseries')

#loading BTC
sp500 <- new.env()
getSymbols('BTC-USD', env = sp500,src="yahoo",from = as.Date('2018-01-01'),to = as.Date("2021-06-28"))
ls()
BTC <- sp500$`BTC-USD`
BTCClose <- BTC$`BTC-USD.Close`
is.na(BTCClose)
BTCClose <- na.omit(BTCClose)

#Graphics
#i.) timeseries of BTCAdj
plot.xts(BTCClose,main = "Bitcoin Price Time Series \n From January 2018 to June 2021",
         xlab = "Date", ylab = "Price")
#a.) Test for data for stationary
#basic statistics
basicStats(BTCClose)
#standardize BTCClose
z <- (BTCClose - mean(BTCClose))/sd(BTCClose)
mean(z)
sd(z)
basicStats(z)
x <- seq(-3,3,0.1)
basicStats(dnorm(x,0,1))
#b.)histogram of standardized BTCClose
hist(z,freq = FALSE,breaks = 25)
x <- seq(-3,3,0.1)
curve(dnorm(x,0,1),add=TRUE,col = "red")

#c.) Lag Plot of Standardized BTCClose
lagmax <- 10*log10(1276/6)
lagmax
lag.plot(z, main = "Lag Plot of Standardized Bitcoin Adjusted Price",diag.col = "forest green")
abline(v=0,h=0)
acf(z, main = "ACF for Standardized BTC Closing Price",xlab ="Lag Number",
    ylab = "ACF",lag.max = length(z))

#d.) Augments Dickey Fuller Test
adf.test(z)
#SSince the Lag plot is positive and linear this implies that an autoregressive model is appropriate. 

#ii.) timeseries of log(BTCClose)
LogBTCClose <- log(BTCClose)
dailyLogBTCClose <- dailyReturn(LogBTCClose)
plot.xts(dailyLogBTCClose, main = "Daily Log Returns of Bitcoin \n From January 2018 to June 2021",
         xlab = "Day", ylab = "Price")
#a.)Test for data for stationary
#basic statistics
basicStats(dailyLogBTCClose)
#Standardize dailyLogBTCAdj
z_1 <- (dailyLogBTCClose - mean(dailyLogBTCClose))/sd(dailyLogBTCClose)
mean(z_1)
sd(z_1)
#basic statistics for standardized dailyLogBTCAdj
basicStats(z_1)
samplemean <- sum(z_1)/length(z_1)
samplemean

samplevar <- sum(z_1 - samplemean)/(length(z_1)-1)
samplevar

truemean <- (samplemean/(1/365))+(samplevar/2)
truemean

truevar <- samplevar*(1/365)
truevar

#b.)Histogram of standardized dailyLogBTCAdj
hist(z_1,breaks = 25,freq = FALSE)
curve(dnorm(x,0,1),col = "red",add = TRUE)


#c.)Lag Plot of the standardized dailyLogBTCClose
plot.new()
frame()
par(mfcol=c(1,2))
lag.plot(z, main = "Lag Plot of Standardized BTC Closing Price",diag.col = "forest green")
acf(z,lag.max = lagmax,xlab = "Lag Number", ylab = "ACF", main = "ACF for Standardized BTC Closing Price")
lag.plot(z_1,main = "Lag Plot of Standardized Log Daily Closing BTC Price ",diag.col = "forest green")
acf(z_1,lag.max = length(z_1), xlab ="Lag Number",
    ylab = "ACF",main = "ACF for Standardized Log BTC Closing Price")

#d.) Augments Dickey Fuller Test
adf.test(z_1)

# ------------------------------------------------------------------------------------------------------------#
#Part 2: Options Analysis
# ------------------------------------------------------------------------------------------------------------#
#Packages
library('pins')
library('skimr')
library("ragtop")
library('ggplot2')
library("ggthemes")

##Setting up the data##
#Loading BTC option data
head(Options_Data_Derebit_Compiled)

#Understanding BTC option data
dim(Options_Data_Derebit_Compiled)
mode(Options_Data_Derebit_Compiled)
summary(Options_Data_Derebit_Compiled)
colnames(Options_Data_Derebit_Compiled)

#Renaming Two Columns
colnames(Options_Data_Derebit_Compiled)[5] <- 'USD Call Price'
colnames(Options_Data_Derebit_Compiled)[7] <- "USD Put Price"
colnames(Options_Data_Derebit_Compiled)

#declaring variables
S_col <- Options_Data_Derebit_Compiled[[1]]
T_col <- Options_Data_Derebit_Compiled[[2]]/365
K_col <- Options_Data_Derebit_Compiled[[3]]
CP <- Options_Data_Derebit_Compiled[[5]]
PP <- Options_Data_Derebit_Compiled[[7]]
predicted_CP <- rep(0,nrow(Options_Data_Derebit_Compiled))
predicted_PP <- rep(0,nrow(Options_Data_Derebit_Compiled))

##Bullet Point #1##
#adding one columns
# Call Price + Present Value of Strike (left side)
ls <- CP + K_col*exp(-0.05*T_col)
ls

#Put + Stock Price
rs <- PP + S_col
rs

#putting lists into a cumulative list
dta <- list(S = S_col,Time = T_col/365, K = K_col,
            Call = CP, Put = PP, Predict.CP = predicted_CP,
            Predict.PP = predicted_PP)
#making lists into a data set
df <- as.data.frame(do.call(cbind,dta))
colnames(df)[6] <- "BSM.Call.Price"
colnames(df)[7] <- "BSM.Put.Price"
View(df)

##Bullet Point #2##
#Computing prices of calls and puts
#call options from the data set
for (i in 1:77) {
  predicted_CP[i] <- blackscholes(1,S0=S_col[i], K = K_col[i], r = 0.05, time = T_col[i],vola = 1)
}

#put options from the data set
for (j in 1:77) {
  predicted_PP[j] <- blackscholes(-1,S0 = S_col[j],K = K_col[j],r = 0.05, time = T_col[j],vola = 1)
}

#graphics
ggplot(df) +
  geom_point(aes(x=as.numeric(K),y=as.numeric(BSM.Call.Price),color = "red"))+ 
  geom_point(aes(x=as.numeric(K),y=as.numeric(Call),color = "orange"))+
  geom_point(aes(x=as.numeric(K),y=as.numeric(Put),color = "purple"))+
  geom_point(aes(x=as.numeric(K),y=as.numeric(BSM.Put.Price), color = "blue"))+
  labs(x = "Strike Price", y = " Option Price", title = " Option Prices versus Black-Scholes-Merton Pricing")+
  theme_clean() + scale_color_manual(values = c(red = "red",orange = "orange",purple = "purple",blue = "blue"),name = "BTC Price", labels = c("BSM Call Option","Call Option", "Put Option","BSM Put Option" )) 













