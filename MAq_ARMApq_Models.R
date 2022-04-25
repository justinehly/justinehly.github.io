'''
AR autoregressive
MA moving average
ARMA autoregressive moving average
AR(p)
MA(q)
ARMA(p,q)
'''
library(tswge)


# Generate MA(1)
gen.arma.wge(n=100, theta=-0.99, sn=5)
gen.arma.wge(n=100, theta=0.99, sn=5)

plotts.true.wge(theta=c(.99))
plotts.true.wge(theta=c(-.99))


# Generate MA(2)
gen.arma.wge(n=100, theta = c(0.9,-0.4), sn=5)
gen.arma.wge(n=100, theta = c(-0.9,0.4), sn=5)
plotts.true.wge(theta = c(0.9,-0.4))

plotts.true.wge(theta = -.17)


# AR(2)
plotts.true.wge(phi=c(1.1,-.9))

# MA(2)
plotts.true.wge(theta = c(-1.1,.9))

# Invertabity
factor.wge(phi = c(1.6,-.9)) # yes invertable because |Z|-1 is < 1

factor.wge(phi = c(-.1,.3))

factor.wge(phi=c(-.1,-.82,.16))

# blend of AR(p) and MA(q)
# AR(4) Factors
plotts.true.wge(phi = c(.3,.9,.1,-.8075))
factor.wge(phi = c(.3,.9,.1,-.8075))


# MA(3) Factors
plotts.true.wge(theta = c(-.9,-.8,-.72))
factor.wge(phi = c(-.9,-.8,-.72))


# ARMA(4,3)
plotts.true.wge(phi = c(.3,.9,.1,-.8075), theta = c(-.9,-.8,-.72))

# 5.5.10 question
plotts.true.wge(phi = c(.1,-.5,.7), theta = c(.72,-.8))


plotts.wge(jetA$Price)

setwd('C:/Users/justi/GitHub/MSDS-6373-Time-Series/Unit 3')
jetA = read.csv('TexasJetA.csv', header = TRUE)
jetA
plotts.wge(jetA$Price)

aic.wge(jetA$Price, p=1, q=0)$value  # this fits AR(1) model and gives the AIC value
aic.wge(jetA$Price, p=2, q=0)$value # fits AR(2) model
aic.wge(jetA$Price, p=1, q=1)$value # fits ARMA(1,1) model
aic5.wge(jetA$Price)  # provides top 5 models based on AIC


swa = read.csv('SWADelay.csv', header = TRUE)
plotts.wge(swa$arr_delay)
plotts.sample.wge(swa$arr_delay)
aic5.wge(swa$arr_delay)

aic5.wge(swa$weather_delay)


# psi-weights for simple MA(1) model X(t)=91-.8B) = a(t)
psi.weights.wge(theta = .8, lag.max = 5)

# psi-weights for simple AR(1) model (1-.8B)X(t) = a(t)
psi.weights.wge(phi = .8, lag.max = 5)

# psi-weights for ARMA(2,1) model (1-1.2B+.6B)X(t) = (1-.5B)a(t)
psi.weights.wge(phi = c(1.2,-.6), theta=c(.5), lag.max = 5)

# concept check
psi.weights.wge(phi = c(1.95,-1.9), lag.max=5)


#####################################
# For live session
# Use Aic5 to assess the use of ARMA models in the Walmart data.

wal = read.csv('Walmart.csv')
summary(wal)
store = wal %>% dplyr::filter(store == 5 & item == 25)
store

plot(store$sales, type='l', 
     main = 'Walmart Store 5, Item 25 Sales',
     ylab = 'Sales')
 parzen.wge(store$sales)

n = 5
store5<- stats::filter(store$sales, rep(1,n)/n)
plot(store5, type='l', 
     main = 'Walmart Store 5, Item 25 Sales 5-point Moving Average',
     ylab = 'Sales')


n = 51
store51<- stats::filter(store$sales, rep(1,n)/n)
plot(store51, type='l', 
     main = 'Walmart Store 5, Item 25 Sales 51-point Moving Average',
     ylab = 'Sales')

aic5.wge(store$sales)
store$date


g = gen.arma.wge(n=200, phi=c(.4,-.5, .6), theta = c(.5,-.6), sn = 42)
p = plotts.true.wge(phi=c(.4,-.5, .6), theta = c(.5,-.6))

setwd('C:/Users/justi/GitHub/MSDS-6373-Time-Series/Unit 5')
swa = read.csv('SWADelay.csv', header = TRUE)
aic5.wge(swa$arr_cancelled)

### 4 - Live
library(tswge)
factor.wge(phi=c(.25,.7))

# AR(2) with peak at 0 and .5
AR1_2R_Both = gen.arma.wge(n = 1000, phi = c(.25,.7))
parzen.wge(AR1_2R_Both)


# AR2 peak at 0
factor.wge(phi=c(1.1, -.3))
AR1_2R_0 = gen.arma.wge(n=100, phi=c(1.1,-.3))
parzen.wge(AR1_2R_0)


# AR2 peak at .5
factor.wge(phi=c(-1.5,-.56))
plotts.true.wge(n=100, phi=c(-1.5,-.56))
ar1_2r_pt5 = gen.arma.wge(n=100, phi=c(-1.5,-.56))
parzen.wge(ar1_2r_pt5)

# complex, peak between 0 and 0.5
factor.wge(phi = c(.8,-.9))
plotts.true.wge(n=100, phi = c(.8,-.9))
ar1_complex_between = gen.arma.wge(n=100, phi = c(.8,-.9))
parzen.wge(ar1_complex_between)


p = c(.8,-.5)
a = plotts.true.wge(theta = p)
a$aut1  # autocorrelations

b = plotts.true.wge(theta=.5)
b$aut1

# psi weights using the phi from the AR side of the model
psi.weights.wge(phi = .8, lag.max = 5)
psi.weights.wge(phi = .8, lag.max = 10)
