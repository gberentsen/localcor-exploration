library(Deriv)

#Compute f, f1, and f11 for gumbel f with shape=a, scale=s
f <- function(x,a,s){1/(s ^ a*gamma(a))*x ^ (a-1)*exp(-(x/s))}
f1 <- Deriv(f,"x")
f11 <- Deriv(f1,"x")

#Compute local parameters
mu <- function(x,a,s){x + f1(x,a,s)*f(x,a,s)/(f1(x,a,s) ^ 2 - f11(x,a,s)*f(x,a,s))}
sigma <- function(x,a,s){sqrt(f(x,a,s) ^ 2/(f1(x,a,s) ^ 2 - f11(x,a,s)*f(x,a,s)))}

#Create normal distribution with the defined local parameters

norm.local <- function(x,a,s){dnorm(x,mean=mu(x,a,s),sd=sigma(x,a,s))}

#Compare, with gumbel shape=5, scale=s. (convex'ish)
x=seq(1,40,1)
plot(x,f(x,5,1),type = "l")
lines(x,norm.local(x,5,1),lty = 2)

