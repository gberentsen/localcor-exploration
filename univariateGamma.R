library(Deriv)

#Compute f, f1, and f11 for gumbel f with shape=a, scale=s
f <- function(x,a,s){1/(s ^ a*gamma(a))*x ^ (a - 1)*exp(-(x/s))}
f1 <- Deriv(f,"x")
f11 <- Deriv(f1,"x")

#Find parameters that makes f log-concave <-- a=5, s = 1 OK
r1 <- function(x,a,s){f1(x,a,s) ^ 2 - f11(x,a,s)*f(x,a,s)}
x <- seq(1,40,1)
plot(x,r1(x,5,1),type = "l")

#Compute local parameters
mu <- function(x,a,s){x + f1(x,a,s)*f(x,a,s)/(f1(x,a,s) ^ 2 - f11(x,a,s)*f(x,a,s))}
sigma <- function(x,a,s){sqrt(f(x,a,s) ^ 2/(f1(x,a,s) ^ 2 - f11(x,a,s)*f(x,a,s)))}

#Create psi,psi1 and psi11 with the defined local parameters
d.norm <- function(x,mean,sd){dnorm(x,mean = mean, sd = sd)}
d1.norm <- Deriv(d.norm,"x")
d11.norm <- Deriv(d1.norm,"x")
psi <- function(x,a,s) dnorm(x,mean = mu(x,a,s),sd = sigma(x,a,s))
psi1 <- function(x,a,s) d1.norm(x,mean = mu(x,a,s), sd = sigma(x,a,s))
psi11 <- function(x,a,s) d11.norm(x,mean = mu(x,a,s), sd = sigma(x,a,s))

#Compare f with psi, with gumbel shape=5, scale=1. (convex) <- not equal as expected
x <- seq(1,40,1)
plot(x,f(x,5,1),col = "red", ylab = "density")
lines(x,psi(x,5,1))
legend(x = 35, y = 0.18, pch = c(1,NA),lty = c(NA,1), col = c("red","black"), bty = "n",
       legend = c("f",expression(psi)))

#Compare f1(x)/f(x) with psi1(x)/psi(x)  <- Result OK!
f1.ratio <- function(x,a,s){f1(x,a,s)/f(x,a,s)}
psi1.ratio <- function(x,a,s){psi1(x,a,s)/psi(x,a,s)}

x <- seq(1,40,1)
plot(x,f1.ratio(x,5,1),col = "red",ylab = "ratio")
lines(x,psi1.ratio(x,5,1))
legend(x = 32,y = 2.8, pch = c(1,NA),lty = c(NA,1), col = c("red","black"), bty = "n",
       legend = c(expression(paste(f[1],"/",f)),expression(paste(psi[1],"/",psi))))

#Compare f11(x)/f(x) with psi11(x)/psi(x) <- Result OK!
f11.ratio <- function(x,a,s){f11(x,a,s)/f(x,a,s)}
psi11.ratio <- function(x,a,s){psi11(x,a,s)/psi(x,a,s)}

x <- seq(1,40,1)
plot(x,f11.ratio(x,5,1),col = "red",ylab = "ratio")
lines(x,psi11.ratio(x,5,1))
legend(x = 32,y = 4.5, pch = c(1,NA),lty = c(NA,1), col = c("red","black"), bty = "n",
       legend = c(expression(paste(f[11],"/",f)),expression(paste(psi[11],"/",psi))))
