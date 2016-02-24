.libPaths("D:/Software/Rpackages")
install.packages("copula")
install.packages("Deriv")
library(copula)
library(Deriv)

# Automatic differentiation of F(x,y)=C(F_1(x),F_2(Y)), with C Gumbel and F_1=F_2=Standard normal

#The copula
Pgumbel <- function(u,v,theta){
  exp(-(((-log(u))^theta)+((-log(v))^theta))^(1/theta))
}

#Defining F(x,y)

F <- function(x,y,theta){
  Pgumbel(pnorm(x),pnorm(y),theta = theta)
}

#Automatic differentiation to find f,f1,f2,f11,f22,f12

F1 <- Deriv(F,x = "x")
f <- Deriv(F1,x = "y")
f1 <- Deriv(f,"x")
f2 <- Deriv(f,"y")
f11 <- Deriv(f1,"x")
f22 <- Deriv(f2,"y")
f12 <- Deriv(f1,"y") 
f21 <- Deriv(f2,"x")


check <- function(x,y,theta){
  return("f"=c(f(x,y,theta),"f1"=f1(x,y,theta),"f2"=f2(x,y,theta),"f11"=f11(x,y,theta),"f22"=f22(x,y,theta),"f12"=f12(x,y,theta),
               "in sqrt1"=f1(x,y,theta = theta)^2-f11(x,y,theta = theta)*f(x,y,theta = theta),
               "in sqrt2"=f2(x,y,theta = theta)^2-f22(x,y,theta = theta)*f(x,y,theta = theta) ))
}

check(3,2,2)
f22(3,2,2)
F(1,2,2)
Pgumbel(1,2,3)
localcor=function(x,y,theta){
  (f12(x,y,theta = theta)*f(x,y,theta = theta) - f1(x,y,theta = theta)*f1(x,y,theta = theta))/
  (sqrt(f1(x,y,theta = theta)^2-f11(x,y,theta = theta)*f(x,y,theta = theta))*
      sqrt(f2(x,y,theta = theta)^2-f22(x,y,theta = theta)*f(x,y,theta = theta)))
}

localcor(3,2,2)
f2(3,2,2)^2-f22(3,2,2)*f(3,2,2)
