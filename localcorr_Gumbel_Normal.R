# Dependencies.
#libPaths("D:/Software/Rpackages")
#install.packages("plot3D")
#nstall.packages("Deriv")
#install.packages("ggplot2")
library(Deriv)
library(ggplot2)
library(plot3D)
library(plotly)
# Automatic differentiation of F(x,y)=C(F_1(x),F_2(Y)), with C Gumbel and F_1=F_2=Standard normal

#The copula
Pgumbel <- function(u,v,theta){
  exp(-(((-log(u)) ^ theta) + ((-log(v)) ^ theta)) ^ (1/theta))
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

#Defining local parameters

localcor <- function(x,y,theta){
  (f12(x,y,theta = theta)*f(x,y,theta = theta) - f1(x,y,theta = theta)*f1(x,y,theta = theta))/
  (sqrt(f1(x,y,theta = theta) ^ 2 - f11(x,y,theta = theta)*f(x,y,theta = theta))*
      sqrt(f2(x,y,theta = theta) ^ 2 - f22(x,y,theta = theta)*f(x,y,theta = theta)))
}

#Defining functions contained in the square roots of the denominator of rho, r1 and r2. 

r1 <- function(x,y,theta){
 f1(x,y,theta) ^ 2 - f11(x,y,theta)*f(x,y,theta)
}

r2 <- function(x,y,theta){
  f2(x,y,theta) ^ 2 - f22(x,y,theta)*f(x,y,theta)
}

# Plot of tiles

tileplot <- function(funct,theta = 2){

reg_grid <- as.matrix(expand.grid(seq(-2,2,0.1),seq(-2,2,0.1)))

x <- reg_grid[,1]
y <- reg_grid[,2]
funct.value <- apply(reg_grid,1,function(x) funct(x[1],x[2],theta = theta))

dat = data.frame(x = x,y = y,funct.value = funct.value,stringsAsFactors = FALSE)  # function value at gridpoints.

g <- ggplot(data = dat, aes(x, y))
g <- g + geom_tile(aes(fill = funct.value)) 
gr <- scale_fill_gradient2(midpoint = 0,low = "red", high = "green", space = "Lab")
g <- g + gr
#g <-  g + geom_text(mapping = aes(x = x, y = y, label = funct.value))
g

}

#Plot of contours

gumb_contour <- function(funct,theta = 2){

reg_grid <- as.matrix(expand.grid(seq(-2,2,0.1),seq(-2,2,0.1)))
x <- reg_grid[,1]
y <- reg_grid[,2]
dat <- data.frame(x=x,y=y)

v <- ggplot(dat,aes(x,y,z = funct(x,y,theta=2)))
v <- v + geom_raster(aes(fill = funct(x,y,theta = 2))) + geom_contour(colour="white")

#gr <- scale_fill_gradient2(midpoint = 0,low = "red", high = "green", space = "Lab")
#v <- v + gr
v
}

#Plot 3D

gumb3D <- function(funct,theta = 2){
x <- seq(-2,2,0.1)
y <- seq(-2,2,0.1)
z <- outer(x,y,function(x,y) funct(x,y,theta=theta))
plot_ly(x = x,y = y,z = z, type = "surface")
}

#Figures of density f
gumb3D(f,theta = 2)

#Figures of restriction function r1 contained in the first square root of the denominator
gumb3D(r1,theta = 2)

#Figures of restriction function r2 contained in the second square root of the denominator
gumb3D(r2,theta = 2)

#Figures of local correlation
gumb3D(localcor,theta = 2)

#Population value along the diagonal

x <- y <- seq(-2,2,.1)
local.cor <- apply(cbind(x,y),1,function(x) localcor(x[1],x[2],theta = 2))
plot(x,local.cor,type = "l",main = "population value rho, along the diagonal")

