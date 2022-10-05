rm(list = ls())

###############
############## Experimental Desing
#############

###########  Statistics  basics
x1 <- c(178,175,158,153)
x1 <- as.character(x1)
u_mean <- function(x){
if(is.numeric(x1)==FALSE){
  print("The variable is not numeric") 
}
else{
  x_i=sum(x)/length(x)
   print(paste("The mean is",x_i,sep = " "))
}
  
}

u_mean(x1)


####### sum of squares

desv <- function(x){
  x_i <- sum(x)/length(x)
  s <- (x-x_i)^2
  ss <- sum(s)
  print(ss)
  }

 desv(x1)

###### variance

o_var <- function(x){
    x_i <- sum(x)/length(x)
  s <- (x-x_i)^2
  variance<- sum(s)/(length(x)-1)
return(variance)
  }

o_var(x1)/166*100

##################################
####### Regression

regression <- function(x1,x2){
  
  za<- mean(x1)-x1
  zb <- mean(x2)-x2
  va <- desv(x1)
  vb <- desv(x2)
  r <- sum(za*zb)/(sqrt(va*vb))
return(r)
  }

regression(A,B)

### Nominal data



