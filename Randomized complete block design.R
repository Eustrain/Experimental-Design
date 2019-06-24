##*******Randomized  Complete Block Design############*********

data <- read.csv("data.csv",head=T)
head(data)
str(data) 
names(data)<-c("REP","TRAT","y")


dat$REP <- as.factor(dat$REP)
dat$TRAT <- as.factor(dat$TRAT)
fm3 <- aov(y~TRAT+REP,data=dat)
summary(fm3)

RCBD("y", "TRAT", "REP", data)
#################RCBD#############################
RCBD<- function(y,Entry, Rep, dataframe){
  
  nt <- as.numeric(dataframe[,y])
  t <- as.factor(dataframe[, Entry])
  r <- as.factor(dataframe[,Rep])
  n <- length(nt)
  ntrat<- length(levels(t))
  nrep <- length(levels(r))
 #######################  degree freedom
  gl_treat <- ntrat-1
  gl_rep <- nrep-1
  gl_error <-(ntrat-1)*(nrep-1)
  gl_total <- n-1
  #############################
  stats <- tapply(dataframe[,y], list(dataframe[,Entry],dataframe[,Rep]), sum, na.rm = TRUE)
  mean <- tapply(dataframe[,y], list(dataframe[,Entry]), mean, na.rm = TRUE)
  ###########################################
  sum_rep <- apply(stats, 2, sum)
  sum_entry <- apply(stats,1,sum)
    
  ################################sums square
  f_c <-sum(sum_entry)^2/(ntrat* nrep)
  ssq_treat <-(sum(sum_entry^2)/nrep)-f_c
  ssq_rep <- (sum(sum_rep^2)/ntrat)-f_c
  ssq_total <-sum(nt^2)-f_c
  ssq_error<- round((ssq_total-ssq_treat- ssq_rep),5)
#################################### Mean square
  ms_treat<-round((ssq_treat/gl_treat),5 )
  ms_rep <- round((ssq_rep/gl_rep),5)
  ms_error <-  ssq_error/gl_error
  ###################################
  FCt<-round((ms_treat/ms_error),4)
  FCr <-round(( ms_rep/ms_error),4) 
  #############################################
  f.v<-c("Treatments","Rep","Error","Total")
  G.L <-c(gl_treat,gl_rep,gl_error,gl_total)
  SS <-c(ssq_treat,ssq_rep,ssq_error,ssq_total)
  SQ <-c(ms_treat,ms_rep,ms_error,".")
  f <-c(FCt,FCr,".",".")
  prt <- round(pf(FCt,gl_treat,gl_error, lower.tail=F),10)
  prr <- round(pf(FCr, gl_rep,gl_error, lower.tail=F),10)
  prob <- c(prt,prr,".",".")
  anv<- data.frame("."=f.v,"d.f"=G.L,"Sum Sq"=SS,"Mean sq"=SQ
                   ,"F value"=f,"Pr(F)"=prob)
  c.v <- (sd(nt)/mean(nt))*100
  resul <- list(anv,mean,c.v)
  print(resul) 
}


  