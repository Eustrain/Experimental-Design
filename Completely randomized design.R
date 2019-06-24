#************Completely randomized design**********#

data <- read.csv("TABLA2.2.csv",head=T)
head(dat)
str(dat) 
nombres <-c("REP","TRAT","y")
names(dat) <- nombres 


### Model Yi=U+Ti+e
##### Ti= Treatment


####scrip

crd("y", "TRAT", "REP", data)

crd <- function(y,Entry, Rep, dataframe) {
  
nt <- as.numeric(dataframe[,y])
t <- as.factor(dataframe[, Entry])
r <- as.factor(dataframe[,Rep])
n <- length(nt)
ntrat<- length(levels(t))
nrep <- length(levels(r))
gl_treat <- ntrat-1
gl_error <-ntrat*(nrep-1)
 gl_total <- n-1
  
stats <- tapply(dataframe[,y], list(dataframe[,Entry]), sum, na.rm = TRUE)
mean <- tapply(dataframe[,y], list(dataframe[,Entry]), mean, na.rm = TRUE)
################################
 f_c <-sum(stats)^2/n
 ssq_treat <-(sum(stats^2)/nrep)-f_c
 ssq_total <-sum(nt^2)-f_c
 ssq_error<- ssq_total-ssq_treat
 ############################
 ms_treat <-ssq_treat/gl_treat 
 ms_error <-  ssq_error/gl_error
 FC<-round(ms_treat/ms_error,4)
 ############################
 f.v<-c("Treatments","Error","Total")
 G.L <-c(gl_treat,gl_error,gl_total)
 SS <-c(ssq_treat,ssq_error,ssq_total)
 SQ <-c(ms_treat,ms_error,".")
 f <-c(FC,".",".")
 pr <- round(pf(FC,gl_treat,gl_error, lower.tail=F),10)
 prob <- c(pr,".",".")
 anv<- data.frame("."=f.v,"d.f"=G.L,"Sum Sq"=SS,"Mean sq"=SQ
                  ,"F value"=f,"Pr(F)"=prob)
   
 c.v <- (sd(nt)/mean(nt))*100
 resul <- list(anv,mean,c.v)
 print(resul)  
 
}


