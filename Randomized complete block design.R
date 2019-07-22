##*******Randomized  Complete Block Design############*********       #
# RCBD(y=variable,Entry=Treatment,Rep=Repetitions,ylab=name of y axis,#  
# xlab=name of x axis,plot=type of plot)                              #
# the plot save automatic in  our  working directory                  # 
#....................................................................#

library(ggplot2)
library(ggpubr)
library(agricolae)

df <- read.csv("data.csv",head=T)
head(df)
str(df) 

df$REP <- as.factor(df$REP)
df$TRAT <- as.factor(df$TRAT)
m1<- aov(y~TRAT+REP,data=df)
summary(m1)

Ex1 <- RCBD("y","TRAT", "REP","Yield","Treatment",plot ="Box",df)

#################RCBD#############################
RCBD<- function(y,Entry, Rep, ylab,xlab,plot=c("Box","Bar"),dataframe){
  
  nt <- as.numeric(dataframe[,y])
  t <- as.factor(dataframe[, Entry])
  r <- as.factor(dataframe[,Rep])
  dp <- data.frame("Entry"=t,"y"=nt)
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
  Trat_mean <- tapply(dataframe[,y], list(dataframe[,Entry]), mean, na.rm = TRUE)
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
  ###### Anova data frame ####################
  
  
  df <-c(gl_treat,gl_rep,gl_error,gl_total)
  SS <-c(ssq_treat,ssq_rep,ssq_error,ssq_total)
  SQ <-c(ms_treat,ms_rep,ms_error,NA)
  f <-c(FCt,FCr,NA,NA)
  prt <- round(pf(FCt,gl_treat,gl_error, lower.tail=F),10)
  prr <- round(pf(FCr, gl_rep,gl_error, lower.tail=F),10)
  prob <- c(prt,prr,NA,NA)
  anv<- data.frame(df,`Sum Sq`=SS,`Mean Sq`=SQ,`F value`=f,`Pr(>F)`=prob, check.names=FALSE)
  rownames(anv) <- c("Treat","Rep","Error","Total")
  class(anv) <- c("anova","data.frame")
  
##################################################

df_mean <- data.frame("Means"=Trat_mean)
  
  ################Plots
mx <- max(nt)+10  
if(plot=="Box"){  
  
 
p <- ggplot(dp,aes(x=Entry,y=y,fill=Entry))+geom_boxplot(color="black")+
    geom_hline(yintercept = mean( Trat_mean), linetype = 2,color="red")+ # Add horizontal line at base mean
    stat_compare_means(method = "anova",label.y = mx)+        # Add global annova p-value
    stat_compare_means(label = "p.signif", method = "t.test",
                    ref.group = ".all.")+
  geom_jitter(shape=16, position=position_jitter(0.2))+
  labs(y=ylab,x=xlab)+theme(legend.position  = "none")
}
else if (plot=="Bar"){

p <- ggplot(dp,aes(x=Entry,y=y,fill=Entry))+
  stat_summary(fun.y = "mean",geom = "bar")+
  stat_summary(fun.data = "mean_se",geom = "errorbar")+
  stat_compare_means(method = "anova",label.y = mx)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+
  labs(y=ylab,x=xlab)+theme(legend.position  = "none")
  
}

ggsave(p, file = paste("plot", plot,".pdf",sep = "_"), scale = 1)

###############output 

c.v <-  ms_error/sqrt(mean(Trat_mean))
resul <- list(ANOVA=anv,Means=df_mean,C.V = c.v,Plot=p)
print(resul) 
}










  