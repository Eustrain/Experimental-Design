#######Plots Statiscs
library(ggplot2)
library(ggpubr)
library(agricolae)

dat <- read.csv("data.csv",head=T)
dat$REP <- as.factor(dat$REP)
head(dat)

#########change the name

df1 <- dplyr::mutate(dat,TRAT= 
                       ifelse(TRAT==1,"T1",
                              ifelse(TRAT==2,"T2",
                                     ifelse(TRAT==3,"T3",
                                            ifelse(TRAT==4,"T4",
                                                   ifelse(TRAT==5,"T5",NA
                                                          )))))
                       
                       )

####### Anova  with the model yij= u+ ti+Rj+ e
m1 <- aov(y~TRAT+REP,data = df1)
summary(m1)

#####comparison of  mean (Tukey)

tk <- TukeyHSD(m1)
out <- HSD.test(m1,"TRAT", group=TRUE,console=TRUE)


p1 <- ggboxplot(df1, x = "TRAT", y = "y",
                color = "TRAT", palette = "jco")+
  stat_compare_means(method = "anova",label.y = 120)+
  theme(legend.position = "none")+
  labs(y="Yield (T ha)",x="Treatment")


p1



p2 <- ggplot(df1,aes(x=TRAT,y=y,fill=TRAT))+
      stat_summary(fun.y = "mean",geom = "bar")+
      stat_summary(fun.data = "mean_se",geom = "errorbar")+
      theme(legend.position = "none")+labs(y="Yield (T ha)",x="Treatments")+
  stat_compare_means(method = "anova", label.y = 120) +
  annotate("text",x=1,y=105,label="a")+
  annotate("text",x=2,y=70,label="bc")+
  annotate("text",x=3,y=84,label="b")+
  annotate("text",x=4,y=60,label="c")+
  annotate("text",x=5,y=50,label="c")
  
  
p2


p3 <- ggbarplot(df1, x = "TRAT", y = "y", add = "mean_se",fill = "TRAT")+
  geom_hline(yintercept = mean(df1$y), linetype = 2,color="red")+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 120)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+
  theme(legend.position = "none")+
  labs(y="Yield (T ha)",x="Treatment")
  

p3

p4 <- ggboxplot(df1, x = "TRAT", y = "y", color = "TRAT", 
                add = "jitter", legend = "none") +
  rotate_x_text(angle = 45)+
  geom_hline(yintercept = mean(df1$y), linetype = 2)+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 120)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")

p4



p5 <- ggplot(df1,aes(x=TRAT,y=y,color=TRAT,fill=TRAT))+geom_boxplot(color="black")+
  geom_hline(yintercept = mean(df1$y), linetype = 2,color="red")+ # Add horizontal line at base mean
  stat_compare_means(method = "anova", label.y = 120)+        # Add global annova p-value
  stat_compare_means(label = "p.signif", method = "t.test",
                     ref.group = ".all.")+
    labs(y="Yield (T ha)",x="Treatment")+
     theme(legend.position  = "none")

p5            
                



