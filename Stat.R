############Function for statistical
##output=Staandar deviation, Mean, Maximum, minimum value and error standar

###Example1
category <- c(rep("young", 3), rep("Middle", 4), rep("old", 5))
fat <- c(1857.87, 1953.90, 1440.70, 1553.81, 1785.91, 1893.82, 1483.75, 1784.99, 2011.01, 2023.04, 2011.05, 1788.81)
BMI <- c(21.1, 23.2, 24.5, 25.6, 21.8, 18.0, 19.2, 20.1, 22.1, 25.0, 26.1, 25.1)
age <- c(25, 23, 27, 55, 58, 62, 45, 75, 80, 75, 83, 89)
df2 <- data.frame(fat, BMI, age, category)



###############Function Estat
Estat<- function(dataset,col_name,...){
  col_name <- enquo(col_name) 
  col_vars <- quos(...)
  d<- dataset %>%
    group_by(!!col_name) %>%
    summarise_at(vars(!!!col_vars), funs( Sd=sd,Mean=mean,Max=max,Min=min,N=dplyr::n()))%>%
    mutate(SE=Sd/sqrt(N))
  
}

out<- Estat(df2,category,age)
out
################for all variables


List <- names(df2)[1:3]

df_out <- data.frame()

for (i in 1:3) {
  
  df <- Estat(df2,category,i)
  d_f <- as.data.frame(df)
  d_f$variable <- List[i]
  d_f<- d_f[,c(8,1,2,3,4,5,6,7)]
  df_out <-rbind(df_out,d_f) 

}


p <- ggplot(df2,aes(x=category,y=fat))+
    stat_summary(fun.y = "mean",geom = "bar",color="black",fill="pink")+
  stat_summary(fun.data  = "mean_se",geom = "errorbar")
p

#####Example 2
#dat <- read.csv("data.csv",head=T)
out <- Estat(dat,TRAT,y)

