
EPI_data <- read.csv("C:/Users/dongm2/Documents/RPI/Coterm_Fall_2024/Data_Analytics/epi2024results06022024.csv") 
View(EPI_data)

attach(EPI_data)
EPI.new

tf <- is.na(EPI.new)
E <- EPI.new[!tf]

summary(EPI.new)
fivenum(EPI.new,na.rm=TRUE)

stem(EPI.new)
hist(EPI.new)
hist(EPI.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new)

boxplot(EPI.new, APO.new) 

hist(EPI.new, seq(20., 80., 1.0), prob=TRUE)
lines(density(EPI.new,na.rm=TRUE,bw=1.))
rug(EPI.new) 

x<-seq(20,80,1)
q<-dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 

plot(ecdf(EPI.new), do.points=FALSE, verticals=TRUE) 

qqnorm(EPI.new); 
qqline(EPI.new) 

qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(250, df = 5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)



EPI_data <- read.csv("C:/Users/dongm2/Documents/RPI/Coterm_Fall_2024/Data_Analytics/epi2024results06022024.csv") 
View(EPI_data)

attach(EPI_data)
APO.new

tf <- is.na(APO.new)
E <- APO.new[!tf]

summary(APO.new)
fivenum(APO.new,na.rm=TRUE)

stem(APO.new)
hist(APO.new)
hist(APO.new, seq(20., 80., 1.0), prob=TRUE) 
lines(density(APO.new,na.rm=TRUE,bw=1.))
rug(APO.new)

boxplot(APO.new, APO.old) 

hist(APO.new, seq(40., 80., 1.0), prob=TRUE)
lines(density(APO.new,na.rm=TRUE,bw=1.))
rug(APO.new) 

x<-seq(20,80,1)
q<-dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,q)
lines(x,.4*q)
q<-dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,.12*q) 

plot(ecdf(APO.new), do.points=FALSE, verticals=TRUE) 

qqnorm(APO.new); 
qqline(APO.new) 

qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)

qqplot(rt(250, df = 5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)
