
library(ggplot2)

### set working directory
setwd("~/RPI/Coterm_Fall_2024/Data_Analytics/Michael-Dong-Data-Analytics/Lab2/")

### read in data
epi.results <- read.csv("epi2024results06022024.csv", header=TRUE)
epi.weights <- read.csv("epi2024weights.csv")

View(epi.results)
View(epi.weights)

#### Exploratory Analysis ####
epi.results$EPI.new

epi.results[1,5]

attach(epi.results)

EPI.new

EPI.new[1]

## NA values
na.indices <- is.na(EPI.new) 

## drop NAs
Epi.new.compl <- EPI.new[!na.indices]

## convert to data frame and add country
Epi.new.compl <- data.frame(Country = country[!na.indices], EPI = EPI.new[!na.indices])

## summary stats
summary(EPI.new)

fivenum(EPI.new,na.rm=TRUE)

## histograms
hist(EPI.new)

hist(EPI.new, seq(20., 80., 2.0), prob=TRUE)

rug(EPI.new)

lines(density(EPI.new,na.rm=TRUE,bw=1))
lines(density(EPI.new,na.rm=TRUE,bw="SJ"))

x <- seq(20., 80., 1.0)
qn<- dnorm(x,mean=42, sd=5,log=FALSE)
lines(x,0.4*qn)

qn<- dnorm(x,mean=65, sd=5,log=FALSE)
lines(x,0.12*qn)

##################

### Comparing distributions of 2 variables

boxplot(MPE.new, MKP.new, BDH.new, names=c("MPE.new","MKP.new", "BDH.new"))


### Quantile-quantile plots
qqnorm(EPI.new)
qqline(EPI.new)

x <- seq(20., 80., 1.0)
qqplot(qnorm(ppoints(200)), x)
qqline(x)

qqplot(qnorm(ppoints(200)),EPI.new)
qqline(EPI.new)

qqplot(rnorm(1000),MPE.new)
qqline(MPE.new)

qqplot(rnorm(1000),MKP.new)
qqline(MKP.new)

qqplot(rnorm(1000),BDH.new)
qqline(BDH.new)

 d1 <- rnorm(10000)
d2 <- rnorm(10000)
qqplot(d1,d1)
qqline(d1)


### Empirical Cumulative Distribution Function
plot(ecdf(EPI.new), do.points=FALSE) 

plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE, main="Norm Dist vs. EPI.new ECDF")
lines(ecdf(EPI.new))

plot(ecdf(MPE.new), do.points=FALSE, col="blue", main="MPE.new vs MKP.new vs BDH.new ECDF")
lines(ecdf(MKP.new), do.points=FALSE, col = "red")
lines(ecdf(BDH.new), do.points=FALSE, col="green")


#### Populations Dataset ####

## read data
populations_2023 <- read.csv("~/RPI/Coterm_Fall_2024/Data_Analytics/Michael-Dong-Data-Analytics/Lab2/countries_populations_2023.csv")

## drop country populations that don't exist in epi results
populations <- populations_2023[-which(!populations_2023$Country %in% epi.results$country),]

## sort populations by country name
populations <- populations[order(populations$Country),]

## drop country results that don't exist in populations
epi.results.sub <- epi.results[-which(!epi.results$country %in% populations$Country),]

## sort results by country name
epi.results.sub <- epi.results.sub[order(epi.results.sub$country),]

## only keep relevant columns
epi.results.sub <- epi.results.sub[,c("country","MPE.old","MPE.new")]

## convert to numeric
epi.results.sub$population <- as.numeric(populations$Population)

## compute population log
epi.results.sub$population_log <- log10(epi.results.sub$population)

boxplot(epi.results.sub$population_log)

attach(epi.results.sub)

## created linear model of EPI.new = a(population_log) + b
lin.mod.mpenew <- lm(MPE.new~population_log,epi.results.sub)

plot(MPE.new~population_log)
abline(lin.mod.mpenew)

summary(lin.mod.mpenew)

plot(lin.mod.mpenew)


ggplot(epi.results.sub, aes(x = population_log, y = MPE.new)) +
  geom_point() +
  stat_smooth(method = "lm")


ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')

