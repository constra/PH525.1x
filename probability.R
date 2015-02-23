library(ggplot2)

### Probability
dat <- read.csv("femaleMiceWeights.csv")
mean(dat[13:24,2]) - mean(dat[1:12,2])

s = split(dat[,2], dat[,1])
stripchart(s, vertical=TRUE, col=1:2)
abline(h=sapply(s, mean), col=1:2)

highfat = s[["hf"]]
highfat

sample(highfat,6)

highfat >30
as.numeric(highfat >30)
sum(highfat >30)

sum(highfat >30)/length(highfat)

population <- read.csv("femaleControlsPopulation.csv")


control <- sample(population[,1,12])
mean(control)

n <- 100000
null <- vector("numeric",n)
for(i in 1:n){
        control <- sample(population[,1],12)
        treatment <- sample(population[,1],12)
        null[i] <- mean(treatment) - mean(control)
}

diff <- mean(dat[13:24,2]) - mean(dat[1:12,2])
mean(null>diff)


mean(population[,1])
population <- population[,1]
sampleMean <- replicate(10000, mean(sample(population, 12)))
plot(sampleMean)
hist(sampleMean)
null <- replicate(10000,mean(sample(population,12))-mean(sample(population,12)))
plot(null)
hist(null)
abline(v=diff,col="red")

# using ggplot2
q<-qplot(null,binwith=.3)
q+geom_vline(xintercept=c(diff,-diff),color="red")

### Probability II
library(devtools)
install_github("jennybc/gapminder")

# load the data
library(gapminder)
data(gapminder)
head(gapminder)

str(gapminder)
x <- gapminder$lifeExp[gapminder$year==1952]
hist(x)
mean(x <= 40)
sum(x<=60 & x>40)/length(x)


# plot accumulative distribution
plot(ecdf(x))
qs <- seq(from=min(x),to=max(x),length=20)
qs
props <- sapply(qs,function(q) mean(x<=q))
hist(x)
plot(props,type="l")
points(props)

## Normal distribution
library(gapminder)
data(gapminder)
str(gapminder)
# Create a vector which gives the population sizes of the countries in 1952.
pop.1952 <- gapminder$pop[gapminder$year==1952]
head(pop.1952)
# Examine the histogram of these population sizes
hist(pop.1952,breaks = 500)
# Now examine the histogram of the log10 of these population sizes.
hist(log10(pop.1952))
# Sd of log10
sd(log10(pop.1952))


# Create a vector 'x' of the log10 of the 1952 population sizes.
x <- log10(pop.1952)
# Examine a Q-Q plot of this vector
qqnorm(pop.1952)
qqnorm(x)
# Standardize the log10 population size vector
z <- (x-mean(x))/sd(x)
# Examine a Q-Q plot of 'z' against the Normal distribution using qqnorm().
qqnorm(z)
abline(0,1)

max(z)


## Expected proportions
f <- function(q) pnorm(q,mean=mean(x),sd=sd(x))
(f(7)-f(6))*length(x)
sum(x >6 & x <= 7)


n = length(x)
ps = ((1:n) - 0.5)/n
qnorm(ps)
min(x)
qnorm()
plot(qnorm(ps), sort(x))
