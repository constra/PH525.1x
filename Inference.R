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

hist(null)

qqnorm(null)
qqline(null)

pops <- read.csv("mice_pheno.csv")
head(pops)
str(pops)

hf <- pops[pops$Diet == "hf" & pops$Sex == "F",3]
chow <- pops[pops$Diet == "chow" & pops$Sex == "F",3]

mean(hf)-mean(chow)

x <- sample(hf,12)
y <- sample(chow,12)
mean(x)-mean(y)
Ns <- c(3,5,10,25)
B <- 10000
res <- sapply(Ns,function(n) {
        sapply(1:B,function(j) {
                mean(sample(hf,n))-mean(sample(chow,n))
        }
})
library(rafalib)
mypar2(2,2)
for(i in seq(along=Ns)){
        qqnorm(res[,i])
}

## Example
dat <- read.csv("femaleMiceWeights.csv")
dat

control <- dat[1:12,2]
treatment <- dat[12+1:12,2]
diff <- mean(treatment)-mean(control)
diff

t.test(treatment,control)

sd(control)
sd(control)/sqrt(length(control))

se <- sqrt(var(treatment)/length(treatment)+var(control)/length(control))
se
tstat <- diff/se
tstat

1-pnorm(tstat)+pnorm(-tstat)

qqnorm(control)
qqline(control)
qqnorm(treatment)
qqline(treatment)

## se is also a random variable
### Question

babies <- read.table("babies.txt",header=T)
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]


mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

## sampling
# the first 30 samples
dat.ns <- bwt.nonsmoke[1:30]
dat.s <- bwt.smoke[1:30]

N=30
X.ns = mean(dat.ns)
sd.ns = sd(dat.ns)
X.s = mean(dat.s)
sd.s = sd(dat.s)
sd.diff = sqrt(sd.ns^2/N+sd.s^2/N)
tval = (X.ns - X.s)/sd.diff
t.test(dat.ns, dat.s)$statistic

pval <- 1-pnorm(tval)+pnorm(-tval)


## Inference II questions
babies = read.table("babies.txt", header=TRUE)
# extract data
bwt.nonsmoke = babies$bwt[babies$smoke==0]
bwt.smoke = babies$bwt[babies$smoke==1]

# sample size
N=30
# sampling
bf <- function(){
s.ns <- sample(bwt.nonsmoke,N)
s.s <- sample(bwt.smoke,N)

mytest <- t.test(s.ns, s.s)
#mytest$p.value
mytest$conf.int
}
bound <- replicate(1000,bf())
dim(bound)
range.bound <- diff(bound)
mean(range.bound)

popdiff = mean(bwt.nonsmoke) - mean(bwt.smoke)
