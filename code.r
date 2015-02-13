dat <- read.csv("femaleMiceWeights.csv")

tab <- read.csv("msleep_ggplot2.csv")
class(tab)
head(tab);dim(tab)

colnames(tab)
head(tab$sleep_total)

c(tab$sleep_total, 1000)

plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log="x")

summary(tab$sleep_total)

mean(tab$sleep_total[tab$sleep_total>18])

which(tab$sleep_total > 18 & tab$sleep_rem < 3)
order(tab$sleep_total)

rank(tab$sleep_total)

match(c("Cow","Owl monkey","Cheetah"), tab$name)

idx = match(c("Cow","Owl monkey","Cheetah"), tab$name)

tab[idx,]

match("Cotton rat",tab$name)


vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)

fac2 = factor(vec, levels=c("blue","green","yellow","orange","red"))
fac2
levels(fac2)

table(tab$order)

sleep <- split(tab$sleep_total,tab$order)
mean(sleep$Rodentia)

sapply(sleep$Primates,sd)
