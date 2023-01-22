#1
x<-c(7,8,9)
dbinom(7,12,1/6)+dbinom(8,12,1/6)+dbinom(9,12,1/6)
vec<-c(pbinom(x,12,1/6,lower.tail=TRUE))
#2
score<-c(84:100)
pnorm(score,mean=72,sd=15.2)

#3
dpois(0,5)
dpois(48,50)+dpois(49,50)+dpois(50,50)

#4
dhyper(3,17,233,5,log=FALSE)
dbinom(3,5,17/250,log=FALSE)

#5a
#X follows binomial distribution with parameters n=31 and 
#the probability of students using wiki is 0.447

#5b
#PMF error in this question .
X=0:31
Y=dbinom(X,31,0.447)
plot(X,Y)
#5c
#CDF
Z=pbinom(X,31,0.447)
plot(X,Z)

#5d
n<-32
p<-0.447
q<-0.553
mean<-n*p
print(mean)
var<-n*p*q
print(var)
sd<-sqrt(var)
print(sd)
