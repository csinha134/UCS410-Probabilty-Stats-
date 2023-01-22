#Assignment 1
#Q1
vec1<-c(10,20,30,40,50)
a<-max(vec1)
print(a)
b<-min(vec1)
print(b)

#Q2
x<-readline('Enter the number :')
fact=1
for (i in 1:x)
  fact=fact*i
print(fact)

#Q3
y<-readline('Enter the limit : ')
a=0
b=1
for( i in 1:y)
{
  c=a+b
  print(a)
  a=b
  b=c
}

#Q4
p<-readline('Enter the operand 1 : ')
q<-readline('Enter the operand 2: ')
p<-as.integer(p)
q<-as.integer(q)
x<-readline('Enter the choice : ')
result=switch(x,
              "1"=cat("Addition=",p+q),
              "2"=cat("Subtraction=",p-q),
              "3"=cat("Multiplication=",p*q),
              "4"=cat("Division=",p/q),
            )
print(result)


#Assignment 2
#Q1
vec<-c(rep("G",20),rep("S",30),rep("B",50))
s<-sample(vec,10,replace=FALSE,prob=NULL)
print(s)

#b
surg<-c("Success","Failure")
result<-sample(surg,10,prob=c(0.9,0.1),replace=TRUE)
print(result)

#Q2
n=1:50
p=numeric(50)
for(i in n){
  q=prod(1-(0:(i-1))/365)
  p[i]=q-1
}
print(p)

#Q2b
n=1:50
p=numeric(50)
for(i in n)
{
  q=prod(1-(0:(i-1))/365)
  if(q<0.5){
    print(i)
    break;
  }
}

#Q3
pcloud=0.4
prain=0.2
pcr=0.85
prc=pcr*prain/pcloud
print(prc)

#Q4
library(datasets)
datasets::iris
head(iris,10)
dim(iris)
str(iris)
vec<-iris$Sepal.Length
print(range(vec))
print(mean(vec))
print(median(vec))
vec=sort(vec)
firstq=length(vec)/4
thirdq=length(vec)*3/4
print(vec[firstq])
print(vec[thirdq])
vec<-iris$Sepal.Length
print(sd(vec))
print(var(vec))

print(summary(vec))

#Q5
getMode<-function(vec){
  un<-unique(vec)
  un<-which.max(tabulate(match(vec,un)))
}

result<-getMode(c(9,2,2,9,1,1,3,3,4,9))
print(result)

#Assignment 3
vec<-c(7,8,9)
dbinom(7,12,1/6,log=FALSE)+dbinom(8,12,1/6,log=FALSE)+dbinom(9,12,1/6,log=FALSE)
#Q2
marks=84:100
pnorm(marks,mean=72,sd=15.2)

#Q3
dpois(0,5)
dpois(48,50)+dpois(49,50)+dpois(50,50)

#Q4
dbinom(3,5,17/250,log=FALSE)
dhyper(3,17,233,5,log=FALSE)

#Q5
X=0:31
Y=dbinom(X,31,0.447)
plot(X,Y)

Z=pbinom(X,31,0.447,lower.tail = TRUE)
plot(X,Z)


n=32
p=0.447
q=1-p

mean<-n*p
print(mean)

var<-n*p*q
print(var)

sd<-sqrt(var)
print(sd)

#Assignment 4
#Q1
x<-c(0,1,2,3,4)
px<-c(0.41,0.37,0.16,0.05,0.01)
result<-sum(x*px)/sum(px)
print(result)

res<-weighted.mean(x,y)
print(res)

#Q2
f<-function(x)
{
  x*0.1*exp(-0.1*x)
}
ans<-integrate(f,lower=0,upper=Inf)
print(ans)
ans$value

#Q3
x<-c(0,1,2,3)
p<-c(0.1,0.2,0.2,0.5)
y<-function(r){
  10*r-12
}
ev=sum(x*p)
y(ev)

#Q4
func1<-function(x){
  x*0.5*exp(-abs(x))
}
m<-integrate(func1,lower=1,upper=10)
print(m)
fm<-m$value

func2<-function(x){
  x*func1(x)
}
sm<-integrate(func2,lower=1,upper=10)
sec<-sm$value
print(sec-fm*fm)

#Q5
func4<-function(x){
  (3/4)*(1/4)^(x-1)
}
x<-c(1,2,3,4,5)
y<-c(1,4,9,16,25)
res5<-sum(func4(3)*9)
print(res5)


#Assignment 5
#Q1
punif(45,min=0,max=60,lower.tail=FALSE)

#Q1b
punif(30,min=0,max=60,lower.tail=FALSE)-punif(20,min=0,max=60,lower.tail=TRUE)

#Q2
x<-3
dexp(3,1/2)
#Q2b
x<-0:5
pexp(5,1/2,lower.tail = TRUE)-pexp(0,1/2)
y<-dexp(d,1/2)
plot(x,y)

#Q2c
x<-3
pexp(x,1/2,lower.tail=TRUE)

#Q2d
d<-0:5
e<-pexp(d,1/2,lower.tail = TRUE)
plot(d,e)

#Q2e
vec<-rexp(1000,1/2)
plot(density(vec))
plot(vec,type='l')

#Q3a
a=2
b=1/3
dgamma(3,a,scale=b)

#Q3b
pgamma(1,shape=a,scale=b,lower.tail=FALSE)

#Q3b
qgamma(0.70,shape=a,scale=b,lower.tail = FALSE)

#Assignment 6
library(pracma)
func1<-function(x,y){
  2*(2*x+3*y)/5
}
integral2(func1,xmin=0,xmax=1,ymin=0,ymax=1)$Q

#Q1b
func2<-function(y){
  2*(2+3*y)/5
}

integrate(func2,lower=0,upper=1)$value
func3<-function(x){
  4*x/5
}

integral(func3,0,1)

func4<-function(x,y){
  x*y*func1(x,y)
}
ex<-integral2(func4,0,1,0,1)
print(ex)

#Q2(Discrete)
func5<-function(x,y){
  (x+y)/30
}
x<-c(0,1,2,3)
y<-c(0,1,2)

c<-matrix(0,4,3)
for (i in 1:length(x)){
  for (j in 1:length(y)){
    c[i,j]<-func5(x[i],y[j])
  }
}
c
print(c)
var<-sum(c)
print(var)  

#Q2c
?apply()
rowsum=apply(c,1,sum)
rowsum

colsum=apply(c,2,sum)
colsum
#Q2d
cdp<-c[1,2]/colsum[2]
print(cdp)

#Q2f
#Ex
sumx=0
for(i in 1:length(rowsum)){
  sumx=sumx+(i-1)*rowsum[i]
}
sumx

#Ey
sumy=0
for(i in 1:length(colsum)){
  sumy=sumy+(i-1)*colsum[i]
}
sumy

#Exy
sumxy=0;
for(i in 1:length(rowsum)){
  for(j in 1:length(colsum)){
    sumxy=sumxy+(i-1)*(j-1)*c[i,j]
  }
}
sumxy

#VarX
sumx2=0
for(i in 1:length(rowsum)){
  sumx2=sumx2+(i-1)*(i-1)*rowsum[i]
}
varx=sumx2-sumx*sumx
print(varx)

#var y
sumy2=0
for(i in 1:length(colsum)){
  sumy2=sumy2+(i-1)*(i-1)*colsum[i]
}
vary=sumy2-sumy*sumy
print(vary)

#cov(x,y)
covxy=sumxy-sumx*sumy
print(covxy)

corr=covxy/sqrt(abs(varx))*sqrt(abs(vary))
corr

#Assignment 7
n=100
df=n-1
rt(n,df)
hist(rt(n,df))

#Q2
n=100
df<-c(2,10,25)
for (i in df){
  print(rchisq(n,i))
}

#Q3
vec<-seq(-6,6,length=100)
print(vec)
colour<-c('red','pink','orange','green')
df2<-c(1,4,10,30)
for(i in df2){
  print(dt(vec,i))
}
plot(vec,dt(vec,df2[4]),type='l',xlab='x',ylab='y',main="Density Function",col='green')

for(i in 1:3){
  lines(vec,dt(vec,df2[i]),type='l',col=colour[i])
}

#Q4
per=0.95
df1=10
df2=20
qf(per,df1,df2)

#Q4b
x=1.5
df1=10
df2=20

##0<x<1.5
pf(1.5,df1,df2,lower.tail=TRUE)
##1.5<x<inf
pf(1.5,df1,df2,lower.tail=FALSE)

#Q4c
df1=10
df2=20
q<-c(0.25,0.5,0.75,0.999)
for (i in q){
  print(qf(i,df1,df2))
}

#Q4d
rf(1000,10,20)
hist(rf(1000,10,20))
