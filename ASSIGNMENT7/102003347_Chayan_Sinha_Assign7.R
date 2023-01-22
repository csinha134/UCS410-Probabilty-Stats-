#Q1
n1=1000
df1=n1-1
val<-rt(n1,df1)
print(val)

hist(val,main="HISTOGRAM PLOT",xlab="X",ylab="Y",border="red",col="yellow")

#Q2
df<-c(2,10,25)
n=100
for(i in df)
{
  print(rchisq(n,i))
}

#Q3
x<-seq(-6,6,length=100)
df2<-c(1,4,10,30)
color=c("black","red","blue","green")
for(i in df2)
{
  print(dt(x,i))
}


plot(x,dt(x,df2[4]),type="l",xlab="t-value",ylab="Density",main="Comparison of t-distribution",col=color[4])

for(i in 1:3)
{
  lines(x,dt(x,df[i]),type="l",col=color[i])
}

#Q4(i)
per=0.95
df1=10
df2=20
print(qf(per,df1,df2))

#Q4(ii)
x=1.5
df1=10
df2=20
##0<x<1.5
print(pf(x,10,20,lower.tail = TRUE))
##1.5<x<INf
print(pf(x,10,20,lower.tail = FALSE))

##Q4(iii)
q<-c(0.25,0.50,0.75,0.999)
df1=10
df2=20
val<-qf(q,df1,df2)
print(val)

##Q4(iv)
n=1000
df1=10
df2=20
val<-rf(n,df1,df2)
print(val)
hist(val,main="HISTOGRAM PLOT",xlab="X",ylab="Y",border="red",col="yellow")
