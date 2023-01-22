#ASSIGNMENT 6
#CHAYAN SINHA
#102003347
#Q1 a
library(pracma)#Practical Numerical Math Functions
f<-function(x,y){
  return (x+3*y)/50
}

val<-integral2(f,0,1,0,1)$Q
print(val)

if(val==1){
  print("THIS IS A PDF")
}else{
  print("NOT A PDF")
}

integral2(f,0,1,0,1)$error

#Q1b
f2<-function(y){
  return (2*(2+3*y)/5)
}

integral(f2 , 0 , 1)

#Q1c
f3<-function(x){
  return (2*(2*x)/5)
}
integral(f3,0,1)

#Q1d
f4<-function(x,y){
  return(x*y*f(x,y))
}
integral2(f4,0,1,0,1)

#Q2a
second_f<-function(x,y){
  return((x+7*y)/15)
}
x<-c(0,1,2,3)
y<-c(0,1,2)
c<-matrix(0,4,3)
for(i in 1:length(x)){
  for(j in 1:length(y)){
    c[i,j]=second_f(x[i],y[j])
  }
}
c
var<-sum(c)
sum(c)
#Q2b
if (var){
  print("THIS IS A JOINT MASS FUNCTION")
}else{
  print("NOT A JOINT MASS FUNCTION")
}

#Q2c
?apply()

#1->rows and 2->columns
row.sum<-apply(c,1,sum)
row.sum
#Q2d
col.sum<-apply(c , 2 , sum)
col.sum
#Q2e
#Conditional Probability
cdp<-c[1,2]/col.sum[2]
cdp

#Q2f
#E(x)
sumx<-0
for (i in 1:length(row.sum))
  sumx=sumx+(i-1)*row.sum[i]
sumx
#E(y)
sumy<-0
for(i in 1:length(col.sum))
  sumy=sumy+(i-1)*col.sum[i]
sumy

#E(xy)
sumxy<-0
for(i in 1:length(row.sum))
  for(j in 1:length(col.sum))
    sumxy=sumxy+(i-1)*(j-1)*(c[i,j])
sumxy

#Var(x)
sumx2<-0
for(i in 1:length(row.sum))
  sumx2=sumx2+(i-1)*(i-1)*row.sum[i]
varx<-sumx2-(sumx*sumx)
varx

#Var(y)
sumy2<-0
for(i in 1:length(col.sum))
  sumy2=sumy2+(i-1)*(i-1)*col.sum[i]
vary<-sumy2-(sumy*sumy)
vary

#COV(x,y)
covxy<-sumxy-(sumx*sumy)
covxy

#CORRF
corrf<-covxy/(sqrt(abs(varx))*sqrt(abs(vary)))
corrf
