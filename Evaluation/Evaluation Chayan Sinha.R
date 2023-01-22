#Evaluation 
#Name : Chayan Sinha
#Roll No : 102003347
#Sub-Group:3CO14

#Ques 1
#a
p<-0.75
df1=10
df2=30
#Applying qf(quantile f)
qf(p,df1,df2)

#b
x1<-2.5
x2<-3.5
df1<-10
df2<-30
##0<x1<2.5
pf(x1,df1,df2,lower.tail = TRUE)
##3.5<x2<inf
pf(x2,df1,df2,lower.tail = FALSE)

#c
q<-c(0.25,0.5,0.75)
df1<-10
df2<-30
for(i in q){
  print(df(i,df1,df2))
}

#Ques2
library(pracma)
func1<-function(x,y){
  (x+3*y)/50
}
var<-integral2(func1,0,1,0,1)
print(var$Q)
d<-var$Q
d
if(d==1){
  print('Joint Distribution Function')
}else{
  print("Not a Joint Density Function")
}

func3<-function(x,y){
  x*y*func1(x,y)
}
ans<-integral2(func3,0,1,0,1)
#Expected Value 
ans$Q

#Marginal Distribution of y
f<-function(x){
  (x+3)/50
}
ans2<-integral(f,0,1)
ans2
#Ques3
getMode<-function(vec){
  uv<-unique(vec)
  print(uv)
  uv[which.max(tabulate(match(vec,uv)))]
}
a<-c(1,2,3,3,5,4,3,2,3,2)
res<-getMode(a)
print(res)



#Ques4
func2<-function(x){
  0.5*exp(-abs(3*x))
}

func3<-function(x){
  x*func2(x)
}
#firstM=Mean
firstm<-integrate(func3,lower=1,upper=10)
print(firstm)

#secondM=Ex^2
func4<-function(x){
  x*x*func2(x)
}
secondm<-integrate(func4,lower=1,upper=10)
print(secondm)
#Mean
res1<-firstm
print(res1)
#Var
fm<-firstm$value
fm
sm<-secondm$value
sm
res2<-sm-fm^2
print(res2)
