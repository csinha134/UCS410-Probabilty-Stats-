#Assignment4
#1a
x<-c(0,1,2,3,4)
y<-c(0.41,0.37,0.16,0.05,0.01)
result<-sum(x*y)/sum(y)
print(result)
#1b
result2<-weighted.mean(x,y)
print(result2)

#1c
print(x%*%y)
#2(assignment 2)
#pbirthday(10,365,2)
#qbirthday(0.5,365,2)

#2
f<-function(t){t*0.1*exp(-0.1*t)}
ans<-integrate(f,lower=0,upper=Inf)
ans$value

#3
x<-c(0,1,2,3)
p<-c(0.1,0.2,0.2,0.5)
y<-function(r){10*r-12}
ev=sum(x*p)
y(ev)


#4
f<-function(x){x*0.5*exp(-abs(x))}
a<-integrate(f,lower=1,upper=10)
p<-a$value
print(p)
f2<-function(x){
  x*x*0.5*exp(-abs(x))}
b<-integrate(f2,lower=1,upper=10)

q<-b$value
print(p)
print(q-p*p)

#5
func5=function(t){
  (3/4)*((1/4)^(t-1))
}
x5<-c(1,2,3,4,5)
y5<-c(1,4,9,16,25)
res5<-sum(func5(3)*9)
print(res5)

funcm=function(t){
  t*(3/4)*((1/4)^(t-1))
}
firmom<-sum(func5(x5)*y5)
print(firmom)

secmom<-sum(func5(x5)*y5*y5)
print(secmom)

var<-secmom-firmom^2
print(var)
