#1a
q<-45
punif(q,min=0,max=60,lower.tail = FALSE)

#1b
q1<-20
q2<-30
punif(q2,min=0,max=60)-punif(q1,min=0,max=60)

#2(lm*e^(-lmx))
#a.
x<-3
dexp(x,1/2)

#b.
d<-0:5
y<-dexp(d,1/2)
print(y)
plot(d,y)

#c
x<-3
pexp(x,1/2)

#d
vec<-0:5
vecy<-pexp(vec,1/2)
plot(vec,vecy)

#e
x_sim <- rexp(1000,rate=1/2)
plot(density(x_sim))
plot(x_sim,type='l')

#3a(i)
x<-3
dgamma(x,shape=2,scale = 1/3)

#(ii)
x<-1
pgamma(x,shape=2,scale=1/3,lower.tail = FALSE)

#3b
p<-0.70
qgamma(p,shape=2,scale=1/3,lower.tail = FALSE)