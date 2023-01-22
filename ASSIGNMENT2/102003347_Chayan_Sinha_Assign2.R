#Q1(a)
samplespace<-c(rep("G",20),rep("B",50),rep("S",30))
print(sample(samplespace,10))

#Q1(b)
temp<-c("Success","Failure")
ps=sample(temp,10,prob=c(0.9,0.1),replace=TRUE)
print(ps)

#(2) (a)
#v = c(rep(1:365),2)
#sample(v,2,replace=FALSE)
n = 1:50
p = numeric(50)
for (i in n)
{
  q = prod(1 - (0:(i-1))/365)
  p[i] = 1-q
}
print(p)

#(2) (b)
n = 1:50
for(i in n)
{
  q = prod(1 - (0:(i-1))/365)
  if(q<0.5)
  {
    print(i)
    break
  }
}

#Q3
result = function(pa,pb,pba)
{
  pab = pba * pa / pb
  return (pab)
}
pcloudy = 0.4
prain = 0.2
pcloudyrain = 0.85
result(prain,pcloudy,pcloudyrain)

#Q4
library(datasets)
data(iris)
#4a Print first few rows of this dataset.
head(iris,10) 
#4b Find the structure of this dataset. 
dim(iris)
str(iris)
#4c Find the range of the data regarding the sepal length of flowers.
vec<-iris$Sepal.Length
print(range(vec))
#print(min(range))
#print(max(range))
#4d Find the mean of the sepal length
print(mean(vec))
#4e Find the median of the sepal length.
print(median(vec))
#4f Find the first and the third quartiles and hence the interquartile range.
vec=sort(vec)
firstq=(length(vec))/4
thirdq=(length(vec))*3/4
print(firstq)
print(vec[firstq])
print(vec[thirdq])
print(summary(iris))

#4g Find the standard deviation and variance.
print(sd(vec))
print(var(vec))

#4i Find the summary
print(summary(iris))



#Q5
getMode <- function(vec){
  uniqv <- unique(vec)
  uniqv[which.max(tabulate(match(vec,uniqv)))]
}

v <- getMode(c(9,2,2,9,1,1,3,3,4,9))

print(v)



      