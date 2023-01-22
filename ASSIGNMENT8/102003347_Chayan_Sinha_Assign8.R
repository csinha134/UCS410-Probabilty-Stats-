#Manpreet Singh
#102003171
#COE-6
#Assignment-8


#Q1
#a
df <- read.csv("Dataset.csv")

#b
head(df)
nrow(df)
dim(df)
head(df,10)

#c
mean(df$Wall.Thickness)
hist(df$Wall.Thickness,col="yellow")

#d
abline(v = 12.80205,col = 'red')


#Q2 
#a
vec=c()
for(i in 1:1000){
  vec[i]=mean(sample(df$Wall.Thickness,10,replace = TRUE))
}
vec
hist(vec)


#b
vec1=c()
k=1
colors=c('red','yellow','green')
size=c(50,500,9000)
par(mfrow=c(1,3))
for( j in size)
{
  
  for(i in 1:1000){
    vec1[i]=mean(sample(df$Wall.Thickness,j,replace = TRUE))
  }
  vec1
  hist(vec1,xlab = "WALL THICKNESS",col = colors[k])
  k=k+1
}

