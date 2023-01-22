vec=c(5,10,15,20,25,30)
print(vec)
print(max(vec))
print(min(vec))

n=readline('Enter the number ')
fact=1
for(i in 1:n)
  fact=fact*i
print(fact)

m=readline('Enter the limit')
a=0
b=1
for(i in 1:m){
  c=a+b
  print(a)
  a=b
  b=c
}
num1=readline("Enter the first number ")
num2=readline("Enter the second number ")
num1=as.integer(num1)
num2=as.integer(num2)
op=readline("1.Addition\n2.Multiplication\n3.Subtraction\n4.Division\nEnter your option\n")
result = switch(
                 op,
  "1"=cat("Addition =",num1 + num2),
  "2"=cat("Multiplication =",num1 * num2),
  "3"=cat("Division =",num1 / num2),
  "4"=cat("Subtraction =",num1 - num2),
)
print(result)
  
max.temp=c(22,27,26,20,33)
barplot(max.temp)

plot(max.temp)
pie(max.temp)

