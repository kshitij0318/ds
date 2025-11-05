5+3
10-3
10/5
5*5

a=5
b=3

sum_result = a-b
sub_result = a -b
mul_result = a *b
div_result <- a / b

print (sum_result)
print(sub_result)
print(mul_result)
print(div_result)
sqrt(16)
log(100)
exp(2)

add <- function(x, y){
  return (x+y)
}
sub <- function(x,y){
  return (x-y)
}
mul <- function(x,y){ return (x*y)}
div <- function(x,y){
  return (x/y)
}
mod <- function(x, y){
  return (x %% y)
}
square <- function(x){
  return (x^2)
}
cube <- function(x){
  return (x^3)
}
print(add(a,b))
print(sub(a,b))
print(mul(a,b))
print(div(a,b))
print(mod(a,b))
print(square(a))
print(cube(a))
print(a==b)

getwd()
setwd("C:/OneDrive/Desktop/Coding projects/dsl")
print(getwd())
write.csv(a,'a.csv')
write.csv(a, "C:/OneDrive/Desktop/Coding projects/dsl/a.csv")
data <- read.csv("a.csv")
print(data)