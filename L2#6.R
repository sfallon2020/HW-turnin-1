source(choose.files())
library(NlcOptim)


profit = function(x){
  x[1]*((10000*(200*x[2]))*(0.5*(950*x[1])/100))-x[2]+(700*(10000+(200*x[2])/10000)*((950*x[1])/100))
}

x  = c(10000, 10000)
A = matrix(c(1,0,0,1), nrow = 2, byrow = T)
B = matrix(c(700, 100000), nrow = 2, byrow = T)

ans = solnl(x, objfun = profit, A = A, B = B)


#Snesitivity Analysis: 




sen_fun = function(r){
  profit = function(x){
    x[1]*((10000*(200*x[2]))*(r*(950*x[1])/100))-x[2]+(700*(10000+(200*x[2])/10000)*((950*x[1])/100))
  }
  
  x  = c(10000, 10000)
  A = matrix(c(1,0,0,1), nrow = 2, byrow = T)
  B = matrix(c(700, 100000), nrow = 2, byrow = T)
  
  ans = solnl(x, objfun = profit, A = A, B = B)
  
}

r = seq(0.3, 0.7, 0.05)
for(i in 1:length(r)){
  ans = sen_fun(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[1] = ans$par[2]
  ans.rev[1] = ans$value
}

result = data.frame(growth_rate = r, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)
