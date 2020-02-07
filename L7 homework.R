source(choose.files())


P = function(x){((339-x[1]-0.003*x[2])*x[1]
                   +(399-0.004*x[1]-0.01*x[2])*x[2]
                   -(400000+195*x[1]+225*x[2]))*-1
}



P.tariff = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1])
          +(399-0.004*x[1]-0.01*x[2])*x[2])
          -(400000+195*x[1]+225*x[2]+(25*x[1]*x[2])*-1)
}



x = c(500,500)


#p-value
ans.P = multi.V(x,P)

ans.tariff = multi.V(x, P.tariff)
#OG FUnvtion val
print(ans.P)

diff = -1*((ans.P$value)-(ans.tariff$value))
print(diff)
#The difference in profit is 720,879.50 dollars when the tarrif is applied. 

#b. It would not be worth moving the buisness because you will still have less of a loss from tarrifs then moving, rebuilfing and manufacturing here. 


#c
result = function(a){
  P.tariff = function(x){
    return(((339-0.01*x[1]-0.003*x[2])*x[1])
           +(399-0.004*x[1]-0.01*x[2])*x[2])
    -(400000+195*x[1]+225*x[2]+(a*x[1]*x[2])*-1)
  }
  x = c(5000,500)
  ans = multi.V(x, P.tariff)
}

a = seq(15, 35, 1)
ans.x1 = 0
ans.x2 = 0

ans.profit = 0

for (i in 1:length(a)){
  ans = result(a[i])
  ans.x1 = ans$par[1]
  ans.x2 = ans$par[2]
  ans.profit[i] = -1*(ans$value)
  
}
result = data.frame(a=a, x1 = ans.x1, x2 = ans.x2, profit = format(ans.profit,big.mark = ","))


