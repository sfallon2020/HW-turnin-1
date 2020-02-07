source(choose.files())

func = function(x,y){
  (-1)*(x[1]*x[2]-2*x[1]-2*x[2]-x[1]^2-x[2]^2)
}
#MUST MULTIPLY FUNCTION BY -1 IN ORDER TO CREATE MINIMIZATION PROBLEM
x = c(0,0)

ans = multi.V(x,func)
print(ans$par)


#Blue Whale harvest rate
Blue = function(x){(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2])}

#Fin Whale harvest rate
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])}


#Revenue from hunting Blue and Fin whales
Rev = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
                     6*(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)}

c = c(50000,50000)
whale = multi.V(c,Rev)
whale$par
whale$value

Blue(whale$par)
Fin(whale$par)


#Sensitivity analysis to examine values of X[1] and X[2] and the effect the r-value has on them. 

R2 = function(r2){
  fr2 = function(x){(12*(0.05*x[1]*(1-x[1]/150000)-1/100000000*x[1]*x[2]) +
     6*(r2*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2]))*(-1)
  }
  c = c(50000,50000)
  ans = multi.V(c,Rev)
  return(ans)
}

v = seq(0.02, 0.12, 0.01)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(v)){
  ans = R2(v[i])
  ans.x1[i] = ans$par[i]
  ans.x2[i] = ans$par[i]
  ans.rev[i] = ans$value
}

result = data.frame(growth_rate = v, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)





