source(choose.files())
#Blue Whales
obj_fun = function(x){
  0.05*x[1]*(1-(x[1]/150000)-(10^-8*x[1]*x[2]))*(-1)
}

x = c(100,100)
obj_fun(x)

ans.blue = multi.V(x,obj_fun)

#Fin Whales
Fin = function(x){(0.08*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])*(-1)}

x = c(100,100)
ans.fin = multi.V(x,Fin)

ans.blue$value
ans.fin$value


#Sensitivity of r for Blue Whales

sen_func = function(r){
  obj_fun = function(x){
    r*x[1]*(1-(x[1]/150000)-(10^-8*x[1]*x[2]))*(-1)
  }
  c = c(50000,50000)
  ans = multi.V(c,obj_fun)
  return(ans)
  
}

r = seq(0.02, 0.12, 0.01)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(r)){
  ans = sen_func(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[1] = ans$par[2]
  ans.rev[1] = ans$value
}

result = data.frame(growth_rate = r, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)


#Sensitivity of R for Fin Whales
sen_func = function(r){
  Fin = function(x){(r*x[2]*(1-x[2]/400000)-1/100000000*x[1]*x[2])*(-1)}
  
  c = c(50000,50000)
  ans = multi.V(c,Fin)
  return(ans)
  
}

r = seq(0.02, 0.12, 0.01)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(r)){
  ans = sen_func(r[i])
  ans.x1[i] = ans$par[1]
  ans.x2[1] = ans$par[2]
  ans.rev[1] = ans$value
}

result = data.frame(growth_rate = r, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)

#Sensitivity for K values for Fin Whales
sen_func = function(k){
  Fin = function(x){(0.08*x[2]*(1-x[2]/k)-1/100000000*x[1]*x[2])*(-1)}
  
  c = c(50000,50000)
  ans = multi.V(c,Fin)
  return(ans)
  
}

k = seq(300000, 400000, 10000)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(k)){
  ans = sen_func(k[i])
  ans.x1[i] = ans$par[1]
  ans.x2[1] = ans$par[2]
  ans.rev[1] = ans$value
}

result = data.frame(max_pop = k, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)

#Sensitivity for K values for Blue Whales
sen_func = function(k){
  obj_fun = function(x){
    0.05*x[1]*(1-(x[1]/k)-(10^-8*x[1]*x[2]))*(-1)
  }
  c = c(50000,50000)
  ans = multi.V(c,obj_fun)
  return(ans)
  
}

k = seq(300000, 400000, 10000)
ans.x1 = 0
ans.x2 = 0
ans.rev = 0
for (i in 1:length(k)){
  ans = sen_func(k[i])
  ans.x1[i] = ans$par[1]
  ans.x2[1] = ans$par[2]
  ans.rev[1] = ans$value
}

result = data.frame(max_pop = k, x1 = ans.x1, x2 = ans.x2, rev = ans.rev)
print(result)