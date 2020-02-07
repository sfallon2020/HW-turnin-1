source(choose.files())

x = seq(0,20,1)
profit = function(t){(0.65-0.1*t)*(200+5*t)-0.45*t}
dProfit = function(t){df(profit,t)}
plot(x,profit(x), "l")

profit.b = function (x){
  return((0.65-0.01*x+0.00004*x^2)*(200+5*x)-.45*x)
}
#Plots for part a
x = seq(0,20,1)
plot(x, profit(x), type = "o")
plot(x,profit.b(x),type="o")


print(profit(10))

x=seq(0,20)
df = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
plot(x,df(profit.b,x))


#Example of a finding a root of a function
#Best time to sell pig with eq. 1.5
dProfit = function(x){df(profit.b,x)}
bisection(dProfit,0,20)
profit.b(bisection(dProfit,0,20))


#Price falling per day = p

ans.time = 0
ans.profit = 0
r = seq(0.00001, 0.0001, 0.00001)
for (i in 1:length(r)){
  profit = function(t){(0.65-0.01*t)+(r[i]*t^2)*(200+5*t)-0.45*t}#r[i] indexes from r
  dProfit = function(t){df(profit,t)}
  ans.time[i] = bisection(dProfit,0,20)
  ans.profit[i] = profit(ans.time[i])
  
}


result = data.frame(Leveling_Value = r, time = ans.time, profit = ans.profit)
print(result)
max(result$profit)



###########################################################3
#Growth rate of the pig = g
g = seq(1,12,0.5)
ans = array(0,length(g))
for (i in 1:length(g)){
  profit = function (x){
    return((0.65-0.01*x)*(200+g[i]*x)-.45*x)
  }
  dProfit = function(x){df(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
}
print(ans)
plot(g,ans,"o",xlab="g(growth/day)",ylab="x(Days to Sell)")
title("Sensitivity of Growth Rate of Pig")

########################################################
p = seq(0.008,0.012,0.001)
ans = array(0,length(p))
for (i in 1:length(p)){
  profit = function (x){
    return((0.65-p[i]*x)*(200+5*x)-.45*x)
  }
  dProfit = function(x){df(profit,x,)}
  ans[i] = bisection(dProfit,0,20,0.0001)
}
print(ans)
plot(p,ans,"o",xlab="p($/day)",ylab="x(Days to Sell)")
title("Sensitivity of Falling Price of Pig")

