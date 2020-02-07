bisection = function(f,a,b,tol=0.0001){
  if (f(a)*f(b) > 0){
    return ("Boundary Conditions Not Met")
  }
  else{
    middle = a
    while (abs(f(middle))>tol){
      middle = (a+b)/2
      if (f(middle)*f(a)>0) (a = middle)
      else (b = middle)
      x=middle
      y=f(middle)

    }
    return (middle)
    
df = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

x = seq(0,20,1)
profit = function(t){(0.65-0.1*t)*(200+5*t)-0.45*t}
dProfit = function(t){df(profit,t)}
plot(x,profit(x), "l")
ans = bisection(dProfit,0,20)
print(ans)

profit(ans)

ans.time = 0
ans.profit = 0
r = seq(0.45, 0.75, 0.01)
for (i in 1:length(r)){
  profit = function(t){(0.65-0.01*t)*(200+7*t)-r[i]*t}#r[i] indexes from r
  dProfit = function(t){df(profit,t)}
  ans.time[i] = bisection(dProfit,0,20)
  ans.profit[i] = profit(ans.time[i])
   
}


result = data.frame(price = r, time = ans.time, profit = ans.profit)
print(result)
max(result$profit)
