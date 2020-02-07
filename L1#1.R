source(choose.files())
profit = function (x){
  return((1500-100*x)*(1+0.15*x))
}
x = seq(0,15,1)
plot(x,profit(x),type="o")
print(profit(x))

x=seq(0,20)
df = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}
plot(x,df(profit,x))

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
      ## if you want to "see" what happens at every step, take off the # of the next line ##
      #cat(sprintf("x-Val: %.4f ; f(x-val): %.4f\n",x,y))
    }
    return (middle)
  }
}
dProfit = function(x){df(profit,x)}
bisection(dProfit,0,20)

#b

g = seq(0,0.5,0.01)
ans = array(0,length(g))
for (i in 1:length(g)){
  profit = function (x){
    return((1500-100*x)*(1+g[i]*x))
  }
  dProfit = function(x){df(profit,x,)}
  ans[i] = bisection(dProfit,-100,50,0.0001)
}
print(ans)
ans


#c.

h = ans[11:15]
print(h)


