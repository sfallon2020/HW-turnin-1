## Define the function using boolean statements such as >, <=, == 
r = 5/7
fT = function(d){500*(200/(r*(d+1)))+(18000+800*200/(r*(d+1)))*d+(200/(r*(d+1))>14)*(10000*(200/(r*(d+1))-14))}

x = seq(0,50,1)
plot(x,fT(x))





#function to find derivative
df = function (f,a,h=0.0001){(f(a+h)-f(a-h))/(2*h)}

#Function to find 0s
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

#finds derivative of our obj function
dt = function(x){df(fT,x)}

#finds the point at which the derivative is equal to 0 [local/global max/min]
bisection(dt,0,20)

#finds the lowest 
cost = fT(x)
ans = data.frame(crews = x, cost = cost)
which(ans$cost == min(ans$cost))
#12 is the index into the data frame which produces the lowest answer. ANS[that index] produces lowest value and associated cost
ans[which(ans$cost == min(ans$cost)),]


#purpose of fine is to enourage faster cleanup
#do a sensitivity on value of fine and the day which the fine is implemented

current.fine = 10000
current.crews = 11.28
current.cost = 508333.3
ans.crew = 0
ans.cost = 0
ans.days = 0
r = 5/7
freeDays = seq(1,50)
fineVals = seq(5000,15000, 1000)
for (i in 1:length(fineVals)){
  fine= fineVals[i]
  fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(fine*(200/(r*(x+1))-14))}
  dt = function(x){df(fT,x)}
  ans.crew[i] = bisection(dt, 0,50)
  ans.cost[i] = fT(ans.crew[i])
  ans.days[i] = 200/(r*(ans.crew[i]+1))
}
result = data.frame(fine = fineVals, crew = ans.crew, cost = ans.cost, days = ans.days)

print(result)

warnings()

#Piecewise example
#x = seq(-5,5,0.01)
#f = function(x)(x<=-2)*1+(x>-2)*(x<3)*(x)+(x>=3)*(-x) 
#f = Vectorize(f)
#plot(x,f(x),type="l")
##############################################################
#Sensitivity Analysis of clean-up Rate
rVals = seq(0.25, 1.25, 0.05)
#freeDays = seq(1,50)
#fineVals = seq(5000,15000, 1000)
for (i in (rVals)){
  r= rVals[i]
  fT = function(x){500*(200/(r*(x+1)))+(18000+800*200/(r*(x+1)))*x+(200/(r*(x+1))>14)*(10000*(200/(r*(x+1))-14))}
  dt = function(x){df(fT, x)}
  print(dt)
  ans.crew[i] = bisection(dt, 0,50)
  ans.cost[i] = fT(ans.crew[i])
  ans.days[i] = 200/(r*(ans.crew[i]+1))
}
result = data.frame(r_val = rVals, crew = ans.crew, cost = ans.cost, days = ans.days)

print(result)



