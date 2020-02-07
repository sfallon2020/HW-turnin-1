source(choose.files())

obj.fun = function(x){
  (10*x[1]^(0.6)*x[2]^(0.4))*-1
}

## Countour Plot ##
X = list(x = seq(0,20,0.1), y = seq(0,20,0.1))
Z = Outer(obj.fun, x) #X's go across, Y goes verticlly, values are f(x,y)

contour(x = X$x, y = X$y, z = -Z, lwd = 2, 
        levels = c(20,37.55, 60, 80, 100,120,140,180))
# constraint : 50 x1 + 30x2 = 300
abline(a = 10, b = -5/3, col = "red", lwd = 2)
#Equality constraint means that feasible values are found only on the lines

install.packages("NlcOptim")
library("MASS")
library("NlcOptim")

Aeq = matrix(c(50,30), nrow = 1)
Beq = matrix(300)
x0 = c(3,5)

ans = solnl(x0, objfun = obj.fun, Aeq = Aeq, Beq = Beq)

print(ans)



###################################### Board Sheet #1#####################
#u = x2, t = x1
obj = function(x){
  (1/2)*(x[2]-32)*(x[1]^2)
}

con = function(x){
  f = null
  f = rbind(f,x[2]^2*x1-10000) #want this to be equak to zero -- hence (-10000)
  return(list(ceq = f, c = NULL))
}
x0 = c(1,100) #needs to solve constraint
solnl(x0, objfun = obj, confun = con)
