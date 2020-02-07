source(choose.files())

func.a = function(x){
  (x[1]^2+x[2]^2)*(-1)
}

X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(func.a,X)
## contour plot ##
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3)

A = matrix(c(1,2,-1,0,0,-1), nrow = 3, byrow = TRUE)
B = matrix(c(5,0,0), nrow = 3, byrow = TRUE)

con = function(x){
  f = NULL
  f = rbind(f,x[1]+2*x[2]-2)
  return(list(ceq = f, c = NULL))
}

library(NlcOptim)

x = c(1,2)
ans = solnl(x, objfun = func.a, confun = con)

############################################################
#CORRECT
func.b = function(x){
  (x[1]^2-x[2]^2)*(-1)
}

con.b = function(x){
  f = NULL
  f = rbind(f, (2*x[2]-x[1]^2))
  return(list(ceq = f, c = NULL))
}

x = c(1,1)

ans = solnl(x, objfun = func.b, confun = con.b)
print(ans)

#################################################################
#correct
func.c = function(x){
  exp((-1*(x[1])*(x[2]))/4)*(-1)
}

con.c = function(x){
  f = NULL
  f = rbind(f, x[1]^2+x[2]^2-1)
  return(list(ceq = f, c = NULL))
}

x = c(1,1)
ans = solnl(x, objfun = func.c, confun = con.c)

##############################################
#max is multiplied by -1. min is not. 
func.d = function(x){
  (x[1]^2+x[2]^2+x[3]^3)*(-1)
}

x = c(1,1,1)
#regular equality const. 
Aeq = matrix(c(1,0,2,1,1,0), nrow = 2, byrow = T)
Beq = matrix(c(6,12))
#non-negativity const. 
A = matrix(c(-1,0,0,0,-1,0,0,0,-1), nrow = 3, byrow = T)
B = matrix(c(0,0,0))
#solution
ans = solnl(x, objfun = func.d, Aeq = Aeq, Beq = Beq, A = A, B = B)
#################################################################

#Problem 2

rev.func = function (x,y){
  (-2*x^2-y^2+x*y+8*x+3*y)*(-1)
}
A = matrix(c(3000, 0, 0, 1000), nrow = 2)
B = matrix(c(10000,10000))

