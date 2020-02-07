source(choose.files())
optim.function = function(x){
  ((600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1]))*(-1)
}

X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(optim.function,X)
## contour plot ##
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3)

aeq = matrix(c(12,5), nrow = 1)
beq = matrix(5000)

library(NlcOptim)

x = c(0,500)
ans = solnl(x, objfun = optim.function, Aeq = aeq, Beq = beq)
print(ans)
##########################################################
optim.function.b = function(x){
  ((600-3*x[1]+x[2])+x[2]*(800-2*x[2]+x[1]))*(-1)
}

X = list(x=seq(0,500),y=seq(0,500))
Z = Outer(optim.function,X)
## contour plot ##
contour(X$x,X$y,-Z)
abline(a=1000,b=-12/5,col="red",lwd=3)

A = matrix(c(12,5,-1,0,0,-1), nrow = 3)
B = matrix(c(5000,0,0), nrow = 3)

ans = solnl(x, objfun = optim.function.b, A = A, B = B)
print(ans)
