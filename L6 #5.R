source(choose.files())
library(NlcOptim)

P = function(x){
  return(((339-0.01*x[1]-0.003*x[2])*x[1]+
          (399-0.004*x[1]-0.01*x[2])*x[2]-
          (400000+195*x[1]+225*x[2]))*-1)-
          (25*x[1]+25$x[2])
}
x = c(500,500)

ans = optim(x,P,method = "BFGS")
ans
