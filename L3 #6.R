source(choose.files())
library(NlcOptim)
prof = function(x){
  (-0.0012*x^2+480*x+10*x-4000000)*(-1)
}

dresult = function(x){fprime(prof, x)}

opt.val = bisection(dresult, 230000, 8000)

plot(x, dresult(x))


#####################################Sensitivity of whaling costs

p = seq(250, 750, 50)

whale_kill = 0


for (i in 1:length(p)){
  p = p[i]
  sen.fun = function(p){
    prof.sens = function(x){
      (-0.012*x^2 + (480+0.02*p)-8000*p)
    }
    dresult = function(x){fprime(prof.sens, x)}
    
    opt.val = bisection(dresult, 230000, 8000)
  }
  
  whale_kill[i] = opt.val
  
}

result = data.frame(cost_boat = p[i], whale_kill = cost[i])
result

############################################# ANalyze sensitivity of cost of whale carcass