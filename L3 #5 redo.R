source(choose.files())
result = function (x){
  (0.08*x)*(1-(x/400000))
}

#x = seq(25000, 400000, 10000)
#plot(x,result(x),type="o")

dresult = function(x){fprime(result, x)}

opt.val = bisection(dresult, 230000, 8000)

#sensitivity of Intrinsic growth rate
b = seq(0.04, 0.05, 0.01)

ans.x = 0


for (i in 1:length(b)){
  
  f = function(b){
    res.sense = function (x){
      (b[i]*x)*(1-(x/400000))
    
    }
    x = NULL
    dresult = function(x){fprime(result, x)}
    opt.val = bisection(dresult, 230000, 8000)
    
  }
  ans = f(b)
  ans.x = ans
}

result = data.frame(Int_Growth_rate = b, Pop = ans.x)
result

#sensitivity of Max Population

c = seq(50000, 90000, 2000)

ans.x.c = 0

for (i in 1:length(c)){
  c = c[i]
  pop.func = function(c){
    res.sense = function (x){
      (b[i]*x)*(1-(x/400000))
    }
    x = NULL
    dresult = function(x){fprime(result, x)}
    opt.val = bisection(dresult, 230000, 8000)
  }
  ans.x.c = pop.func(c)
  ans.x = ans.x.c
  }
result.c = data.frame(Max_Pop = c, Pop = ans.x)
result.c
