source(choose.files())
result = function (x){
  (0.08*x)*(1-(x/400000))
}

x = seq(25000, 400000, 10000)
plot(x,result(x),type="o")
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

#After looking at the plot, it appears that the deriviative will be equal to zero around 200,000
x = NULL
x = c(200000, 8000)
dresult = function(x){df(result,x)}

newton(dresult, 220000, 8000)



  