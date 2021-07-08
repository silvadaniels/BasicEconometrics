# Script to calculate economic indicators: Net Present Value and Internal Rate of Return

  # Net Present Value (NPV), i= interest rate, cf=net income vector (negative or positive), invest=initial investment (always negative)
    npv <- function(i, cf, invest, t=seq(along=cf)) sum(cf/(1+i)^t) +invest
  
  # Interest Rate of return (IRR):
    irr <- function(cf) { uniroot(npv, c(0,1), cf=values, inv)$root }
    
# test
  inv=-12526
  values=c(0,-4242,0,-2983,0,-2332,0,-2332,0,46192,-4886,-1190)
  rate= 0.1
  
  npv(rate, values, inv)
  irr(cf)
