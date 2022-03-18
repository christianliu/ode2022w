##### van der Pol
# d^2x/(dt)^2 - \mu(1-x^2)dx/dt + x = 0


# Update function
# Takes in input value (x,xdot), ODE parameter mu, time step dt
# Outputs next position (x,xdot)
update <- function(x,xdot,mu,dt){
  xdotdot <- mu*(1-x^2)*xdot - x
  return(c(x + xdot*dt, xdot + xdotdot*dt))
}

# Generates array of positions
# Takes in starting position (x,xdot), ODE parameter mu, time step dt,
# and number of iterations to plot
simulate <- function(x_0,xdot_0,mu,dt,n){
  xs <- vector(length = n+1)
  xdots <- vector(length = n+1)
  xs[1] <- x_0
  xdots[1] <- xdot_0
  
  for (i in 1:n){
    xxdot <- update(xs[i],xdots[i],mu,dt)
    xs[i+1] = xxdot[1]
    xdots[i+1] = xxdot[2]
  }
  return(data.frame(xs,xdots))
}

graph <- function(x_0s,xdot_0s,mu,dt,n){
  data1 <- simulate(x_0s[1],xdot_0s[1],mu,dt,n)
  data2 <- simulate(x_0s[2],xdot_0s[2],mu,dt,n)
  data3 <- simulate(x_0s[3],xdot_0s[3],mu,dt,n)
  plot(data1$xs,data1$xdots, type = "l",
       xlab = "x(t)",
       ylab = "x'(t)",
       xlim = c(-5,5),
       ylim = c(-5,5),
       main = paste0("Phase curves of van der Pol equation with mu=",mu))
  lines(data2$xs,data2$xdots, type = "l", lwd = 1, col = "red")
  lines(data3$xs,data3$xdots, type = "l", lwd = 1, col = "green")
}


graph(c(0,1,3),c(1,0,3),2,.001,100000)
graph(c(0,1,3),c(1,0,3),1,.001,100000)
graph(c(0,1,3),c(1,0,3),.5,.001,100000)
graph(c(0,1,3),c(1,0,3),.1,.001,100000)
graph(c(0,1,3),c(1,1,3),0,.001,100000)




