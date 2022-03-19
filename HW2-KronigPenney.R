##### Kronig-Penney

H <- 3
V <- function(x){
  if ((x%%2 < 1) && (x%%2 != 0)) {
    return(0)
  } else {
    return(H)
  }
}


# Update function
# Takes in input position (y,ydot), ODE parameter E, time position x, step size dx
# Outputs next position (y,ydot)
update <- function(y,ydot,x,E,dx){
  ydotdot <- -(V(x) - E)*y
  return(c(y + ydot*dx, ydot + ydotdot*dx))
}

# Generates array of positions
# Takes in starting position (y,ydot), ODE parameter E, time position x, step size dx
# and number of iterations to simulate
simulate <- function(y_0,ydot_0,x,E,dx,n){
  ys <- vector(length = n+1)
  ydots <- vector(length = n+1)
  ys[1] <- y_0
  ydots[1] <- ydot_0
  x_curr <- x
  
  for (i in 1:n){
    yydot <- update(ys[i],ydots[i],x_curr,E,dx)
    ys[i+1] <- yydot[1]
    ydots[i+1] <- yydot[2]
    x_curr <- x_curr + dx
  }
  return(data.frame(ys,ydots))
}

data <- simulate(0,1,0,0,.001,100000)

par(mfrow=c(5,H))
Es <- seq(from = -5*H, to = 5*H, by = 1)
colors <- c("red", "blue","green","orange","brown")
for (i in 1:length(Es)){
  data <- simulate(0,1,0,Es[i],.001,100000)
  plot(data$ys, xlab = "x", ylab = "y", type = "l")
#  lines(data$ys, type = "l", col = colors[i])
}

par(mar=c(4,4,4,4))
par(mfrow=c(1,1))
stable <- c(T,T,T,T,T,T,T,T,T,T,T,T,T,T,F,F,T,F,F,F,F,F,F,F,F,F,F,F,F,F,F)
plot(Es, rep(0,10*H+1), type = 'o', pch = '|', ylab="",yaxt="n")
points(Es, rep(0,10*H+1), pch = 10, col = ifelse(stable,"blue","red"))
