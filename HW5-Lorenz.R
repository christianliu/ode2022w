##### Lorenz
# dx/dt = a(y-x), dy/dt = x(b-z) - y, dz/dt = xy - cz
library(plotly)

# Update function
# Takes in input value (x,y,z), ODE parameter a,b,c, time step dt
# Outputs next position (x,y,z)
update <- function(x,y,z,a,b,c,dt){
  xdot <- a*(y-x)
  ydot <- x*(b-z)-y
  zdot <- x*y - c*z
  return(c(x + xdot*dt, y + ydot*dt, z + zdot*dt))
}

# Generates array of positions
# Takes in starting position (x,y,z), ODE parameter a,b,c, time step dt
# and number of iterations to plot
simulate <- function(x_0,y_0,z_0,a,b,c,dt,n){
  xs <- vector(length = n+1)
  ys <- vector(length = n+1)
  zs <- vector(length = n+1)
  xs[1] <- x_0
  ys[1] <- y_0
  zs[1] <- z_0
  
  for (i in 1:n){
    xyz <- update(xs[i],ys[i],zs[i],a,b,c,dt)
    xs[i+1] <- xyz[1]
    ys[i+1] <- xyz[2]
    zs[i+1] <- xyz[3]
  }
  return(data.frame(xs,ys,zs))
}


## Graphing phase curves based on a cloud of initial conditions
x_0s <- c(1,1,1,1.2,1.2,1.2,1,1.2)*10^2
y_0s <- c(1,1.2,1,1,1.2,1,1.2,1.2)*10^2
z_0s <- c(1,1,1.2,1,1,1.2,1.2,1.2)*10^2
# x_0s <- c(0,0,0,0.2,0.2,0.2,0,0.2)
# y_0s <- c(0,0.2,0,0,0.2,0,0.2,0.2)
# z_0s <- c(0,0,0.2,0,0,0.2,0.2,0.2)

p <- plot_ly() %>% layout(title = "Phase curves")
## Add the traces one at a time
for(i in 1:length(x_0s)){
  data <- simulate(x_0s[i],y_0s[i],z_0s[i],10,28,8/3,.001,100000)
  p <- p %>% add_trace(x=data$xs, y=data$ys, z=data$zs, 
                       type = "scatter3d", mode = "lines")
}

p

## Graphing x trajectories
data <- simulate(x_0s[1],y_0s[1],z_0s[1],10,28,8/3,.001,100000)
plot(data$xs, type = "l", xlab="t", ylab="x", main = "Evolution of x")
color = c("red", "green", "blue")
for(i in 2:4){
  data <- simulate(x_0s[i],y_0s[i],z_0s[i],10,28,8/3,.001,100000)
  lines(data$xs, type = "l", col = color[i-1])
}
