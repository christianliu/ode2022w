##### Lotka Volterra
# dx/dt = ax - bxy
# dy/dt = -cy + dxy


# Update function
# Takes in input position (x,y), ODE parameters a,b,c,d>0, time step dt
# Outputs next position (x,y)
update <- function(x,y,a,b,c,d,dt){
  xdot <- a*x - b*x*y
  ydot <- -c*y + d*x*y
  return(c(x + xdot*dt, y + ydot*dt))
}

# Generates array of positions
# Takes in starting position (x_0,y_0), ODE parameters a,b,c,d>0, time step dt,
# and number of iterations to plot
simulate <- function(x_0,y_0,a,b,c,d,dt,n){
  xs <- vector(length = n+1)
  ys <- vector(length = n+1)
  xs[1] <- x_0
  ys[1] <- y_0
  
  for (i in 1:n){
    xy <- update(xs[i],ys[i],a,b,c,d,dt)
    xs[i+1] = xy[1]
    ys[i+1] = xy[2]
  }
  return(data.frame(xs,ys))
}

graph <- function(x_0s,y_0s,a,b,c,d,dt,n){
  data1 <- simulate(x_0s[1],y_0s[1],a,b,c,d,dt,n)
  data2 <- simulate(x_0s[2],y_0s[2],a,b,c,d,dt,n)
  data3 <- simulate(x_0s[3],y_0s[3],a,b,c,d,dt,n)
  plot(data1$xs,data1$ys, type = "l",
       xlab = "Population x",
       ylab = "Population y",
       xlim = c(0,2),
       ylim = c(0,2),
       main = paste0("Phase curves of dx/dt = ",a,"x - ",b,"xy, dy/dt = -",c,"y + ",d,"xy"))
  lines(data2$xs,data2$ys, type = "l", lwd = 1, col = "red")
  lines(data3$xs,data3$ys, type = "l", lwd = 1, col = "green")
  legend("bottomright", title = "Starting position",
         legend = c(paste0("(",x_0s[1],",",y_0s[1],")"),
                    paste0("(",x_0s[2],",",y_0s[2],")"),
                    paste0("(",x_0s[3],",",y_0s[3],")")),
         lwd = 3, col = c("black", "red", "green"), )
}

graph(c(.5,.8,1),c(.8,1,.5),1,1,1,1,.0001,100000)
graph(c(.5,.8,1),c(.8,1,.5),1,.5,1,1,.0001,100000)
graph(c(.5,.8,1),c(.8,1,.5),1,1,.5,1,.0001,100000)




