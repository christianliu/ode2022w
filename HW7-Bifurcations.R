##### Bifurcations

n <- 1000
rs <- seq(from = 0, to = 4, by = 1/n)

# Given x_n, returns x_{n+1}
update <- function(r, x){
  return(r*x*(1-x))
}

xs <- vector(length = 2*n+1)
xs[1] <- .5
plot(0, 0,
    xlim = c(0,4),
    ylim = c(0,1),
    xlab = "r",
    ylab = "orbits",
    pch=20, cex = .01)
for (r in rs){
  for (i in 1:(2*n)){
    xs[i+1] <- update(r, xs[i])
  }
  lines(rep(r,n+1), xs[(n+1):(2*n+1)],
       type = "p",
       xlim = c(0,4),
       ylim = c(0,1),
       pch=20, cex = .5)
}

