##### Lissajous curves

# controls step sizes for parameterization
index <- seq(0, 100, by = 0.001)

# plots standard circle for reference
xs <- cos(index)
ys <- sin(index)
plot(xs,ys, type = "l",
     xlab = "x",
     ylab = "y",
     xlim = c(-1.5,2.5),
     ylim = c(-1.5,1.5),
     main = "Lissajous curves")

# graphs curve given parameters and color
graph <- function(a,b,color){
  xs <- cos(a*index)
  ys <- sin(b*index)
  lines(xs,ys,col=color)
}

# graphs with a = 1 and b varying
as <- c(1,1,1,1)
# bs <- c((1+sqrt(5))/2,3+sqrt(3),4+sqrt(32),pi)
bs <- c(2,3,4,10)
colors <- c("red","green","purple","orange")
for(i in 1:4){
  graph(as[i],bs[i],colors[i])
}
legend("bottomright", title = "Parameters",
       legend = c(paste0("a=1, b=1"),
                  paste0("a=",as[1],", b=",bs[1]),
                  paste0("a=",as[2],", b=",bs[2]),
                  paste0("a=",as[3],", b=",bs[3]),
                  paste0("a=",as[4],", b=",bs[4])),
       lwd = 3,
       col = c("black","red","green","purple","orange"))


##### Benford's law
n <- 1000
index <- index <- seq(1, n, by = 1)
powers <- 13^index
firstdig <- as.numeric(substr(powers, 1, 1))
hist(firstdig,
     breaks = seq(0,9,by=1),
     xlab = "First digit",
     ylab = "Count",
     main = "First digit of 3^n")
