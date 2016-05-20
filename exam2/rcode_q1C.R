## pred prey function

pred.prey <- function(t, y, p) {
  H <- y[1]
  Z <- y[2]
  with(as.list(p), {
    dH.dt <- r * H * (1 - H/K) - b * H * Z
    dZ.dt <- c * H * Z - m * Z
    return(list(c(dH.dt, dZ.dt)))
  })
}

## starting params + pop sizes
p <- c('r' = 1,
       'b' = 1,
       'c' = 1,
       'm' = 0.1,
       'K' = 1)
y0 <- c('H' = 1, 'Z' = .1)
t <- 1:100

## run sim
sim <- ode(y = y0, times = t, func = pred.prey, parms = p, method = 'lsoda')
sim <- as.data.frame(sim)

## plot pop sizes over time
plot(H ~ time, data = sim, type = 'l', col = 'darkgreen', bty = 'l', lwd = 2, ylim = c(0,1.5), ylab = "Population size", xlab = "Time")
points(Z ~ time, data = sim, type = 'l', col = 'purple', lty = 2, lwd = 2)


## calculate human to zombie ratio at t = 100
sim$H[100]/sim$Z[100]
