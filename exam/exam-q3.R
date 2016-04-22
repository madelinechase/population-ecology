library(deSolve)

#function for theta growth
theta.growth <- function(t, y, p){
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1-(N/K)^theta)
    return(list(dN.dt))
  })
}

#params for tomatot and y0, t
p1 <- c('K' = 1.05, 'r' = 0.2, 'theta' = 1.05)
y0 <- c('N' = 0.01)
t <- 1:100

#params for grape
p2 <- c('K' = 0.75, 'r' = 0.28, 'theta' = 1.25)

#params for peach
p3 <- c('K' = 1 , 'r' = 0.15, 'theta' = 1)

#sim for tomato
sim1 <- ode(y = y0, times = t, func = theta.growth, parms = p1, method = 'lsoda')
sim1 <- as.data.frame(sim1)
#growth rate for tomato
growth1 <- c(diff(sim1$N), NA)

#sim for grape
sim2 <- ode(y = y0, times = t, func = theta.growth, parms = p2, method = 'lsoda')
sim2 <- as.data.frame(sim2)
#growth for grape
growth2 <- c(diff(sim2$N), NA)

#sime for peach
sim3 <- ode(y = y0, times = t, func = theta.growth, parms = p3, method = 'lsoda')
sim3 <- as.data.frame(sim3)
#growth for peach
growth3 <- c(diff(sim3$N), NA)

#plot per capita growth rate vs pop size
plot(growth1~sim1$N, type = "l", col = "red", xlab = "Pop size  (Individuals)", ylab = "Growth rate", ylim = c(0,0.07))
points(growth2~sim2$N, type = "l", col = "green")
points(growth3~sim3$N, type = "l", col = "blue")
#add legend
legend("topright", legend = c('tomato', 'grapes', 'peach'), col = c("red","green", "blue"), lty =1, inset = c(0.05,0.05))
