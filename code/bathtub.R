library( tidyverse)

## ========================================= ##
#      Define some initial values
#      n.steps is number of time steps after t = 0
## ========================================= ##
# volume at time zero
V0 <- 50

# time step, number of steps, and time vector
dt <- 1
n.steps <- 20
time <- seq( from = 0, by = dt, length.out = n.steps + 1)

## ========================================= ##
#      Define some flow values
# note: length(Qi) == length(Qo) == n.steps + 1
## ========================================= ##
Qi <- c( 0, rep( 0, 10), rep( 5, 10))
Qo <- c( 0, rep( 5, 20))

# plot the flows
par( mfrow = c( 1, 2))
plot( time, Qi, ylim = c( 0, 10), type = 'l',
      main = 'Bathtub, base model', ylab = 'Inflow, gal/min', xlab = 'time, min')
plot( time, Qo, ylim = c( 0, 10), type = 'l',
      main = 'Bathtub, base model', ylab = 'Outflow, gal/min', xlab = 'time, min')
par( mfrow = c( 1, 1))

## ========================================= ##
#      calculate the volume in the tub across time
## ========================================= ##
# initialize volume vector
V <- c()
V[1] <- V0

# calculate volumes 
for( t in 1:n.steps){
  V[t + 1] <- V[t] + (Qi[t + 1] - Qo[t + 1]) * dt
}

## ========================================= ##
#      plot volume across time in base model
## ========================================= ##
plot( time, V, 
      # plot aesthetics
      ylim = c( 0, 50), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Bathtub base case',
      ylab = 'Volume, gal',
      xlab = 'time, min')

## ========================================= ##
#   Feedback #1: balancing
#      calculate the volume in the tub across time
#      with feedback loop on inflow:
#        Qin == 2 if V[t-1] >  45
#        Qin == 10 if V[t-1] <= 45
## ========================================= ##
# initialize volume vector (fb1 stands for feedback 1)
V_fb1 <- c()
V_fb1[1] <- V0

# we'll start by assuming Qi_fb1 is the same as 
# Qi in the base case
Qi_fb1 <- Qi

# calculate volumes 
for( t in 1:n.steps){
  Qi_fb1[t + 1] <- ifelse( V_fb1[t] > 45, 2, 10)
  V_fb1[t + 1] <- V_fb1[t] + (Qi_fb1[t + 1] - Qo[t + 1]) * dt
}

## ========================================= ##
#      plot volume across time in feedback model #1
## ========================================= ##
# plot volume
plot( time, V_fb1, 
      # plot aesthetics
      ylim = c( 0, 50), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Bathtub, feeback #1',
      ylab = 'Volume, gal',
      xlab = 'time, min')

# plot the flows
par( mfrow = c( 1, 2))
plot( time, Qi_fb1, ylim = c( 0, 10), type = 'l', 
      main = 'Bathtub, feeback #1', ylab = 'Inflow, gal/min', xlab = 'time, min')
plot( time, Qo, ylim = c( 0, 10), type = 'l', 
      main = 'Bathtub, feeback #1', ylab = 'Outflow, gal/min', xlab = 'time, min')
par( mfrow = c( 1, 1))

## ========================================= ##
#   Feedback #2: balancing + reinforcing
#      calculate the volume in the tub across time
#      with feedback loop on inflow:
#        Qin ==  2 if V[t-1] >  45
#        Qin ==  6 if V[t-1] <= 45
#        Qout == 10 if V[t-1] >= 45
#        Qout == 8  if V[t-1] <  45
## ========================================= ##
# initialize volume vector (fb2 stands for feedback 2)
V_fb2 <- c()
V_fb2[1] <- V0

# we'll start by assuming Qi_fb2 and Qo_fb2 are the same as 
# Qi and Qo in the base case
Qi_fb2 <- Qi
Qo_fb2 <- Qo

# calculate volumes 
for( t in 1:n.steps){
  Qi_fb2[t + 1] <- ifelse( V_fb2[t] >  45, 2, 6)
  Qo_fb2[t + 1] <- ifelse( V_fb2[t] >= 45, 10, 8)
  V_fb2[t + 1] <- V_fb2[t] + (Qi_fb2[t + 1] - Qo_fb2[t + 1]) * dt
}

## ========================================= ##
#      plot volume across time in feedback model #1
## ========================================= ##
# plot volume
plot( time, V_fb2, 
      # plot aesthetics
      ylim = c( 0, 100), # y axis limits
      type = 'l', # 'l' here means line plot
      main = 'Bathtub, feeback #2',
      ylab = 'Volume, gal',
      xlab = 'time, min')

# plot the flows
par( mfrow = c( 1, 2))
plot( time, Qi_fb2, ylim = c( 0, 10), type = 'l', 
      main = 'Bathtub, feeback #2', ylab = 'Inflow, gal/min', xlab = 'time, min')
plot( time, Qo_fb2, ylim = c( 0, 10), type = 'l', 
      main = 'Bathtub, feeback #2', ylab = 'Outflow, gal/min', xlab = 'time, min')
par( mfrow = c( 1, 1))







