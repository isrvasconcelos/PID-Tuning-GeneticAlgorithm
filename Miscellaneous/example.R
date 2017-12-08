library(control)
## 
## Attaching package: 'control'
## The following object is masked from 'package:base':
## 
##     append
R <- C <- L <- 1
R2  <-  20
t  <-  seq(0,250,0.05)
G1 <-  tf(c(1/(R*C), 0), c(1, 1/(R*C), 1/(L*C) ) )
G2  <-  tf(c(1/(R2*C), 0), c(1, 1/(R2*C), 1/(L*C) ) )
print(G1)
## 
## y1:
##              s 
##   - - - - - - - - -
##      s^2 +  s + 1 
## 
## 
## Transfer Function: Continuous time model
tf2ss(G1) # example: convert the transfer function to state-space representation
## 
##  sys.A = 
## 
##      x1   x2
## x1   -1   -1
## x2    1    0
##    
## sys.B = 
## 
##      u1
## x1    1
## x2    0
##    
## sys.C = 
## 
##      x1   x2
## y1    1    0
##    
## sys.D = 
## 
##      u1
## y1    0
##    
## 
##  State-Space system: Continuous time model
c2d(G1, 0.1) # example: convert the system G1 from continuous-time to discrete-time model
## 
## y1:
##   0.09500408 z^1 - 0.09500408 
##   - - - - - - - - - - - - - - - - -
##      z^2 - 1.895329 z + 0.9048374 
## 
## 
## Sample Time = 0.1 
## Transfer function: Discrete time model
response <- lsim(G2,sin(t),t)
par(mfrow = c(3,1))
plot(t, sin(t), type = "l", xlab = "Time, secs", ylab = "Amplitude", main = "Linear Simulation Response - w = 1", col = "lightgray")
lines(t, response$y, type = "l", col = "blue", lwd = 2)
grid(5,5, col = "lightgray")

response2 <- lsim(G2, sin(0.9*t), t)
plot(t, sin(t), type = "l", xlab = "Time, secs", ylab = "Amplitude", main = "Linear Simulation Response - w = 0.9", col = "lightgray")
lines(t, response2$y, type = "l", col = "blue", lwd = 2)
grid(5,5, col = "lightgray")

response3 <- lsim(G2, sin(1.1*t), t)
plot(t, sin(t), type = "l", xlab = "Time, secs", ylab = "Amplitude", main = "Linear Simulation Response - w = 1.1", col = "lightgray")
lines(t, response3$y, type = "l", col = "blue", lwd = 2)
grid(5,5, col = "lightgray")

