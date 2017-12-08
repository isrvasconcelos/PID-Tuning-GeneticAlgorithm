library(control)

###########################################
# Controller Settings

Kp <- 62 # Proportional Gain
Ki <- 2 # Integral Gain
Kd <- 96.5 # Derivative Gain

Controller <- tf(-c(Kd,Kp,Ki), c(0, 1, 0))


###########################################
# Actuator Settings
nA <- 1
nAs <- 0
dA <- 1
dAs <- 0


numActuator <- c(nAs, nA)
denActuator <- c(dAs, dA)

if (dAs == 0 && dA == 0)
	stop("Invalid parameters")

Actuator <- tf(numActuator, denActuator)


###########################################
# Plant Settings
	nP <- 0.435
	nPs <- 1
	nPs2 <- 0
	nPs3 <- 0
	nPs4 <- 0
	dP <- 0.1
	dPs <- 0.2
	dPs2 <- 10
	dPs3 <- 0
	dPs4 <- 0

numPlant <- -c(nPs4, nPs3, nPs2, nPs, nP)
denPlant <-  c(dPs4, dPs3, dPs2, dPs, dP)

if (dPs2 == 0 && dPs == 0 && dP == 0)
	stop("Invalid parameters")

Plant <- tf(numPlant, denPlant)


###########################################
# Sensor Settings
Ks  <- 1 # Sensor gain

Sensor <- tf(c(-Ks, 0), c(0, 1))


###########################################
# State-Space System Model

SetPoint <- 5 # Reference value
Act_Plant <- series(Actuator, Plant)
Act_Plant_Sensor <- feedback(Act_Plant, Sensor)

Act_Plant_Sensor_Controller <- series(Controller, Act_Plant_Sensor)
System <- feedback(Act_Plant_Sensor_Controller, 1/SetPoint)


###########################################
# Response Analysis and Manipulations - Part I
# Global Parameters 

StepTime <- 100
Precision <- .1
TimeInterval <- seq(0, StepTime, Precision)

SystemResponse <- step(System,t = TimeInterval) # Running Experiment

MSR <- SystemResponse$y[1,] # MSR: Magnitude of System Response
Error <- abs(SetPoint - MSR)/SetPoint # Evaluating the Befavior


###########################################
# Response Analysis and Manipulations - Part II
# Parameters to optmize (Minimization)

## Rise Time
RiseTimeIndex <- min(which(MSR>SetPoint*.9)) # Index of corresponding rise time
RiseTime <- StepTime
if (is.finite(RiseTimeIndex)) 
	RiseTime <- SystemResponse$t[RiseTimeIndex]

## Settling Time
LowDerivIndexes <- which(abs(diff(MSR))<.0001) # Indexes which derivatives are near to 0
LowErrorIndexes <- which( Error < .05 ) # Indexes which errors are less than 5%
LowDerivTimestamps <- SystemResponse$t[LowDerivIndexes]
LowErrorTimestamps <- SystemResponse$t[LowErrorIndexes]

SettlingTime <- max(SystemResponse$t)
if (is.finite(min(intersect(LowDerivTimestamps, LowErrorTimestamps)))) 
	SettlingTime <- min(intersect(LowDerivTimestamps, LowErrorTimestamps))

## Overshoot
OvershootIndex <- min(which( diff(MSR)<0 )) # Index to get the First Local Maxima
Overshoot <- max(MSR)
if (is.finite(OvershootIndex)) 
	Overshoot <- MSR[OvershootIndex-1]


###########################################
# Plotting and Outputs
stepplot(System, t = TimeInterval)
abline(h=SetPoint, col="red", lwd=3, lty=2)

plot(x=TimeInterval , y=Error*100, xlab='Time (s)', ylab='Error (%)', type='l', main='Step Response Error in Relation to the Setpoint ')

