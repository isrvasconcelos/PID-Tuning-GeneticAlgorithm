library(control)

###########################################
# Controller Settings

	# Kp: Proportional Gain
	# Ki: Integral Gain
	# Kd: Derivative Gain

#	args <- commandArgs(TRUE)

#	args
#	Kp <- as.double(args[1])
#	Ki <- as.double(args[2])
#	Kd <- as.double(args[3])


#	if(length(args) != 3) {
#		print("WARNING: Number of parameters does not match. Setting Defaults.")
#		Kp = 1; Ki = 1;	Kd = 1;
#	}

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


###########################################
# Sensor Settings
	Ks  <- 1 # Sensor gain


###########################################
# State-Space System Model Defaults
	SetPoint <- 5 # Reference value


###########################################
# Response Analysis and Manipulations Defaults
	StepTime <- 100
	Precision <- .1
	TimeInterval <- seq(0, StepTime, Precision)


###########################################
# Fitness Function for Genetic Algorithm
fitness <- function(Kp, Ki, Kd) {

	Controller <- tf(-c(Kd,Kp,Ki), c(0, 1, 0))
	Actuator <- tf(numActuator, denActuator)
	Plant <- tf(numPlant, denPlant)
	Sensor <- tf(c(-Ks, 0), c(0, 1))

	###########################################
	# State-Space System Model Wiring

	Act_Plant <- series(Actuator, Plant)
	Act_Plant_Sensor <- feedback(Act_Plant, Sensor)

	Act_Plant_Sensor_Controller <- series(Controller, Act_Plant_Sensor)
	System <- feedback(Act_Plant_Sensor_Controller, 1/SetPoint)

	SystemResponse <- step(System, t=TimeInterval) # Running Experiment

	MSR <- SystemResponse$y[1,] # MSR: Magnitude of System Response - (Samples)
	Error <- abs(SetPoint - MSR)/SetPoint # Evaluating the Behavior

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
		Overshoot <- SystemResponse$t[OvershootIndex-1]

	Fit <- ((RiseTime+SettlingTime)/StepTime+ abs(Overshoot-SetPoint)/SetPoint)/2

	return(data.frame(Fit, Error, TimeInterval, SettlingTime))

}


###########################################
# Plotting and Outputs

plotResponse <- function() {
	stepplot(System, t = TimeInterval)
	abline(h=SetPoint, col="red", lwd=3, lty=2)

	plot(x=seq(0, StepTime, Precision) , y=Error*100, xlab='Time (s)', ylab='Error (%)')
}
