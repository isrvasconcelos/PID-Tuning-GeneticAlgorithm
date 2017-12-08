library(control)

###########################################
# Controller Settings

Kp <- 10 # Proportional Gain
Ki <- 1 # Integral Gain
Kd <- 1 # Derivative Gain

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
dPs2 <- 1
dPs3 <- 3
dPs4 <- 1

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

Threshold <- 5 # Reference value
Act_Plant <- series(Actuator, -Plant)
Act_Plant_Sensor <- feedback(Act_Plant, Sensor)

Act_Plant_Sensor_Controller <- series(Controller, Act_Plant_Sensor)
System <- feedback(Act_Plant_Sensor_Controller, 1/Threshold)


###########################################
# Response Analysis and Manipulations
SystemResponse <- step(System,t = seq(0,60,0.1))

stepplot(System, t = seq(0,60,0.1))
abline(h=Threshold, col="red", lwd=3, lty=2)
