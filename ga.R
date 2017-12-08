source("fitness.R")

geneticAlgorithm <- function(population) {

	firstRun <- TRUE
	qtArgs <- 3
	maxiter <- 250
	popsize <- 20
	currentPop <- 1
	range <- seq(from=.5, to=100, by=.5)
	elitism <- 5
	mutation <- 40 # Mutation probability in %


	winner <- matrix(nrow=elitism, ncol=qtArgs)
	fitVector <-  vector(mode="double", length=popsize)

	if(is.null(population)) {
		population <- matrix(nrow=popsize, ncol=qtArgs)
	}

	else {
		auxPop <- matrix(nrow=popsize, ncol=qtArgs)

		currentPop <- nrow(population)

		print(currentPop)

		auxIt <- 1
		for(i in currentPop:1){
			auxPop[auxIt,1]<-pop[(i),1]
			auxPop[auxIt,2]<-pop[(i),2]
			auxPop[auxIt,3]<-pop[(i),3]

			auxIt=auxIt+1
			if(auxIt > popsize)
				break
		}

		population <- auxPop
		firstRun=FALSE
	}

	colnames(population) <- c("Kp","Ki","Kd")

	###########################################
	# Start Population
	population[currentPop:popsize,] <- sample(range, size=length(currentPop:popsize)*qtArgs)

	print("###################")
	print("Current Population:")
	print(population)
	print("###################")

	for(i in 1:maxiter) {

		###########################################
		# Fill fitVector andEvaluate Fitness
		for(j in 1:popsize) {
			Kp = population[j,1]
			Ki = population[j,2]
			Kd = population[j,3]

			suppressWarnings(fitVector[j] <- fitness(Kp, Ki, Kd)$Fit)
		}

		###########################################
		# Selecting the best ones
		sortedFitVector <- sort(fitVector)
		for(j in 1:elitism) {

			winner[j,1] <- population[which(fitVector==sortedFitVector[j]),1][1]
			winner[j,2] <- population[which(fitVector==sortedFitVector[j]),2][1]
			winner[j,3] <- population[which(fitVector==sortedFitVector[j]),3][1]

			population[j,] <- winner[j,]
		}


		###########################################
		# Crossover
		for(j in (popsize-elitism+1):popsize) {
			for(k in 1:qtArgs) {
				population[j,k] <- sample(winner[,k], size=1)
			}
		}


		###########################################
		# Mutation
		for(j in (popsize-elitism+1):popsize) {
			roulette <- sample(1:100, size=1)
			if(roulette > 100-mutation)
				population[j,] <- sample(range, size=qtArgs)
		}

		if(firstRun && i==1) {
			OutputInfo <- fitness(winner[1,1], winner[1,2], winner[1,3])
			Error <- OutputInfo$Error
			TimeInterval <- OutputInfo$TimeInterval
			SettlingTime <- OutputInfo$SettlingTime
			setEPS()
			postscript("graphic-firstIteration (RandomPop).eps")
			plot(x=TimeInterval , y=Error*100, xlab='Time (s)', ylab='Error (%)', type='l', main='Step Response Error in Relation to the Setpoint ')
			abline(v=SettlingTime, h=5, col="red", lwd=3, lty=2)	
			dev.off()
		}
	
		print(paste("Best fit set:", toString(winner[1,])))

	}


	OutputInfo <- fitness(winner[1,1], winner[1,2], winner[1,3])
	Error <- OutputInfo$Error
	TimeInterval <- OutputInfo$TimeInterval
	SettlingTime <- OutputInfo$SettlingTime
	setEPS()
	postscript("graphic-tunedPID.eps")
	plot(x=TimeInterval , y=Error*100, xlab='Time (s)', ylab='Error (%)', type='l', main='Step Response Error in Relation to the Setpoint ')
	abline(v=SettlingTime, h=5, col="red", lwd=3, lty=2)
	dev.off()

	return(winner[1,])

}

pop=NULL
suppressWarnings(try( pop <- read.csv("args.dat", header=FALSE), silent=TRUE ))
result <- geneticAlgorithm(pop)
result <- toString(result)
write.table(result, file="args.dat", append=TRUE, col.names=FALSE, row.names=FALSE, quote=FALSE)

