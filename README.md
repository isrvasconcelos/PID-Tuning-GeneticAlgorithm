# PID-Tuning-GeneticAlgorithm
Application using the just released R-Libraries for control systems (https://github.com/rstats-gsoc/gsoc2017/wiki/control:-Control-Systems-toolbox) and evaluating a Genetic Algorithm to calibrate PID controllers.

The script run.sh calls the genetic algorithm in parallel, the best chromosomes are stored inside args.dat in order to feedback the procedure with a support knowledge and improve the results.
