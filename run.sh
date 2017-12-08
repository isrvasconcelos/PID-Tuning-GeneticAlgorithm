threads=5
replications=(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)

for i in "${replications[@]}" # Repeat until succeed
do
	if [ `expr $i % $threads` -eq 0 ] # Parallelism control HERE 
	then
		Rscript ga.R
	else
		nohup Rscript ga.R &
	fi
done
