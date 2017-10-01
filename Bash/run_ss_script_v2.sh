#!/bin/sh
for m in `ls -d M*`
do
echo $m
echo -n "enter simulation time in ps:"
read timeps
echo -n "enter start position:"
read start
echo -n "enter lenght of peptide:"
read length


cd $m
for f in `ls -d Rep*/ss`;
do
echo $f

	cd $f
	traj=`ls *.xtc`
	traj_ss_dat=`ls *.dat`
	echo "running getTimepercentageofSS on $traj_ss_dat"
	~/Desktop/scripts/getTimePercentageOfSS_v2.pl -n 10 -ls $timeps -s $start -lp $length -f $traj_ss_dat
	cd ../..

done
cd ..
#break
done
	
