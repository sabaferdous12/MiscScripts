#!/bin/sh
echo -n "Enter Name:"
read name
for m in `ls -d M*`;
do
    cd $m
     for r in `ls -d Rep*`;
	do
	cd $r

#    IFS='_' read -a fn <<<"$f"
#    name=${fn[0]}"_"${fn[1]}
   
     newName=$name$m"_"$r
echo $newName

    ~/Desktop/scripts/doitGROMACS_v2.1/doitGROMACS.sh -g -b acrm -n $newName -t 500 -s input_500.tpr -f $newName"_500.xtc" <<< 'h20'
    ~/Desktop/scripts/doitGROMACS_v2.1/doitGROMACS.sh -g -b acrm -n $newName -t 500 -s input_500.tpr -f $newName"_500.xtc" <<< 'cond'
    ~/Desktop/scripts/doitGROMACS_v2.1/doitGROMACS.sh -g -b acrm -n $newName -t 500 -s input_500.tpr -f $newName"_500.xtc" <<< 'rmsdfg'
    ~/Desktop/scripts/doitGROMACS_v2.1/doitGROMACS.sh -g -b acrm -n $newName -t 500 -s input_500.tpr -f $newName"_500.xtc" <<< 'ggplot'

   	mkdir -p ss
   	cp input_500.tpr ss/
   	cp $newName"_500.xtc" ss/
   	cd ss
   	traj=`ls *.xtc`
   	ss=`ls *.dat`
   	echo "running trajSSHandling on $newName"
   
   	perl ~/Desktop/scripts/trajSSHandling.pl -s input_500.tpr -t $traj <<< $'1\n'
   	
   	cd ..   
   	cd ..
	
	done 
	cd ..
done
