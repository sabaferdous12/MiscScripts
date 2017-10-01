#!/bin/bash
# Script to generate .csv file by taking time (each residue) from each ss file
for m in `ls -d M*`;
do
echo $m;
cd $m
 for r in `ls -d Rep*`;
   do 
    echo $r;
    file=`ls $r/ss/*.ss`;
    timings=`head -21 $file|cut -d ':' -f2 > $r/ss/timings`; 
   done
   header=`ls -d Rep*|tr '\n' '\t'` 
   printf "$m\t$header" > $m.results
   echo >> $m.results
   paste ../Residue_Names.txt Rep*/ss/timings >> $m.results
cd ../
done

# paste M*/*.results > saba.csv

