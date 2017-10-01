load aligned_1A14_1.pdb
bg_color white
turn x, 90
turn y, 90
turn x, 90
turn y, 90
show cartoon
hide lines
select light, chain L
remove light
select heavy, chain H
remove heavy
color cyan, chain N
select regions, resi 328-332
color red, regions
select regions, resi 341-344
color red, regions
select regions, resi 366-372
color red, regions
select regions, resi 400-403
color red, regions
select fragments, resi 432
color green, fragments
ray 800,600
png 1A14_1.pdb.png
quit
