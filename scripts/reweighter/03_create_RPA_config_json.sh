
#!/bin/bash

cd ../Input_Files/
for m in BRPC CCC CMRPC FRCOG MAPC MRPC MVC MVPC NMCOG NPEDC OCPC PVPC SRPEDD
do
	cd $m
	sed s/PUMS2019/PUMS2019_$m/ ../reweighting_config_2019_20230322.json > reweighting_config_2019_$m\.json
	cd ../
done
