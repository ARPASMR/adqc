#!/bin/bash
#=============================================================================
export https_proxy="http://proxy2:8080"
export http_proxy="http://proxy2:8080"

numsec=3600 # 1 ora 
./dqc_driver_recenti.sh
sleep $numsec
while [ 1 ]
do
  if [ $SECONDS -ge $numsec ]
  then
    SECONDS=0
    ./dqc_driver_recenti.sh
    sleep $numsec
  fi
done
