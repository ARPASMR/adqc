#!/bin/bash
#=============================================================================
export https_proxy="http://proxy2:8080"
export http_proxy="http://proxy2:8080"

numsec=7200 # 2 ore 
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
