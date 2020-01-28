#
#
#
R=/usr/bin/R
check_S1a=check_S1a.sh
check_T1=check_T1.sh
check_T2=check_T2.sh
check_T1_log=Test_T1_recenti.log
check_T2_log=Test_T2_recenti.log
#
DMA_T=DMA-T.R
DMA_UR=DMA-UR.R
DMA_PA=DMA-PA.R
DMA_PP=DMA-PP.R
DMA_RG=DMA-RG.R
DMA_RN=DMA-RN.R
DMA_VV=DMA-VV.R
DMA_DV=DMA-DV.R
# Temperatura
##echo "checkS1a.sh `date` > START ------------------------------------------------------------------"
###$check_S1a -s `/bin/date --date "2 days ago" +\%Y\%m\%d`0100 -e `/bin/date --date "1 hour ago" +\%Y\%m\%d\%H`00
##echo "checkS1a.sh `date` >   END ------------------------------------------------------------------"
# Tutte le tipologie
echo "checkT1.sh `date` > START ------------------------------------------------------------------"
rm -f $check_T1_log 
$check_T1
cat $check_T1_log 
echo "checkT1.sh `date` >   END ------------------------------------------------------------------"
echo "checkT2.sh `date` > START ------------------------------------------------------------------"
rm -f $check_T2_log 
$check_T2
cat $check_T2_log 
echo "checkT2.sh `date` >   END ------------------------------------------------------------------"
#=========
# DMAs
#=========
# DMA T
echo "DMA T `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_T
echo "DMA T `date` >   END ------------------------------------------------------------------"
# DMA UR
echo "DMA UR `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_UR
echo "DMA UR `date` >   END ------------------------------------------------------------------"
# DMA PA
echo "DMA PA `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_PA
echo "DMA PA `date` >   END ------------------------------------------------------------------"
# DMA PP
echo "DMA PP `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_PP
echo "DMA PP `date` >   END ------------------------------------------------------------------"
# DMA RG
echo "DMA RG `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_RG
echo "DMA RG `date` >   END ------------------------------------------------------------------"
# DMA RN
echo "DMA RN `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_RN
echo "DMA RN `date` >   END ------------------------------------------------------------------"
# DMA VV
echo "DMA VV `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_VV
echo "DMA VV `date` >   END ------------------------------------------------------------------"
# DMA DV
echo "DMA DV `date` > START ------------------------------------------------------------------"
$R --vanilla `/bin/date --date "15 days ago" +\%Y-\%m-\%d" "%H:`00:00 `/bin/date +\%Y-\%m-\%d" "%H:` < $DMA_DV
echo "DMA DV `date` >   END ------------------------------------------------------------------"
#
exit 0
