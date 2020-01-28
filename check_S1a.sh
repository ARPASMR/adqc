#!/bin/bash
#===============================================================================
# <check_S1a.sh>
#
# DESCRIZIONE:
# ===========
#  destinatari predefiniti.
#
# RIGA DI COMANDO:
# ================
#
# CODICI D'USCITA:
# ================
# 0 -> successo!
# 1 -> errore!
#
# METODO:
# =======
#
# INFORMAZIONI ACCESSORIE PRODOTTE:
# =================================
# 
# FORMATO DEI PRINCIPALI FILES COINVOLTI:
# =======================================
#
# REFERENTE: Cristian Lussana (c.lussana@arpalombardia.it)
# ==========
#
# STORIA:
# =======
#  data        commento
# ------      ----------
# 2009/02/09  C.Lussana - Codice originale
# 2009/11/26  C.Lussana - cambiato 'Failure' -> 'Fail' - 'Warning' -> 'Suspect'
#========================================================================================
#-------------------------------
# [0.0] Lettura riga di comando 
#-------------------------------
  flagDATAI=0
  flagDATAF=0
  while getopts "s:e:" Option
  do
    case $Option in
    s ) flagDATAI=1
    DATAI=$OPTARG
    ;;
    e ) flagDATAF=1
    DATAF=$OPTARG
    ;;
    * ) echo " Opzione non riconosciuta ";;
    esac
  done
# checks
  if [ "$flagDATAI" -eq 0 ] || [ "$flagDATAF" -eq 0 ]
  then
    echo "errore nell'inserimento delle date nella linea di comando"
    exit 1
  fi
#
  /usr/bin/R --vanilla $DATAI $DATAF S1a Suspect < $HOME/programmi/dqc/tests/oit2mdqc_v04_new.R 
  /usr/bin/R --vanilla $DATAI $DATAF S1a Fail < $HOME/programmi/dqc/tests/oit2mdqc_v04_new.R
#
  exit 0 
