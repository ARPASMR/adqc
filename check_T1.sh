#!/bin/bash
#===============================================================================
# <check_T1.sh>
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
# 2009/03/17  C.Lussana - Codice originale
# 2020/01/27  MR & MS - dockerizzazione 
#========================================================================================
#
  /usr/bin/R --no-save --no-restore < test_T1_recenti.R 
#
  exit 0 
