#########################################      
###       DMA-PA.R                    ###
###                                   ###
### Maria Ranci           02/09/2009  ###
###                                   ###
# 26/11/09 MR e CR. Passaggio Flag_automatica 'G' -> 'P' e 'W'->'S'
#########################################      
#

library(DBI)
library(RMySQL)


#==============================================================================
# LEGGI ARGOMENTI RIGA DI COMANDO
#
# Riga di comando:
# R --vanilla inizio fine < DMA-PA.R
#     inizio > marca temporale del primo record da elaborare
#              formato "2009-02-16 00:00:00" 
#       fine > marca temporale dell'ultimo record da elaborare
#              formato "2009-02-16 00:00:00" 
#..............................................................................
arguments <- commandArgs()
arguments

inizio1   <- arguments[3] #"2009-02-16" 
inizio2   <- arguments[4] #"00:00:00" 
fine1     <- arguments[5] #"2009-03-04"
fine2     <- arguments[6] #"01:00:00"
inizio<-paste(inizio1,inizio2,sep=" ")
fine<-paste(fine1,fine2,sep=" ")
file_log  <- paste('DMA-PA_',inizio1,'_',fine1,'_rem2.log',sep='')
tipologia<-"Barometri"
#
anno_inizio<-as.numeric(substr(inizio1,0,4))
anno_fine<-as.numeric(substr(fine1,0,4))
anni<-anno_inizio:anno_fine
#___________________________________________________
#    SUBROUTINES       
#___________________________________________________
##############  GESTIONE DEGLI ERRORI
neverstop<-function(){
  cat("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n",file=file_log,append=T)
}
options(show.error.messages=FALSE,error=neverstop)

#==============================================================================
# BODY - BODY - BODY - BODY - BODY - BODY - BODY - BODY - BODY - BODY -BODY 
#==============================================================================
cat ( "ESECUZIONE DMA-PA ", date()," \n\n" , file = file_log)
cat ( " tipologia > Barometri" , file = file_log,append=T)
cat ( "\n" , file = file_log,append=T)
cat ( " argomenti riga di comando:\n" , file = file_log,append=T)
cat ( paste("   inizio > ",inizio,"\n") , file = file_log,append=T)
cat ( paste("     fine > ",fine,"\n") , file = file_log,append=T)
cat ( "----------------------------\n" , file = file_log,append=T)
#___________________________________________________
#    COLLEGAMENTO AL DB
#___________________________________________________
cat("collegamento al DB\n",file=file_log,append=T)
#definisco driver
drv<-dbDriver("MySQL")
#apro connessione con il db 
conn<-try(dbConnect(drv, user=as.character(Sys.getenv("MYSQL_USR")), password=as.character(Sys.getenv("MYSQL_PWD")), dbname=as.character(Sys.getenv("MYSQL_DBNAME")), host=as.character(Sys.getenv("MYSQL_HOST")),port=as.numeric(Sys.getenv("MYSQL_PORT"))))
#___________________________________________________
# ciclo sulle tipologie di sensori
#___________________________________________________
#
nome_tavola_recente <- paste("M_", tipologia            , sep="")
nome_tavola_DQC     <- paste("M_", tipologia, "DQC"     , sep="")
#___________________________________________________
# estraggo info da tavola DQC 
#___________________________________________________
# estraggo dalla tabella DQC le coppie sensore-istante segnalate su cui poi ciclare per l'assegnazione della flag di validita'
query_coppie <- paste ("select distinct IDsensore, Data_e_ora from ",nome_tavola_DQC," where Data_e_ora>'",inizio,"' and Data_e_ora<'",fine ,"' and ( (Test='P1a' and Result='F')  or (Test='T2a' and Result='F') )", sep="")
q_coppie <- try( dbGetQuery(conn,query_coppie), silent=TRUE )
if (inherits( q_coppie, "try-error")) {
  quit(status=1)
}
#    print(q_coppie)

coppia <- 1
while(coppia < length(q_coppie$IDsensore) + 1){
  flag   ='P'
  auxP1aF=NULL
  auxT2aF=NULL
#  print("------------------------")
#  print(coppia)
#___________________________________________________
# estraggo esito test relativo alla coppia 
#___________________________________________________
  query_esito <- paste ("select Test, Result from ", nome_tavola_DQC ," where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"'"  , sep="")
  q_esito <- try( dbGetQuery(conn,query_esito), silent=TRUE )
  if (inherits( q_esito, "try-error")) {
    quit(status=1)
  }
  cat ( paste(" elaborazione sensore/data > ",q_coppie$IDsensore[coppia],q_coppie$Data_e_ora[coppia],"\n"),
        file = file_log,append=T)
#___________________________________________________
# assegno flag di validita' 
#___________________________________________________
# q_esito e' un vettore colonna delle dimensione del numero di test
#  che ritornano F o S per la coppia univoca (IDsens,Data) in esame
#      print(q_esito)
#..............................................
# l'operazione:
#   (q_esito$Test %in% 'P1a') & (q_esito$Result %in% 'F')
# restituisce un vettore colonna delle dimensioni di q_esito e di tipo LOGICO
# con TRUE nella posizione i-esima se Test='P1a' e Result='F'
# e  FALSE altrimenti
# l'operazione:
#   any( (q_esito$Test %in% 'P1a') & (q_esito$Result %in% 'F')  )
# restituisce un solo valore LOGICO:
# TRUE se esiste almeno un record del vettore TRUE
#..............................................
  auxP1aF <- any( (q_esito$Test %in% 'P1a') & (q_esito$Result %in% 'F') )
  auxT2aF <- any( (q_esito$Test %in% 'T2a') & (q_esito$Result %in% 'F') )
# DMA-PA-3
  if( (auxP1aF == TRUE) ) flag='F'
# DMA-PA-4
  if( (auxT2aF == TRUE) ) flag='F'
#
  cat( paste(" Risultati: P1a F? T2a F? ",auxP1aF,auxT2aF,'allora risultato finale =',flag,"\n"),
       file=file_log, append=T)


#___________________________________________________
# prima di scrivere nelle tabelle dati l'esito dei test 
# verifico il valore della flag_automantica dell'ultimo update. 
# solo se cambia la scrivo anche in DQCinDBUNICO per passare 
# l'informazione al REM, altrimenti no
#___________________________________________________
  query_select_flag <- paste ("select Flag_automatica from ", nome_tavola_recente  ," where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"'"  , sep="")
  q_select_flag  <- try( dbGetQuery(conn,query_select_flag ), silent=TRUE )
    if (inherits( q_select_flag , "try-error")) {
      quit(status=1)
    }
  flag_precedente<-q_select_flag$Flag_automatica

#___________________________________________________
# update flag nelle tavole dei dati annuale  
# la prima query assegna la flag automatica
# la query bis allinea le Flag_manuale_DBunico in caso di F
# la query tris allinea le Flag_manuale_DBunico in caso di P
#___________________________________________________
  for (anno in anni) {
    nome_tavola_annuale <- paste("M_", tipologia, "_", anno , sep="")

    query_update_annuale <- paste ("update ", nome_tavola_annuale ," set Flag_automatica='",flag, "', Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"'"  , sep="")

    q_update_annuale <- try( dbGetQuery(conn,query_update_annuale), silent=TRUE )
    if (inherits( q_update_annuale, "try-error")) {
      quit(status=1)
    }
    query_update_annuale_bis <- paste ("update ", nome_tavola_annuale ," set Flag_manuale_DBunico=5, Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"' and Flag_automatica='F' and Flag_manuale_DBunico in (-1,0,1,2)"  , sep="")

    q_update_annuale_bis <- try( dbGetQuery(conn,query_update_annuale_bis), silent=TRUE )
    if (inherits( q_update_annuale_bis, "try-error")) {
      quit(status=1)
    }
    query_update_annuale_tris <- paste ("update ", nome_tavola_annuale ," set Flag_manuale_DBunico=-1, Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"' and Flag_automatica='P' and Flag_manuale_DBunico in (0,5)"  , sep="")

    q_update_annuale_tris <- try( dbGetQuery(conn,query_update_annuale_tris), silent=TRUE )
    if (inherits( q_update_annuale_tris, "try-error")) {
      quit(status=1)
    }
  }

#___________________________________________________
# update flag nelle tavole dei dati recenti 
# la prima query assegna la flag automatica
# la query bis allinea le Flag_manuale_DBunico in caso di F
# la query tris allinea le Flag_manuale_DBunico in caso di P
#___________________________________________________
  query_update_recente <- paste ("update ", nome_tavola_recente ," set Flag_automatica='",flag, "', Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"'"  , sep="")

  q_update_recente <- try( dbGetQuery(conn,query_update_recente), silent=TRUE )
  if (inherits( q_update_recente, "try-error")) {
    quit(status=1)
  }
  query_update_recente_bis <- paste ("update ", nome_tavola_recente ," set Flag_manuale_DBunico=5, Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"' and Flag_automatica='F' and Flag_manuale_DBunico in (-1,0,1,2)"  , sep="")

  q_update_recente_bis <- try( dbGetQuery(conn,query_update_recente_bis), silent=TRUE )
    if (inherits( q_update_recente_bis, "try-error")) {
      quit(status=1)
    }

  query_update_recente_tris <- paste ("update ", nome_tavola_recente ," set Flag_manuale_DBunico=-1, Autore='DMA-PA',Data='",Sys.time(),"' where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"' and Flag_automatica='P' and Flag_manuale_DBunico in (0,5)"  , sep="")

  q_update_recente_tris <- try( dbGetQuery(conn,query_update_recente_tris), silent=TRUE )
    if (inherits( q_update_recente_tris, "try-error")) {
      quit(status=1)
    }
#___________________________________________________
# update flag nella tavola DQCinDBUNICO 
#___________________________________________________
 if(flag_precedente!=flag){

  query_update_DQCinDBUNICO <- paste ("REPLACE INTO DQCinDBUNICO_dati SELECT * from ", nome_tavola_recente," where IDsensore=",q_coppie$IDsensore[coppia], " and Data_e_ora='",q_coppie$Data_e_ora[coppia],"'"  , sep="")

  q_update_DQCinDBUNICO <- try( dbGetQuery(conn,query_update_DQCinDBUNICO), silent=TRUE )
  if (inherits( q_update_DQCinDBUNICO, "try-error")) {
    quit(status=1)
  }
 }
  coppia <- coppia + 1
}  # fine ciclo sulle copie 
#___________________________________________________
#    DISCONNESSIONE DAL DB
#___________________________________________________
# chiudo db
cat ( "chiudo DB \n" , file = file_log , append = TRUE )
dbDisconnect(conn)
rm(conn)
dbUnloadDriver(drv)


cat ( "PROGRAMMA ESEGUITO CON SUCCESSO alle ", date()," \n" , file = file_log , append = TRUE )



