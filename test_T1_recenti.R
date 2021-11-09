#########################################      
###                                   ###
### filtro con test di STEP T1 per    ###
### dati in tabelle recenti.          ###
### Estrae dati dall'archivio, esegue ###
### il test e scrive in tabelle ..DQC ###
### i fallimenti                      ###
###                                   ###
### Maria Ranci           09/10/2008  ###
###                                   ###
# 2009-11-26 MR. e CR. cambio Flag_automatica 'G' -> 'P' e 'W' -> 'S'
# 2020-01-27 MR. e MS dockerizzazione 
#########################################      
#
#
# cicla sulle tipologie,
# interroga la tavola DQCinfo relativa per estrarre i valori di riferimento,
# filtra su tutti i dati della tabella recente (15 giorni)
# e effettua eventuali segnalazioni nelle tabelle DQC 
#
##################################################

library(DBI)
library(RMySQL)

file_log        <- 'Test_T1_recenti_rem2.log'

tipologia <- c("Termometri","RadiometriG","RadiometriN","Pluviometri","Igrometri","Barometri","AnemometriVV","AnemometriDV")

test       <- c("T1a"   # = test di consistenza temporale STEP = T1a
                 )

#___________________________________________________
#    SUBROUTINES       
#___________________________________________________

############# ESTRAZIONE DALL'ARCHIVIO DEI RIFERIMENTI PER I TEST 

riferimento <- function(tavola,test){
  query <- paste("select Rif1 from ", tavola," where Test='", test,"'", sep="")
  delta <- try( dbGetQuery(conn,query), silent=TRUE )
  if (inherits( delta, "try-error")) {
   quit(status=1)
  }
  delta <- as.numeric(delta)
  cat ( "riferimento per test ", test, " = ",delta,"\n" , file = file_log , append = TRUE )
  return(delta)
}

##############  GESTIONE DEGLI ERRORI

neverstop<-function(){
  cat("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n",file=file_log,append=T)
}
options(show.error.messages=FALSE,error=neverstop)


###############    ESTRAZIONE DALL'ARCHIVIO DEI DATI 

richiesta <- function(tavola,tipp){

  dati_estratti <- NULL

# ricavo data inizio e data fine 
  query_richiesta_inizio <- paste("select min(Data_e_ora) from ", tavola, sep="")
  query_richiesta_fine   <- paste("select max(Data_e_ora) from ", tavola, sep="")

  data_inizio <- try(dbGetQuery(conn, query_richiesta_inizio),silent=TRUE)
  if (inherits(data_inizio,"try-error")) {
    cat(data_inizio,"\n",file=file_log,append=T)
    quit(status=1)
  }
 
  data_inizio <- as.character(data_inizio)

  data_fine <- try(dbGetQuery(conn, query_richiesta_fine),silent=TRUE)
  if (inherits(data_fine,"try-error")) {
    cat(data_fine,"\n",file=file_log,append=T)
    quit(status=1)
  }

  data_fine <- as.character(data_fine)

  print(data_inizio)
  print(data_fine)

# proseguo solo se esiste almeno un dato in tabella
  if(is.na(data_inizio) == FALSE){

# richiesta al DB
    query_richiesta <- paste("select * from M_", tipp,
                             " where Misura IS NOT NULL and Data_e_ora>='", data_inizio,
                             "' and Data_e_ora<='", data_fine ,"'"  , sep="")  

    print(query_richiesta)
    q_richiesta <- try(dbGetQuery(conn, query_richiesta), silent=TRUE)
    if (inherits(q_richiesta, "try-error")) {
      cat(q_richiesta,"\n", file=file_log, append=T)
      quit(status=1)
    }

# vettore con tutte e sole le date presenti nell'archivio per i sensori richiesti
    date_nel_DB<-as.POSIXct(strptime(q_richiesta[2]$Data_e_ora,format="%Y-%m-%d %H:%M:%S"),"UTC")
    date_riformattate <- format(date_nel_DB,"%Y/%m/%d %H:%M") 

# assegno NA ai dati che non hanno passato il test di RANGE
    iii <- which(q_richiesta[6]$Flag_automatica=="F")
    q_richiesta[3]$Misura[iii]=NA

# ordino per sensore e per data
    jj<-order(q_richiesta[1]$IDsensore,date_riformattate)
    date_riformattate<-date_riformattate[jj]
#  id sensore
    q_richiesta[1]<-q_richiesta[jj,1]
#  valore osservato
    q_richiesta[3]<-q_richiesta[jj,3]
#  flag validita'
    q_richiesta[4]<-q_richiesta[jj,4]
#  data.frame con i dati misurati di tutti i sensori ordinati per marca temporale
    dati_estratti <- data.frame(q_richiesta[1],date_riformattate,q_richiesta[3],q_richiesta[4])
  } # fine esistenza dati in tabella
  return(dati_estratti)
}


###################   FINE  SUBROUTINES   ######################


cat ( "ESECUZIONE TEST DQC ", date()," \n\n" , file = file_log)

#___________________________________________________
#    COLLEGAMENTO AL DB
#___________________________________________________

cat("collegamento al DB\n",file=file_log,append=T)

#definisco driver
drv<-dbDriver("MySQL")

#apro connessione con il db descritto nei parametri del gruppo "DQC"
#nel file "/home/swiluppo/.my.cnf
conn<-try(dbConnect(drv, user=as.character(Sys.getenv("MYSQL_USR")), password=as.character(Sys.getenv("MYSQL_PWD")), dbname=as.character(Sys.getenv("MYSQL_DBNAME")), host=as.character(Sys.getenv("MYSQL_HOST")),port=as.numeric(Sys.getenv("MYSQL_PORT"))))

#___________________________________________________
# ciclo sulle tipologie di sensori
#___________________________________________________

tip<-1

while(tip < length(tipologia) + 1){

  cat ( "\n",tipologia[tip],"\n",sep=" ", file = file_log, append=T)

  nome_tavola_recente <- paste("M_", tipologia[tip]            , sep="")
  nome_tavola_DQC     <- paste("M_", tipologia[tip], "DQC"     , sep="")
  nome_tavola_DQCinfo <- paste("M_", tipologia[tip], "_DQCinfo", sep="")


# richiedo tutte le misure non già etichettate come non valide
  cat ( "eseguo richiesta\n", file = file_log, append=T)
  dati <- richiesta(nome_tavola_recente,tipologia[tip])
#___________________________________________________
#    STEP TEST - T1       
#___________________________________________________

# ricavo riferimento da DB
  delta <- riferimento(nome_tavola_DQCinfo, test)
  cat ( " delta  " ,delta,"\n",  file = file_log , append = TRUE )
  if (tip==4) {
    delta_est <- riferimento(nome_tavola_DQCinfo, "T1b")
    cat ( " delta estate  " ,delta_est,"\n",  file = file_log , append = TRUE )
  }


  cat ( " eseguo test  \n" , file = file_log , append = TRUE )
  differenza_misura  <- abs(diff(dati$Misura))
  differenza_sensore <- abs(diff(dati$IDsensore))
  differenza_ore     <- as.integer(diff(dati$date_riformattate))
  mesi  <- as.integer(substr(dati$date_riformattate[2:length(dati$date_riformattate)],6,7))
  
  salto_di_ore       <- which(differenza_ore!=1)
  cambio_sensore     <- which(differenza_sensore > 0)

# 2-eseguo test
  fallimento <- "S"
  if(any(differenza_misura > delta, na.rm=TRUE)){
# 
 if (tip==4) {
   superamento_incremento_aux <- which(differenza_misura > delta & (mesi<6 | mesi>8))
   superamento_incremento_est <- which(differenza_misura > delta_est & mesi>=6 & mesi<=8)
   superamento_incremento <- superamento_incremento_aux | superamento_incremento_est
 } else {
   superamento_incremento <- which(differenza_misura > delta)
 }
# evito di segnalare cambio di sensore
 superamento_incremento <- superamento_incremento[which((superamento_incremento %in% cambio_sensore)==FALSE)]
# evito di segnalare se il salto non e' relativo ad ore consecutive 
 superamento_incremento <- superamento_incremento[which((superamento_incremento %in% salto_di_ore)==FALSE)]

# COSI SEGNALA IL PRIMO DATO SOSPETTO DEL SALTO, ORA SEGNALO ANCHE IL SECONDO:
 superamento_incremento <- unique(c(superamento_incremento,superamento_incremento+1))
 cat("superamento individuato su ", length(superamento_incremento) , "misure \n", file=file_log, append=T)

 cat ( " segnalo in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
 
 pp <- 1
 while(pp<length(superamento_incremento)+1){

# cat("pp ",pp,"\n")
# cat(dati$IDsensore[superamento_incremento[pp]],
#     as.character(dati$date_riformattate[superamento_incremento[pp]]),
#     dati$Misura[superamento_incremento[pp]],
#     "\n\n",file=file_log,append=T)

## insert in DQC 
#" 3-memorizzo record del data.frame da segnalare
  Autore <- "test_T1_recenti.R"
# maria 18marzo, specifico le ore nell inserimento del record
#  Data   <- paste("'", as.character(Sys.Date()),"'", sep="")
  Data   <- paste("'", as.character(Sys.time()),"'", sep="")

  stringa <- paste("(",dati$IDsensore[superamento_incremento[pp]], 
                     ", '",as.character(dati$date_riformattate[superamento_incremento[pp]]),
                     "','", test,"','",fallimento, "','", Autore, "',",Data,",NULL)",sep="")
  query_insert <- paste("insert into ",nome_tavola_DQC,
                        " values ",stringa,
                        " on duplicate key update  Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
  q_insert <- try(dbGetQuery(conn, query_insert),silent=TRUE)
  if (inherits(q_insert, "try-error")) {
   cat(q_insert,"\n", file=file_log, append=T)
   quit(status=1)
  }
  pp <- pp + 1
 }
}
#_______________________________________________________________

tip <- tip + 1
}  # fine ciclo sulle tipologie

#___________________________________________________
#    DISCONNESSIONE DAL DB
#___________________________________________________

# chiudo db
cat ( "chiudo DB \n" , file = file_log , append = TRUE )
dbDisconnect(conn)
rm(conn)
dbUnloadDriver(drv)


cat ( "PROGRAMMA ESEGUITO CON SUCCESSO alle ", date()," \n" , file = file_log , append = TRUE )



