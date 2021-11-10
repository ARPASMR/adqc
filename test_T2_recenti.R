#########################################      
###       Test.R                      ###
###                                   ###
### filtro con test di persistenza T2a###
### dati in tabelle recenti.          ###
### Estrae dati dall'archivio, esegue ###
### il test e scrive in tabelle ..DQC ###
### i fallimenti                      ###
###                                   ###
### Maria Ranci           06/02/2009  ###
###                                   ###
#########################################      
#
#
# cicla sulle tipologie,
# interroga la tavola DQCinfo relativa per estrarre i valori di riferimento,
# filtra su tutti i dati della tabella recente (15 giorni)
# e effettua eventuali segnalazioni nelle tabelle DQC 
#
# Storia:
#  data         commento
#  ----         --------
# 2009-04-17    CR. sostituito:
#                 fine_persistenza <- persist[pp]
#                con:
#                 fine_persistenza <- (persist[pp]+1)
# 2009-05-06    CR. Corretto errore nella function "richiesta".
#                   Modificato output su file di log 
# 2009-09-02    MR e CR. Introdotti test T2b e T2c. Variati anche i test T2a.
# 2009-09-10    CR. Ottimizzata la scrittura sul DB
# 2009-11-26    CR. cambio Flag_automatica 'G'->'P' e 'W'->'S'
# 2020-01-27    MS & MR dockerizzazione
################################################################################

library(DBI)
library(RMySQL)

file_log        <- 'Test_T2_recenti_rem2.log'

tipologia <- c("Termometri","RadiometriG","RadiometriN","Igrometri","Barometri","AnemometriVV","AnemometriDV")
#tipologia <- c("Barometri")

test       <- c("T2a","T2b","T2c")   # = test di consistenza temporale PERSISTENZA = T2a 

#___________________________________________________
#    SUBROUTINES       
#___________________________________________________

############# ESTRAZIONE DALL'ARCHIVIO DEI RIFERIMENTI PER I TEST 

riferimento <- function(tavola,test){
  query <- paste("select Rif1,Rif2 from ", tavola," where Test='", test,"'", sep="")
  rife <- try( dbGetQuery(conn,query), silent=TRUE )
  if (inherits( rife, "try-error")) {
    quit(status=1)
  }
  rife <- as.numeric(rife)
  return(rife)
}

riferimento3 <- function(tavola,test){
  query <- paste("select Rif3 from ", tavola," where Test='", test,"'", sep="")
  rife <- try( dbGetQuery(conn,query), silent=TRUE )
  if (inherits( rife, "try-error")) {
    quit(status=1)
  }
  rife <- as.numeric(rife)
  return(rife)
}

##############  GESTIONE DEGLI ERRORI

neverstop<-function(){
  cat("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:\n",file=file_log,append=T)
}
options(show.error.messages=TRUE,error=neverstop)


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
  cat(" richiesta da ",data_inizio," a ",data_fine," \n",file=file_log,append=T)
#
# proseguo solo se esiste almeno un dato in tabella
  if(is.na(data_inizio) == FALSE){
# richiesta al DB
    query_richiesta <- paste("select * from M_", tipp,
                             " where Misura IS NOT NULL and Data_e_ora>='", data_inizio,
                             "' and Data_e_ora<='", data_fine ,"'"  , sep="")  
    q_richiesta <- try(dbGetQuery(conn, query_richiesta), silent=TRUE)
    if (inherits(q_richiesta, "try-error")) {
      cat(q_richiesta,"\n", file=file_log, append=T)
      quit(status=1)
    }
#    cat( rbind(q_richiesta$IDsensore[q_richiesta$IDsensore==5937],
#               q_richiesta$Data_e_ora[q_richiesta$IDsensore==5937],
#               q_richiesta$Misura[q_richiesta$IDsensore==5937],
#               q_richiesta$Flag_manuale_DBunico[q_richiesta$IDsensore==5937],"\n"),file=file_log,append=T)
# vettore con tutte e sole le date presenti nell'archivio per i sensori richiesti
    date_nel_DB<-as.POSIXct(strptime(q_richiesta[2]$Data_e_ora,format="%Y-%m-%d %H:%M:%S"),"UTC")
#    print(q_richiesta[2]$Data_e_ora[1])
#    print(q_richiesta$Data_e_ora[1])
    date_riformattate <- format(date_nel_DB,"%Y/%m/%d %H:%M","UTC") 
#    cat( rbind( q_richiesta$Data_e_ora[q_richiesta$IDsensore==5937],
#                date_riformattate[q_richiesta$IDsensore==5937],"\n"),file=file_log,append=T)
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
#    cat( rbind(dati_estratti$IDsensore[dati_estratti$IDsensore==5937],
#               dati_estratti$date_riformattate[dati_estratti$IDsensore==5937],
#               dati_estratti$Misura[dati_estratti$IDsensore==5937],
#               dati_estratti$Flag_manuale_DBunico[dati_estratti$IDsensore==5937],"\n"),file=file_log,append=T)
    
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

while(tip <= length(tipologia) ){
#  print(tip)
#  print(tipologia)

  cat ( "\n tipologia in esame: ",tipologia[tip],"\n", file = file_log, append=T)

  nome_tavola_recente <- paste("M_", tipologia[tip]            , sep="")
  nome_tavola_DQC     <- paste("M_", tipologia[tip], "DQC"     , sep="")
  nome_tavola_DQCinfo <- paste("M_", tipologia[tip], "_DQCinfo", sep="")


# richiedo tutte le misure non già etichettate come non valide
  cat ( "eseguo richiesta\n", file = file_log, append=T)
  dati <- NULL
  dati <- richiesta(nome_tavola_recente,tipologia[tip])
#  cat ( rbind(dati) "\n tipologia in esame: ",tipologia[tip],"\n",sep=" ", file = file_log, append=T)
#  cat(dati, file = file_log, append=T)
#  print(dati[1,])
  cat( rbind(dati$IDsensore,dati$date_riformattate,dati$Misura,dati$Flag_manuale_DBunico,"\n"),file=file_log,append=T)
#  print(dati[1,])
#  print( cbind( dati$IDsensore[as.numeric(dati$IDsensore)==6486],
#                format.Date(as.POSIXct(dati$date_riformattate[as.numeric(dati$IDsensore)==6486],"UTC"),"%Y-%m-%d %H"),
#                dati$Misura[as.numeric(dati$IDsensore)==6486]) )

#___________________________________________________
#    TEST DI PERSISTENZA  - T2a       
#___________________________________________________

# ricavo riferimento da DB
  fallimento_a<-"X"
  fallimento_b<-"X"
  fallimento_c<-"X"
  if (tipologia[tip] == "Termometri")    fallimento_a <- "F"

  if (tipologia[tip] == "Igrometri") {
                                         fallimento_a <- "S"
                                         fallimento_b <- "S"
                                         fallimento_c <- "S"
  }

  if (tipologia[tip] == "Barometri")     fallimento_a <- "F"

  if (tipologia[tip] == "RadiometriG")   fallimento_a <- "F"

  if (tipologia[tip] == "RadiometriN")   fallimento_a <- "F"

  if (tipologia[tip] == "AnemometriDV")  fallimento_a <- "S"

  if (tipologia[tip] == "AnemometriVV") {
                                         fallimento_a <- "S"
                                         fallimento_b <- "F"
  }
  if (fallimento_a!="X"){
    inc_e_durata_a <- NULL 
    inc_e_durata_a <- riferimento(nome_tavola_DQCinfo, test[1])
    incremento_a   <- inc_e_durata_a[1]
    durata_a       <- inc_e_durata_a[2]
    cat ( "riferimento per test ", test[1] , file = file_log , append = TRUE )
    cat ( " -->   incremento_a: ",incremento_a, ",  durata_a:  " ,durata_a,"\n",  file = file_log , append = TRUE )
  }
  if (fallimento_b!="X"){
    inc_e_durata_b <- NULL 
    inc_e_durata_b <- riferimento(nome_tavola_DQCinfo, test[2])
    incremento_b   <- inc_e_durata_b[1]
    durata_b       <- inc_e_durata_b[2]
    cat ( "riferimento per test ", test[2] , file = file_log , append = TRUE )
    cat ( " -->   incremento_b: ",incremento_b, ",  durata_b:  " ,durata_b,"\n",  file = file_log , append = TRUE )
    if (tipologia[tip] == "Igrometri") {
      whiskers <- as.numeric(riferimento3(nome_tavola_DQCinfo, test[2])[1])
      cat ( " Igrometri! Allora ho anche whiskers ",whiskers, "\n",  file = file_log , append = TRUE )
    }
  }
  if (fallimento_c!="X"){
    inc_e_durata_c <- NULL 
    inc_e_durata_c <- riferimento(nome_tavola_DQCinfo, test[3])
    incremento_c   <- inc_e_durata_c[1]
    durata_c       <- inc_e_durata_c[2]
    cat ( "riferimento per test ", test[3] , file = file_log , append = TRUE )
    cat ( " -->   incremento_c: ",incremento_c, ",  durata_c:  " ,durata_c,"\n",  file = file_log , append = TRUE )
  }
#
  cat ( " eseguo i tests  \n" , file = file_log , append = TRUE )
  Autore <- "test_T2_recenti"
#  Data   <- paste("'", as.character(Sys.Date()),"'", sep="")
  Data   <- paste("'", as.character(Sys.time()),"'", sep="")

#-------------------------------------------------------------------------------------------------------------
# Test T2a ---------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# Il test T2a e' comune per tutte le variabili, cambia il livello di segnalazione (F o S)
#-------------------------------------------------------------------------------------------------------------
  if (fallimento_a!="X"){
    cat ( " test T2a  \n" , file = file_log , append = TRUE )

    differenza_misura  <- NULL 
    differenza_sensore <- NULL
    differenza_ore     <- NULL
#  dati 1...N  //  differenza_XXX 1...(N-1)  //  differenza(i)=dati(i+1)-dati(i) 
    differenza_misura  <- diff(dati$Misura)
    if (tipologia[tip] == "AnemometriDV") differenza_misura<-min(differenza_misura,360-differenza_misura)  
    differenza_sensore <- abs(diff(dati$IDsensore))
    differenza_ore     <- as.integer(diff(as.POSIXct(strptime(dati$date_riformattate , format="%Y/%m/%d %H:%M"),"UTC")))
    #
    salto_di_ore       <- NULL 
    cambio_sensore     <- NULL 
    salto_di_ore       <- which(differenza_ore!=1)
    cambio_sensore     <- which(differenza_sensore > 0)
# inizializzo array
    vettore <- numeric()
    vettore1 <- numeric()
    posizione <- numeric()
    persist<-numeric()


# cerco ENTRO range:
# posizione(i) -> i-esimo elemento del vettore contiene la posizione (riferita al vettore 
#                 "differenza_misura") dell'i-esimo elemento per cui "differenza_misura" < incremento stabilito
    if(any(abs(differenza_misura) < incremento_a,na.rm=TRUE)) posizione <- which( abs(differenza_misura) < incremento_a)

# calcolo persistenza vera e propria

#    cat(paste("incremento_a=",incremento_a,"\n"),file="tmp.log")
#    cat(paste("durata_a=",durata_a,"\n"),file="tmp.log",append=T)
#    cat("misura \n",file="tmp.log",append=T)
#    cat(paste(seq(1:length(dati$Misura)),dati$Misura,"\n"),file="tmp.log",append=T)
#    cat("differenza_misura \n",file="tmp.log",append=T)
#    cat(paste(seq(1:length(differenza_misura)),differenza_misura,"\n"),file="tmp.log",append=T)
#    cat("posizione \n",file="tmp.log",append=T)
#    cat(paste(seq(1:length(posizione)),posizione,"\n"),file="tmp.log",append=T)

    if (length(posizione) != 0 ){
# vettore(i)  <- se differenza_misura(i) <= incremento_a => vettore(i)=differenza_misura(i) 
#                se differenza_misura(i) >  incremento_a => vettore(i)=NA
# vettore1(i) <- se differenza_misura(i) <= incremento_a => vettore1(i)=1 
#                se differenza_misura(i) >  incremento_a => vettore1(i)=NA
      vettore1[posizione] <- 1
      vettore[posizione] <- differenza_misura[posizione]

#      cat("vettore \n",file="tmp.log",append=T)
#      cat(paste(seq(1:length(vettore))[!is.na(vettore)],vettore[!is.na(vettore)],"\n"),file="tmp.log",append=T)
#      cat(paste(seq(1:length(vettore)),vettore,"\n"),file="tmp.log",append=T)

      if (length(vettore) >= durata_a){
# COMMENTO a filtro: usa un vettore di dimensione "durata_a" che ripete il numero "1" e lo sposta
#  come una specie di finestra mobile lungo i vettori "vettore" e "vettore1". L'elemento i-esimo
#  di filtro e' cosi' determinato:
# filtro(i)= vettore(i)+vettore(i-1)+vettore(i-2)+...+vettore(i-durata_a), se (i >= durata_a) and (vettore(k)!=NA per ogni k={i,...,i-durata_a})
# filtro(i)= NA, se (i < durata_a) or (vettore(k)==NA per almeno un k={i,...,i-durata_a})
        filtro  <- filter( vettore  , rep(1,durata_a) , method = "convolution" , sides = 1 )
        filtro1 <- filter( vettore1 , rep(1,durata_a) , method = "convolution" , sides = 1 )
#        cat("filtro \n",file="tmp.log",append=T)
#        cat(paste(seq(1:length(filtro))[!is.na(filtro)],filtro[!is.na(filtro)],filtro1[!is.na(filtro)],"\n"),file="tmp.log",append=T)
#        cat(paste(seq(1:length(filtro)),vettore,vettore1,filtro,filtro1,"\n",sep="  "),file="tmp.log",append=T)
#        if ( any ( filtro == durata_a , na.rm = TRUE )) persist <- which ( filtro == durata_a )
        if ( any ( abs(filtro) <= incremento_a , na.rm = TRUE )) persist <- which ( abs(filtro) <= incremento_a & filtro1 == durata_a)
      }  #fine if lunghezza vettore sufficiente
    }  #fine if c'è qualcosa in "posizione"
#
# segnalazioni in tavola DQC 
    if (length(persist)==0){
      cat ( " Nessuna segnalazione in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
    } else {
      cat ( " segnalo in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
# ATTENZIONE:    "indice_da_segnalare" contiene le posizioni relative ai vettori "dati" 
#                                      delle possibili persistenze
#             "indice_da_segnalare_01" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure
#             "indice_da_segnalare_02" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure senza ripetizioni
      indice_da_segnalare<-vector()
      indice_da_segnalare_01<-vector()
      indice_da_segnalare_02<-vector()
      for(pp in 1:length(persist)){
#   ogni persistenza segnalata persist(i) si riferisce alla copia dati(i+1)-dati(i)
#   per cui la fine dell'intervallo interessante e' "i+1"
        fine_persistenza <- (persist[pp]+1)
#   segnalo persistenza su tutta la durata, con riferimento stavolta ai vettori tipo "dati$Misura" 
        indice_da_segnalare <- fine_persistenza - c(0:durata_a)
        if ( diff(range(dati$Misura[indice_da_segnalare],na.rm=T)) > incremento_a ) next
# evito di segnalare se all'interno della persistenza c'e' un cambio di sensore
        ii<-0
        ii<- which((indice_da_segnalare %in% cambio_sensore) == T) 
        if(length(ii)>0)  next
# evito di segnalare se la persistenza non e' relativa ad ore consecutive 
        ii<-0
        ii<- which((indice_da_segnalare %in% salto_di_ore) == T )
        if(length(ii)>0)  next
# OK la persistenza e' da segnalare, allora aggiorno il vettore indice_da_segnalare_01
# memorizzo record del data.frame da segnalare
        aux_01<-length(indice_da_segnalare_01)+1
        aux_00<-aux_01-1+length(indice_da_segnalare)
        indice_da_segnalare_01[aux_01:aux_00]<-as.numeric(indice_da_segnalare[1:length(indice_da_segnalare)])
      }
# se esiste almeno una persistenza da segnalare allora inserisci dati nel DB
      if ( length(indice_da_segnalare_01)!=0 ) {
#       indice_da_segnalare_02 <- elimino le ripetizioni in indice_da_segnalare_01
        indice_da_segnalare_02 <- which( seq(1:max(indice_da_segnalare_01,by=1)) %in% indice_da_segnalare_01  )
        stringa <- toString(paste(" (",dati$IDsensore[indice_da_segnalare_02], 
                                  ", '",as.character(dati$date_riformattate[indice_da_segnalare_02]),
                                  "','", test[1],"','",fallimento_a, "','", Autore, "',",Data,",NULL)",sep=""))
        query_insert <- paste("insert into ",nome_tavola_DQC,
                              " values ",stringa,
                              " on duplicate key update  Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
        cat ( " query > ",query_insert," \n",file = file_log, append=T)
        q_insert <- try(dbGetQuery(conn, query_insert),silent=TRUE)
        if (inherits(q_insert, "try-error")) {
          cat(q_insert,"\n", file=file_log, append=T)
          quit(status=1)
        }
      } # finese length(indice_da_segnalare_01)!=0 
    } # finese-else length(persist)==0
  } # finese fallimento_a!="X"
#-------------------------------------------------------------------------------------------------------------
# Test T2b ---------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# Il test T2b e' eseguito per due tipologie:
#  + AnemometriVV -> e' come il test T2a ma con soglie piu' restrittive (si vedano i commenti del test T2a)
#  + Igrometri -> PERSISTENZA + analisi di un'indice di variabilita' dei dati
#-------------------------------------------------------------------------------------------------------------
  if (fallimento_b!="X"){
    cat ( " test T2b  \n" , file = file_log , append = TRUE )
# Test T2b AnemometriVV
    if (tipologia[tip] == "AnemometriVV") {

      differenza_misura  <- NULL 
      differenza_sensore <- NULL
      differenza_ore     <- NULL
#  dati 1...N  //  differenza_XXX 1...(N-1)  //  differenza(i)=dati(i+1)-dati(i) 
      differenza_misura  <- diff(dati$Misura)
      differenza_sensore <- abs(diff(dati$IDsensore))
      differenza_ore     <- as.integer(diff(as.POSIXct(strptime(dati$date_riformattate , format="%Y/%m/%d %H:%M"),"UTC")))
      
      salto_di_ore       <- NULL 
      cambio_sensore     <- NULL 
      salto_di_ore       <- which(differenza_ore!=1)
      cambio_sensore     <- which(differenza_sensore > 0)

# inizializzo array
      vettore <- numeric()
      vettore1 <- numeric()
      posizione <- numeric()
#
      persist<-numeric()

      if(any(abs(differenza_misura) < incremento_b,na.rm=TRUE)) posizione <- which(abs(differenza_misura) < incremento_b)

      if (length(posizione) != 0 ){
        vettore1[posizione] <- 1
        vettore[posizione] <- differenza_misura[posizione]
        if (length(vettore) >= durata_b){
          filtro  <- filter( vettore  , rep(1,durata_b) , method = "convolution" , sides = 1 )
          filtro1 <- filter( vettore1 , rep(1,durata_b) , method = "convolution" , sides = 1 )
          if ( any ( abs(filtro) <= incremento_b , na.rm = TRUE )) persist <- which ( abs(filtro) <= incremento_b & filtro1 == durata_b)
        }  #fine if lunghezza vettore sufficiente
      }  #fine if c'è qualcosa in "posizione"
#

      if (length(persist)==0) {
        cat ( " Nessuna segnalazione in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
      } else {
# segnalazioni in tavola DQC 
        cat ( " segnalo in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
# ATTENZIONE:    "indice_da_segnalare" contiene le posizioni relative ai vettori "dati" 
#                                      delle possibili persistenze
#             "indice_da_segnalare_01" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure
#             "indice_da_segnalare_02" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure senza ripetizioni
        indice_da_segnalare<-vector()
        indice_da_segnalare_01<-vector()
        indice_da_segnalare_02<-vector()
        for(pp in 1:length(persist)){
# per ogni persistenza segnalata:
          fine_persistenza <- (persist[pp]+1)

# segnalo persistenza su tutta la durata_b
          indice_da_segnalare <- fine_persistenza - c(0:durata_b) 
          if ( diff(range(dati$Misura[indice_da_segnalare],na.rm=T)) > incremento_b ) next
# evito di segnalare se all'interno della persistenza c'e' un cambio di sensore
          ii<-0
          ii<- which((indice_da_segnalare %in% cambio_sensore) == T) 
          if(length(ii)>0)  next
# evito di segnalare se la persistenza non e' relativa ad ore consecutive 
          ii<-0
          ii<- which((indice_da_segnalare %in% salto_di_ore) == T )
          if(length(ii)>0)  next
# OK la persistenza e' da segnalare, allora aggiorno il vettore indice_da_segnalare_01
# memorizzo record del data.frame da segnalare
          aux_01<-length(indice_da_segnalare_01)+1
          aux_00<-aux_01-1+length(indice_da_segnalare)
          indice_da_segnalare_01[aux_01:aux_00]<-as.numeric(indice_da_segnalare[1:length(indice_da_segnalare)])
        }
# se esiste almeno una persistenza da segnalare allora inserisci dati nel DB
        if ( length(indice_da_segnalare_01)!=0 ) {
#         indice_da_segnalare_02 <- elimino le ripetizioni in indice_da_segnalare_01
          indice_da_segnalare_02 <- which( seq(1:max(indice_da_segnalare_01,by=1)) %in% indice_da_segnalare_01  )
          stringa <- toString(paste(" (",dati$IDsensore[indice_da_segnalare_02], 
                                    ", '",as.character(dati$date_riformattate[indice_da_segnalare_02]),
                                    "','", test[2],"','",fallimento_a, "','", Autore, "',",Data,",NULL)",sep=""))
          query_insert <- paste("insert into ",nome_tavola_DQC,
                                " values ",stringa,
                                " on duplicate key update  Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
          cat ( " query > ",query_insert," \n",file = file_log, append=T)
          q_insert <- try(dbGetQuery(conn, query_insert),silent=TRUE)
          if (inherits(q_insert, "try-error")) {
            cat(q_insert,"\n", file=file_log, append=T)
            quit(status=1)
          }
        } # finese length(indice_da_segnalare_01)!=0
      } # finese-else length(persist)==0
    } # finese tipologia[tip] == "AnemometriVV"
#
# Test T2b Igrometri 
    if (tipologia[tip] == "Igrometri") {
      differenza_misura  <- NULL 
      differenza_sensore <- NULL
      differenza_ore     <- NULL
#  dati 1...N  //  differenza_XXX 1...(N-1)  //  differenza(i)=dati(i+1)-dati(i) 
      differenza_misura  <- diff(dati$Misura)
      differenza_sensore <- abs(diff(dati$IDsensore))
      differenza_ore     <- as.integer(diff(as.POSIXct(strptime(dati$date_riformattate , format="%Y/%m/%d %H:%M"),"UTC")))

      salto_di_ore       <- NULL 
      cambio_sensore     <- NULL 
      salto_di_ore       <- which(differenza_ore!=1)
      cambio_sensore     <- which(differenza_sensore > 0)

# inizializzazione vettori 
      vettore <- numeric()
      vettore1 <- numeric()
      posizione <- numeric()
#
      persist<-numeric()

      if(any(abs(differenza_misura) < incremento_b,na.rm=TRUE)) posizione <- which(abs(differenza_misura) < incremento_b)

      if (length(posizione) != 0 ){
        vettore1[posizione] <- 1
        vettore[posizione] <- differenza_misura[posizione]
        if (length(vettore) >= durata_b){
          filtro  <- filter( vettore  , rep(1,durata_b) , method = "convolution" , sides = 1 )
          filtro1 <- filter( vettore1 , rep(1,durata_b) , method = "convolution" , sides = 1 )
          if ( any ( abs(filtro) <= incremento_b , na.rm = TRUE )) persist <- which ( abs(filtro) <= incremento_b & filtro1 == durata_b)
        }  #fine if lunghezza vettore sufficiente
      }  #fine if c'è qualcosa in "posizione"
#

      if (length(persist)==0) {
        cat ( " Nessuna segnalazione in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
      } else {
        cat ( " segnalo in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
# ATTENZIONE:    "indice_da_segnalare" contiene le posizioni relative ai vettori "dati" 
#                                      delle possibili persistenze
#             "indice_da_segnalare_01" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure
#             "indice_da_segnalare_02" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure senza ripetizioni
        indice_da_segnalare<-vector()
        indice_da_segnalare_01<-vector()
        indice_da_segnalare_02<-vector()
# segnalazioni in tavola DQC 
        for(pp in 1:length(persist)){
# per ogni persistenza segnalata:
          fine_persistenza <- (persist[pp]+1)
# segnalo persistenza su tutta la durata_b
          indice_da_segnalare <- fine_persistenza - c(0:durata_b) 
          if ( diff(range(dati$Misura[indice_da_segnalare],na.rm=T)) > incremento_b ) next
# controllo sulla variabilita'
#  per il test T2b degli igrometri si assume che il range massimo-minimo stia entro una soglia 
#  definita E che un indice di variabilita' sia sufficientemente piccolo.
#  L'indice di variabilita' scelto e' la distanza fra i due whiskers nella funzione boxplot,
#  ovvero si considera qualcosa in piu' del semplice IQR ma si cerca di non considerare
#  gli outlier
          bxp<-boxplot( dati$Misura[indice_da_segnalare]~rep(1,length(indice_da_segnalare)), plot=F)
          if ( (bxp$stats[5,1]-bxp$stats[1,1])>whiskers) next
# evito di segnalare se all'interno della persistenza c'e' un cambio di sensore
          ii<-0
          ii<- which((indice_da_segnalare %in% cambio_sensore) == T) 
          if(length(ii)>0)  next
# evito di segnalare se la persistenza non e' relativa ad ore consecutive 
          ii<-0
          ii<- which((indice_da_segnalare %in% salto_di_ore) == T )
          if(length(ii)>0)  next
# OK la persistenza e' da segnalare, allora aggiorno il vettore indice_da_segnalare_01
# memorizzo record del data.frame da segnalare
          aux_01<-length(indice_da_segnalare_01)+1
          aux_00<-aux_01-1+length(indice_da_segnalare)
          indice_da_segnalare_01[aux_01:aux_00]<-as.numeric(indice_da_segnalare[1:length(indice_da_segnalare)])
        }
# se esiste almeno una persistenza da segnalare allora inserisci dati nel DB
        if ( length(indice_da_segnalare_01)!=0 ) {
#         indice_da_segnalare_02 <- elimino le ripetizioni in indice_da_segnalare_01
          indice_da_segnalare_02 <- which( seq(1:max(indice_da_segnalare_01,by=1)) %in% indice_da_segnalare_01  )
          stringa <- toString(paste(" (",dati$IDsensore[indice_da_segnalare_02], 
                                    ", '",as.character(dati$date_riformattate[indice_da_segnalare_02]),
                                    "','", test[2],"','",fallimento_a, "','", Autore, "',",Data,",NULL)",sep=""))
          query_insert <- paste("insert into ",nome_tavola_DQC,
                                " values ",stringa,
                                " on duplicate key update  Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
          cat ( " query > ",query_insert," \n",file = file_log, append=T)
          q_insert <- try(dbGetQuery(conn, query_insert),silent=TRUE)
          if (inherits(q_insert, "try-error")) {
            cat(q_insert,"\n", file=file_log, append=T)
            quit(status=1)
          }
        } # finese length(indice_da_segnalare_01)!=0
      } # finese-else length(persist)==0
    } # tipologia[tip] == "Igrometri"
  } # finese fallimento_b!="X"
#-------------------------------------------------------------------------------------------------------------
# Test T2c ---------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------------------
# Il test T2c e' eseguito per una sola tipologia:
#  + Igrometri -> PERSISTENZA solo per valori inferiori a 99%
#-------------------------------------------------------------------------------------------------------------
  if (fallimento_c!="X"){
    cat ( " test T2c  \n" , file = file_log , append = TRUE )
# Test T2c Igrometri 
    if (tipologia[tip] == "Igrometri") {

      differenza_misura  <- NULL 
      differenza_sensore <- NULL
      differenza_ore     <- NULL
#  dati 1...N  //  differenza_XXX 1...(N-1)  //  differenza(i)=dati(i+1)-dati(i) 
      differenza_misura  <- diff(dati$Misura)
      differenza_sensore <- abs(diff(dati$IDsensore))
      differenza_ore     <- as.integer(diff(as.POSIXct(strptime(dati$date_riformattate , format="%Y/%m/%d %H:%M"),"UTC")))

      salto_di_ore       <- NULL 
      cambio_sensore     <- NULL 
      salto_di_ore       <- which(differenza_ore!=1)
      cambio_sensore     <- which(differenza_sensore > 0)

# inizializzo array
      vettore <- numeric()
      vettore1 <- numeric()
      posizione <- numeric()
#
      persist<-numeric()

      if(any(abs(differenza_misura) < incremento_c,na.rm=TRUE)) posizione <- which(abs(differenza_misura) < incremento_c)

      if (length(posizione) != 0 ){
        vettore1[posizione] <- 1
        vettore[posizione] <- differenza_misura[posizione]
        if (length(vettore) >= durata_c){
          filtro  <- filter( vettore  , rep(1,durata_c) , method = "convolution" , sides = 1 )
          filtro1 <- filter( vettore1 , rep(1,durata_c) , method = "convolution" , sides = 1 )
          if ( any ( abs(filtro) <= incremento_c , na.rm = TRUE )) persist <- which ( abs(filtro) <= incremento_c & filtro1 == durata_c)
        }  #fine if lunghezza vettore sufficiente
      }  #fine if c'è qualcosa in "posizione"
#

      if (length(persist)==0) {
        cat ( " Nessuna segnalazione in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
      } else {
# segnalazioni in tavola DQC 
        cat ( " segnalo in ", nome_tavola_DQC,"\n" , file = file_log , append = TRUE )
# ATTENZIONE:    "indice_da_segnalare" contiene le posizioni relative ai vettori "dati" 
#                                      delle possibili persistenze
#             "indice_da_segnalare_01" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure
#             "indice_da_segnalare_02" contiene le posizioni relative ai vettori "dati"
#                                      delle persistenze sicure senza ripetizioni
        indice_da_segnalare<-vector()
        indice_da_segnalare_01<-vector()
        indice_da_segnalare_02<-vector()
        for(pp in 1:length(persist)){
# per ogni persistenza segnalata:
          fine_persistenza <- (persist[pp]+1)

# segnalo persistenza su tutta la durata_c
          indice_da_segnalare <- fine_persistenza - c(0:durata_c) 
          if ( diff(range(dati$Misura[indice_da_segnalare],na.rm=T)) > incremento_c ) next
# se la persistenza coinvolge valori maggiori di 99% allora non segnalare nulla
          if ( any(dati$Misura[indice_da_segnalare]>=99) ) next
# evito di segnalare se all'interno della persistenza c'e' un cambio di sensore
          ii<-0
          ii<- which((indice_da_segnalare %in% cambio_sensore) == T) 
          if(length(ii)>0)  next
# evito di segnalare se la persistenza non e' relativa ad ore consecutive 
          ii<-0
          ii<- which((indice_da_segnalare %in% salto_di_ore) == T )
          if(length(ii)>0)  next
# OK la persistenza e' da segnalare, allora aggiorno il vettore indice_da_segnalare_01
# memorizzo record del data.frame da segnalare
          aux_01<-length(indice_da_segnalare_01)+1
          aux_00<-aux_01-1+length(indice_da_segnalare)
          indice_da_segnalare_01[aux_01:aux_00]<-as.numeric(indice_da_segnalare[1:length(indice_da_segnalare)])
        }
# se esiste almeno una persistenza da segnalare allora inserisci dati nel DB
        if ( length(indice_da_segnalare_01)!=0 ) {
#         indice_da_segnalare_02 <- elimino le ripetizioni in indice_da_segnalare_01
          indice_da_segnalare_02 <- which( seq(1:max(indice_da_segnalare_01,by=1)) %in% indice_da_segnalare_01  )
          stringa <- toString(paste(" (",dati$IDsensore[indice_da_segnalare_02], 
                                    ", '",as.character(dati$date_riformattate[indice_da_segnalare_02]),
                                    "','", test[3],"','",fallimento_a, "','", Autore, "',",Data,",NULL)",sep=""))
          query_insert <- paste("insert into ",nome_tavola_DQC,
                                " values ",stringa,
                                " on duplicate key update  Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
          cat ( " query > ",query_insert," \n",file = file_log, append=T)
          q_insert <- try(dbGetQuery(conn, query_insert),silent=TRUE)
          if (inherits(q_insert, "try-error")) {
            cat(q_insert,"\n", file=file_log, append=T)
            quit(status=1)
          }
        } # finese length(indice_da_segnalare_01)!=0
      } # finese-else length(persist)==0
    } # tipologia[tip] == "Igrometri"
  } # finese fallimento_c!="X"
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



