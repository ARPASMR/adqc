#------------------------------------------------------------------------------
#
#
#
# 2009/11/26 MR. e cr. Cambio Flag_automatico 'G' -> 'P' e 'W' -> 'S'.
#                      'Failure' -> 'Fail'  // 'Warning' -> 'Suspect'
#------------------------------------------------------------------------------
library(DBI)
library(RMySQL)
library(RODBC)
#..............................................................................
# GestioneTest di eventuali errori
neverstop<-function(){
  print("EE..ERRORE durante l'esecuzione dello script!! Messaggio d'Errore prodotto:")
}
options(show.error.messages=TRUE,error=neverstop,digits=20)
#==============================================================================
# funzione per costruire il background: INIZIO
AA<-matrix(nrow=5,ncol=5,data=0.)
vv<-vector(length=5,mode="numeric")
aux_vv<-vector(length=5,mode="numeric")
id5<-matrix(data=0.,nrow=5,ncol=5)
id5[row(id5)==col(id5)]=1.

XZinv<-function(b_x,b_z,b_yo,dz) {
  aux_length<-length(b_yo)
  param<-vector(length=8,mode="numeric")
  resvec<-vector(length=aux_length,mode="numeric")
  resvecz<-vector(length=aux_length,mode="numeric")
  aux_yb<-vector(length=aux_length,mod="numeric")
  aux_yb_noinv<-vector(length=aux_length,mode="numeric")
  b_yb<-vector(length=aux_length,mode="numeric")
  aux_vv1_min<-NA
  aux_vv2_min<-NA
  aux_vv3_min<-NA
  aux_vv4_min<-NA
  aux_vv5_min<-NA
  aux_length<-length(b_yo)
#  print("X  Z  Yo")
#  print(matrix(cbind(b_x,b_z,b_yo),ncol=3,byrow=FALSE))
#  print(paste("aux_length = ",aux_length,sep=""))
  if (aux_length<=0) { return(NA) }
  avx<-mean(b_x)
  avz<-mean(b_z)
  avt<-mean(b_yo)
  vv[1]<-sum(b_yo)
#  print(paste("avx=",avx," avz=",avz," avt=",avt," vv[1]=",vv[1],sep=""))
  resmin<-1e+25
  zinv<-0
# background senza inversione
  ax2<-sum((b_x-avx)**2.)
  az2<-sum((b_z-avz)**2.)
  azx<-sum((b_z-avz)*(b_x-avx))
  axt<-sum((b_yo-avt)*(b_x-avx))
  azt<-sum((b_yo-avt)*(b_z-avz))
  det<-ax2*az2-azx*azx
  alp1<-(az2*axt-azx*azt)/det
  gam1<-(ax2*azt-azx*axt)/det
  aux_yb_noinv<-avt+alp1*(b_x-avx)+gam1*(b_z-avz)
  res_noinv<-sum((b_yo-aux_yb_noinv)**2)
#  print("Yb noinv:")
#  print(aux_yb_noinv)
# background con inversione
  for (z in seq(10,2000,by=10) ) {
    if (length(b_x[b_z>z])<6|length(b_x[b_z<=z])<6) {
      next
    }
#    print(paste("=#=# z >",z,sep=""))
#    print("b_x[b_z>z] b_z[b_z>z]")
#    print(matrix(cbind(b_x[b_z>z],b_z[b_z>z]),ncol=2,byrow=FALSE))
#    print("b_x[b_z<=z] b_z[b_z<=z]")
#    print(matrix(cbind(b_x[b_z<=z],b_z[b_z<=z]),ncol=2,byrow=FALSE))
    AA[1,1]<-length(b_yo)
    AA[1,2]<-sum(b_x[b_z>z]-avx)
    AA[1,3]<-sum(b_z[b_z>z]-z)
    AA[1,4]<-sum(b_x[b_z<=z]-avx)
    AA[1,5]<-sum(b_z[b_z<=z]-z)
    AA[2,1]<-AA[1,2]
    AA[2,2]<-sum((b_x[b_z>z]-avx)**2)
    AA[2,3]<-sum((b_z[b_z>z]-z)*(b_x[b_z>z]-avx))
    AA[2,4]<-0.
    AA[2,5]<-0.
    AA[3,1]<-AA[1,3]
    AA[3,2]<-AA[2,3]
    AA[3,3]<-sum((b_z[b_z>z]-z)**2)
    AA[3,4]<-0.
    AA[3,5]<-0.
    AA[4,1]<-AA[1,4]
    AA[4,2]<-0.
    AA[4,3]<-0.
    AA[4,4]<-sum((b_x[b_z<=z]-avx)**2)
    AA[4,5]<-sum((b_z[b_z<=z]-z)*(b_x[b_z<=z]-avx))
    AA[5,1]<-AA[1,5]
    AA[5,2]<-0.
    AA[5,3]<-0.
    AA[5,4]<-sum((b_x[b_z<=z]-avx)*(b_z[b_z<=z]-z))
    AA[5,5]<-sum((b_z[b_z<=z]-z)**2)
    vv[2]<-sum(b_yo[b_z>z]*(b_x[b_z>z]-avx))
    vv[3]<-sum(b_yo[b_z>z]*(b_z[b_z>z]-z))
    vv[4]<-sum(b_yo[b_z<=z]*(b_x[b_z<=z]-avx))
    vv[5]<-sum(b_yo[b_z<=z]*(b_z[b_z<=z]-z))
#    print("AA")
#    print(AA)
#    print("id5")
#    print(id5)
    InvAA<-try(solve(AA,id5))
    if (inherits(InvAA,"try-error")) {
      quit(status=1)
    }
#    print("InvAA")
#    print(InvAA)
#    print("InvAA%*%AA")
#    print(InvAA%*%AA)
    aux_vv<-InvAA %*% vv
#    print(paste("aux_vv [1,2,3,4,5]",aux_vv[1],aux_vv[2],aux_vv[3],aux_vv[4],aux_vv[5],sep=" "))
#    print(paste("    vv [1,2,3,4,5]",vv[1],vv[2],vv[3],vv[4],vv[5],sep=" "))
    aux_yb<-NA
    zab<-z+dz
    zbe<-z-dz
    bfab<-aux_vv[1]+aux_vv[2]*(b_x-avx)+aux_vv[3]*(b_z-z)
    bfbe<-aux_vv[1]+aux_vv[4]*(b_x-avx)+aux_vv[5]*(b_z-z)
    aux_yb[b_z>zab]<-aux_vv[1]+aux_vv[2]*(b_x[b_z>zab]-avx)+aux_vv[3]*(b_z[b_z>zab]-z)
    aux_yb[b_z<=zbe]<-aux_vv[1]+aux_vv[4]*(b_x[b_z<=zbe]-avx)+aux_vv[5]*(b_z[b_z<=zbe]-z)
    aux_yb[(b_z>zbe)&(b_z<=zab)]<-( bfab[(b_z>zbe)&(b_z<=zab)]*(b_z[(b_z>zbe)&(b_z<=zab)]-zbe) +
                                      bfbe[(b_z>zbe)&(b_z<=zab)]*(zab-b_z[(b_z>zbe)&(b_z<=zab)]) ) /
                                    (zab-zbe)
    aux_yb<-matrix(aux_yb,ncol=1)
#    aux_yb[b_z>z]<-aux_vv[1]+aux_vv[2]*(b_x[b_z>z]-avx)+aux_vv[3]*(b_z[b_z>z]-z)
#    aux_yb[b_z<=z]<-aux_vv[1]+aux_vv[4]*(b_x[b_z<=z]-avx)+aux_vv[5]*(b_z[b_z<=z]-z)
#    aux_yb<-matrix(aux_yb,ncol=1)
#    print(paste("aux_vv [1,2,3,4,5]",aux_vv[1],aux_vv[2],aux_vv[3],aux_vv[4],aux_vv[5],sep=" "))
#    print("z x yo yb")
#    ii<-order(b_z)
#    print(matrix(cbind(b_z[ii],b_x[ii],b_yo[ii],aux_yb[ii]),nrow=aux_length,ncol=4,byrow=FALSE))
    res<-sum((b_yo-aux_yb)**2)
    resvec[as.integer(z/10)]<-res
    resvecz[as.integer(z/10)]<-z
#    print(paste("        res resmin",res,resmin,sep=" "))
    if ( abs(aux_vv[2]<=2)&abs(aux_vv[4])<=2&res<resmin ) {
#      print(paste(" ZINV nuovo = ",z,sep=""))
      b_yb<-aux_yb
      aux_vv1_min<-aux_vv[1]
      aux_vv2_min<-aux_vv[2]
      aux_vv3_min<-aux_vv[3]
      aux_vv4_min<-aux_vv[4]
      aux_vv5_min<-aux_vv[5]
      b_yb<-aux_yb
      zinv<-z
      resmin<-res
    }
  }
#  postscript(file="caccola.ps")
#  plot(b_yb,b_z,col="gray",xlim=c(min(c(b_yb,b_yo)),max(c(b_yb,b_yo))),ylim=c(0,3500))
#  points(b_yo,b_z,col="red")
#  abline(v=(-16))
#  par(new=T)
#  plot(resvec,resvecz,xlim=c(0,10000000),type="l",ylim=c(0,3500),axes=F)
#  axis(3)
#  dev.off()
#  print(paste(" ZINV = ",zinv,sep=""))
#  print("z x yo yb")
#  ii<-order(b_z)
#  print(matrix(cbind(b_z[ii],b_x[ii],b_yo[ii],b_yb[ii]),nrow=aux_length,ncol=4,byrow=FALSE))
  if (resmin<res_noinv) {
    param[1]<-zinv
    param[2]<-avx
    param[3]<-aux_vv1_min
    param[4]<-resmin
    param[5]<-aux_vv2_min
    param[6]<-aux_vv3_min
    param[7]<-aux_vv4_min
    param[8]<-aux_vv5_min
  } else {
    param[1]<-avz
    param[2]<-avx
    param[3]<-avt
    param[4]<-res_noinv
    param[5]<-alp1
    param[6]<-gam1
    param[7]<-(-99999.)
    param[8]<-(-99999.)
  }
  return(param)
}
# funzione per costruire il background: FINE
#==============================================================================
# BODy - BODy - BODy - BODy - BODy - BODy - BODy - BODy - BODy - BODy - BODy
#==============================================================================
# [] Leggi riga di comando
arguments <- commandArgs()
arguments
data_inizio <- arguments[3]
data_fine <- arguments[4]
IDtest <- arguments[5]
Livello <- arguments[6]
# [] Operazioni su istanti temporali
data_inizio <- strptime(data_inizio,"%Y%m%d%H%M","UTC")
data_fine <- strptime(data_fine,"%Y%m%d%H%M","UTC")
time<-as.POSIXlt(seq(as.POSIXlt(data_inizio),as.POSIXlt(data_fine),by="1 hours"),"UTC")
# LOG
print("OIt2mdqc versione 4.0 (con coefficiente di urbanita')")
print("argomenti in riga di comando:")
print(paste("data inizio=",data_inizio," data fine=",data_fine," IDtest=",IDtest," Livello=",Livello,sep=""))
print(paste("numero di istanti temporali = ",length(time$hour),sep=""))
#
if (Livello=="Fail") {
  Livello_test="F"
  Livello_notest="S"
} else if (Livello=="Suspect") {
  Livello_test="S"
  Livello_notest="F"
}
#-------------------------------------------------------------------
# [] COLLEGAMENTO AL DB
MySQL(max.con=16,fetch.default.rec=500,force.reload=FALSE)
#definisco driver
drv<-dbDriver("MySQL")
#apro connessione con il db descritto nei parametri del gruppo "GestioneTest"
#nel file "/home/swiluppo/.my.cnf
conn<-dbConnect(drv,group="GestioneTest")
#-------------------------------------------------------------------
# [] Inizializzazione parametri analisi/DQC
#ini<-read.table(file=fileini,header=FALSE,sep="=",colClasses = "character")
query_richiesta_TDQCinfo <- paste("select Rif3,Suspect,Fail from M_Termometri_DQCinfo where Test='",IDtest,"'",sep="")
q_richiesta_TDQCinfo <- dbGetQuery(conn, query_richiesta_TDQCinfo)
if (inherits(q_richiesta_TDQCinfo,"try-error")) {
  quit(status=1)
}
print(q_richiesta_TDQCinfo)
length_Rif3=length(strsplit(q_richiesta_TDQCinfo$Rif3," ")[[1]])
aux_Rif3<-vector(length=4,mode="numeric")
for (i in seq(1:length_Rif3) ) {
  aux_Rif3[i]=as.numeric(strsplit(strsplit(q_richiesta_TDQCinfo$Rif3," ")[[1]][i],"=")[[1]][2])
}
sig2o_T<-aux_Rif3[1]
eps2_T<-aux_Rif3[2]
Dh_T<-aux_Rif3[3]
Dz_T<-aux_Rif3[4]
# Check for consistency
  if (!is.numeric(sig2o_T)|(sig2o_T>5)) {
    print("oit2mdqc_V04 > Errore! VARobs must be numeric and < 5 C^2")
    quit(status=1)
  }
  if (!is.numeric(eps2_T)|(eps2_T>1)) {
    print("oit2mdqc_V04 > Errore! EPS2 must be numeric and < 1")
    quit(status=1)
  }
  if (!is.numeric(Dh_T)|(Dh_T>100)) {
    print("oit2mdqc_V04 > Errore! Dh must be numeric and < 100 Km")
    quit(status=1)
  }
  if (!is.numeric(Dz_T)|(Dz_T>1000)) {
    print("oit2mdqc_V04 > Errore! Dz must be numeric and < 1000 m")
    quit(status=1)
  }
#
  if (Livello=="Fail") {
    Livello_stringa=q_richiesta_TDQCinfo$Fail
  } else if (Livello=="Suspect") {
    Livello_stringa=q_richiesta_TDQCinfo$Suspect
  } else {
    print("oit2mdqc_V04 > Errore! Il livello di controllo specificato puo' essere solo:")
    print("  Suspect")
    print("  Fail")
    quit(status=1)
  }
  print(Livello_stringa)
  print(strsplit(Livello_stringa,">")[[1]][2])
  T2_T=as.numeric(strsplit(strsplit(Livello_stringa,">")[[1]][2],"\\(")[[1]][1])
  if (!is.numeric(T2_T)|(T2_T>100)) {
    print("oit2mdqc_V04 > Errore! T2 must be numeric and < 100")
    quit(status=1)
  }
  print("ANALYSIS and DQC parameters")
  print(paste("VARobs[C^2] EPS2 Dh[Km] Dz[m] > ",round(sig2o_T,3),round(eps2_T,3),round(Dh_T,3),round(Dz_T,3),sep=" "))
  print(paste("T^2 = ",round(T2_T,3),sep=" "))
#-------------------------------------------------------------------
# [] LETTURA FILE DI RICHIESTA DATI
# q_richiesta_T_wrong > e' un array di controllo. Contiene quei dati che sono stati rigettati
#  dal test di plausibilita' (P1a) ma che per qualche strana ragione figurano come dati
#  "buoni" nella tabella dei dati buoni.
query_richiesta_T_wrong <- paste("select t1.IDsensore,t1.Data_e_ora,t2.Test,t2.Result from M_Termometri as t1, M_TermometriDQC as t2 where t1.Data_e_ora<= '", data_fine,"' and t1.Data_e_ora >= '",data_inizio,"'"," and t1.Flag_automatica='P' and t2.IDsensore=t1.IDsensore and t2.Data_e_ora=t1.Data_e_ora and t2.Test='P1a'",sep="")
query_richiesta_T_ok <- paste("select t1.IDstazione,t2.IDsensore,t2.Data_e_ora,t2.Misura from A_Sensori as t1, M_Termometri as t2 where t2.Data_e_ora<= '", data_fine,"' and t2.Data_e_ora >= '",data_inizio,"'"," and t2.Flag_automatica='P' and t1.IDsensore=t2.IDsensore",sep="")
q_richiesta_T_ok <- dbGetQuery(conn, query_richiesta_T_ok)
if (inherits(q_richiesta_T_ok,"try-error")) {
  quit(status=1)
}
q_richiesta_T_wrong <- dbGetQuery(conn, query_richiesta_T_wrong)
if (inherits(q_richiesta_T_wrong,"try-error")) {
  quit(status=1)
}
#
#print("Richiesta dati [ok]:")
#print(q_richiesta_T_ok)
#print("Numero di record presenti in Richiesta dati [ok]:")
#print(length(q_richiesta_T_ok$IDsensore))
#print("Richiesta dati [wrong]:")
#print(q_richiesta_T_wrong)
#
condi_aux<-vector(length=length(q_richiesta_T_ok$IDsensore),mode="logical")
condi_aux[]<-T
for ( i in seq(1:length(q_richiesta_T_ok$IDsensore)) ) {
  condi_aux[i]=T
  for (j in seq(1:length(q_richiesta_T_wrong$IDsensore))) {
    if (q_richiesta_T_ok$IDsensore[i]==q_richiesta_T_wrong$IDsensore[j]) {
      if (q_richiesta_T_ok$Data_e_ora[i]==q_richiesta_T_wrong$Data_e_ora[j]) {
        condi_aux[i]=F
        print("oit2mdqc_v04 > @~@~@ ATTENZIONE! Si e' rilevata la presenza di una misura con flag GOOD ma con rilevato un fallimento del test di soglia (P1a_F):")
        print(paste(" IDsens data-e-ora > ",q_richiesta_T_wrong$IDsensore[j],q_richiesta_T_wrong$Data_e_ora[j],sep=" "))
      }
    }
  }
}
#condi_aux=!((q_richiesta_T_ok$IDsensore%in%q_richiesta_T_wrong$IDsensore)&(q_richiesta_T_ok$Data_e_ora%in%q_richiesta_T_wrong$Data_e_ora))
#if (length(condi_aux)==0) {
#  condi_aux<-vector(length(q_richiesta_T_ok$IDstazione),mode="logical")
#  condi_aux[]<-TRUE
#}
idi_stz<-vector()
idi_sns<-vector()
date<-vector()
val<-vector()
idi_stz<-q_richiesta_T_ok$IDstazione[condi_aux]
idi_sns<-q_richiesta_T_ok$IDsensore[condi_aux]
date<-q_richiesta_T_ok$Data_e_ora[condi_aux]
val<-q_richiesta_T_ok$Misura[condi_aux]
Trec<-data.frame(cbind(idi_stz,idi_sns,date,val))
idi_stz<-vector()
idi_sns<-vector()
date<-vector()
val<-vector()
print(Trec[1,])
print("=============================================================================================================")
#-------------------------------------------------------------------
# Cancella i risultati ottenuti con test S1a precedenti
query_delete <- paste("delete from M_TermometriDQC where Data_e_ora<= '", data_fine,"' and Data_e_ora >= '",data_inizio,"' and Result='",Livello_test,"' and Test='",IDtest,"'",sep="")
print(query_delete)
q_delete <- try(dbGetQuery(conn, query_delete))
if (inherits(q_delete,"try-error")) {
  quit(status=1)
}
#-------------------------------------------------------------------
#    LETTURA FILE DI ANAGRAFICA
# verifico esistenza riga nella tavola
query_anagrafica_T<-paste("select distinct t1.IDstazione,t2.IDsensore,t1.CGB_Nord,t1.CGB_Est,t1.Quota,t3.UrbanWeight from A_Stazioni as t1, A_Sensori as t2, P_SupportoOI as t3 where (t1.IDstazione=t2.IDstazione and t1.IDstazione=t3.IDstazione) and (t2.NOMEtipologia='T' or t2.NOMEtipologia='TV')",sep="")
#l'output dell'istruzione precedente e':
# | IDstazione | IDsensore | CGB_Nord | CGB_Est | Quota | UrbanWeight |
q_anagrafica_T <- dbGetQuery(conn, query_anagrafica_T)
if (inherits(q_anagrafica_T,"try-error")) {
  quit(status=1)
}
print(q_anagrafica_T[1,])
print("=============================================================================================================")
#
# [] Operazioni sull'anagrafica
idi_stz<-vector()
idi_sns<-vector()
gb_nord<-vector()
gb_est<-vector()
z<-vector()
uw<-vector()
j<-0
for ( i in seq(1:length(q_anagrafica_T$IDsensore)) ) {
  q_anagrafica_T$IDsensore[i] 
  if (!is.na(q_anagrafica_T$IDstazione[i]) & 
      !is.na(q_anagrafica_T$IDsensore[i])  &
      !is.na(q_anagrafica_T$CGB_Nord[i])  &
      !is.na(q_anagrafica_T$CGB_Est[i])  &
      !is.na(q_anagrafica_T$Quota[i])  &
      !is.na(q_anagrafica_T$UrbanWeight[i]) ) {
    j<-j+1
    idi_stz[j]<-q_anagrafica_T$IDstazione[i]    
    idi_sns[j]<-q_anagrafica_T$IDsensore[i]
    gb_nord[j]<-q_anagrafica_T$CGB_Nord[i]
    gb_est[j]<-q_anagrafica_T$CGB_Est[i] 
    z[j]<-q_anagrafica_T$Quota[i] 
    uw[j]<-q_anagrafica_T$UrbanWeight[i] 
  }
}
Tanag<-data.frame(cbind(idi_stz,idi_sns,gb_nord,gb_est,z,uw))
print(Tanag)
print("=============================================================================================================")
#-------------------------------------------------------------------
# [] Interpolazione
length_totale=length(Trec$val)
length_Tanag=length(Tanag$idi_sns)
sele_inTanag<-Trec$idi_sns %in% Tanag$idi_sns
sele_NOTisnaTrecVal<-!is.na(Trec$val)
dzbf<-100.
# [] ciclo temporale
for (t in seq(1:length(time$hour))){
#  aux0: vettore logico con TRUE dove si ha osservazione (rilevate al tempo t-esimo) E
#                                                        (appartenente all'anagraficaOK) E
#                                                        (valore diverso da NA)
  aux0<-vector(mode="logical",length=length_totale)
  print(paste("=================================================================="))
  print(paste("-- ",1900+time[t]$year,"/",1+time[t]$mon,"/",time[t]$mday," ",time[t]$hour,":00",sep=""))
  aux0<-time[t]==as.POSIXlt(Trec$date,"UTC") & sele_NOTisnaTrecVal & sele_inTanag
  numdata<-length(aux0[aux0])
  print(paste(" numero di osservazioni (non NA) E (contenute in anagraficaOK) presenti = ",numdata,sep=""))
  if (numdata<=40) {
    print(" ATTENZIONE!! Poche osservazioni passo all'istante successivo")
    next 
  }
  id_rej_sens<-vector(length=numdata,mode="numeric")
  id_rej_staz<-vector(length=numdata,mode="numeric")
  id_rej_sens[]<-(-1)
  id_rej_staz[]<-(-1)
  id_rej_count<-0
# [] ciclo DQC 
  repeat {
#    aux00: vettore logico con TRUE dove si ha una osservazione (rilevata al tempo t-esimo) E 
#                                                               (in anagraficaOK) E 
#                                                               (valore diverso da NA) E
#                                                               (non rigettata in precedenza)
    aux00<-vector(mode="logical",length=length_totale)
    aux00<-aux0 & !(Trec$idi_sns %in% id_rej_sens)
    length_iter<-length(aux00[aux00])
    print(paste(" numero di osservazioni valide presenti per questa iterazione = ",length_iter,sep=""))
    if (length_iter<=0) {break}
# [] crea la matrice di covarianza Dt al tempo i-esimo
    VecY<-vector(mode="numeric",length=length_iter)
    VecX<-vector(mode="numeric",length=length_iter)
    VecZ<-vector(mode="numeric",length=length_iter)
    VecS<-vector(mode="numeric",length=length_iter)
    VecSNS<-vector(mode="numeric",length=length_iter)
    VecUrb<-vector(mode="numeric",length=length_iter)
    yo<-vector(mode="numeric",length=length_iter)
    yb<-vector(mode="numeric",length=length_iter)
    yb_all<-vector(mode="numeric",length=length_iter)
    ya<-vector(mode="numeric",length=length_iter)
    ya_all<-vector(mode="numeric",length=length_iter)
    yav<-vector(mode="numeric",length=length_iter)
    VecY<-as.numeric(as.vector(Tanag$gb_nord[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    VecX<-as.numeric(as.vector(Tanag$gb_est[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    VecZ<-as.numeric(as.vector(Tanag$z[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    VecS<-as.numeric(as.vector(Tanag$idi_stz[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    VecSNS<-as.numeric(as.vector(Tanag$idi_sns[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    VecUrb<-as.numeric(as.vector(Tanag$uw[Tanag$idi_sns %in% Trec$idi_sns[aux00]]))
    for (s in seq(1:length_iter)) {
      indice<-Trec$idi_sns[aux00] %in% VecSNS[s]
      yo[s]<-as.numeric(as.vector(Trec$val[aux00][indice]))
    }
#    print("IDstaz IDsens X Y Z UW Yo")
#    print(matrix(rbind(as.matrix(VecS),as.matrix(VecSNS),as.matrix(VecX),as.matrix(VecY),as.matrix(VecZ),as.matrix(VecUrb),as.matrix(yo)),nrow=length(VecS),ncol=7,byrow=FALSE))
# [] matrici S e (S+R)
    Disth<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    Distz<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    Urbano<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    att<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    Dt<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    St<-matrix(ncol=length_iter,nrow=length_iter,data=0.)
    Disth<-(outer(VecY,VecY,FUN="-")**2.+outer(VecX,VecX,FUN="-")**2.)**0.5/1000.
#    print("Disth")
#    print(Disth)
    Distz<-abs(outer(VecZ,VecZ,FUN="-"))
#    print("Distz")
#    print(Distz)
    Urbano<-abs(outer(VecUrb,VecUrb,FUN="-"))
    attmin=0.4
    att<-1.-(1.-attmin)*Urbano
#    print(att)
    Dt<-exp(-0.5*(Disth/Dh_T)**2.-0.5*(Distz/Dz_T)**2.) * att
    St<-Dt
    Dt[row(Dt)==col(Dt)]<-Dt[row(Dt)==col(Dt)]+eps2_T
# [] matrice G
    Disth<-matrix(ncol=length_iter,nrow=length_Tanag,data=0.)
    Distz<-matrix(ncol=length_iter,nrow=length_Tanag,data=0.)
    Urbano<-matrix(ncol=length_iter,nrow=length_Tanag,data=0.)
    att<-matrix(ncol=length_iter,nrow=length_Tanag,data=0.)
    Gt<-matrix(ncol=length_iter,nrow=length_Tanag,data=0.)
    Disth<-(outer(Tanag$gb_nord,VecY,FUN="-")**2.+outer(Tanag$gb_est,VecX,FUN="-")**2.)**0.5/1000.
#    print("Disth")
#    print(Disth)
    Distz<-abs(outer(Tanag$z,VecZ,FUN="-"))
#    print("Distz")
#    print(Distz)
    Urbano<-abs(outer(Tanag$uw,VecUrb,FUN="-"))
    attmin=0.4
    att<-1.-(1.-attmin)*Urbano
    Gt<-exp(-0.5*(Disth/Dh_T)**2.-0.5*(Distz/Dz_T)**2.) * att
# [] inversione di Dt
#    print("detDt")
#    print(detDt)
    Mt<-length_iter
#    print("Mt")
#    print(Mt)
    ide<-matrix(data=0,ncol=Mt,nrow=Mt)
    ide[row(ide)==col(ide)]=1
    InvDt<-solve(Dt,ide)
#    auxyo<-matrix(Trec$val[aux00],ncol=1,byrow=TRUE)
    yb<-vector(length=length(yo))
    yb_all<-vector(length=length(Tanag$z))
    param_yb<-XZinv(VecX,VecZ,yo,dzbf)
#    print(paste("@-@-@",
#          round(param_yb[1],3),
#          round(param_yb[2],3),
#          round(param_yb[3],3),
#          round(param_yb[4],3),
#          round(param_yb[5],3),
#          round(param_yb[6],3),
#          round(param_yb[7],3),
#          round(param_yb[8],3),
#          sep=" "))
    if (param_yb[8]==(-99999.)) {
      yb<-param_yb[3]+param_yb[5]*(VecX-param_yb[2])+param_yb[6]*(VecZ-param_yb[1])
      yb_all<-param_yb[3]+param_yb[5]*(as.vector(Tanag$gb_est)-param_yb[2])+param_yb[6]*(as.vector(Tanag$z)-param_yb[1])
    } else {
      zinv<-param_yb[1]
      zabov<-zinv+dzbf
      zbelo<-zinv-dzbf
      bfabov<-param_yb[3]+param_yb[5]*(VecX-param_yb[2])+param_yb[6]*(VecZ-zinv)
      bfbelo<-param_yb[3]+param_yb[7]*(VecX-param_yb[2])+param_yb[8]*(VecZ-zinv)
      bfabov_all<-param_yb[3]+param_yb[5]*(Tanag$gb_est-param_yb[2])+param_yb[6]*(Tanag$z-zinv)
      bfbelo_all<-param_yb[3]+param_yb[7]*(Tanag$gb_est-param_yb[2])+param_yb[8]*(Tanag$z-zinv)
      yb[VecZ>zabov]<-param_yb[3]+param_yb[5]*(VecX[VecZ>zabov]-param_yb[2])+param_yb[6]*(VecZ[VecZ>zabov]-zinv)
      yb[VecZ<=zbelo]<-param_yb[3]+param_yb[7]*(VecX[VecZ<=zbelo]-param_yb[2])+param_yb[8]*(VecZ[VecZ<=zbelo]-zinv)
      yb[(VecZ>zbelo)&(VecZ<=zabov)]<-( bfabov[(VecZ>zbelo)&(VecZ<=zabov)]*(VecZ[(VecZ>zbelo)&(VecZ<=zabov)]-zbelo) +
                                        bfbelo[(VecZ>zbelo)&(VecZ<=zabov)]*(zabov-VecZ[(VecZ>zbelo)&(VecZ<=zabov)]) ) /
                                      (zabov-zbelo)
      yb_all[Tanag$z>zabov]<-param_yb[3]+param_yb[5]*(Tanag$gb_est[Tanag$z>zabov]-param_yb[2])+param_yb[6]*(Tanag$z[Tanag$z>zabov]-zinv)
      yb_all[Tanag$z<=zbelo]<-param_yb[3]+param_yb[7]*(Tanag$gb_est[Tanag$z<=zbelo]-param_yb[2])+param_yb[8]*(Tanag$z[Tanag$z<=zbelo]-zinv)
      yb_all[(Tanag$z>zbelo)&(Tanag$z<=zabov)]<-( bfabov_all[(Tanag$z>zbelo)&(Tanag$z<=zabov)]*
                                                       (Tanag$z[(Tanag$z>zbelo)&(Tanag$z<=zabov)]-zbelo) +
                                                bfbelo_all[(Tanag$z>zbelo)&(Tanag$z<=zabov)]*
                                                       (zabov-Tanag$z[(Tanag$z>zbelo)&(Tanag$z<=zabov)]) ) /
                                                (zabov-zbelo)
#      yb[VecZ>param_yb[1]]<-param_yb[3]+param_yb[5]*(VecX[VecZ>param_yb[1]]-param_yb[2])+param_yb[6]*(VecZ[VecZ>param_yb[1]]-param_yb[1])
#      yb[VecZ<=param_yb[1]]<-param_yb[3]+param_yb[7]*(VecX[VecZ<=param_yb[1]]-param_yb[2])+param_yb[8]*(VecZ[VecZ<=param_yb[1]]-param_yb[1])
#      yb_all[Tanag$z>param_yb[1]]<-param_yb[3]+param_yb[5]*(Tanag$gb_est[Tanag$z>param_yb[1]]-param_yb[2])+param_yb[6]*(Tanag$z[Tanag$z>param_yb[1]]-param_yb[1])
#      yb_all[Tanag$z<=param_yb[1]]<-param_yb[3]+param_yb[7]*(Tanag$gb_est[Tanag$z<=param_yb[1]]-param_yb[2])+param_yb[8]*(Tanag$z[Tanag$z<=param_yb[1]]-param_yb[1])
    }
#    iii<-order(VecZ)
#    print("Z Yo Yb")
#    print(cbind(VecZ[iii],yo[iii],yb[iii]))
#    iii<-order(as.numeric(as.vector(Tanag$z)))
#    print("Z Yb_all")
#    print(cbind(as.numeric(as.vector(Tanag$z))[iii],yb_all[iii]))
    innov<-matrix(yo-yb,ncol=1,byrow=TRUE)
#    print("innov:")
#    print(innov)
# [] calcola l'analisi
    Wt<-St%*%InvDt
    Kt<-Gt%*%InvDt
    ya<-yb+Wt%*%innov
    yav<-yo+1/(1-diag(Wt))*(ya-yo)
    ya_all<-yb_all+Kt%*%innov
#    yafor<-matrix(m[,9][aux00],ncol=1,byrow=TRUE)
#    yafor1<-matrix(yafor[ii],ncol=1,byrow=TRUE)
#    yavfor<-matrix(m[,10][aux00],ncol=1,byrow=TRUE)
#    yavfor1<-matrix(yavfor[ii],ncol=1,byrow=TRUE)
    auxkk<-(yo-yav)*(yo-ya)/sig2o_T
    kk<-order(auxkk)
    print("IDstaz z yo yb ya yav tau")
    print(cbind(VecS[kk],VecZ[kk],yo[kk],round(yb[kk],3),round(ya[kk],3),round(yav[kk],3),round(auxkk[kk],3)))
# DEBUG: start
#    if (t<10) {
#      postscript(file=paste("analisi_vert_t0",t,"_i",id_rej_count,".ps",sep=""))
#    } else {
#      postscript(file=paste("analisi_vert_t",t,"_i",id_rej_count,".ps",sep=""))
#    }
#    plot(yo,VecZ,col="gray",xlim=c(min(c(yb,yo,ya)),max(c(yb,yo,ya))),ylim=c(0,3500))
#    points(yb,VecZ,col="red")
#    points(ya,VecZ,col="blue")
#    abline(h=seq(0,3500,by=500),lty=2,col="gray")
#    abline(v=seq(-50,50,by=10),lty=2,col="gray")
#    abline(v=seq(-55,55,by=10),lty=3,col="gray")
#    abline(v=0)
#    abline(h=0)
#    dev.off()
# DEBUG: end
# [] DQC CHECK - Spatial Continuity Check
    if (any(auxkk>T2_T)) {
      id_rej_count<-id_rej_count+1
      id_rej_sens[id_rej_count]<-VecSNS[auxkk==max(auxkk)]
      id_rej_staz[id_rej_count]<-VecS[auxkk==max(auxkk)]
      print(paste("@@ Scartato IDsensore IDstazione > ",id_rej_sens[id_rej_count],id_rej_staz[id_rej_count],sep="  "))
#     insert in DQC
      stringa <- paste( "(",   id_rej_sens[id_rej_count],
                        ", '", time[t], 
                        "','", IDtest,
                        "','", Livello_test,
                        "','OIt2mdqc_v04', ",
                       # paste("'",as.character(Sys.Date()),"',NULL)",sep=""),
                        paste("'",as.character(Sys.time()),"',NULL)",sep=""),
                        sep="")
      query_insert <- paste("insert into M_TermometriDQC  values ",stringa," on duplicate key update Result=values(Result), Autore=values(Autore), Data=values(Data)", sep="")
      print("@@@@")
      print(query_insert)
      q_insert <- try(dbGetQuery(conn, query_insert))
      if (inherits(q_insert,"try-error")) {
        quit(status=1)
      }
    } else { 
      print(paste("@@@ Nessuna stazione scartata",sep=""))
      break
    }
#----------------------------------------------------
  }
  if (length_iter<=0) {
    print(" ATTENZIONE!! Poche osservazioni passo all'istante successivo")
    next
  }
}
#
#    DISCONNESSIONE DAL DB
# chiudo db
dbDisconnect(conn)
rm(conn)
dbUnloadDriver(drv)
quit(status=0)
