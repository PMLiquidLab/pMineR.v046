# =============================================================================
#' Some useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.
# =============================================================================
expand.grid.unique <- function(x, y, include.equals=FALSE) {
  x <- unique(x)
  y <- unique(y)
  g <- function(i){
    z <- setdiff(y, x[seq_len(i-include.equals)])
    if(length(z)) cbind(x[i], z, deparse.level=0)
  }
  do.call(rbind, lapply(seq_along(x), g))
}
utils <- function() {
  dectobin <- function(y) {
    # find the binary sequence corresponding to the decimal number 'y'
    stopifnot(length(y) == 1, mode(y) == 'numeric')
    q1 <- (y / 2) %/% 1
    r <- y - q1 * 2
    res = c(r)
    while (q1 >= 1) {
      q2 <- (q1 / 2) %/% 1
      r <- q1 - q2 * 2
      q1 <- q2
      res = c(r, res)
    }
    return(res)
  } 
  is.included<-function( a , b ) {
    if(sum(is.element(a,b)) == length(a)) return(TRUE)
    else return(FALSE)
  }
  cleanUTF <- function( dati , colonna.evento , def.val.to.substitute = 95 ){
    for (riga in 1:nrow(dati)){
      arr <- as.numeric(charToRaw(x = as.character(dati[riga,colonna.evento]) ))
      arr[ which(arr > 127)]<-def.val.to.substitute 
      dati[riga,colonna.evento] <- intToUtf8(arr)
    }
    return(dati)
  }  
  format.data.for.csv<-function(listaProcessi, lista.validi, typeOfRandomDataGenerator="dayAfterDay",output.format.date = "%d/%m/%Y %H:%M:%S") { 
    big.csv<-c()
    ct <- 1
    
    for(i in names(listaProcessi)) {
      numeroElementi<-length(listaProcessi[[i]])
      
      if(typeOfRandomDataGenerator=="dayAfterDay") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=1))
      if(typeOfRandomDataGenerator=="randomDay1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) )
      if(typeOfRandomDataGenerator=="randomWeek1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 7)
      if(typeOfRandomDataGenerator=="randomMonth1-4") giorni.da.sommare <- as.integer(runif(n = numeroElementi,min=1,max=4) * 30)
      
      array.Date <- as.character(format(as.Date("01/01/2000 12:00:00",format=output.format.date) + cumsum(giorni.da.sommare) ,format=output.format.date) )
      matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],array.Date,rep(as.character(lista.validi[ct]),numeroElementi) )
      big.csv<-rbind(big.csv,matrice )
      ct <- ct + 1
    }
    if(!is.null(dim(big.csv))) {
      colnames(big.csv)<-c("patID","event","date","valido")
    }
    return(big.csv)
  }  
  # check if the file is pure ASCII
  # 
  # a simple function able to check if the indicated file is a pure ASCII file
  # the name of the file that need to be checked  
  IsASCII<-function( fileName ) {
    return(c_IsASCII(fileName = fileName))
  }  
  get.IRON.Palette<- function( alpha, n ) {
    arr <- get.HOT_IRON(alpha = alpha )
    # passo <- floor(length(arr)/9)
    passo <- floor(length(arr)/n)
    res <- c()
    for(i in seq(1,length(arr),by = passo) ) {
      res <- c( res , arr[i])
    }
    return(res)
  }
  get.HOT_IRON<-function( alpha ) {
    hotIron <- c(0,0,0,2,0,0,4,0,0,6,0,0,8,0,0,10,0,0,12,0,0,14,0,0,16,0,0,18,0,0,20,0,0,22,0,0,24,0,0,26,0,0,28,0,0,30,0,0,32,0,0,34,0,0,36,0,0,38,0,0,40,0,0,42,0,0,44,0,0,46,0,0,48,0,0,50,0,0,52,0,0,54,0,0,56,0,0,58,0,0,60,0,0,62,0,0,64,0,0,66,0,0,68,0,0,70,0,0,72,0,0,74,0,0,76,0,0,78,0,0,80,0,0,82,0,0,84,0,0,86,0,0,88,0,0,90,0,0,92,0,0,94,0,0,96,0,0,98,0,0,100,0,0,102,0,0,104,0,0,106,0,0,108,0,0,110,0,0,112,0,0,114,0,0,116,0,0,118,0,0,120,0,0,122,0,0,124,0,0,126,0,0,128,0,0,130,0,0,132,0,0,134,0,0,136,0,0,138,0,0,140,0,0,142,0,0,144,0,0,146,0,0,148,0,0,150,0,0,152,0,0,154,0,0,156,0,0,158,0,0,160,0,0,162,0,0,164,0,0,166,0,0,168,0,0,170,0,0,172,0,0,174,0,0,176,0,0,178,0,0,180,0,0,182,0,0,184,0,0,186,0,0,188,0,0,190,0,0,192,0,0,194,0,0,196,0,0,198,0,0,200,0,0,202,0,0,204,0,0,206,0,0,208,0,0,210,0,0,212,0,0,214,0,0,216,0,0,218,0,0,220,0,0,222,0,0,224,0,0,226,0,0,228,0,0,230,0,0,232,0,0,234,0,0,236,0,0,238,0,0,240,0,0,242,0,0,244,0,0,246,0,0,248,0,0,250,0,0,252,0,0,254,0,0,255,0,0,255,2,0,255,4,0,255,6,0,255,8,0,255,10,0,255,12,0,255,14,0,255,16,0,255,18,0,255,20,0,255,22,0,255,24,0,255,26,0,255,28,0,255,30,0,255,32,0,255,34,0,255,36,0,255,38,0,255,40,0,255,42,0,255,44,0,255,46,0,255,48,0,255,50,0,255,52,0,255,54,0,255,56,0,255,58,0,255,60,0,255,62,0,255,64,0,255,66,0,255,68,0,255,70,0,255,72,0,255,74,0,255,76,0,255,78,0,255,80,0,255,82,0,255,84,0,255,86,0,255,88,0,255,90,0,255,92,0,255,94,0,255,96,0,255,98,0,255,100,0,255,102,0,255,104,0,255,106,0,255,108,0,255,110,0,255,112,0,255,114,0,255,116,0,255,118,0,255,120,0,255,122,0,255,124,0,255,126,0,255,128,4,255,130,8,255,132,12,255,134,16,255,136,20,255,138,24,255,140,28,255,142,32,255,144,36,255,146,40,255,148,44,255,150,48,255,152,52,255,154,56,255,156,60,255,158,64,255,160,68,255,162,72,255,164,76,255,166,80,255,168,84,255,170,88,255,172,92,255,174,96,255,176,100,255,178,104,255,180,108,255,182,112,255,184,116,255,186,120,255,188,124,255,190,128,255,192,132,255,194,136,255,196,140,255,198,144,255,200,148,255,202,152,255,204,156,255,206,160,255,208,164,255,210,168,255,212,172,255,214,176,255,216,180,255,218,184,255,220,188,255,222,192,255,224,196,255,226,200,255,228,204,255,230,208,255,232,212,255,234,216,255,236,220,255,238,224,255,240,228,255,242,232,255,244,236,255,246,240,255,248,244,255,250,248,255,252,252,255,255,255)
    hotIron <- hotIron /max(hotIron)
    newHI <- c()
    for( i in seq(1,length(hotIron),by = 3) ) {
      newHI <- c(newHI,rgb(  hotIron[i+1], hotIron[i+2], hotIron[i],alpha = alpha))
    }
    return(newHI)
  }
  return(list(
    "dectobin" = dectobin,
    "is.included" = is.included,
    "format.data.for.csv" = format.data.for.csv,
    "cleanUTF"=cleanUTF,
    "IsASCII"=IsASCII,
    "get.IRON.Palette"=get.IRON.Palette,
    "get.HOT_IRON"=get.HOT_IRON
    
  ))
}
# =============================================================================
#' textObj
#' Una classe ad uso interno per manipolare testi
# =============================================================================
textObj<-function() {
  testo<-'';
  add<-function( stringa, carriage=TRUE) {
    if(length(stringa)>1) stringa<-paste(stringa,collapse='')
    if(carriage==TRUE)
      testo <<- paste( c(testo,'\n',stringa), collapse = ''  ) 
    else
      testo <<- paste( c(testo,stringa), collapse = ''  ) 
  }
  get<-function() {
    return(testo)
  } 
  costructor<-function() {
    testo<<-'';
  }
  return(list("add"=add,"get"=get))
}

#' some data processing useful tools
#' 
#' @description  A class which provide some tools. pMineR intarnal use only.
dataProcessor<-function() {
  #=================================================================================
  # buildMMMatrices.and.other.structures
  # costruisce la MM matrix ed anche altra robaccia
  #=================================================================================    
  buildMMMatrices.and.other.structures<-function(mydata, EVENT.list.names, 
                                                 EVENTName, EVENTDateColumnName=NA, 
                                                 ID.act.group,
                                                 max.char.length.label = 50,
                                                 verbose.mode = TRUE) {
    
    # costruisci la matrice
    MM<-matrix(0, ncol=length(unique(mydata[[EVENT.list.names]]))+2, nrow=length(unique(mydata[[EVENT.list.names]]))+2 )
    colnames(MM)<-c("BEGIN","END",unique(as.character(mydata[[EVENT.list.names]])))
    rownames(MM)<-colnames(MM)
    # browser()
    if(("" %in% trimws(colnames(MM))) == TRUE) {
      return( list("error"=TRUE, "errCode"=1)  )
    }
    
    if(max(nchar(colnames(MM)))>max.char.length.label)  {
      return( list("error"=TRUE, "errCode"=2)  )
    }
    if(length(grep("'", colnames(MM))))  {
      return( list("error"=TRUE, "errCode"=3)  )
    }    
    
    # Creiamo anche la matrice con le density dei tempi di transizione
    # (ma solo se c'e' un campo DATA TIME)
    MM.den.list<-list()
    MM.den.list.high.det<-list()
    lst.aaa <- list()
    
    # ora scorri la storia dei singoli pazienti per estrarre le ricorrenze
    # per ogni paziente
    if( verbose.mode == TRUE ) pb <- txtProgressBar(min = 0, max = length(ID.act.group), style = 3)
    for(patID in seq(1,length(ID.act.group))) {
      if( verbose.mode == TRUE ) setTxtProgressBar(pb, patID)
      # su ogni elemento del percorso clinico
      # t e' il "tempo" in senso di "step"
      for(t in seq(1,nrow(ID.act.group[[patID]]))) {
        # vedi se devi legare il BEGIN
        if( t == 1) {
          valore<-MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]
          MM[ "BEGIN", ID.act.group[[patID]][ t ,EVENT.list.names] ]<-valore+1
        }
        # vedi se devi legare l'END   
        if( t == nrow(ID.act.group[[patID]])) {
          nomeCampo<-ID.act.group[[patID]][t,EVENT.list.names]
          MM[nomeCampo,"END"]<-MM[nomeCampo,"END"]+1
        }

        # tutti gli altri
        if( t < nrow(ID.act.group[[patID]])) {
          nomeCampo.pre<-ID.act.group[[patID]][t,EVENT.list.names]
          nomeCampo.post<-ID.act.group[[patID]][t+1,EVENT.list.names]
          MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
          if(EVENTDateColumnName!='' & ! is.na(EVENTDateColumnName)){
            delta.date<-as.numeric(difftime(as.POSIXct(ID.act.group[[patID]][t+1,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(ID.act.group[[patID]][t,EVENTDateColumnName], format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
            if(length(MM.den.list[[ nomeCampo.pre]])==0) MM.den.list[[ nomeCampo.pre]]<-list()
            if(length(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]])==0) MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c()
            MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]]<-c(MM.den.list[[ nomeCampo.pre]][[ nomeCampo.post ]],delta.date)
          }
        }    
      }
      # invoca il programma in C per estrarre i tempi reciproci fra TUTTI
      iii <- unlist(lapply(ID.act.group[[patID]][,EVENT.list.names] , function(x) which(colnames(MM)==x) ))
      massimo <-max(iii)
      out.MM<-rep( 0 , (massimo)*(massimo) )
      out.delta<-c()
      nuovoOut <- c()
      
      # aaa <- transitionsTime( iii , ID.act.group[[patID]][,"pMineR.deltaDate"], max(iii) );
      
      # -im 
      kkk <- c()
      righe.massime <- nrow(ID.act.group[[patID]])

      tmp.lll <- lapply(1:(righe.massime-1), function(riga){
        kkk <- ID.act.group[[patID]][(riga+1:righe.massime),c(EVENTName,"pMineR.deltaDate")]
        kkk[,c("pMineR.deltaDate")] <- kkk[,c("pMineR.deltaDate")] -   ID.act.group[[patID]][riga,"pMineR.deltaDate"]
        tmp.z <- lapply(unique(kkk[,1]),function(evt.tmp) {
          tempo <- kkk$pMineR.deltaDate[which(kkk[[EVENTName]]==evt.tmp)[1]]
          return(c(evt.tmp,tempo))
        })
        tmp.z <- matrix(unlist(tmp.z),ncol=2,byrow = T )
        tmp.z <- cbind( rep(ID.act.group[[patID]][riga,EVENTName],nrow(tmp.z)), tmp.z )
        return(tmp.z)
      })
      tmp.ll <- do.call(rbind,tmp.lll)
      tmp.ll <- tmp.ll[which(!is.na(tmp.ll[,2])),]

      if( length(tmp.ll)>3) {
        tmptmp <- apply(tmp.ll,MARGIN = 1,function(riga) {
          # browser()
          if(length(MM.den.list.high.det[[ riga[1] ]])==0) MM.den.list.high.det[[ riga[1] ]] <<-list()
          if(length(MM.den.list.high.det[[ riga[1] ]][[ riga[2] ]])==0) MM.den.list.high.det[[ riga[1] ]][[ riga[2] ]] <<- list()
          MM.den.list.high.det[[ riga[1] ]][[ riga[2] ]] <<- c(MM.den.list.high.det[[ riga[1] ]][[ riga[2] ]],as.numeric(riga[3]))
          
        })
        
      }
      # -fm

      # browser()
      # 
      # mm.in <- matrix(c(iii,ID.act.group[[patID]][,"pMineR.deltaDate"]),nrow=2,byrow = T)
      # mm.out <- t(matrix(c(aaa$from,aaa$to,aaa$time),nrow=3,byrow = T))
      # for( riga in seq(1,nrow(mm.out))) {
      #   int.from <-colnames(MM)[mm.out[riga,1]];
      #   int.to <-colnames(MM)[mm.out[riga,2]];
      #   delta.tempo <-mm.out[riga,3];
      #   if(length(MM.den.list.high.det[[ int.from ]])==0) MM.den.list.high.det[[ int.from]]<-list()
      #   if(length(MM.den.list.high.det[[ int.from]][[ int.to ]])==0) MM.den.list.high.det[[ int.from]][[ int.to ]]<-c()
      #   MM.den.list.high.det[[ int.from]][[ int.to ]]<-c(MM.den.list.high.det[[ int.from]][[ int.to ]],delta.tempo)
      # }
    }
    
    for(primo in names(MM.den.list.high.det)) {
      for(secondo in names(MM.den.list.high.det[[primo]])) {
        MM.den.list.high.det[[primo]][[secondo]] <- unlist(MM.den.list.high.det[[primo]][[secondo]])
      }
    }
    # browser()
    if( verbose.mode == TRUE ) close(pb)
    quanti.da.fare<-length(names(MM.den.list)) * length(names(MM.den.list))
    # save(lst.aaa,file = "c://projects/lst.aaa.RData")
    # Calcola la matrice delle medie dei tempi
    # Sarebbe bello avere le density... vabbe'. piu' avanti
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.time<-MM
      MM.mean.time[ 1:nrow(MM.mean.time) , 1:ncol(MM.mean.time)   ]<-Inf
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]])
        }        
      }
    }
    
    # CALCOLO LA MATRICE DEI FLUSSI FUORI DALLO STATO
    
    if(EVENTDateColumnName!='' & !is.na(EVENTDateColumnName)){
      MM.mean.outflow.time<-MM
      MM.mean.outflow.time[ 1:nrow(MM.mean.outflow.time) , 1:ncol(MM.mean.outflow.time)   ]<-NA
      for(state.from in names(MM.den.list))  {
        for(state.to in names(MM.den.list[[state.from]]))  {
          MM.mean.outflow.time[state.from,state.to ]<-mean(MM.den.list[[ state.from]][[ state.to ]][which(MM.den.list[[ state.from]][[ state.to ]] >=0 & state.from != state.to)])
        }
      }
    }
    
    # costruisci una semplice versione, con le parole (come piace tanto a Van der Aalst)
    wordSequence.TMP01<-list();
    for(i in seq(1,length(ID.act.group))) {
      IDPat<-names(  ID.act.group)[i]
      wordSequence.TMP01[[IDPat]]<-ID.act.group[[ IDPat ]][[EVENTName]]
    }    
    return(list( "arrayAssociativo" = rownames(MM),
                 "footPrint"="",
                 "MMatrix"=MM,
                 "MM.mean.time"=MM.mean.time,
                 "MM.density.list"=MM.den.list,
                 "MM.mean.outflow.time"=MM.mean.outflow.time,
                 "MM.den.list.high.det" = MM.den.list.high.det,
                 "pat.process"=ID.act.group,
                 "wordSequence.raw"=wordSequence.TMP01,
                 "error"=FALSE) )    
  }  
  
  #=================================================================================
  # createSequenceMatrix
  # crea una matrice di transizione a partire da una mera sequenza di eventi.
  # Creata per poter evitare di dover usare il pacchetto markovChain
  #=================================================================================      
  createSequenceMatrix<-function( sequence2parse ) {
    
    sequenza.simboli <- unique(as.character(sequence2parse))
    MM<-matrix(0, ncol=length(sequenza.simboli), nrow=length(sequenza.simboli) )  
    colnames(MM)<-sequenza.simboli
    rownames(MM)<-sequenza.simboli
    
    # cicla su ogni elemento della sequenza ed incrementa la relativa posizione nella 
    # matrice di transizione from=>to
    for(t in seq(1,length(sequence2parse)-1)) {
      # tutti gli altri
      nomeCampo.pre<-sequence2parse[t]
      nomeCampo.post<-sequence2parse[t+1]
      MM[ nomeCampo.pre, nomeCampo.post ]<-MM[ nomeCampo.pre, nomeCampo.post ]+1
    }
    return(list(
      "transitionCountMatrix" = MM
    ))
  }
  
  return(list(
    "buildMMMatrices.and.other.structures"=buildMMMatrices.and.other.structures,
    "createSequenceMatrix" = createSequenceMatrix
  ))
}

#' calcola EFT
#' 
#' @description  Funzione per calcolare la EFM
#' @export
calcolaEnhancedFootPrintTable.pat.process <- function( dataLoaderOBJ , skip.less.than = 0, threshold.perc = 0.00000001 ) {

  EFPT.neverAfter<-dataLoaderOBJ$MMatrix;  EFPT.neverAfter[,]<-"!>>"
  EFPT.alwaysAfter<-dataLoaderOBJ$MMatrix;  EFPT.alwaysAfter[,]<-">>"
  # EFPT.neverBefore<-dataLoaderOBJ$MMatrix;  EFPT.neverBefore[,]<-"!<"
  eventi.possibili <- colnames(dataLoaderOBJ$MMatrix)
  
  for( ID in names(dataLoaderOBJ$pat.process) ) {
    sequenza <- c("BEGIN",dataLoaderOBJ$pat.process[[ID]][,dataLoaderOBJ$csv.EVENTName],"END")
    # browser()
    for( ct in seq(1,length(sequenza)) ) {
      if(ct < (length(sequenza)) ){
        # if(sequenza[ ct ]=="Death") browser()
        EFPT.neverAfter[sequenza[ ct ] ,  sequenza[ (ct+1):length(sequenza)] ] <- "";
        mancanti <- eventi.possibili[ !(eventi.possibili %in% sequenza[ (ct+1):length(sequenza)]) ]
        EFPT.alwaysAfter[sequenza[ ct ] ,  mancanti ] <- "";
      }
      # if(ct >= 2){
      #   EFPT.neverBefore[sequenza[ ct ] ,  sequenza[ 1:(ct-1)] ] <- "";
      # }
    }
  }
  # EFPT.neverBefore[ "BEGIN" ,  ] <- "!<";
  EFPT.alwaysAfter[ "END" ,  ] <- "";
  return( list( "EFPT.hasNeverAfter" = EFPT.neverAfter,
                "EFPT.hasAlwaysAfter" = EFPT.alwaysAfter
                # "EFPT.neverBefore" = EFPT.neverBefore
                )
          )
}
  
#' calcola FT
#' 
#' @description  Funzione per calcolare la FM
#' @export
calcolaFootPrintTable.pat.process <- function( dataLoaderOBJ , skip.less.than = 0, threshold.perc = 0.00000001 ) {
  
  # rbind.data.frame <- do.call(rbind.data.frame, dataLoaderOBJ$pat.process )

  FPT<-dataLoaderOBJ$MMatrix;  FPT[,]<-"#"
  FPT.numbers.R<-dataLoaderOBJ$MMatrix;  FPT.numbers.R[,]<-0
  
  for( ID in names(dataLoaderOBJ$pat.process) ) {
    sequenza <- c("BEGIN",dataLoaderOBJ$pat.process[[ID]][,dataLoaderOBJ$csv.EVENTName],"END")
    # browser()
    for( ct in seq(1,length(sequenza)-1) ) {
      num <- FPT.numbers.R[ sequenza[ ct ] , sequenza[ (ct+1) ] ]
      FPT.numbers.R[ sequenza[ ct ] , sequenza[ (ct+1) ] ] <- num + 1
    }
  }
  
  FPT.numbers.R[  which(FPT.numbers.R < skip.less.than,arr.ind = T) ] <- 0
  
  ooo <- FPT.numbers.R
  for( riga in seq(1,nrow(ooo)) ) {
    for( colonna in seq(1,ncol(ooo)) ) {
      ooo[ riga, colonna ] <- abs(FPT.numbers.R[ riga, colonna ] )/abs(FPT.numbers.R[ riga, colonna ]+FPT.numbers.R[ colonna, riga ] )
    }
  }
  FPT.numbers.R.perc <- ooo
  FPT.numbers.R.perc[ which(is.nan(FPT.numbers.R.perc),arr.ind = T) ] <- 0
  
  
  for( riga in rownames(FPT.numbers.R.perc) ) {
    for( colonna in colnames(FPT.numbers.R.perc) ) {
      if( FPT.numbers.R.perc[ riga, colonna ] >= threshold.perc & 
          FPT.numbers.R.perc[ colonna, riga ] <= threshold.perc ) {
        FPT[ riga, colonna ] <- "->"
        FPT[ colonna, riga ] <- "<-"
      }
      if( FPT.numbers.R.perc[ riga, colonna ] >= threshold.perc & 
          FPT.numbers.R.perc[ colonna, riga ] <= threshold.perc ) {
        FPT[ riga, colonna ] <- "->"
        FPT[ colonna, riga ] <- "<-"
      }
      if( FPT.numbers.R.perc[ riga, colonna ] >= threshold.perc & 
          FPT.numbers.R.perc[ colonna, riga ] >= threshold.perc ) {
        FPT[ riga, colonna ] <- "||"
        FPT[ colonna, riga ] <- "||"
      }
      
    }
  }
  return( list("FPT"=FPT, "FPT.numbers.R"=FPT.numbers.R, "FPT.numbers.R.perc"=FPT.numbers.R.perc))
}

# -----------------------------------------------------------------------
# funzione plotPatientReplayedTimelineFunction
# -----------------------------------------------------------------------
#' Some useful tools new version
#' 
#' @description  A class which provide some tools. pMineR intarnal use only. wow
plotPatientReplayedTimelineFunction<-function( list.computation.matrix , patientID,
                                               text.cex=.7, y.intra.gap = 40, x.offset = 100,
                                               thickness=5 , 
                                               bar.border = "Navy",bar.volume = "lightsteelblue1",
                                               text.date.cex =.6) {
  
  date.notevoli <-c()
  durate.notevoli <- c()
  matrice <- list.computation.matrix$list.computation.matrix$stati.timeline[[patientID]]
  tempo.max <- max(  as.numeric(matrice[,4])  )
  numero.stati <- length(unique(matrice[,1]))
  arr.stati <- c()
  for( tmp in 1:length(matrice[,1])) {
    if( !(matrice[tmp,1] %in%arr.stati)) { arr.stati <- c(arr.stati,matrice[tmp,1]) }
  }
  # browser()
  par(mar=c(2,0,2,0)+0)
  plot( x=c(), y=c(), 
        xlim = c(0,tempo.max + x.offset+ 15) , 
        ylim=c(0,(numero.stati+1)*y.intra.gap ), 
        bty='n',axes = FALSE, xlab='', ylab='' )
  
  lista.boxes<- list()
  lista.points<- list()
  lista.date<-list()
  
  for( index in seq(1,length(arr.stati) )) {
    ypos.line <- (numero.stati+1)*y.intra.gap - index * y.intra.gap
    stato <- arr.stati[ index ]
    # text(x = 0,y = ypos.line,labels = stato, cex = text.cex, pos = 4)
    
    # lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset,tempo.max+x.offset), "y"=c( ypos.line,ypos.line ))
    
    sub.matrice <- matrice[ which(matrice[ ,1]==stato )  ,]
    numero.righe.sub.matrice <- length(sub.matrice)/4
    # Se e' almeno una matrice (se ho almeno due rilevazioni)
    if(numero.righe.sub.matrice>1) {
      l.from <- NA
      l.to <- NA
      for( i in seq(1,numero.righe.sub.matrice )) {
        if(sub.matrice[i,2]=="begin") { 
          l.from <- as.numeric(sub.matrice[i,4]) 
          durate.notevoli <- c(durate.notevoli, l.from )
          date.notevoli <- c(date.notevoli, sub.matrice[i,3] )
          lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.from ,x.offset + l.from), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[i,3],"label.durata"=sub.matrice[i,4])          
        }
        if(sub.matrice[i,2]=="end") {
          l.to <- as.numeric(sub.matrice[i,4] )
          lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.to ,x.offset + l.to), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[i,3],"label.durata"=sub.matrice[i,4])          
          lista.boxes[[length(lista.boxes)+1]]<-list( "x"=c( l.from ,l.to, l.to, l.from, l.from ) + x.offset, "y"=c( -thickness, -thickness, thickness, thickness , -thickness)+ypos.line )
          durate.notevoli <- c(durate.notevoli, l.to )
          date.notevoli <- c(date.notevoli, sub.matrice[i,3]  )
        }
      }
    }
    # Se c'e' solo una riga!
    if(numero.righe.sub.matrice==1) {
      l.pos <- as.numeric(sub.matrice[4] )
      durate.notevoli <- c(durate.notevoli, l.pos )
      date.notevoli <- c(date.notevoli, sub.matrice[3]  )   
      lista.date[[length(lista.date)+1]] <- list("x"=c(x.offset + l.pos ,x.offset + l.pos), "y"=c( -5, (numero.stati+1)*y.intra.gap +5),"label.data"=sub.matrice[3], "label.durata"=sub.matrice[4])
      
      # Se e' un END 
      if(sub.matrice[2]=="end" |  as.numeric(sub.matrice[4])==tempo.max ) {
        lista.points[[length(lista.points)+1]]<-list("x"=l.pos + x.offset,"y"=ypos.line)
      }
      # Se e' un BEGIN
      if(sub.matrice[2]=="begin" & as.numeric(sub.matrice[4])!=tempo.max) {
        l.from <- l.pos
        l.to <- tempo.max
        lista.boxes[[length(lista.boxes)+1]]<-list( "x"=c( l.from ,l.to, l.to, l.from, l.from ) + x.offset, "y"=c( -thickness, -thickness, thickness, thickness , -thickness)+ypos.line )
      }
    }    
    
  }
  
  # plotta le verticali delle date
  number <- 1
  old.x <- c()
  for(i in seq(1, length(lista.date))) {
    if(! (lista.date[[i]]$x[1]  %in% old.x) ) {
      number <- number + 1 
      # points(x =lista.date[[i]]$x, y = lista.date[[i]]$y , type='l', col="grey", lty = 4 )
      points(x =lista.date[[i]]$x, y = lista.date[[i]]$y -15, type='l', col="grey", lty = 4 )
      text(x = lista.date[[i]]$x , y = lista.date[[i]]$y[1] + (number * 10)-5, labels = str_replace_all(string = lista.date[[i]]$label.data,pattern = " ",replacement = "\n"), cex = text.date.cex, col='black')
      text(x = lista.date[[i]]$x , y = (numero.stati+1)*y.intra.gap + (number * 10) -25, labels = as.integer(as.numeric(lista.date[[i]]$label.durata)), cex = text.date.cex, col='black')
      if(number >= 3) number <- 0
      old.x <- c(old.x, lista.date[[i]]$x[1] )
    }
  }
  # plotta gli assi degli stati
  for( index in seq(1,length(arr.stati) )) {
    points( x = c(x.offset,x.offset+tempo.max), 
            y = c( (numero.stati+1)*y.intra.gap - index * y.intra.gap, (numero.stati+1)*y.intra.gap - index * y.intra.gap),
            type='l' , col= "grey")     
  }
  # plotta i GANTT
  for(i in seq(1, length(lista.points))) {
    points( x = lista.points[[i]]$x, 
            y = lista.points[[i]]$y,
            pch=13 , col= bar.border)     
    # points(x =lista.date[[i]]$x, y = lista.date[[i]]$y , type='l', col="grey", lty = 4 )
  }  
  for(i in seq(1, length(lista.boxes))) {
    points( x = lista.boxes[[i]]$x, 
            y = lista.boxes[[i]]$y,
            type='l' , col= bar.border)
    polygon( x = lista.boxes[[i]]$x, 
             y = lista.boxes[[i]]$y,
             col= bar.volume) 
  }    
  
  for( index in seq(1,length(arr.stati) )) { 
    ypos.line <- (numero.stati+1)*y.intra.gap - index * y.intra.gap
    stato <- arr.stati[ index ]
    text(x = 0,y = ypos.line,labels = stato, cex = text.cex, pos = 4)
  }
  # list.computation.matrix
} 


#' @import stringr
#' @export
dateTimeWizard <- function( ) {
  
  guess_date_format <- function( arr.strings, theBest = TRUE ) {
    res <- lapply(arr.strings, function(stringa) {
      return(single.guess_date_format(stringa))
    })
    
    tabella <- table(unlist(res))
    tabella[order(tabella,decreasing = T)]
    if( theBest == TRUE) return( names(tabella)[1])
    return(tabella)
  }
  single.guess_date_format <- function( stringa ) {
    separatore <- unique(unlist(str_extract_all(string = stringa,pattern = "[^0-9]")))
    if( length( separatore ) != 1 ) return( NA )
    arr.elementi <- unlist(str_split(string = stringa,pattern = separatore))
    if( length( arr.elementi ) !=3 ) return( NA )
    
    if( as.numeric(arr.elementi[1]) > 31 ) {
      primo <- c("%Y") } else {
        if( as.numeric(arr.elementi[1]) > 12 )  {
          primo <- c("%Y","%d")
        } else {
          primo <- c("%Y","%m","%d")
        }
      }
    
    if( as.numeric(arr.elementi[2]) > 31 ) {
      secondo <- c("%Y") } else {
        if( as.numeric(arr.elementi[2]) > 12 )  {
          secondo <- c("%Y","%d")
        } else {
          secondo <- c("%Y","%m","%d")
        }
      }
    
    if( as.numeric(arr.elementi[3]) > 31 ) {
      terzo <- c("%Y") } else {
        if( as.numeric(arr.elementi[3]) > 12 )  {
          terzo <- c("%Y","%d")
        } else {
          terzo <- c("%Y","%m","%d")
        }
      }
    
    mtr.opzioni <- expand.grid( primo, secondo, terzo )
    
    mtr.opzioni[,1] <- as.character(mtr.opzioni[,1])
    mtr.opzioni[,2] <- as.character(mtr.opzioni[,2]) 
    mtr.opzioni[,3] <- as.character(mtr.opzioni[,3]) 
    
    new.mtr <- c()
    tmp <- lapply(1:nrow(mtr.opzioni),function(i) {
      if(length(unique(unlist(mtr.opzioni[i,]))) == 3) {
        new.mtr <<- rbind(new.mtr,mtr.opzioni[i,])
      }
    })
    
    arr.opzioni <- unlist(lapply(1:nrow(new.mtr),function(i) { 
      return( paste(new.mtr[i,],collapse = separatore)  )
    }))
    return(arr.opzioni)
  }
  guess_datetime_format <- function( stringa ) {
    
  }
  guess_time_format <- function( arr.strings, theBest = TRUE ) {
    # browser()
    res <- lapply(arr.strings, function(stringa) {
      return(single.guess_time_format(stringa))
    })
    
    tabella <- table(unlist(res))
    if( length(tabella) == 0 ) return()
    tabella[order(tabella,decreasing = T)]
    if( theBest == TRUE) return( names(tabella)[1])
    return(tabella)
    
  }
  
  single.guess_time_format <- function( stringa ) {
    if(is.na(stringa)) return(stringa)
    separatore <- unique(unlist(str_extract_all(string = stringa,pattern = "[^0-9]")))
    res <- paste( c("%H","%m","%s"), collapse = separatore)
    return( res )
  }
  
  guess_datetime_format <- function( arr.string ) {
    tmp.1 <- unlist(lapply( arr.string, function(x){ return(substr(x,1,10))}))
    tmp.2 <- unlist(lapply( arr.string, function(x){ return(substr(x,12,str_length(x)))}))
    # browser()
    str.data <- guess_date_format(arr.strings = tmp.1)
    str.time <- guess_time_format(arr.strings = tmp.2)
    final.proposal <- str_trim(paste(c( str.data," ",str.time),collapse = ''))
    return( final.proposal )
  }
  
  return( list(
    "guess_date_format"=guess_date_format,
    "single.guess_date_format"=single.guess_date_format,
    "guess_datetime_format"=guess_datetime_format,
    "guess_time_format"=guess_time_format,
    "single.guess_time_format"=single.guess_time_format
  ))
}


#' A function to plot nice timeline
#'
#' @description  wow
#' @export
plotCohortTimeline <- function(  objDL.obj, lst.to.join=list() , lst.spike=list() , UM="days", y.grid = NA,
                                   ordered = FALSE, decreasing = TRUE , y.grid.col = "lightgrey", x.grid.col = "lightgrey") {
  
  
  # lst.to.join <- list( "Covid" = list( "from" ="Covid_BEGIN","to" = "Covid_END", "col"="red"), 
  #                      "Rehabilitation" = list( "from" = "Rehabilitation_BEGIN" , "to" = "Rehabilitation_END", col = "green"), 
  #                      "SubIntensive" = list( "from" = "SubIntensive_BEGIN" , "to" = "SubIntensive_END", col = "orange") 
  # )
  # lst.spike <- list( "Tested_Positive"=list( "col" = "blue", "lwd" = 2, "pch"= 8),
  #                    "Discharge"=list( "col" = "blue", "lwd" = 1, "pch"= 25)
  # )
  # plot.cohort.timeline(objDL.obj = objDL.new.export,lst.to.join = lst.to.join, lst.spike = lst.spike,y.grid = 90,
  #                      ordered = TRUE,decreasing = TRUE)
  
  
  
  objDL.new.export <- objDL.obj
  y.step.each <- y.grid
  
  if( UM == "mins") conversion <- 1
  if( UM == "hours") conversion <- 60
  if( UM == "days") conversion <- 24*60
  if( UM == "weeks") conversion <- 24*60*7
  if( UM == "months") conversion <- 24*60*7*4
  
  
  minThickness <- 10
  internalMargin <- 2
  numberOfPatient <- length(objDL.new.export$pat.process)
  biggestTime <- max(do.call(rbind,objDL.new.export$pat.process)$pMineR.deltaDate)/conversion
  if(is.na(y.step.each)) y.step.each <- as.integer(biggestTime/10)
  
  max.time.per.patient <- unlist(lapply(names(objDL.new.export$pat.process), function(ID) { max(objDL.new.export$pat.process[[ID]]$pMineR.deltaDate) } ))
  
  if(ordered == TRUE) {
    ordered.patients <- order(max.time.per.patient,decreasing = !decreasing)
  } else { 
    ordered.patients <- 1:length(objDL.new.export$pat.process)
  }
  
  arr.posizioni <- seq( 0, (numberOfPatient+1)* minThickness, by= ((numberOfPatient+1)* minThickness) / numberOfPatient)
  arr.posizioni <- arr.posizioni[1:(length(arr.posizioni)-1)] + (minThickness/2)
  
  plot(0,0,xlim=c(0,biggestTime),ylim=c(0,(numberOfPatient+1)* minThickness ), axes=FALSE,xlab=UM, ylab="" , col="white")
  abline( v = seq(0,biggestTime,by = y.step.each) ,lty = 2, col=y.grid.col )
  
  tmp.arr.from <- unlist(lapply(names(lst.to.join),function(i) { lst.to.join[[i]]$from } ))
  tmp.arr.to <- unlist(lapply(names(lst.to.join),function(i) { lst.to.join[[i]]$to } ))
  tmp.arr.col <- unlist(lapply(names(lst.to.join),function(i) { lst.to.join[[i]]$col } ))
  arr.y.to.put.labels <- c()
  cursore <- 1
  tmp.1 <- lapply( ordered.patients, function( no.patient ) {
    y.start <- (cursore * minThickness)+internalMargin ; y.stop <- ((cursore+1)*minThickness)-internalMargin
    y.median <- (y.start + (y.stop-y.start)/2) 
    arr.y.to.put.labels <<- c( arr.y.to.put.labels , y.median )
    abline( h = y.median ,lty = 2, col=x.grid.col)
    arr.righe.2.skip <- c()
    tmp.2 <- lapply( 1:nrow(objDL.new.export$pat.process[[no.patient]]), function(riga) {

      if( !riga %in% arr.righe.2.skip) {
        arr.tutto <- objDL.new.export$pat.process[[no.patient]][[ objDL.new.export$csv.EVENTName ]]
        current.event <- objDL.new.export$pat.process[[no.patient]][[ objDL.new.export$csv.EVENTName ]][riga]
        # browser()
        quale <- which( tmp.arr.from == current.event )
        if( length(quale)>0  ){
          da.trovare <- tmp.arr.to[quale]
          quale.to <- which(arr.tutto[(riga+1):length(arr.tutto)] == da.trovare)[1]
          quale.to <- quale.to + riga
          arr.righe.2.skip <- c( arr.righe.2.skip , quale.to )

          x.from <- (objDL.new.export$pat.process[[no.patient]]$pMineR.deltaDate[riga]/conversion)
          x.to <- (objDL.new.export$pat.process[[no.patient]]$pMineR.deltaDate[quale.to]/conversion)
          
          polygon( c(x.from , x.to, x.to , x.from , x.from  ),
                   c(y.start  , y.start ,y.stop , y.stop , y.start), lwd=2, 
                   col=tmp.arr.col[quale], border = NA)
          
        } else {
          if( current.event %in% names(lst.spike)) {
            
            points( (objDL.new.export$pat.process[[no.patient]]$pMineR.deltaDate[riga]/conversion), (y.start + (y.stop - y.start)/2),
                    col = lst.spike[[ current.event ]]$col, 
                    pch = lst.spike[[ current.event ]]$pch,
                    lwd = lst.spike[[ current.event ]]$lwd,
            )    
          }
        }      
      }
    } )
    cursore <<- cursore + 1
  })
  
  axis(2, arr.y.to.put.labels, labels=names(objDL.new.export$pat.process),las=2)
  axis(1, seq(0,biggestTime,by = y.step.each), labels=seq(0,biggestTime,by = y.step.each),las=1)
}
# plotTimeline <- function(  objDL.obj, arr.ID = c(), max.time = Inf , UM = "days", arr.events = c(), 
#                            arr.evt.pch = c(), evt.pch.default.value = 3,
#                            ID.on.y.label  = TRUE, y.label.cex = 0.7,
#                            Time.on.x.label = TRUE, x.label.cex = 0.7,
#                            plot.legend = TRUE, legend.Pos = "topright", legend.cex = 0.7,
#                            ID.ordering = TRUE, ID.ordering.desc = TRUE
# ) {
#   
#   max.time.window <- max.time
#   if( UM == "hours" ) { max.time.window <- max.time.window * 60 }
#   if( UM == "days" ) { max.time.window <- max.time.window * 60*24  }
#   if( UM == "weeks" ) { max.time.window <- max.time.window *  60*24*7  }
#   if( UM == "months" ) { max.time.window <- max.time.window * 60*24*30  }
#   
#   arr.ID <- sample(names(objDL.out$pat.process),size = 30)
#   evtName <- objDL.out$csv.EVENTName
#   
#   bigM <- do.call(rbind,objDL.out$pat.process[arr.ID])
#   time.range <- range(bigM$pMineR.deltaDate)
#   maxTime <- min(max(bigM$pMineR.deltaDate),max.time.window)
#   max.x <- maxTime
#   
#   
#   if( ID.ordering == TRUE) {
#     arr.ID <- arr.ID[order(unlist(lapply(arr.ID, function(ID)  {  max(objDL.out$pat.process[[ID]]$pMineR.deltaDate)   } )),decreasing = ID.ordering.desc)]  
#   }
#   
#   if( length(arr.ID) == 0 ) arr.ID <- names(objDL.out$pat.process)
#   x.offset <- 0; y.offset <- 0
#   if(ID.on.y.label==TRUE) { x.offset <- 0.1 }
#   if(Time.on.x.label==TRUE) { y.offset <- 0.1 }
#   
#   
#   n.patients <- length(arr.ID)
#   if( length(arr.events) == 0 ) { arr.events <- unlist(unique(bigM[ evtName ])) }
#   arr.col <- rainbow(n = length(arr.events)); names(arr.col) <- arr.events
#   # browser()
#   # set the array of pch
#   add.arr.evt.pch <- c()
#   tmp.n <- unlist(lapply( arr.events, function(i) {   if( !(i %in% names(arr.evt.pch)) ) return(i) }))
#   tmp.v <- rep(evt.pch.default.value,length(tmp.n)); names(tmp.v) <- tmp.n
#   arr.evt.pch <- c(arr.evt.pch ,tmp.v )
#   
#   minThickness <- 5
#   
#   frameIt <- function( x , y ) {
#     abs.x.offset <- max.x * x.offset;  
#     m <- (max.x - abs.x.offset) / max.x
#     x <- m * x + abs.x.offset
#     return( c(x,y) )
#   }
#   
#   par(mar=c(1,1,1,1))
#   plot( 0 , 0 , xlim = c( 0 , max.x ) , ylim = c( 0, (n.patients+1) * minThickness ), axes = FALSE, xlab = UM , ylab="" , col="white")
#   if( Time.on.x.label == TRUE ) {
#     xy.1 <- frameIt( 0, 0 ); xy.2 <- frameIt( maxTime, 0 )
#     points( x = c(xy.1[1],xy.2[1]), y = c(xy.1[2],xy.1[2]), col="black", type='l')
#     if( UM == "mins" ) { x.sequenza <- seq( 0 , maxTime ) }
#     if( UM == "hours" ) { x.sequenza <- seq( 0 , maxTime , by = 60 ) }
#     if( UM == "days" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24 ) }
#     if( UM == "weeks" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24*7 ) }
#     if( UM == "months" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24*30 ) }
#     x.sequenza <- x.sequenza[1:(length(x.sequenza)-1)]
#     # browser()
#     for( i in 1:length(x.sequenza) ) {
#       xy <- frameIt( x.sequenza[i], 0 );
#       points( x = xy[1], y = xy[2], col="black", pch=3)
#       label <- x.sequenza[i]
#       if( UM == "hours" ) { label <- label/60  }
#       if( UM == "days" ) { label <- label/ (60*24)  }
#       if( UM == "weeks" ) { label <- label/ (60*24*7)  }
#       if( UM == "months" ) { label <- label/ (60*24*30)  }
#       
#       text(xy[1],xy[2],label,pos = 1 ,cex = x.label.cex )
#     }
#   }
#   
#   tmp <- lapply(1:length( arr.ID ),function( riga ) {
#     ID <- arr.ID[riga]
#     subMM <- objDL.out$pat.process[[ID]]
#     maxTime <- max(subMM$pMineR.deltaDate)
#     
#     xy.1 <- frameIt( 0, (riga*minThickness) ); xy.2 <- frameIt( maxTime, (riga*minThickness) )
#     points( x = c(xy.1[1],xy.2[1]), y = c(xy.1[2],xy.1[2]), col="grey", type='l')
#     
#     if( ID.on.y.label == TRUE ) { 
#       text( (max.x / 100 ), (riga*minThickness) , ID, cex = y.label.cex  )
#     }
#     
#     for( linea in 1:nrow( subMM )) {
#       if( subMM[[ evtName ]][linea] %in%  arr.events ) {
#         pch.val <- arr.evt.pch[subMM[[ evtName ]][linea]]
#         if( grepl("[0-9]+",pch.val) ) pch.val <- as.numeric(pch.val)
#         xy <- frameIt( subMM$pMineR.deltaDate[linea]  ,  (riga*minThickness) )
#         points( x = xy[1] ,  y = xy[2] , pch = pch.val, lwd=1, col=arr.col[subMM[[ evtName ]][linea]]  )   
#       }
#     }
#   })
#   if( plot.legend == TRUE) {
#     legend( legend.Pos, legend = arr.events, col =  arr.col, lty=1, lwd=2 , cex = legend.cex)
#   }
#   
# }