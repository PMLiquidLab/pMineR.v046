#' A class to train First Order Markov Models
#'
#' @description  This is an implementation of the First Order Markov Model (FOMM) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'                \itemize{
#'                \item \code{FOMM( ) } is the costructor
#'                \item \code{loadDataset( ) } loads data taken from a dataLoader::getData() method, into a FOMM object
#'                \item \code{trainModel( ) } train a model using the previously loaded dataset
#'                \item \code{replay( ) } re-play a given event log on the internal FOMM model
#'                \item \code{play( ) } play the internal FOMM model a desired number of times, in order to simulate new event-logs. This methods can also, if desired, simulate event-logs which does not complies with the internal FOMM model.
#'                \item \code{plot( ) } plot the internal model
#'                \item \code{KaplanMeier( ) } build a Kaplan Meier curve through an indicated pathway
#'                \item \code{distanceFrom( ) } calculate the scalar distance to another passed FOMM model, passed as argument. The default metric returns a scalar value
#'                \item \code{getModel( ) } return the trained internal FOMM model
#'                \item \code{getInstanceClass( ) } return the instance class Name and description (version, etc.)
#'                \item \code{plot.delta.graph( ) } plot a graph, in the desired modality, representing the difference between the internal FOMM and a passed one.
#'                \item \code{build.PWF( ) } build automatically a PWF XML definition script.
#'                \item \code{findReacheableNodes( ) } and return the array containing the reacheable states, starting from the passed one.
#'                }
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'
#'              The consturctor admit the following parameters:
#' parameters.list a list containing possible parameters to tune the model.
#' @param parameters.list a list containing the parameters. The possible ones are: 'considerAutoLoop' and 'threshold'. 'considerAutoLoop' is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @import progress DiagrammeR
#' @import survival
#' @export
FOMM<-function( parameters.list = list(),verbose.mode=F ) {
  MMatrix<-''
  footPrint<-''
  model.grViz<-'';
  is.dataLoaded<-FALSE
  parameters<-list()
  MMatrix.perc<-NA
  MMatrix.perc.noLoop<-NA
  MMatrix.mean.time<-NA
  MMatrix.density.list<-NA
  MM.den.list.high.det <- NA
  MM.pat.process<-NA
  MM.csv.parameters <- NA
  istanceClass<-list()
  obj.log<-NA
  global.personal.ID<-NA
  param.verbose<-''


  # ***************************************************************************************************
  # WRAPPING METHODS
  # ***************************************************************************************************
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {

    transMatrix<-dataList$MMatrix
    MMatrix<<-transMatrix

    # calcola la matrice delle percentuali e quella delle percentuali senza i loop
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} }
    MMatrix.perc<<-MM

    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} }
    MMatrix.perc.noLoop<<-MM

    MMatrix.mean.time<<-dataList$MM.mean.time
    MMatrix.density.list<<-dataList$MM.density.list
    MM.den.list.high.det<<- dataList$MM.den.list.high.det

    MM.pat.process  <<- dataList$pat.process

    MM.csv.parameters<<-list()
    MM.csv.parameters$csv.column.names <<- dataList$csv.column.names
    MM.csv.parameters$csv.IDName <<- dataList$csv.IDName
    MM.csv.parameters$csv.EVENTName <<- dataList$csv.EVENTName
    MM.csv.parameters$csv.dateColumnName <<- dataList$csv.dateColumnName
    MM.csv.parameters$csv.date.format <<- dataList$csv.date.format
    MM.csv.parameters$csv.max.pMineR.internal.ID.Evt <<- dataList$csv.max.pMineR.internal.ID.Evt


    # dichiara che i dati sono stati caricati
    is.dataLoaded<<-TRUE
  }
  #===========================================================
  # play
  #===========================================================
  play<-function(numberOfPlays = 1, min.num.of.valid.words = NA,
                 toReturn="csv") {
    obj.utils <- utils()
    res<-list()
    tempo<-list()
    data.ora<-list()
    # browser()
    for(i in seq(1,numberOfPlays)) {
      chiave <- as.character(i)
      res[[chiave]]<-play.Single()

      # costruisci le parole
      tempo[[chiave]]<-c(0)
      data.ora.tm2 <- as.POSIXct("01/01/2000 00:00:01", format = "%d/%m/%Y %H:%M:%S")
      data.ora[[chiave]] <- c( as.character(data.ora.tm2)  )

      if(length(res[[chiave]])>=2) {
        for( contatore in seq(1,length(res[[chiave]])-1 ) ) {
          ppp <- MM.den.list.high.det[[ res[[chiave]][contatore] ]][[ res[[chiave]][contatore+1] ]]

          if( length(ppp)==1)  {
            deltaMinuti <- ppp
            tempo[[chiave]]  <- c(tempo[[chiave]],deltaMinuti)
            data.ora.tm2 <- data.ora.tm2 + 60 * deltaMinuti
            if(is.na(data.ora.tm2)) browser()
            data.ora[[chiave]] <- c(data.ora[[chiave]],as.character(data.ora.tm2))
          }
          else {
            min.cum.sum = cumsum(density(ppp)$y)
            # Normalizza a 1
            min.cum.sum <- min.cum.sum / max(min.cum.sum)
            dado.lanciato <- runif(n = 1,min = min(min.cum.sum+0.001),max = .95)

            deltaMinuti <- max(density(ppp)$x[( min.cum.sum<dado.lanciato )])
            deltaMinuti <- max(0,deltaMinuti)

            tempo[[chiave]] <- c(tempo[[chiave]],deltaMinuti)
            data.ora.tm2 <- data.ora.tm2 + 60 * deltaMinuti
            if(is.na(data.ora.tm2)) browser()
            data.ora[[chiave]] <- c(data.ora[[chiave]],as.character(data.ora.tm2))
          }
        }
      }
    }

    # Se devi generare alcune sequenze invalida, provvedi
    arr.quanti.invalidi <- c()
    if(!is.na(min.num.of.valid.words)) {

      sequenze.da.invalidare <- numberOfPlays - min.num.of.valid.words
      if(sequenze.da.invalidare>0) {

        sottomatrice <- MMatrix[ !(colnames(MMatrix) %in% c("BEGIN","END")), !(rownames(MMatrix) %in% c("BEGIN","END"))  ]
        posizione.zeri <- which(sottomatrice==0,arr.ind = TRUE)

        for( i in seq(1,sequenze.da.invalidare))  {
          if( length(res[[i]])>1 ) {
            dado.innesto <- as.integer(runif(1,min=1,max =length(res[[i]])))
            dato.righe.matrice <- as.integer(runif(1,min=1,max = nrow(posizione.zeri)))
            res[[i]][dado.innesto] <- rownames(sottomatrice)[posizione.zeri[ dato.righe.matrice,1 ]]
            res[[i]][dado.innesto+1] <- colnames(sottomatrice)[ posizione.zeri[ dato.righe.matrice,2 ]]
          }
          arr.quanti.invalidi<-c(arr.quanti.invalidi,rep(FALSE,length(res[[i]])))
        }
      }
    }
    if(length(res) == 0 ) { cat("\n WARNING: no sequences generated...."); return(); }
    res <- local.format.data.for.csv(listaProcessi = res,
                                     lista.validi = rep(TRUE,numberOfPlays),
                                     data.ora = data.ora)

    if(length(arr.quanti.invalidi)>=0 & !is.null(arr.quanti.invalidi)) res[,"valido"][1:length(arr.quanti.invalidi)]<-arr.quanti.invalidi


    if(!is.null(dim(res))) res<-as.data.frame(res)
    if(toReturn=="csv") { daRestituire <- res  }
    if(toReturn=="dataLoader"){
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader()
      daRestituire$load.data.frame(mydata = res,
                                   IDName = "patID",EVENTName = "event",
                                   dateColumnName = "date",format.column.date = "%Y-%m-%d %H:%M:%S")
    }
    return(daRestituire)
  }

  local.format.data.for.csv<-function(listaProcessi, lista.validi,
                                      data.ora = data.ora, typeOfRandomDataGenerator="") {
    big.csv<-c()
    ct <- 1

    for(posizione in seq(1,length(listaProcessi))) {
      i <- names(listaProcessi)[posizione]
      numeroElementi<-length(listaProcessi[[i]])
      array.Date <- data.ora[[i]]
      matrice<-cbind(rep(ct,numeroElementi),listaProcessi[[i]],array.Date,rep(as.character(lista.validi[ct]),numeroElementi) )
      big.csv<-rbind(big.csv,matrice )
      ct <- ct + 1
    }
    if(!is.null(dim(big.csv))) {
      colnames(big.csv)<-c("patID","event","date","valido")
    }
    return(big.csv)
  }
  #=================================================================================
  # distanceFrom - WRAPPER Function
  # Funzione WRAPPER per il calcolo delle distanze rispetto ad un oggetto omologo
  #=================================================================================
  distanceFrom<-function( objToCheck, metric="default") {

    if( metric == "default" ) return( distanceFrom.default( objToCheck = objToCheck) )
    if( metric == "binaryCount" ) return( distanceFrom.binaryCount( objToCheck = objToCheck) )

    obj.log$sendLog(msg= "The requested metric is not yet available" , type="NMI" )
  }
  #===========================================================
  # trainModel
  #===========================================================
  trainModel<-function(debug.mode = FALSE) {
    # setta la soglia a zero, cosi' per sport...
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0
    if(!is.null(parameters$considerAutoLoop)) {
      considerAutoLoop<-parameters$considerAutoLoop
    }
    else
    {
      considerAutoLoop<-TRUE
    }
    if(debug.mode==TRUE) browser()
    # copia la tabella delle  transizioni in una un po' piu' facile
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) { MM<-MMatrix.perc; }
    else {
      diag(MMatrix)<<-0
      diag(MMatrix.mean.time) <<- Inf
      diag(MMatrix.perc)<<-0
      MM <- MMatrix.perc.noLoop
    }
    # sistema la threshold
    # browser()
    aa<- MMatrix.perc; bb <- MMatrix
    bb[ which(aa<=threshold,arr.ind = T) ]<-0
    aa[ which(aa<=threshold,arr.ind = T) ]<-0
    for( i in seq( 1 , nrow(aa)) ) {if(sum(aa[i,])>0)  {aa[i,]<-aa[i,]/sum(aa[i,]);} }
    MMatrix.perc<<-aa ; MMatrix<<-bb
    grafo<-build.graph.from.table( MM = MMatrix.perc, threshold  = threshold)

    model.grViz<<-grafo;
  }
  #===========================================================
  # KaplanMeier
  #===========================================================
  KaplanMeier<-function( fromState, toState,
                         passingThrough=c(), passingNotThrough=c(), stoppingAt=c(),
                         stoppingNotAt=c(), PDVAt=c(), withPatientID=c() , UM="mins" )  {


    res <- lapply( MM.pat.process , function(x)  {
      wrklst <- list()
      wrklst$inPath <- FALSE
      wrklst$fromRow <- NA
      wrklst$toRow <- NA
      eventsInPath <- c()
      # browser()
      # Riproduci il calcolo, fra gli stati 'from' e 'to'
      for(riga in seq(1,nrow(x))) {

        # trigger the begin
        if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] == fromState & is.na(wrklst$fromRow) ) {
          wrklst$inPath <-TRUE
          wrklst$fromRow <- riga
        }
        # trigger the end (if they have a begin)
        if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] == toState ) {
          if(wrklst$inPath == TRUE ) {
            wrklst$inPath <- FALSE
            wrklst$toRow <- riga
          }
        }
        # trigger the PDV (if they have a begin)
        if( x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] %in% PDVAt ) {
          if(wrklst$inPath == TRUE ) {
            wrklst$inPath <- FALSE
            wrklst$toRow <- riga
            eventsInPath <- c( eventsInPath ,  x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] )
          }
        }
        if(wrklst$inPath == TRUE) {
          eventsInPath <- c( eventsInPath ,  x[ riga , MM.csv.parameters[["csv.EVENTName"]] ] )
        }
      }
      # ora verifica se le transizioni soddisfano le condizioni dei parametri in ingresso
      possibleCandidate <- TRUE
      if (is.na(wrklst$toRow)) {
        possibleCandidate <- FALSE
      }
      ultimoStato <- x[ nrow(x) , MM.csv.parameters[["csv.EVENTName"]] ]
      if( !is.na(wrklst$fromRow) & !is.na(wrklst$toRow)  ) {
        # browser()
        if( FALSE %in% (passingThrough %in% eventsInPath) & length(passingThrough)>0 ) {
          possibleCandidate <- FALSE
        }
        if( TRUE %in% (passingNotThrough %in% eventsInPath) & length(passingNotThrough)>0 ) {
          possibleCandidate <- FALSE
        }
        if( FALSE %in% (ultimoStato %in% stoppingAt) & length(stoppingAt)>0 ) {
          possibleCandidate <- FALSE
        }
        if( TRUE %in% (ultimoStato %in% stoppingNotAt) & length(stoppingNotAt)>0 ) {
          possibleCandidate <- FALSE
        }
      }

      if( length(withPatientID) > 0 ) {
        if( !(unique(x[,MM.csv.parameters$csv.IDName]) %in% withPatientID) ) {
          possibleCandidate <- FALSE
        }
      }

      # qui ragionare su withPatientID
      if( TRUE %in% (PDVAt %in% eventsInPath) ) {
        event.censored <- 0
      } else {
        event.censored <- 1
      }

      deltaT<-NA
      if( !is.na(wrklst$fromRow) & !is.na(wrklst$toRow) ) {
        deltaT <- x[ wrklst$toRow, "pMineR.deltaDate" ] - x[ wrklst$fromRow, "pMineR.deltaDate" ]
      }

      lista.res <- list( "eligible" = possibleCandidate,
                         "event.censored" = event.censored,
                         "deltaT" = deltaT,
                         "arr.evt" = eventsInPath,
                         "error" = 0)
      return(lista.res)
    })
    # browser()
    matrice.KM <- c()
    for( ID in names(res) ) {
      if( res[[ID]]$eligible == TRUE ) {
        matrice.KM <- rbind( matrice.KM, c(ID, res[[ID]]$deltaT, res[[ID]]$event.censored ))
      }
    }

    if(!is.null(matrice.KM)){
      colnames(matrice.KM)<-c("ID","time","outcome")
      matrice.KM <- as.data.frame(matrice.KM)
      if(is.factor(matrice.KM$time)){
        matrice.KM$time<- as.numeric(levels(matrice.KM$time))[matrice.KM$time]
      }else{
        matrice.KM$time<-as.numeric(matrice.KM$time)
      }

      if(is.factor(matrice.KM$outcome)){
        matrice.KM$outcome <- as.numeric(levels(matrice.KM$outcome))[matrice.KM$outcome]
      }else{
        matrice.KM$outcome <- as.numeric(matrice.KM$outcome)
      }

      # matrice.KM$time <- as.numeric(levels(matrice.KM$time))[matrice.KM$time]
      # matrice.KM$outcome <- as.numeric(levels(matrice.KM$outcome))[matrice.KM$outcome]

      if( UM == "days") matrice.KM$time <- matrice.KM$time / 1440
      if( UM == "hours") matrice.KM$time <- matrice.KM$time / 60
      if( UM == "weeks") matrice.KM$time <- matrice.KM$time / (1440 * 7)
      if( UM == "months") matrice.KM$time <- matrice.KM$time / (43800)


      KM0 <- survfit(Surv(time, outcome)~1,   data=matrice.KM)
      error.mess<-"0: successful"
    }else{
      KM0<-NULL
      error.mess<-"1: No matches for the specified path"
    }
    # browser()




    return( list("table"=matrice.KM, "KM"=KM0, "ID"=matrice.KM$ID, "error"=error.mess ) )

  }
  replay<-function( dataList ,  col.toCheckPerformances=NA ) {
    res<-list()
    res$words<-list()
    res$success<-c()
    declared.correctness<- c()

    if( !is.na(col.toCheckPerformances) )  {
      if( !(col.toCheckPerformances %in% colnames(dataList$pat.process[[1]])) ) {
        obj.log$sendLog( msg = c("The indicated column name (in 'col.toCheckPerformances') does not exist in the dataset!")  ,type="ERR");
        return();
      }
    }

    for(patId in names(dataList$pat.process)) {
      parola <- unlist(dataList$pat.process[[patId]][[dataList$csv.EVENTName]])
      parola <- c("BEGIN",parola)
      success <- TRUE;
      path.attuale<-c()
      for( caratt.i in seq(1,(length(parola)-1)) ) {
        caratt.s <- parola[ caratt.i  ]
        if(!(caratt.s %in% colnames(MMatrix.perc))) { success = FALSE; break; }
        if(!(parola[ caratt.i +1 ] %in% colnames(MMatrix.perc))) { success = FALSE; break; }

        jump.prob <- MMatrix.perc[ parola[ caratt.i  ], parola[ caratt.i+1 ]  ]
        if(jump.prob>0) path.attuale <- c(path.attuale,parola[ caratt.i  ])
        if(jump.prob==0) { success = FALSE; break; }
        caratt.s %in% colnames(MMatrix.perc)
      }
      res$words[[patId]]<-path.attuale
      res$success<-c(res$success,success)

      if( !is.na(col.toCheckPerformances) )  {
        declared.correctness <- c(declared.correctness,as.character(dataList$pat.process[[patId]][[col.toCheckPerformances]][1]))
      }
    }

    if( !is.na(col.toCheckPerformances) ) {
      conf.matrix <- table(res$success, declared.correctness)
      res$performances <- calculate.performances(conf.matrix)
    }
    return( res )
  }
  #===========================================================
  # calculate.performances
  #===========================================================
  calculate.performances<-function( conf.matrix  ) {
    # calculate accuracy
    if( "TRUE" %in% rownames(conf.matrix) & "TRUE" %in% colnames(conf.matrix)) {
      TP <- conf.matrix[ "TRUE","TRUE" ]
    } else { TP <- 0 }
    if( "FALSE" %in% rownames(conf.matrix) & "FALSE" %in% colnames(conf.matrix)) {
      TN <- conf.matrix[ "FALSE","FALSE" ]
    } else { TN <- 0 }
    if( "TRUE" %in% rownames(conf.matrix) & "FALSE" %in% colnames(conf.matrix)) {
      FP <- conf.matrix[ "TRUE","FALSE" ]
    } else { FP <- 0 }
    if( "FALSE" %in% rownames(conf.matrix) & "TRUE" %in% colnames(conf.matrix)) {
      FN <- conf.matrix[ "FALSE","TRUE" ]
    } else { FN <- 0 }

    return(list(
      "TP" = TP,
      "TN" = TN,
      "FP" = FP,
      "FN" = FN,
      "accuracy" = (TP + TN)/(TP + TN + FP + FN),
      "sensitivity" = (TP )/(TP + FN ),
      "specificity" = (TN )/(FP + TN ),
      "precision" = (TP )/(TP + FP ),
      "recall" = (TP )/(TP + FN ),
      "F1score" = ( 2 * TP )/( 2 * TP + FP + FN ),
      "conf.matrix"=conf.matrix
    ))
  }
  #===========================================================
  # findReacheableNodes
  # Funzione
  #===========================================================
  findReacheableNodes<-function( nodoDiPatenza = 'BEGIN'  ) {
    findReacheableNodes.recursiveLoop(
      nodoAttuale = nodoDiPatenza,
      nodi.raggiunti = c(nodoDiPatenza)
    )
  }
  findReacheableNodes.recursiveLoop<-function( nodoAttuale , nodi.raggiunti  ) {
    lista.nodi <- colnames(MMatrix.perc)
    nodi.raggiunti <- unique(c(nodi.raggiunti,nodoAttuale))
    for( nodoDestinazione in lista.nodi) {
      if( !(nodoDestinazione %in% nodi.raggiunti ) & MMatrix.perc[nodoAttuale,nodoDestinazione]>0) {
        aa <- findReacheableNodes.recursiveLoop( nodoAttuale = nodoDestinazione ,
                                                 nodi.raggiunti = nodi.raggiunti)
        nodi.raggiunti <- unique(c(nodi.raggiunti , aa))
      }
    }
    return(nodi.raggiunti)
  }
  #===========================================================
  # convert2XML - future
  #===========================================================
  convert2XML<-function(  ) {
  }
  #===========================================================
  # getModel
  #===========================================================
  getModel<-function(kindOfOutput) {
    if(kindOfOutput=="grViz") return( model.grViz )
    if(kindOfOutput=="MMatrix") return( MMatrix )
    if(kindOfOutput=="MMatrix.perc") return( MMatrix.perc )

    obj.log$sendLog( msg = c("The requested model is not available yet! Only 'grViz', 'MMatrix' and 'MMatrix.perc' are admitted. ")  ,type="ERR");
  }
  #===========================================================
  # plot
  #===========================================================
  plot<-function(  plotIt = TRUE, giveItBack = FALSE ){
    if( plotIt == TRUE ) {
      grViz( getModel(kindOfOutput = "grViz" ) )
    }
    if( giveItBack == TRUE ) {
      return(  getModel(kindOfOutput = "grViz" ) )
    }
  }
  #===========================================================
  # setIstanceClass
  #===========================================================
  setInstanceClass<-function( className, classType = "default") {
    istanceClass[[classType]]<<-className
  }
  #===========================================================
  # setIstanceClass
  #===========================================================
  getInstanceClass<-function( className, classType = "default") {
    return(istanceClass[[classType]])
  }
  # ***************************************************************************************************
  # MODEL SPECIFIC PUBLIC METHODS
  # ***************************************************************************************************
  #===========================================================
  # plot.delta.graph
  # Funzione per il plotting delle distanze rispetto ad una data metrica
  #===========================================================
  plot.delta.graph<-function( objToCheck, threshold=0, type.of.graph="overlapped",
                              threshold.4.overlapped=.3 ,giveBackGrViz = FALSE,
                              plotIt = TRUE, returnDeltaMatrix = FALSE, layout = "dot", rankDirLR = FALSE) {

    if( type.of.graph != "overlapped" & type.of.graph !="delta") stop("\n Not yet implemented: err.cod. %43547g8fd")
    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc

    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)
    listaStati<-unique(combinazioni[,1])
    matrice<-array(0,dim=c(length(listaStati),length(listaStati)))
    m.int<-matrice; m.ext<-matrice;
    colnames(matrice)<-listaStati;     rownames(matrice)<-listaStati
    colnames(m.int)<-listaStati;     rownames(m.int)<-listaStati
    colnames(m.ext)<-listaStati;     rownames(m.ext)<-listaStati

    for(riga in seq(1,nrow(combinazioni))) {
      matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- abs(combinazioni[riga,"int"] - combinazioni[riga,"ext"])
      m.int[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- combinazioni[riga,"int"]
      m.ext[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<- combinazioni[riga,"ext"]
      #      if( matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ] <threshold) matrice[  combinazioni[riga,"from"] , combinazioni[riga,"to"]   ]<-0
    }

    if(type.of.graph=="delta")
      grafo<-build.graph.from.table(MM = matrice, threshold = threshold, layout = layout , rankDirLR = rankDirLR)
    if(type.of.graph=="overlapped")
      grafo<-build.graph.from.table(MM = m.int, threshold = threshold,
                                    second.MM = m.ext,
                                    threshold.second.MM = threshold.4.overlapped, type.of.graph = type.of.graph, layout = layout, rankDirLR = rankDirLR)

    if(plotIt==TRUE)   grViz(grafo);
    if(returnDeltaMatrix==TRUE) return( list("matrix"=matrice,"m.int"=m.int,"m.ext"=m.ext) );
    if(giveBackGrViz == TRUE) return(grafo);
  }
  #=================================================================================
  # get.time.transition.Prob
  # It tries to predict the probability to reach a final state (starting from a known starting state)
  # in "at least" K days
  #   initialState : the initial state
  #   finalState : the final state
  #   num.of.transitions : the max number of allowed days for reaching the final state
  #   debugString : (TRUE/FALSE) is a debug string print for debuggin issues
  #   killAutoLoop : (TRUE/FALSE) suppress autoloop during computation?
  #=================================================================================
  get.time.transition.Prob<-function( initialState, finalState, maxTime,debugString=NA,killAutoLoop=FALSE){
    a<-getTimeProb( timeAttuale=0, maxTime =maxTime,
                    statoAttuale=initialState, statoGoal =finalState,
                    debugString=debugString,killAutoLoop=killAutoLoop, comsumedTime=0)
    return(a)
  }
  getTimeProb<-function( timeAttuale=0, maxTime =1, statoAttuale="BEGIN", statoGoal ="END", debugString=NA,killAutoLoop=FALSE,comsumedTime=0) {
    if(is.na(debugString)) debugString<-statoAttuale;

    if( statoAttuale==statoGoal ) {
      cat('\n ',debugString);
      return( 1 );
    }

    if( timeAttuale > maxTime & (statoAttuale!=statoGoal) ) {
      return( 1 );
    }
    if( killAutoLoop == FALSE ) { MMPerc<-MMatrix.mean.time; MMProb<-MMatrix.perc }
    if( killAutoLoop == TRUE ) { stop("Not yet implemented err.cod -hg85h78g4578") }
    prob<-0;

    for( possibileNuovoStato in rownames(MMPerc)) {
      giorni.stimati.per.transizione<-MMPerc[statoAttuale,possibileNuovoStato]
      if(giorni.stimati.per.transizione<1) giorni.stimati.per.transizione=1
      if(MMPerc[statoAttuale,possibileNuovoStato]!=Inf & (timeAttuale+giorni.stimati.per.transizione) <= maxTime ) {
        newdebugString<-paste(c(debugString,'==(',as.character(giorni.stimati.per.transizione),',',(timeAttuale+giorni.stimati.per.transizione),',',MMProb[statoAttuale,possibileNuovoStato],')==>',possibileNuovoStato),collapse='');
        addendo<-MMProb[statoAttuale,possibileNuovoStato] * getTimeProb( timeAttuale+giorni.stimati.per.transizione, maxTime,  possibileNuovoStato ,  statoGoal , debugString = newdebugString, killAutoLoop=killAutoLoop);
        prob<-prob + addendo
      }
    }
    return(prob);
  }
  #=================================================================================
  # build.PWF
  # IT builds a Pseudo-Workflow XML file
  #=================================================================================
  build.PWF<-function() {
    testo <- "<xml>"
    testo <- str_c(testo,"\n\t<workflow>")
    # ora i nodi
    nomeNodi <- colnames(MMatrix)[!(colnames(MMatrix) %in% "BEGIN")]
    for( i in nomeNodi ) {
      if(i != "END")
        testo <- str_c(testo,"\n\t<node name='",i,"' />")
      else
        testo <- str_c(testo,"\n\t<node name='END' type='END' />")
    }
    # Ora i trigger
    for(from.node in rownames(MMatrix)) {
      for(to.node in colnames(MMatrix)) {
        # Se e' diverso da zero e, soprautto, se non si tratta dello stesso nodo
        # (senno' mi trovero' un SET ed un UNSET sullo stesso nodo con conseguente errore
        # a runtime per ambiguita')
        if(MMatrix[from.node,to.node] >0 & from.node!=to.node) {
          testo <- str_c(testo, "\n\t <trigger name='from.",from.node,".to.",to.node,"'>")
          testo <- str_c(testo, "\n\t\t <condition>")
          testo <- str_c(testo, "\n\t\t\t","'",from.node,"' %in% $st.ACTIVE$ AND $ev.NOW$=='",to.node,"'")
          testo <- str_c(testo, "\n\t\t </condition>")
          testo <- str_c(testo, "\n\t\t <unset>'",from.node,"'</unset>")
          testo <- str_c(testo, "\n\t\t <set>'",to.node,"'</set>")
          testo <- str_c(testo, "\n\t </trigger>")
        }
      }
    }
    testo <- str_c(testo,"\n\t</workflow>")
    testo <- str_c(testo,"\n</xml>")

    return(testo)
  }
  #=================================================================================
  # get.transition.Prob
  # It tries to predict the probability to reach a final state (starting from a known starting state)
  # in "at least" K transitions
  #   initialState : the initial state
  #   finalState : the final state
  #   num.of.transitions : the max number of allowed transitions for reaching the final state
  #   debugString : (TRUE/FALSE) is a debug string print for debuggin issues
  #   killAutoLoop : (TRUE/FALSE) suppress autoloop during computation?
  #=================================================================================
  get.transition.Prob<-function( initialState, finalState, num.of.transitions,debugString=NA,killAutoLoop=FALSE){
    a<-getProb( stepAttuale=0, maxNumStep =num.of.transitions,
                statoAttuale=initialState, statoGoal =finalState,
                debugString=debugString,killAutoLoop=killAutoLoop)
    return(a)
  }
  getProb<-function( stepAttuale=0, maxNumStep =1, statoAttuale="BEGIN", statoGoal ="END", debugString=NA,killAutoLoop=FALSE) {
    if(is.na(debugString)) debugString<-statoAttuale;

    if( statoAttuale==statoGoal ) {
      cat('\n ',debugString);
      return( 1 );
    }
    if( stepAttuale == maxNumStep & (statoAttuale!=statoGoal) ) {
      return( 0 );
    }
    if( killAutoLoop == FALSE ) MMPerc<-MMatrix.perc
    if( killAutoLoop == TRUE ) MMPerc<-MMatrix.perc.noLoop
    prob<-0;
    for( possibileNuovoStato in rownames(MMPerc)) {
      if(MMPerc[statoAttuale,possibileNuovoStato]>0) {
        newdebugString<-paste(c(debugString,'=>',possibileNuovoStato),collapse='');
        addendo<-MMPerc[statoAttuale,possibileNuovoStato] * getProb( stepAttuale+1, maxNumStep,  possibileNuovoStato ,  statoGoal , debugString = newdebugString, killAutoLoop=killAutoLoop);
        prob<-prob + addendo
      }
    }
    return(prob);
  }
  #===========================================================
  # play.Single
  #===========================================================
  play.Single<-function() {

    ct<-1;
    res<-c();
    if(!is.null(parameters$considerAutoLoop)) considerAutoLoop<-parameters$considerAutoLoop
    else considerAutoLoop<-TRUE
    if ( !("END" %in% findReacheableNodes(nodoDiPatenza = "BEGIN") )) {
      cat("\n WARNING: END is not reacheable from 'BEGIN'! Maybe the threshold is too high? ")
      return(c() ) ;
    }

    # copia la tabella delle transizioni in una un po' piu' facile
    # da maneggiare (almeno come nome)
    if ( considerAutoLoop == TRUE) MM<-MMatrix.perc
    else MM<-MMatrix.perc.noLoop
    statoAttuale<-"BEGIN"
    while( statoAttuale != "END") {
      sommaCum<-cumsum(MM[statoAttuale,])
      dado<-runif(n = 1,min = 0,max = 0.99999999999999)
      posizione<-which( (cumsum(MM[statoAttuale,])-dado)>=0  )[1]
      nuovoStato<-colnames(MM)[posizione]
      if(is.na(nuovoStato)) browser()
      if ( ("END" %in% findReacheableNodes(nodoDiPatenza = nuovoStato) )) {
        res<-c(res,statoAttuale)
        statoAttuale<-nuovoStato
      }
    }
    res<-c(res,"END")
    res<-res[ which( !(res %in%  c('BEGIN','END') ))    ]
    return(res);
  }
  #===========================================================
  # build.graph.from.table
  #===========================================================
  build.graph.from.table<-function(MM, threshold, second.MM = NA, threshold.second.MM=.2 , type.of.graph= "delta" , layout = "dot", rankDirLR = FALSE) {

    if( type.of.graph != "overlapped" & type.of.graph !="delta") stop("\n Not yet implemented: err.cod. %43547g8fd")

    # prendi la lista dei nomi
    listaNodi<-colnames(MM)
    # la lista dei nodi raggiungibili da BEGIN
    listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]>threshold)]
    # la lista dei nodi che vanno a END
    listaNodiToEnd<-listaNodi[which(MM[,"END"]>threshold)]

    rigaBEGIN<-''

    for( i in listaNodiFromBegin) {
      rigaBEGIN<-paste(   c(rigaBEGIN, "'BEGIN'->'",i,"' "), collapse = '')
    }
    rigaEND<-''
    for( i in listaNodiToEnd) {
      rigaEND<-paste(   c(rigaEND, "'",i,"'->'END' "), collapse = '')
    }

    arr.nodi.con.archi <- c()
    stringaNodiComplessi<-''
    for(i in seq(1,nrow(MM))) {
      listaNodiRiga<-listaNodi[which(MM[i,]>=threshold)]
      if(length(listaNodiRiga)>0) {
        for( ct in seq(1,length(listaNodiRiga))) {

          peso<-as.numeric(MM[i, listaNodiRiga[ct]])
          peso.rounded <- round(peso, digits = 2)
          denominatore <- sum(MMatrix[i, ])
          numeratore <- MMatrix[i, listaNodiRiga[ct]]

          if (peso.rounded==0) {peso.rounded <- "<0.01"}
          penwidth<- peso*3 + 0.01
          if(penwidth<0.4) penwidth=0.4
          fontSize = 5+peso*9
          colore = as.integer(100-(30+peso*70))
          if( type.of.graph == "overlapped") {
            second.peso<-as.numeric(second.MM[i, listaNodiRiga[ct]])
            second.peso.rounded <- round(second.peso, digits = 2)
            if (second.peso.rounded==0) {second.peso.rounded <- "<0.01"}
            if( abs(peso - second.peso) >= threshold.second.MM ) {
              delta.peso<-as.numeric((peso - second.peso))
              penwidth<- max(peso,abs(delta.peso))*3 + 0.01
              fontSize = 5+max(peso,abs(delta.peso))*9
              if(delta.peso>0) colore.delta.peso<-"Red"
              else colore.delta.peso<-"Green"
              if(peso > threshold | second.peso > threshold) {
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso.rounded,"/",second.peso.rounded,"', style='dashed', fontcolor='",colore.delta.peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = ",colore.delta.peso,"]\n"), collapse = '')
                arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
              }
            } else{
              if(peso > threshold) {
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso.rounded,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')
                arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
              }
            }
          } else {
            if(peso > threshold) {
              stringa.arco <- paste(c(numeratore,"/",denominatore,"\n( ",peso.rounded," )"),collapse = '')
              # stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso.rounded,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')
              stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",stringa.arco,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')
              arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
            }
          }

        }
      }
    }

    listaNodiToPrint<-''
    arr.nodi.con.archi <- unique(arr.nodi.con.archi)

    listaNodi <- listaNodi[ listaNodi %in% arr.nodi.con.archi ]

    for(i in seq(1,length(listaNodi))) {
      if(i<length(listaNodi)) listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"';"), collapse=''    )
      else listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"'"), collapse=''    )
    }

    if( rankDirLR == FALSE ) {direzione <- ""
    }    else {
      direzione <- ", rankdir = LR"
    }
    # now plot it
    a<-paste(c("digraph boxes_and_circles {

               # a 'graph' statement
               graph [overlap = true, fontsize = 10, layout = ",layout," ",direzione,"]

               # several 'node' statements
               node [shape = oval,
               fontname = Helvetica,
               style = filled]

               node [fillcolor = green]
               'BEGIN';

               node [fillcolor = red]
               'END';

               node [fillcolor = orange]
               ",listaNodiToPrint,"

               edge [arrowsize = 1 ]
               # several edge
               ",stringaNodiComplessi,"
  }"), collapse='')
    return(a)

  }
  #===========================================================
  # distanceFrom.default
  # Metrica di default. In questo caso la metrica di default e' semplicemente la somma
  # dei valori assoluti delle differenze di probabilita' fra le matrici di transizione
  # di stato.
  #===========================================================
  distanceFrom.default<-function( objToCheck) {

    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc

    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)

    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }
  #===========================================================
  # distanceFrom.binaryCount
  # Una banale metrica: costruisce una maschera binaria degli stati a transizione
  # con p()>0 e li setta a uno: a seguire fa la somma degli XOR fra le colonne
  # (conta '1' ogni volta che una e' '0' mentre l'altra e' '1')
  #===========================================================
  distanceFrom.binaryCount<-function( objToCheck) {

    ext.MM <- objToCheck$getModel(kindOfOutput = "MMatrix.perc")
    int.MM <- MMatrix.perc

    combinazioni<-calcolaMatriceCombinazioni( ext.MM = ext.MM, int.MM = int.MM)

    combinazioni[ combinazioni[,"int"]>0, "int"] <-1
    combinazioni[ combinazioni[,"ext"]>0, "ext"] <-1

    distance<-sum(abs(combinazioni$ext - combinazioni$int))
    return( list(   "distance" = distance )      )
  }
  #===========================================================
  # calcolaMatriceCombinazioni
  # Funzione di comodo che calcola in una matrice le differenze di probabilita'
  # fra due FSM. Di fatto e' un pre-processing per funzioni che calcolano metriche
  #===========================================================
  calcolaMatriceCombinazioni<-function( ext.MM, int.MM) {
    unione.nomi<-unique(c(colnames(ext.MM),colnames(int.MM)))
    combinazioni<-expand.grid(unione.nomi,unione.nomi)
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    combinazioni<-cbind( combinazioni,  rep(0,nrow(combinazioni)  ) )
    colnames(combinazioni)<-c("from","to","int","ext")
    combinazioni$from<-as.character(combinazioni$from)
    combinazioni$to<-as.character(combinazioni$to)

    for(riga in seq(1,nrow(combinazioni))) {

      if(  combinazioni[riga, "from"] %in%  colnames(int.MM)  &
           combinazioni[riga, "to"] %in%  colnames(int.MM)
      ) {
        combinazioni[riga, "int"]<-int.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
      if(  combinazioni[riga, "from"] %in%  colnames(ext.MM)  &
           combinazioni[riga, "to"] %in%  colnames(ext.MM)
      ) {
        combinazioni[riga, "ext"]<-ext.MM[  combinazioni[riga, "from"] , combinazioni[riga, "to"]     ]
      }
    }
    return(combinazioni)
  }
  #===========================================================
  # KaplanMeier
  #===========================================================
  # KaplanMeier <- function( fromState, toState,
  #                          passingThrough=c(), passingNotThrough=c(), stoppingAt=c(),
  #                          stoppingNotAt=c(), PDVAt=c(), withPatientID=c() )  {
  #
  #
  #
  # }
  #===========================================================
  # getClass
  # restituisce il tipo della classe corrente
  #===========================================================
  getClass<-function(){
    return(list(
      "class"="FOMM",
      "obj.ID"=global.personal.ID,
      "version"="0.44"
    ))
  }
  LogRankTest<-function( KM1 , KM2, col.1, col.2 )  {
    # browser()
    path.1 <- KM1
    path.2 <- KM2
    new.df <- rbind(path.1$table,path.2$table)
    new.df$KM <- c(rep("KM1",nrow(path.1$table) ),rep("KM2",nrow(path.2$table) ))
    logRank <- survdiff(Surv(time, outcome) ~ KM,data=new.df)

    fitting <- survfit(Surv(time, outcome) ~ KM,data=new.df)

    return( list("survdiff"=logRank , "survfit"=fitting) )
  }


  # ***************************************************************************************************
  # PREDICTIVE PROCESS DISCOVERY METHODS
  # ***************************************************************************************************

  #===========================================================
  # Missing value attributes selection Function:
  # ROW
  #===========================================================

  remove.miss.row<-function(df_tot, n.pat.delete=4){
    missing.mat<-matrix(0,nrow = nrow(df_tot),ncol = (ncol(df_tot)-2))
    rownames(missing.mat)<-df_tot[,"ID"]
    colnames(missing.mat)<-colnames(df_tot)[-c(1,ncol(df_tot))]
    tmp<-lapply(1:ncol(missing.mat), function(colonna){
      tmp<-lapply(1:nrow(missing.mat), function(riga){
        if(is.na(df_tot[riga,colonna])){missing.mat[riga,colonna]<<-1}
      })
    })

    rep.pat<-unique(missing.mat[rownames(missing.mat[which(duplicated(missing.mat)),]),])
    if(nrow(rep.pat)<=1){
      list.out<-list()
    }else{
      rep.count<-matrix(0,nrow = nrow(rep.pat),ncol = 1)
      att.miss.pat<-matrix(0,nrow = nrow(rep.pat),ncol = 1)
      tmp<-lapply(1:nrow(rep.pat), function(riga.rep){
        att.miss.pat[riga.rep]<<-sum(rep.pat[riga.rep,])
        if(length(which(rep.pat[riga.rep,]==rep(0,ncol(rep.pat))))==ncol(rep.pat)){
          rep.count[riga.rep,1]<<- 0
          del.row<-0
        }else{
          del.row<-lapply(1:nrow(missing.mat), function(riga.miss){
            if(length(which(missing.mat[riga.miss,]==rep.pat[riga.rep,]))==ncol(missing.mat)){
              rep.count[riga.rep,1]<<- rep.count[riga.rep,1]+1
              return(riga.miss)
            }else{
              return(NULL)
            }
          })
        }
        return(unlist(del.row))
      })

      missing.pat<-cbind(rep.pat,rep.count,att.miss.pat)
      colnames(missing.pat)[(ncol(missing.pat)-1)]<-"count"
      colnames(missing.pat)[ncol(missing.pat)]<-"n.att.miss"
      missing.pat<-missing.pat[order(missing.pat[,"count"],decreasing = T),]

      if(n.pat.delete>nrow(missing.pat)) n.pat.delete=nrow(missing.pat)

      sum.miss<-unlist(lapply(1:ncol(missing.mat), function(colonna){sum(missing.mat[,colonna])}))
      names(sum.miss)<-colnames(missing.mat)
      sum.miss<-sort(sum.miss,decreasing = T)

      id.to.del<-lapply(1:n.pat.delete, function(riga.pat){
        tmp<-lapply(rownames(missing.mat), function(id.paz){
          if(length(which(missing.mat[id.paz,]==missing.pat[riga.pat,1:(ncol(missing.pat)-2)]))==ncol(missing.mat)){
            return(id.paz)
          }else{
            return(NULL)
          }})
        return(unlist(tmp))
      })

      df_tot.row<-df_tot[-which(df_tot[,"ID"] %in% unlist(id.to.del)),]

      list.out<-list("df.clean.row"=df_tot.row,
                     "missing.pat"=missing.pat,
                     "missing.mat"=missing.mat,
                     "counts.missing"=sum.miss)


    }
    return(list.out)
  }

  #===========================================================
  # Missing value attributes selection Function:
  # COL
  #===========================================================

  remove.miss.col<-function(df_tot, n.pat.delete=4, tec.type="backward", clean.row=T){

    lst.clean.row<-remove.miss.row(df_tot =df_tot, n.pat.delete=n.pat.delete)
    if(nrow(lst.clean.row$df.clean.row)==0 || length(lst.clean.row)==0){
      list.out<-NULL
    }else{
      if(clean.row){
        tmp_df<-lst.clean.row$df.clean.row
        tmp.miss.mat<-lst.clean.row$missing.mat[tmp_df$ID,]
      }else{
        tmp_df<-df_tot
        tmp.miss.mat<-lst.clean.row$missing.mat
      }

      count.miss<-unlist(lapply(1:ncol(tmp.miss.mat), function(colonna){sum(tmp.miss.mat[,colonna])}))
      names(count.miss)<-colnames(tmp.miss.mat)
      count.miss<-sort(count.miss,decreasing = T)

      switch (tec.type,
              "backward" = {
                continue<-TRUE
                arr.att<-names(lst.clean.row$counts.missing)
                while (continue) {
                  to.del<- arr.att[1]
                  arr.att<-arr.att[-which(arr.att==arr.att[1])]
                  tmp_df<-subset(tmp_df,select = colnames(tmp_df)[-which(colnames(tmp_df) %in% to.del)])
                  tmp.miss.mat<-subset(tmp.miss.mat,select = colnames(tmp.miss.mat)[-which(colnames(tmp.miss.mat) %in% to.del)])
                  if(nrow(na.omit(tmp_df))>nrow(na.omit(df_tot))){
                    continue<-FALSE
                  }}
              }
      )

      df.clean.col<-subset(tmp_df,select = colnames(tmp_df)[which(colnames(tmp_df) %in% c("ID","y",arr.att))])
      tmp.median<-lapply(1:ncol(df.clean.col), function(col.med){
        if(length(which(is.na(df.clean.col[,col.med])))>0){
          ind.na<-which(is.na(df.clean.col[,col.med]))
          df.clean.col[ind.na,col.med]<<-median(df.clean.col[,col.med],na.rm = T)
        }
      })

      counts.missing<-count.miss[which(names(count.miss) %in% arr.att)]

      list.out<-list("df.clean.col"=df.clean.col,
                     "missing.mat"=tmp.miss.mat,
                     "missing.mat"=lst.clean.row$missing.mat,
                     "counts.missing"=counts.missing)
    }

    return(list.out)
  }



  #===========================================================
  # Search value function
  #===========================================================
  search_value<-function(sub.path,eventoDiPartenza,arr.attributi,type="att"){
    if(type=="att"){
      riga<-which(sub.path[,MM.csv.parameters$csv.EVENTName]==eventoDiPartenza)
      values<-sub.path[riga,][arr.attributi]
    }else{
      values<-rep.int(NA,times = length(arr.attributi))
    }
    return(as.data.frame(values))
  }

  #===========================================================
  # Model Performances:
  # ev.param<- "AUC", "acc"
  #===========================================================

  compute.perf.fun<-function(model, df, ev.param= "acc",thr.acc=0.5){
    pred<-predict(model,newdata= df, type="response")
    df$y_prob<-pred
    df<-df[order(df$y_prob),]
    df$y_pred<-"0"
    arr.thr<-df$y_prob
    list_roc<-lapply(arr.thr, function(thr){

      df$y_pred[which(df$y_prob>thr)]<-"1"

      TP<-length(which(df$y=="1" & df$y_pred=="1"))
      TN<-length(which(df$y=="0" & df$y_pred=="0"))
      FP<-length(which(df$y=="0" & df$y_pred=="1"))
      FN<-length(which(df$y=="1" & df$y_pred=="0"))

      x_FPR<-FP/(FP+TN)
      y_TPR<- TP/(TP+FN)
      acc<-(TP+TN)/(TP+TN+FP+FN)
      return(c(thr,x_FPR,y_TPR,acc))

    })

    df_roc<-as.data.frame(do.call('rbind',list_roc))
    colnames(df_roc)<-c("threshold","FPR","TPR","accuracy")

    #calcolo AUC
    FPR<-df_roc$FPR[order(df_roc$FPR)]
    TPR<-df_roc$TPR[order(df_roc$TPR)]
    dFPR <- c(0,diff(FPR))
    dTPR <- c(0,diff(TPR))
    AUC <- sum(TPR * dFPR) + sum(dTPR * dFPR)/2



    #calcolo pval x att
    tab<-summary(model)
    pval<-tab$coefficients[,4][2:nrow(tab$coefficients)]

    if(ev.param=="acc"){
      df$y_pred[which(df$y_prob>thr.acc)]<-"1"

      TP<-length(which(df$y=="1" & df$y_pred=="1"))
      TN<-length(which(df$y=="0" & df$y_pred=="0"))
      FP<-length(which(df$y=="0" & df$y_pred=="1"))
      FN<-length(which(df$y=="1" & df$y_pred=="0"))
      df_roc$accuracy<-(TP+TN)/(TP+TN+FP+FN)
    }

    # switch (ev.param,
    #   "acc" = {
    #     ev.value=acc
    #   },
    #   "AUC"= {
    #     ev.value= AUC
    #   }
    # )

    return(list("df_roc" = df_roc,"AUC" = AUC,"pval"= pval))
  }

  #===========================================================
  # train LR model:
  # logistic regression model and performances (on train/test or both)
  # input: df_train, df_test, chosen.digit (for performaces approx), perf.train/perf.test (data on which compute perf)
  #===========================================================

  train.FOMM.LR <- function( df_train,df_test=data.frame(),chosen.digit = 4,perf.train = TRUE,perf.test=TRUE,ev.param="acc",thr.acc=0.5) {
    if(nrow(df_test)==0){
      perf.train=TRUE
      perf.test=FALSE
    }
    df_train_tmp<-df_train
   # df_train_tmp<-df_train[,2:ncol(df_train)]
    glm.fit<-glm( y ~ ., data=df_train_tmp, family= binomial(link = "logit"))
    if(perf.test & perf.train){
      lst.perf.test<-compute.perf.fun(glm.fit,df_test,ev.param)
      lst.perf.train<-compute.perf.fun(glm.fit,df_train,ev.param)
      to_ret<-list("model"=glm.fit,"roc_test"= lst.perf.test$df_roc,"pval_test"= lst.perf.test$pval,"AUC_test" = lst.perf.test$AUC,
                   "roc_train"= lst.perf.train$df_roc,"pval_train"=lst.perf.train$pval,"AUC_train"= lst.perf.train$AUC)
    }else if(perf.train & !perf.test){
      lst.perf.train<-compute.perf.fun(model = glm.fit,df = df_train,ev.param)
      to_ret<-list("model"=glm.fit,"roc_train"= lst.perf.train$df_roc,"pval_train"= lst.perf.train$pval,"AUC_train"= round(lst.perf.train$AUC,digits = chosen.digit))
    }else{
      lst.perf.test<-compute.perf.fun(glm.fit,df_test,ev.param)
      to_ret<-list("model"=glm.fit,"roc_test"= lst.perf.test$df_roc,"pval_test"= lst.perf.test$pval,"AUC_test"= round(lst.perf.test$AUC,digits = chosen.digit))
    }

    return(to_ret)
  }

  #===========================================================
  # LR model + FS function---> compute LR model.
  # oversamp.tec=> bootstrap, SMOTE
  # perf.ind => AUC, acc
  # acc.ind => accuracy, b.accuracy
  #
  #
  # OUTPUT: la funzione ritorna in output una lista che contiene:
  #         - "run.check" <- parametro T/F che serve per capire se è stato possibile apprendere il modello (problemi di cardinalità)
  #         - "res"       <- lista composta da:
  #
  #                                           - "$lst.models.fold": lista contenente un numero di elementi pari al numero di fold scelti per la cross fold (k)
  #                                                                 ogni elemento contiene: - il modello calcolato dalla glm function
  #                                                                                         - tabella x ROC su train
  #                                                                                         - tabella x ROC su test
  #                                                                                         - pvalue train, pval test, misura di performance train e test
  #                                          - "$final.model"    : lista che contiene: - conteggio numero di pazienti y=1 e y=0 nel training set
  #                                                                                    - modello appreso su tutto il training set e le sue performance di accuratezza
  #                                          - "$mat.perf.total" : matrice con performance riassunte
  #===========================================================

  Predictive.model<-function(eventStart, eventGoal, arr.attributes, obj.out, arr.ID.train, arr.ID.test=c(), feature.selection=TRUE, k=1, p.train=0.7,
                             p.thr=0.05, n.att=2, n.digit.out=4, passing=c(), NOTpassing=c(), max.time=Inf, min.time=0, UM="days", pred.disc=FALSE,
                             arr.cand=c(), oversamp.tec="bootstrap", perf.ind="acc",acc.ind="accuracy",omit.missing=F,ev.param="acc",thr.acc=0.5){

    check.flag<-TRUE

    #creo oggetto QOD che mi aiuterà a ricostuire le classi di appartenenza dei pazienti
    obj.QOD<- pMineR::QOD(UM = UM )
    obj.QOD$loadDataset(dataList = obj.out)

    if(pred.disc){
      #caso predictive pd: id.1=paz evGoal, id0=tutti gli altri paz che transitano in evStart
      id.1<-obj.QOD$query(from = eventStart,to = eventGoal,time.range = c(min.time,max.time),step.range = c(1,1))
      arr.cand<-arr.cand[-which(arr.cand==eventGoal)]

      id.0<-unlist(lapply(arr.cand, function(ev.to){
        if(param.verbose) print(paste("query ev.to:",ev.to))
        return(obj.QOD$query(from = eventStart,to = ev.to, time.range = c(min.time,max.time),step.range = c(1,1)))}))
      }else{
        #caso predictive model
        id.1<-obj.QOD$query(from = eventStart,to = eventGoal,arr.passingThrough = passing,arr.NOTpassingThrough = NOTpassing,time.range = c(min.time,max.time))
        id.0<-obj.QOD$query(from = eventStart,to = "END",arr.NOTpassingThrough = c(eventGoal,NOTpassing),arr.passingThrough = passing, time.range = c(min.time,max.time))
      }

    #check output query fun:
    #controllo che gli id1 e id0 non siano nulli e che id1 siano almeno un TOT (quanto deve valere questo tot? probabilmente almeno quanto il K dello smote)

    if(length(which(is.na(id.1)))>1 || length(which(is.na(id.0)))>1 || length(id.1)<3){
      check.flag<-FALSE
      df_tot<-NULL

    }else{

      #query ha restituito degli id
      ID<-c(id.1,id.0)

      #creo il data set utile per il calcolo del modello:
      #dunque un data set che associa ad ogni ID il valore delle covariate (registrate all'istante temporale in cui avviene nodeStart)
      tmp<-lapply(ID, function(id){
        att.val<-search_value(sub.path=obj.out$pat.process[[id]],eventStart,arr.attributes)
        if(id %in% id.1){
          y.val<-1
        }else{
          y.val<-0
        }
        return(cbind(id,att.val,y.val))
      })

      df_tot<-as.data.frame(do.call("rbind",tmp))
      colnames(df_tot)<-c("ID",arr.attributes,"y")


      #solo covariate numeriche
      for (i in c(1:length(arr.attributes))) {
        df_tot[,arr.attributes[i]]<-as.numeric(df_tot[,arr.attributes[i]])
      }

      df_tot$y<-as.factor(df_tot$y)

      #richiamo fun per gestione missing values
      if(omit.missing){
        df_tot<-na.omit(df_tot)
      }else{
        df_tot<-remove.miss.col(df_tot)$df.clean.col
      }

      arr.attributes<-colnames(df_tot)[-which(colnames(df_tot) %in% c("ID","y"))]

      if(n.att==Inf | n.att>length(arr.attributes)) n.att<-length(arr.attributes)
    }


    #check righe di df_toto (post na.omit): se OK allora passo alla suddivisione in train e test
    if(is.null(df_tot) || nrow(df_tot[which(df_tot$ID %in% id.1),])<=3 || is.na(arr.attributes)){
      check.flag<-FALSE
      data_tot<-NULL
      to_ret<-list("res"=NULL,"run.check"=check.flag)
    }else{
      #TRAIN
      df_train <-df_tot[which(df_tot$ID %in% arr.ID.train),]

      #check sullo sblinaciamento delle classi:
      #prima controllo che il numero di pazienti sia superiore almeno a 3 (PARAMATRIZZARE!!!)
      #poi controllo che il numero di pazienti y=0 sia superiore al 20% dei paz in classe Y=1:
      #se questo non succede, applico tecnica di oversampling
      if(table(df_train$y)[2]>3 & table(df_train$y)[1]>3){
        if(table(df_train$y)[2]<round(nrow(df_train)*0.2)){
          #calcolo di quanto deve aumentare la classe di minoranza affichè numero di 1 sia >20% tot
          switch (oversamp.tec,
                  "bootstrap" = {
                    # unders_size<-round(sum(table(df_train$y))*0.2)-table(df_train$y)[2]
                    unders_size<-round(0.25*table(df_train$y)[1]-table(df_train$y)[2])
                    df1<-df_train[sample(x =  which(df_train$y==1), size = unders_size,replace = T ),]
                    # df1<-df_train[which(df_train$y ==1),]
                    df_train<-rbind(df_train,df1)
                    df_train<-df_train[,-1]
                  },
                  "smote"={
                    d_size<-round(0.25*(table(df_train$y)[1]/table(df_train$y)[2]))
                    smote <- SMOTE(df_train[,-c(1,ncol(df_train))], df_train$y,dup_size = d_size,K = 3)
                    df_train<-smote$data
                    colnames(df_train)[length(colnames(df_train))]<-"y"
                    df_train$y<-as.factor(df_train$y)
                  },
                  "none"={
                    df_train<-df_train[,-1]
                  }
                  )
        }else{
          df_train<-df_train[,-1]
        }

        #TRAIN:
        #potrebbe essere che non venga esplicitato un set di test
        if(is.null(arr.ID.test)){
          df_test<-data.frame()
        }else{

          df_test<-df_tot[which(df_tot$ID %in% arr.ID.test),]
          df_test<-df_test[,-1]
          # if(table(df_test$y)[2]<nrow(df_test)*0.1){
          #   d_size<-round(0.25*(table(df_test$y)[1]/table(df_test$y)[2]))
          #   smote <- SMOTE(df_test[,-c(1,ncol(df_test))], df_test$y,dup_size = d_size,K = 3)
          #   df_test<-smote$data
          #   colnames(df_test)[length(colnames(df_test))]<-"y"
          #   df_test$y<-as.factor(df_test$y)
          # }else{
          #   df_test<-df_test[,-1]
          # }
        }


        if(feature.selection){

          lst.model<-lapply(1:k, function(ind.fold.test){
            if(param.verbose){
              print(paste("k fold:",ind.fold.test))
            }

            #suddivido il TRAINNG in TRAIN e VALIDATION
            p.strat<-round(table(df_train$y)[2]/sum(table(df_train$y)),digits = 2)
            continue<-TRUE
            while (continue) {
              ind.train_set<-sample(x=rownames(df_train),size = nrow(df_train)*p.train)
              train_set<-df_train[which(rownames(df_train) %in% ind.train_set),]
              test_set <-df_train[-which(rownames(df_train) %in% ind.train_set),]
              # if(param.verbose){print(table(train_set$y))}

              perc.train<-table(train_set$y)[2]/sum(table(train_set$y))
              perc.test<-table(test_set$y)[2]/sum(table(test_set$y))

              # fatto check sulla stratificazione (((perc.test>=(p.strat-0.1)) & (perc.test<=(p.strat+0.05))))
              if( ((perc.train>=(p.strat-0.1)) & (perc.train<=(p.strat+0.1))) & (perc.test>0 & perc.test<1) ) continue<-FALSE
            }

            if(param.verbose){
              print(paste("------>",table(train_set$y),"<------------"))
              print(ind.train_set)
              print("*******START STEPWISE**************")
            }


            #applico stepwise per la scelta delle covariate:
            #ciclo un numero di volte pari ad n.att sulle covariate che si vogliono inserire nel modello.
            #alla prima iterata calcolo un modello con una sola covariata e dopo averle provate tutte, scelgo la covariata con p value più basso
            #alla seconda itarata calcolo un nuovo modello composto da covariata scelta all'iterata precedente e ciascuna delle altre rimaste.
            #scelgo come seconda covariata quella che garantisca che il pvalue della prima cov non scenda sotto il valore soglia (es.0.05) e con il pval più basso
            #... vado vanti un numero di volte pari ad n.att
            chosen.att<-c()
            lst.stepwise<-lapply(1:n.att, function(att){
              if(param.verbose){
                print(paste("n att:",att, "for fold test:", ind.fold.test))
              }


              if(is.null(chosen.att) || is.na(chosen.att)){
                arr.att<-arr.attributes
              }else{
                arr.att<-arr.attributes[-which(arr.attributes %in% chosen.att)]
              }


              lst.perf<-list()
              pval<-list()
              AUC<-c()
              roc_test<-list()

              for (i in c(1:length(arr.att))) {
                df.train<-subset(train_set,select = c(c(chosen.att,arr.att[i]),"y"))
                df.test<-subset(test_set, select = c(c(chosen.att,arr.att[i]),"y"))
                lst.perf[[i]]<-train.FOMM.LR(df_train = df.train,df_test = df.test, chosen.digit = 4,perf.train = TRUE,ev.param = ev.param,thr.acc)

                pval[[i]]<-lst.perf[[i]]$pval_train
                AUC[i]<-lst.perf[[i]]$AUC_test
                roc_test[[i]]<-lst.perf[[i]]$roc_test[which(lst.perf[[i]]$roc_test$accuracy==max(lst.perf[[i]]$roc_test$accuracy)),]
                names(AUC)[i]<-paste0(i,"-",arr.att[i])
              }

              pval_tot<-do.call('rbind',pval)
              roc_test_all<-do.call('rbind',roc_test)

              if(ncol(pval_tot)==1){
                ind.pval.ok<-which(pval_tot<=p.thr)
              }else{
                mat_log<-pval_tot<=p.thr

                for(j in c(1:(ncol(pval_tot)-1))){
                  new_col<-"&"(mat_log[,1],mat_log[,2])
                  mat_log<-mat_log[,-c(j,j+1)]
                  mat_log<-cbind(mat_log,new_col)
                }
                ind.pval.ok<-which(new_col)
              }

              if(identical(ind.pval.ok,integer(0))){
                #caso in cui non ho trovato righe con pvalue su train <p.thr
                chosen.att<<-c(chosen.att,NULL)
                if(param.verbose){
                  print("non ci sono p bassi")
                }
                to_ret<-NULL
              }else{
                AUC.tmp<-AUC
                AUC.tmp[-ind.pval.ok]<-0
                ind.chosen<-which(AUC.tmp==max(AUC.tmp))[1]
                chosen.att<<-c(chosen.att,arr.att[ind.chosen])
                if(param.verbose){
                  print(paste("currente chosen:",chosen.att))
                }
                to_ret<-list("model_perf"=lst.perf[[ind.chosen]],
                             "chosen_att"=chosen.att)
              }

              return(to_ret)

            })

            if(param.verbose){
              print("***********END STEPWISE***************")
            }




            names(lst.stepwise)<-paste0("att_",as.character(seq_along(1:n.att)))

            if(length(which(lengths(lst.stepwise)==0))>0){
              lst.stepwise<-lst.stepwise[-which(lengths(lst.stepwise)==0)]
            }

            if(length(lst.stepwise)==0){
              lst.stepwise.final<-list()
              to_ret<-NULL
            }else{
              lst.stepwise.final<-lst.stepwise[[length(lst.stepwise)]]

              if(k!=1){
                # fold.to.compute<-c(1:k)[-which(c(1:k) %in% ind.fold.test)]
                AUC.all.fold<-lapply(1:(k-1), function(fold.comp.test){
                  continue<-TRUE
                  while (continue) {
                    ind.train_set<-sample(x=rownames(df_train),size = nrow(df_train)*p.train)
                    train_set<-df_train[which(rownames(df_train) %in% ind.train_set),]
                    test_set <-df_train[-which(rownames(df_train) %in% ind.train_set),]
                    # if(param.verbose){print(table(train_set$y))}

                    perc.train<-table(train_set$y)[2]/sum(table(train_set$y))
                    perc.test<-table(test_set$y)[2]/sum(table(test_set$y))

                    # fattto check sulla stratificazione
                    if( ((perc.train>=(p.strat-0.1)) & (perc.train<=(p.strat+0.1))) & (perc.test>0 & perc.test<1)  ) continue<-FALSE

                  }

                  df.train<-subset(train_set,select = c(lst.stepwise.final$chosen_att,"y"))
                  df.test<-subset(test_set, select = c(lst.stepwise.final$chosen_att,"y"))
                  fold.to.check.mod <-train.FOMM.LR(df.train,
                                                    df.test,
                                                    chosen.digit = 4,
                                                    perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)

                  ret_list<-list("model_perf" =fold.to.check.mod,
                                   "class count"=table(test_set$y))



                  return(ret_list)
                })

                names(AUC.all.fold)<-paste0("test_fold",seq_along(1:k)[-ind.fold.test])
                to_ret<-list()
                comp_fold_name<-paste0("test_fold",ind.fold.test)
                to_ret[[comp_fold_name]]<-lst.stepwise.final
                for (i in c(1:length(names(AUC.all.fold)))) {
                  to_ret[[names(AUC.all.fold)[i]]]<-AUC.all.fold[[names(AUC.all.fold)[i]]]
                }

              }else{
                # final.perf <-train.FOMM.LR(df_train = rbind(df.train,df.test),
                #                            chosen.digit = 4, perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)

                to_ret<-lst.stepwise.final
              }
            }

            return(to_ret)
          })

          names(lst.model)<-paste0("test.fold_",as.character(seq_along(1:k)))

          # addestramento final model (modello costruito su tutto il training set con le covariate selzionate)

          if(length(which(lengths(lst.model)>0))>0){

            lst.model<-lst.model[which(lengths(lst.model)>0)]

            #CASO HOLD OUT:
            if(k==1){
              best.att<-lst.model$test.fold_1$chosen_att
              best_acc_ind<-which(lst.model$test.fold_1$model_perf$roc_test$accuracy==max(lst.model$test.fold_1$model_perf$roc_test$accuracy))
              mat.att<-matrix(ncol = length(best.att)+3)
              mat.att[1,]<-c(best.att,
                             lst.model$test.fold_1$model_perf$AUC_test,
                             lst.model$test.fold_1$model_perf$roc_test$threshold[best_acc_ind][1],
                             lst.model$test.fold_1$model_perf$roc_test$accuracy[best_acc_ind][1])
              colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(best.att))),"AUC.on.test.fold","threshold","accuracy")
              arr.acc<-NULL
              arr.AUC<-NULL
              mat.att.total<-mat.att

              if(nrow(df_test)==0){
                final.model<-train.FOMM.LR(df_train[,c(best.att,"y")],
                                            chosen.digit = 4,
                                            perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
              }else{
                final.model<-train.FOMM.LR(df_train[,c(best.att,"y")],
                                            df_test[,c(best.att,"y")],
                                            chosen.digit = 4,
                                            perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
              }

              final.models<-list("final.model"=final.model,
                   "count_train"=table(df_train$y),
                   "count_test"=nrow(df_test)
                   )

            #CASO CROSS FOLD
            }else{

              best.att<-NULL
              add.col<-3
              mat.att<-matrix("",nrow = length(lst.model),ncol = n.att+add.col)
              mat.att.total<-matrix("",nrow = length(lst.model),ncol = n.att+4)


              for (i in c(1:length(lst.model))) {
                chosen.att<-lst.model[[i]][[1]][[2]]
                arr.AUC<-lapply(lst.model[[i]],function(inner.fold) {
                  #QUI METTO CONDIZIONI SE VOGLIO UN ALTRO SCORE DIVERSO DA AUC.MEAN
                  return(inner.fold$model_perf$AUC_test)
                })

                arr.acc<-lapply(lst.model[[i]],function(inner.fold){
                  #accuratezza cambiare qui
                  if(!is.null(inner.fold$model_perf)){

                    #metrica di accuratezza per il calcolo del migliore dei k modelli allenati nella cross fold (viene scelto quello con il parametro di accuratezza maggiore)
                    switch (acc.ind,
                            "accuracy" = {
                              max.acc<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                              ret<-c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])},

                            "b.accuracy"={
                              max.acc.test<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                              max.acc.train<-max(inner.fold$model_perf$roc_train$accuracy,na.rm = T)

                              ret<-c(max.acc.test*0.632+max.acc.train*0.368,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])
                            }
                    )
                    # max.acc<-max(inner.fold$model_perf$roc_test$accuracy,na.rm = T)
                    # ret<-c(max.acc,inner.fold$model_perf$roc_test$threshold[which(inner.fold$model_perf$roc_test$accuracy==max.acc)])
                  }else{
                    ret<-NULL
                  }
                  return(ret)
                })


                if(length(chosen.att)<(ncol(mat.att)-add.col)){
                  ind.diff<-(ncol(mat.att)-add.col)-length(chosen.att)
                  riga<-c(chosen.att,
                          rep("",ind.diff),
                          arr.AUC[[1]],
                          arr.acc[[1]][2],
                          arr.acc[[1]][1]
                  )
                  riga.total<-c(chosen.att,
                                rep("",ind.diff),
                                round(mean(unlist(arr.AUC)),digits = n.digit.out),
                                round(sd(unlist(arr.AUC)),digits = n.digit.out),
                                round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
                                round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out))
                }else{
                  riga<-c(chosen.att,
                          arr.AUC[[1]],
                          arr.acc[[1]][2],
                          arr.acc[[1]][1])
                  riga.total<-c(chosen.att,
                                round(mean(unlist(arr.AUC)),digits = n.digit.out),
                                round(sd(unlist(arr.AUC)),digits = n.digit.out),
                                round(mean(unlist(lapply(arr.acc, function(i){return(i[[1]])}))),digits = n.digit.out),
                                round(sd( unlist(lapply(arr.acc, function(i){return(i[[1]])}))) ,digits = n.digit.out))
                }
                mat.att[i,]<-riga
                mat.att.total[i,]<-riga.total
              }

              colnames(mat.att)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.on.test.fold","threshold","accuracy")
              colnames(mat.att.total)<-c(paste0("covariate_",seq_along(1:n.att)),"AUC.mean.folds","AUC.sd.folds","acc.mean.folds","acc.sd.folds")
              col_to_del<-lapply(1:(ncol(mat.att.total)-add.col-1), function(colonna){
                if(length(which(mat.att.total[,colonna]==""))==nrow(mat.att.total)) to_del<-colonna
              })


              if(!is.null(unlist(col_to_del))) mat.att.total<-mat.att.total[,-unlist(col_to_del)]

              final.models<-lapply(1:length(lst.model), function(i){
                if(nrow(df_test)==0){
                  final<-train.FOMM.LR(df_train[,c(lst.model[[i]][[1]][[2]],"y")],
                                       chosen.digit = 4,
                                       perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
                }else{
                  final<-train.FOMM.LR(df_train[,c(lst.model[[i]][[1]][[2]],"y")],
                                       df_test[,c(lst.model[[i]][[1]][[2]],"y")],
                                       chosen.digit = 4,
                                       perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)}

                return(list("final.model"=final,
                            "count_train"=table(df_train$y),
                            "count_test"=nrow(df_test)))
              })

              names(final.models)<-paste0("model",seq_along(1:length(lst.model)))
            }

          }else{
            check.flag<-FALSE
            return(list("res"=NULL,
                        "run.check"=check.flag))
          }

          lst_to_ret<-list("final.model"=final.models,
                           "lst.models.fold"=lst.model,
                           "mat.perf.total"=mat.att.total)
          to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)

        }else{
          if(length(arr.ID.test)>1){
            dfTest<-df_test[,c(arr.attributes,"y")]
          }else{
            dfTest<-data.frame()
          }
          final.model<-train.FOMM.LR(df_train = df_train[,c(arr.attributes,"y")],
                                     df_test = dfTest,
                                     chosen.digit = 4,
                                     perf.train = TRUE,ev.param = ev.param,thr.acc = thr.acc)
          mat.att<-matrix(ncol = length(arr.attributes)+3)
          if(length(arr.ID.test)>1){

            mat.att[1,]<-c(arr.attributes,
                           final.model$AUC_test,
                           final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"accuracy"][1],
                           final.model$roc_test[which(final.model$roc_test$accuracy==max(final.model$roc_test$accuracy)),"threshold"][1]
            )
            colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.test.fold","threshold","accuracy")

          }else{
            mat.att[1,]<-c(arr.attributes,
                           final.model$AUC_train,
                           final.model$roc_train[which(final.model$roc_train$accuracy==max(final.model$roc_train$accuracy)),"accuracy"][1],
                           final.model$roc_train[which(final.model$roc_train$accuracy==max(final.model$roc_train$accuracy)),"threshold"][1]
            )
            colnames(mat.att)<-c(paste0("covariate_",seq_along(1:length(arr.attributes))),"AUC.on.train.fold","accuracy","threshold")
          }
          arr.acc<-NULL
          arr.AUC<-NULL
          mat.att.total<-mat.att




          lst_to_ret<-list("final.model"=final.model,
                           "lst.models.fold"=NULL,
                           "mat.perf.total"=mat.att.total)
          to_ret<-list("res"=lst_to_ret,"run.check"=check.flag)
        }

      }else{
        to_ret<-list("res"=NULL,"run.check"=FALSE)
      }
    }

    return(to_ret)

  }

  #===========================================================
  # predictiveProcessDiscovery: funzione che permette di calcolare per ogni nodo, il modello sui suoi archi
  # INPUT: nodeStart         <- nodo sui cui archi si vuole calcolare il modello
  # per i restanti input vedere compute.gen.perf function
  #
  #===========================================================

  Pred.mod.node<-function(arr.ID.train,nodeStart,feature.selection=T,k=5,arr.att,n.att,obj.out,p.thr=0.05,omit.missing=F,vitro.test=F,ev.param="acc",thr.acc=0.5,oversamp.tec="none"){
    #estraggo i nodi figli del nodo start a partire dalla MMatrix
    candidates<-colnames(obj.out$MMatrix)[which(obj.out$MMatrix[nodeStart,]!=0)]

    #se il nodo ha più di un possibile nodo figlio passo a calcolare il modello per ogni arco
    if(length(candidates)>1){
      lst.pred.mod<-lapply(candidates,function(eventGoal){
        if(param.verbose) print(eventGoal)
        #per ciascun nodo figlio di nodeStart (cioè per ogni elemento di candidates) calcolo modello:
        first.out<-Predictive.model(eventStart = nodeStart,eventGoal = eventGoal,
                                        obj.out = obj.out,arr.attributes = arr.att,
                                        arr.ID.train = arr.ID.train,
                                        arr.ID.test = c(),
                                        feature.selection = feature.selection,k = k,n.att = n.att,pred.disc = T,arr.cand = candidates,p.thr = p.thr,
                                        omit.missing = omit.missing,ev.param=ev.param,thr.acc=thr.acc,oversamp.tec = oversamp.tec)

        if(first.out$run.check){
          #modifico qui se voglio cambiare metrica per best model: nrow(first.out$res$mat.perf.total)==1 !!!!!!!
          if(!is.matrix(first.out$res$mat.perf.total) || !feature.selection){
            # to_ret<-first.out$res$final.model[[1]]
            to_ret<-first.out$res$final.model
            if(vitro.test){
              to_ret<-list("best_model"=to_ret,"performances"=first.out$res$mat.perf.total)
            }
          }else if(nrow(first.out$res$mat.perf.total)==1){
            to_ret<-first.out$res$final.model
          }else{
            ind.best.model<-which(first.out$res$mat.perf.total[,"AUC.mean.folds"]==max(as.numeric(first.out$res$mat.perf.total[,"AUC.mean.folds"])))[1]
            to_ret<-first.out$res$final.model[[names(first.out$res$final.model)[ind.best.model]]]
            if(vitro.test){
              to_ret<-list("best_model"=to_ret,"performances"=first.out$res$mat.perf.total)
            }
          }

          }else{
            to_ret<-obj.out$MMatrix.perc[nodeStart,eventGoal]
            #caso in cui l'allenamento del modello non è andato a buon fine
          }
        return(to_ret)
        })
      names(lst.pred.mod)<-candidates
      return_val<-lst.pred.mod

      #se nodoStart ha un solo figlio allora restituisco in output solo la probabilità con cui dal nodo Start passo al nodo figlio
      }else if(length(candidates)==1){
        return_val<-obj.out$MMatrix.perc[nodeStart,candidates]

        #se nodoStart non ha figli allora restituisco "END"
        }else{
          return_val<-"END"
        }

    return(return_val)
  }

  #===========================================================
  # Process DIscovery all nodes
  #===========================================================

  Predictive.PD<-function(arr.ID.train,feature.selection=T,k=5,arr.att,n.att,obj.out,ev.param="acc",thr.acc=0.5){
   all.node.mod<-lapply(row.names(obj.out$MMatrix), function(nodeStart){
     if(param.verbose) print(paste("EVENT START:",nodeStart))
     Pred.mod.node(arr.ID.train = arr.ID.train, nodeStart = nodeStart, feature.selection = feature.selection, k = k,arr.att = arr.att ,n.att = n.att, obj.out = obj.out,ev.param = ev.param,thr.acc = thr.acc)
   })
   names(all.node.mod)<-row.names(obj.out$MMatrix)
  }

  #===========================================================
  # Gradient descent function
  #===========================================================

  grad.desc.fun<-function(arr.ID.test,nodeStart,obj.out,alpha.min=0.1,alpha.max=1,alpha.step=0.01, models, find.alpha=T,alpha.ratio=c(),comp.acc=TRUE,ev.param="acc",feat.flag=T){
    tmp<-lapply(arr.ID.test, function(id){
      print(id)
      sub.path=obj.out$pat.process[[id]]
      ind.eventStart<-which(sub.path[,obj.out$csv.EVENTName]==nodeStart)
      if(length(ind.eventStart)){

        #caso in cui il paz di test sperimenta l'evento di start
        #inizio calcolando il suo y vero
        next.ev<-sub.path[,obj.out$csv.EVENTName][ind.eventStart+1]
        for(i in c(1:length(next.ev))){
          if(is.na(next.ev[i]) & (ind.eventStart[i]+1)>nrow(sub.path)) next.ev[i]<-"END"
        }


        lst.mat<-lapply(1:length(next.ev),function(ind.ev){
          ev<-next.ev[ind.ev]
          pred.mat<-matrix(NA,ncol = 3,nrow = length(names(models)))
          row.names(pred.mat)=names(models)
          colnames(pred.mat)=c("y","y_prob","perf")
          tmp<-lapply(1:nrow(pred.mat), function(riga){
            print(riga)

            mod<-models[[rownames(pred.mat)[riga]]]
            #caso in cui non ho appreso nessun modello
            if(length(mod)==1){
              pred<-MMatrix.perc[nodeStart,rownames(pred.mat)[riga]]
              mat.row<-c(ev,pred,pred)

              #in cui ho diretto final model
            }else if(length(mod)==30){
              model.cov<-names(mod$coefficients)[-1]
              att.val<-as.data.frame(search_value(sub.path=obj.out$pat.process[[id]],nodeStart,model.cov)[ind.ev,])
              colnames(att.val)<-model.cov
              if(length(which(is.na(att.val)))){
                pred<-MMatrix.perc[nodeStart,rownames(pred.mat)[riga]]
                mat.row<-c(ev,pred,pred)
              }else{
                pred<-predict.glm(mod,newdata = att.val,type = "response")
                names(pred)=NULL
                #aggiungo controllo metrica
                switch (ev.param,
                        "acc" = {
                          mat.row<-c(ev,pred,"mod$final.model$roc_train$accuracy[1]")
                        },
                        "AUC"={
                          mat.row<-c(ev,pred,"mod$final.model$AUC_train")
                        }
                )

              }

              }else{
                if(!feat.flag){
                  model.cov<-names(mod$pval_train)
                  att.val<-as.data.frame(search_value(sub.path=obj.out$pat.process[[id]],nodeStart,model.cov)[ind.ev,])
                  colnames(att.val)<-model.cov

                  if(length(which(is.na(att.val)))){

                    pred<-MMatrix.perc[nodeStart,rownames(pred.mat)[riga]]
                    mat.row<-c(ev,pred,pred)
                  }else{
                    pred<-predict.glm(mod$model,newdata = att.val,type = "response")
                    names(pred)=NULL
                    #aggiungo controllo metrica
                    switch (ev.param,
                            "acc" = {
                              mat.row<-c(ev,pred,mod$roc_train$accuracy[1])
                            },
                            "AUC"={
                              mat.row<-c(ev,pred,mod$AUC_train)
                            }
                    )

                  }
                }else{
                  model.cov<-names(mod$final.model$pval_train)
                  att.val<-as.data.frame(search_value(sub.path=obj.out$pat.process[[id]],nodeStart,model.cov)[ind.ev,])
                  colnames(att.val)<-model.cov
                  if(length(which(is.na(att.val)))){

                    pred<-MMatrix.perc[nodeStart,rownames(pred.mat)[riga]]
                    mat.row<-c(ev,pred,pred)
                  }else{
                    pred<-predict.glm(mod$final.model$model,newdata = att.val,type = "response")
                    names(pred)=NULL
                    #aggiungo controllo metrica
                    switch (ev.param,
                      "acc" = {
                        mat.row<-c(ev,pred,mod$final.model$roc_train$accuracy[1])
                      },
                      "AUC"={
                        mat.row<-c(ev,pred,mod$final.model$AUC_train)
                      }
                    )

                  }

                }
            }
            pred.mat[riga,]<<-mat.row
          })
          return(pred.mat)
        })

        to_ret<-list("pred.mat"=lst.mat,"y"=next.ev)
      }else{
        to_ret<-NULL
      }
      return(to_ret)
    })

    names(tmp)<-arr.ID.test
    if(length(which(lengths(tmp)==0))>0){
      lst.ID.test<-tmp[-which(lengths(tmp)==0)]
    }else{
      lst.ID.test<-tmp
    }



    if(length(lst.ID.test)>1){
      if(find.alpha){
        #costruisco la matrice degli alpha
        arr.ratio <- seq(alpha.min, alpha.max, by = alpha.step)
        arr.a1 <- rep(1,length(arr.ratio))
        arr.a2 <- arr.a1 * arr.ratio
        y<-arr.a1 #cost
        x<-arr.a2

        MM<-matrix(NA,ncol = length(x),nrow = 1)
        colnames(MM)<-x
        rownames(MM)<-y[1]

        tmp<-lapply(1:ncol(MM), function(colonna){
          alpha2<-as.numeric(colnames(MM)[colonna])
          MM[1,colonna]<<-pred.accuracy.fun(alpha1 = arr.a1[1],alpha2 = alpha2 ,lst.ID.test)

        })

        # graphics::plot(x = arr.ratio,y = MM[1,],type = "l",xlab = "alpha ratio",ylab = "accuracy")
        best_ratio<-colnames(MM)[which(MM[1,]==max(MM[1,]))][1]
        ret_val<-best_ratio
      }else{
        if(!is.na(alpha.ratio)){
          ret_val<-pred.accuracy.fun(alpha1 = 1,alpha2 = alpha.ratio ,lst.ID.test,comp.acc)
        }else{
          ret_val<-NA
        }

      }

    }else if(length(lst.ID.test)==1 & !comp.acc){
      ret_val<-pred.accuracy.fun(alpha1 = 1,alpha2 = alpha.ratio ,lst.ID.test,comp.acc)
    }else{
      ret_val<-NA
    }

    return(ret_val)

  }

  #===========================================================
  # Compute.gen.perf: train global model
  # INPUT: arr.ID <- patient ID array
  #        prop.train <- proportion of training set
  #        prop.test  <- proportion of test set
  #        prop.param <- proportion of arr.ID used for hyper-param
  #        k          <- number of fold for cross fold val (if k=1--> hold out)
  #        arr.att    <- arr of attribute to consider as covariate
  #        n.att      <- numero massimo di covariate da usare nel modello
  #        grid_search=c(0.01,0.05,0.1,0.15) <- grid search x i pvalue
  #        omit.missing=F,    <- uso na.omit per dati mancanti S/N
  #        p.val.digit=4,     <- numero di cifre decimali pvalue
  #        AUC.digit=3,       <- numero di cifre decimali AUC
  #        best.perf=F,       <- flag per farsi restituire in output le performance dei migliori archi
  #        n.best.arc=10      <- se best.arc=T decido di quanti "best archi" farmi restituire le performance
  #        ev.param="acc"     <- tipo di indicatore di performace da utilizzare (acc or AUC)
  #        thr.acc=0.5        <- soglia per il calcolo dell'accuratezza
  #        oversamp.tec="none"<- tecnica di over sampling da applicare in caso di dati sbilanciati
  #
  #        HYPER-PARAM INPUT:
  #        alpha.min=0.01     <-minimo valore del parametro alpha
  #        alpha.max=1,       <- massimo val di alpha
  #        alpha.step=0.01,   <- step con cui varia alpha nella grid search degli hyper param
  #===========================================================

  compute.gen.perf<-function(arr.ID,prop.train,prop.test,prop.param,feature.selection=T,k=5,arr.att,n.att,obj.out,alpha.min=0.01,alpha.max=1,alpha.step=0.01, grid_search=c(0.01,0.05,0.1,0.15),omit.missing=F,p.val.digit=4, AUC.digit=3,best.perf=F,n.best.arc=10,ev.param="acc",thr.acc=0.5,oversamp.tec="none"){

    #Suddividiamo la popolazione in training-test-trainging parametri
    n.train.param<-length(arr.ID)*prop.param
    n.train.set<-length(arr.ID)*prop.train
    n.test.set<-length(arr.ID)*prop.test


    arr.ID.tot<-as.character(sample(arr.ID))
    arr.ID.train<-sample(arr.ID.tot,size = n.train.set)
    train.param<-sample(arr.ID.tot[-which(arr.ID.tot %in% arr.ID.train)],size = n.train.param)
    test.final<-arr.ID.tot[-(which(arr.ID.tot %in% c(train.param,arr.ID.train)))]


    # train.param<-sample(arr.ID[-which()],size = length(arr.ID.train)*0.3)
    # arr.ID.train<-arr.ID.train[-which(arr.ID.train %in% train.param)]
    # test.final<-arr.ID.test

    # test.hyper.param<-sample(arr.ID.test,size = length(arr.ID.test)*0.7)
    # test.param<-sample(test.hyper.param,size = length(test.hyper.param)*0.5)
    # test.final<-test.hyper.param[-which(test.hyper.param %in% test.param)]
    # test.val<-arr.ID.test[-which(arr.ID.test %in% c(test.param,test.final))]


    all.events<-rownames(obj.out$MMatrix)[-which(rownames(obj.out$MMatrix) %in% c("BEGIN","END"))]

    #per ciasun evento viene calcolato il modello sui suoi archi
    tmp<-lapply(all.events, function(nodeStart){
      if(param.verbose) print(paste("analyzing node:",nodeStart))

      #per ogni valore soglia di p-value esplicitato nella grid search
      tmp.grid<-lapply(grid_search, function(p.thr){
        if(param.verbose) print(paste("analyzing node:",nodeStart, "p.thr",p.thr))

        lst.pred<-Pred.mod.node(arr.ID.train = arr.ID.train,nodeStart = nodeStart,feature.selection = feature.selection,k = k,
                                arr.att = arr.att,n.att = n.att,obj.out = obj.out,p.thr = p.thr,omit.missing = omit.missing,ev.param = ev.param,thr.acc = thr.acc,oversamp.tec = oversamp.tec)
        if(length(lst.pred)==1 || lst.pred=="END" ||  length(which(lengths(lst.pred)==1))==length(lst.pred)){
          acc_val<-NULL
          lst.pred<-NULL
          alpha.ratio<-NULL
        }else{
          alpha.ratio<-grad.desc.fun(train.param,nodeStart,obj.out,alpha.min=alpha.min,alpha.max=alpha.max,alpha.step=alpha.step, models= lst.pred,ev.param = ev.param,feat.flag=feature.selection)
          acc_val<-grad.desc.fun(test.final,nodeStart,obj.out, models= lst.pred,find.alpha = F,alpha.ratio = as.numeric(alpha.ratio),ev.param = ev.param,feat.flag=feature.selection)
          # acc_val<-cbind(acc_val,acc_val_tmp)
        }
        return(list("list.pred"=lst.pred,"acc_val"=acc_val,"p.val.thr"=p.thr,"alpha.ratio"=alpha.ratio))
      })


      names(tmp.grid)<-paste0("p.thr.",seq_along(1:length(grid_search)))
      return(tmp.grid)
    })


    names(tmp)=all.events
    # save(tmp,file = "C:/Users/maria/Desktop/salvataggio_parziale.Rdata")
    # save(test.final,file = "C:/Users/maria/Desktop/test_final_parziale.Rdata")
    # save(train.param,file = "C:/Users/maria/Desktop/test_param_parziale.Rdata")
    # return(tmp)
    lst.local.report<-report.fun(list.model = tmp,level = "local",grid_search =grid_search,p.val.digit=p.val.digit, AUC.digit=AUC.digit,obj.out = obj.out)
    global.report<-report.fun(list.model = tmp,level = "global",grid_search =grid_search,p.val.digit=p.val.digit, AUC.digit=AUC.digit,obj.out = obj.out,test.final=test.final)
    # performances<-validation.fun(arr.ID.val=test.val,obj.out=obj.out,lst.local.rep=lst.local.report, global.rep=global.report, grid_search=grid_search, lst.model=tmp)
    ret_list<-list("model.list"=tmp,"local.rep"=lst.local.report,"global.rep"=global.report,"perf"=NULL)


    if(best.perf){
      best.arc.tab<-c()
      tmp<-lapply(lst.local.report, function(tab.local.rep){
        if(!is.null(tab.local.rep)){
          print(tab.local.rep[1,1])
          tab<-tab.local.rep[which(tab.local.rep[,"model"]!="used FOMM"),]
          if(is.null(nrow(tab))){
            best.arc.tab<<-rbind(best.arc.tab,tab)
          }else{
            if(nrow(tab)>0 & nrow(tab)<=n.best.arc){
              best.arc.tab<<-rbind(best.arc.tab,tab)
            }else if(nrow(tab)>n.best.arc){
              AUC.vec<-sort(tab[,"AUC"],decreasing = T)
              if(length(AUC.vec)<n.best.arc){
                max.len<-length(AUC.vec)
              }else{
                max.len<-n.best.arc
              }
              best.arc.tab<<-rbind(best.arc.tab,tab[names(AUC.vec)[1:max.len],])
            }

          }



        }


      })
      rownames(best.arc.tab)<-NULL
      ret_list<-list("model.list"=tmp,"local.rep"=lst.local.report,"global.rep"=global.report,"best.tran"=best.arc.tab)
    }




    return(ret_list)
  }

  #===========================================================
  # Report.fun: generate report
  #===========================================================

  report.fun<-function(list.model,level="local",grid_search,p.val.digit=4,AUC.digit=3,obj.out,test.final=c()){
    tmp<-list.model

    ### CONTROLLARE BENE BEST.THR ACCURATEZZE

    best.thr<-lapply(tmp, function(lst.nodeStart){
      lst.acc<-lapply(lst.nodeStart, function(val){return(val$acc_val)})
      lst.acc[sapply(lst.acc, is.null)] <- NA
      acc<-unlist(lst.acc)
      # acc<-unlist(lapply(lst.nodeStart, function(val){return(val$acc_val)}))
      lst.alpha<-lapply(lst.nodeStart, function(val){return(val$alpha.ratio)})
      lst.alpha[sapply(lst.alpha, is.null)] <- NA
      alpha.param<-unlist(lst.alpha)
      if(is.null(acc)){
        to_ret<-c(1,"","")
      }else if(length(which(is.na(acc)))>1 || is.na(acc)){
        to_ret<-c(NA,"","")
      }else{

        to_ret<-c(max(acc,na.rm = T),grid_search[which(acc==max(acc,na.rm = T))][1],alpha.param[which(acc==max(acc,na.rm = T))][1])

      }
      return(to_ret)
    })


    switch (level,
      "local" = {
        lst.MM.report<-lapply(names(tmp), function(nodeStart){
          print(nodeStart)

         if(best.thr[[nodeStart]][2]!=""){
           MM.report<-matrix("",nrow = length(names(tmp[[nodeStart]][[which(grid_search==best.thr[[nodeStart]][2])]]$list.pred)),ncol = 8)
           colnames(MM.report)<-c("nodeStart","nodeGoal","model","AUC","pval","covariate","id.0","id.1")
           MM.report[,"nodeStart"]<-nodeStart
           MM.report[,"nodeGoal"]<-names(tmp[[nodeStart]][[which(grid_search==best.thr[[nodeStart]][2])]]$list.pred)

           if(best.thr[[nodeStart]][2]!=""){
             list.pred<-tmp[[nodeStart]][[paste0("p.thr.",which(grid_search==best.thr[[nodeStart]][2]))]][[1]]

             arr.AUC<-lapply(names(list.pred), function(eventGoal){

               #è 1 quando non ho modello
               if(length(list.pred[[eventGoal]])==1){
                 if(length(list.pred[[eventGoal]][[1]])!=1){
                   to_ret<-c(list.pred[[eventGoal]][[1]]$final.model$AUC_train,paste(round(as.numeric(list.pred[[eventGoal]]$final.model$pval_train),digits = p.val.digit),collapse = " "),paste(names(list.pred[[eventGoal]]$final.model$pval_train),collapse = " "))
                 }else{
                   to_ret<-c(list.pred[[eventGoal]],"","")
                 }
               }else{
                 if(length(list.pred[[eventGoal]])==3){
                   to_ret<-c(list.pred[[eventGoal]]$final.model$AUC_train,paste(round(as.numeric(list.pred[[eventGoal]]$final.model$pval_train),digits = p.val.digit),collapse = " "),paste(names(list.pred[[eventGoal]]$final.model$pval_train),collapse = " "))
                 }else{
                   to_ret<-c(list.pred[[eventGoal]]$AUC_train,paste(round(as.numeric(list.pred[[eventGoal]]$pval_train),digits = p.val.digit),collapse = " "),paste(names(list.pred[[eventGoal]]$pval_train),collapse = " "))
                 }

                 # list.pred[[eventGoal]]$final.model$AUC_train

               }
             })

             arr.count<-lapply(names(list.pred), function(eventGoal){
               #caso no modello
               if(length(list.pred[[eventGoal]])==1){
                 to_ret<-c("","")
               }else{
                 # list.pred[[eventGoal]]$final.model$AUC_train
                 to_ret<-list.pred[[eventGoal]]$count_train
               }
             })

             model.sign<-lapply(names(list.pred), function(eventGoal){
               if(length(list.pred[[eventGoal]])==1){

                 to_ret<-c("used FOMM")
               }else{

                 if(length(lst.pred[[eventGoal]])==3){
                   to_ret<-paste(list.pred[[eventGoal]]$final.model$model$call[1:3],collapse = " ")
                 }else{
                   to_ret<-paste(list.pred[[eventGoal]]$model$call[1:3],collapse = " ")
                 }
                 # list.pred[[eventGoal]]$final.model$AUC_train

               }
             })


             MM.report[,"model"]<-unlist(model.sign)
             for(i in c(1:length(arr.AUC))){
               print(i)
               MM.report[i,"AUC"]<-round(as.numeric(arr.AUC[[i]][1]),digits = AUC.digit)
               MM.report[i,"pval"]<-arr.AUC[[i]][2]
               MM.report[i,"covariate"]<-arr.AUC[[i]][3]
               if(is.null(arr.count[[i]])){
                 MM.report[i,"id.0"]<-""
                 MM.report[i,"id.1"]<-""
               }else{
                 MM.report[i,"id.0"]<-arr.count[[i]][1]
                 MM.report[i,"id.1"]<-arr.count[[i]][2]
               }

             }
             table.to.ret<-as.table(MM.report)
           }else{
             table.to.ret<-NULL
           }

         }else{
           table.to.ret<-NULL
         }

          return(table.to.ret)

        })

        names(lst.MM.report)<-names(tmp)
        to.ret.rep<-lst.MM.report


      },
      "global"={
        MM.report<-matrix("",nrow = length(names(tmp)),ncol = 5)
        colnames(MM.report)<-c("NodeName","acc.FOMM","acc.LR","pval_thr","alpha.ratio")
        MM.report[,"NodeName"]<-names(tmp)
        arr.perf<-acc.FOMM(test.final,obj.out)
        for (i in c(1:length(names(tmp)))) {
          if(!is.na(arr.perf$arr.acc[names(tmp)[i]])){
            MM.report[i,"acc.FOMM"]<-arr.perf$arr.acc[names(tmp)[i]]
          }else{
            MM.report[i,"acc.FOMM"]<-NA
          }
        }
        # MM.report[,"acc.FOMM"]<-unlist(lapply(names(tmp), function(riga){return(max(obj.out$MMatrix.perc[riga,]))}))
        for (i in c(1:length(best.thr))) {
          MM.report[i,"acc.LR"]<-best.thr[[i]][1]
          MM.report[i,"pval_thr"]<-best.thr[[i]][2]
          MM.report[i,"alpha.ratio"]<-best.thr[[i]][3]

        }
        to.ret.rep<-as.table(MM.report)
      }
    )



    return(to.ret.rep)


  }


  acc.FOMM<-function(test.final,obj.out){
    objDL.train.out<-obj.out

    objDL.test<-dataLoader()
    objDL.test$load.data.frame(mydata = obj.out$original.CSV[which(obj.out$original.CSV[,obj.out$csv.IDName] %in% test.final),],IDName = obj.out$csv.IDName,EVENTName = obj.out$csv.EVENTName,dateColumnName = obj.out$csv.dateColumnName,format.column.date = obj.out$csv.date.format)
    objDL.test.out<-objDL.test$getData()


      lst.nodi <- list()
      tmp <- lapply( 1:length(objDL.test.out$wordSequence.raw), function(indice) {
        for( cursore in 1:(length(objDL.test.out$wordSequence.raw[[indice]])-1)) {
          nodo.from <- objDL.test.out$wordSequence.raw[[indice]][cursore]
          nodo.to <- objDL.test.out$wordSequence.raw[[indice]][cursore+1]

          if( !(nodo.from %in% names(lst.nodi))) lst.nodi[[ nodo.from ]]  <<- c()
          res <- 0
          if( nodo.from %in% rownames(objDL.train.out$MMatrix.perc)) {
            riga <- objDL.train.out$MMatrix.perc[ nodo.from , ]
            nodo.destinatario.previsto <- names(riga[order(riga,decreasing = T)][1])
            if( nodo.to == nodo.destinatario.previsto ) res <- 1
          }
          lst.nodi[[nodo.from]] <<- c( lst.nodi[[ nodo.from]] , res )
        }
      })

      arr.perf <- unlist(lapply(1:length(lst.nodi),function(i){  return(  sum(lst.nodi[[i]])/length(lst.nodi[[i]])  )   }))
      names(arr.perf) <- names(lst.nodi)
      global.acc <- sum(unlist(lst.nodi))/length(unlist(lst.nodi))

      return(
        list( "global.list" = lst.nodi, "arr.acc" = arr.perf, "arr.global.acc" = global.acc )
      )


  }

   #===========================================================
  # Prediction Accuracy function
  #===========================================================

  validation.fun<-function(arr.ID.val,obj.out,lst.local.rep, global.rep, grid_search, lst.model){
    tmp.ID.traces<-lapply(arr.ID.val, function(id.paz){
      paz.trace<-obj.out$pat.process[[id.paz]][,obj.out$csv.EVENTName]
      paz.trace.pred<-paz.trace
      paz.trace.fomm<-paz.trace
      for(i in seq(1,(length(paz.trace)-1))){

        nodeStart<-paz.trace[i]
        nodeStart.rep<-lst.local.rep[[nodeStart]]

        if(is.null(nodeStart.rep)) {
          nodeNext<-names(which(obj.out$MMatrix.perc[nodeStart,]==max(obj.out$MMatrix.perc[nodeStart,])))
        }else{

          #cosa mi serve per calcolare predizioni?
          #modelli allenati per ogni possibile nodo futuro + alpha1 e alpha 2 x ranking
          chosen.p.thr<-which(grid_search==as.numeric(global.rep[which(global.rep[,"NodeName"]==nodeStart),4]))
          chosen.alpha.param<-as.numeric(global.rep[which(global.rep[,"NodeName"]==nodeStart),5])
          MM.matrix.pred<-grad.desc.fun(arr.ID.val,nodeStart,obj.out,find.alpha = F, models= lst.model[[nodeStart]][[chosen.p.thr]]$list.pred, comp.acc = F, alpha.ratio = chosen.alpha.param)

          ind.next<-which(startsWith(x = rownames(MM.matrix.pred),prefix = id.paz))
          nodeNext<-MM.matrix.pred[ind.next,1]

          if(ind.next[1]<=i){
            nodeNext<-nodeNext[1]
          }
          # nodeNext<-MM.matrix.pred[id.paz,1]

        }
        paz.trace.pred[i+1]<-nodeNext
        paz.trace.fomm[i+1]<-names(which(obj.out$MMatrix.perc[nodeStart,]==max(obj.out$MMatrix.perc[nodeStart,])))
      }

      return(list("trace"=paz.trace,"trace.pred"=paz.trace.pred,"trace.fomm"=paz.trace.fomm))

    })

    names(tmp.ID.traces)<-arr.ID.val


    lst.tr<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace,"END"))})
    lst.tr.pred<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace.pred,"END"))})
    lst.tr.fomm<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace.fomm,"END"))})



    lst.tr.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace)})
    lst.tr.pred.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace.pred)})
    lst.tr.fomm.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace.fomm)})


    MM.acc<-matrix(0,nrow =length(lst.tr.comp),ncol = max(lengths(lst.tr.comp)))
    MM.acc.fomm<-MM.acc
    rownames(MM.acc)<-names(lst.tr.comp)
    rownames(MM.acc.fomm)<-names(lst.tr.comp)
    lapply(1:length(lst.tr.comp),function(ind.paz){
      true.tr<-lst.tr.comp[[ind.paz]]
      pred.tr<-lst.tr.pred.comp[[ind.paz]]
      fomm.tr<-lst.tr.fomm.comp[[ind.paz]]
      paz<-names(lst.tr.comp)[ind.paz]

      if(length(true.tr)<ncol(MM.acc)){
        riga<-c(true.tr==pred.tr,rep(NA,(ncol(MM.acc)-length(true.tr))))
        riga.fomm<-c(true.tr==fomm.tr,rep(NA,(ncol(MM.acc)-length(true.tr))))

      }else{
        riga<-true.tr==pred.tr
        riga.fomm<-true.tr==fomm.tr
      }

      # for(ev in c(1:(length(true.tr-1)))){
      MM.acc[paz,]<<-riga
      MM.acc.fomm[paz,]<<-riga.fomm
    })

    n.cells<-(dim(MM.acc)[1]*dim(MM.acc)[2])-length(which(is.na(MM.acc)))
    performances<-list("fomm"=(sum(MM.acc.fomm,na.rm = T)/n.cells),"pred"=(sum(MM.acc,na.rm = T)/n.cells))

    return(performances)

  }




  #===========================================================
  # Prediction Accuracy function
  #===========================================================

  pred.accuracy.fun<-function(alpha1,alpha2,lst.ID.test,comp.acc=TRUE){
    tmp<-lapply(lst.ID.test, function(lst.pred.id){
      y_vera<-lst.pred.id$y
      fun.rank.out<-lapply(lst.pred.id$pred.mat, function(pred.mat){
        fun.rank<-lapply(1:nrow(pred.mat), function(riga){
          # to_ret<-alpha1*(as.numeric(lst.pred.id$pred.mat[riga,"y_prob"])+as.numeric(lst.pred.id$pred.mat[riga,"AUC"]))
          to_ret<-alpha1*as.numeric(pred.mat[riga,"y_prob"])+alpha2*as.numeric(pred.mat[riga,"perf"])
          return(to_ret)
        })
        names(fun.rank)<-rownames(pred.mat)
        arr.prest<-unlist(fun.rank)
        best.pred<-names(arr.prest)[which(arr.prest==max(arr.prest,na.rm = T))][1]
        return(best.pred)
      })
      # return(list(unlist(fun.rank.out),y_vera))

      return(unlist(fun.rank.out))
    })


    tmp<-unlist(tmp)

    MM.pred<-matrix(NA, nrow = length(tmp), ncol = 3)
    rownames(MM.pred)<-names(tmp)
    colnames(MM.pred)<-c("y_pred","y","correct.pred")
    MM.pred[,1]<-unlist(tmp,use.names = FALSE)
    MM.pred[,2]<-unlist(lapply(lst.ID.test, function(lst.y){return(lst.y$y)}))

    for(i in c(1:nrow(MM.pred))){

      if(MM.pred[i,1]==MM.pred[i,2]){
        MM.pred[i,3]=1
      }else{
        MM.pred[i,3]=0
      }
    }

    acc<-sum(as.numeric(MM.pred[,3]))/nrow(MM.pred)

    if(comp.acc){
      to_return<-acc
    }else{
      to_return<-MM.pred
    }
    return(to_return)

  }


  #===========================================================
  # Validation graph
  #===========================================================

  validation.graph<-function(arr.ID.val,obj.out,lst.local.rep, global.rep, grid_search, lst.model, on.global=F){
    tmp.ID.traces<-lapply(arr.ID.val, function(id.paz){
      paz.trace<-obj.out$pat.process[[id.paz]][,obj.out$csv.EVENTName]
      paz.trace.pred<-paz.trace
      paz.trace.fomm<-paz.trace
      for(i in seq(1,(length(paz.trace)-1))){

        nodeStart<-paz.trace[i]
        nodeStart.rep<-lst.local.rep[[nodeStart]]

        if(is.null(nodeStart.rep)) {
         nodeNext<-names(which(obj.out$MMatrix.perc[nodeStart,]==max(obj.out$MMatrix.perc[nodeStart,])))
        }else{

          #cosa mi serve per calcolare predizioni?
          #modelli allenati per ogni possibile nodo futuro + alpha1 e alpha 2 x ranking
          chosen.p.thr<-which(grid_search==as.numeric(global.rep[which(global.rep[,"NodeName"]==nodeStart),4]))
          chosen.alpha.param<-as.numeric(global.rep[which(global.rep[,"NodeName"]==nodeStart),5])
          MM.matrix.pred<-grad.desc.fun(arr.ID.val,nodeStart,obj.out,find.alpha = F, models= lst.model[[nodeStart]][[chosen.p.thr]]$list.pred, comp.acc = F, alpha.ratio = chosen.alpha.param)

          ind.next<-which(startsWith(x = rownames(MM.matrix.pred),prefix = id.paz))
          nodeNext<-MM.matrix.pred[ind.next,1]

          if(ind.next[1]<=i){
            nodeNext<-nodeNext[1]
          }
          # nodeNext<-MM.matrix.pred[id.paz,1]

        }
        paz.trace.pred[i+1]<-nodeNext
        paz.trace.fomm[i+1]<-names(which(obj.out$MMatrix.perc[nodeStart,]==max(obj.out$MMatrix.perc[nodeStart,])))
      }

      return(list("trace"=paz.trace,"trace.pred"=paz.trace.pred,"trace.fomm"=paz.trace.fomm))

    })

    names(tmp.ID.traces)<-arr.ID.val


    lst.tr<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace,"END"))})
    lst.tr.pred<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace.pred,"END"))})
    lst.tr.fomm<-lapply(tmp.ID.traces, function(paz){return(c("BEGIN",paz$trace.fomm,"END"))})



    lst.tr.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace)})
    lst.tr.pred.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace.pred)})
    lst.tr.fomm.comp<-lapply(tmp.ID.traces, function(paz){return(paz$trace.fomm)})


    MM.acc<-matrix(0,nrow =length(lst.tr.comp),ncol = max(lengths(lst.tr.comp)))
    MM.acc.fomm<-MM.acc
    rownames(MM.acc)<-names(lst.tr.comp)
    rownames(MM.acc.fomm)<-names(lst.tr.comp)
    lapply(1:length(lst.tr.comp),function(ind.paz){
      true.tr<-lst.tr.comp[[ind.paz]]
      pred.tr<-lst.tr.pred.comp[[ind.paz]]
      fomm.tr<-lst.tr.fomm.comp[[ind.paz]]
      paz<-names(lst.tr.comp)[ind.paz]

      if(length(true.tr)<ncol(MM.acc)){
        riga<-c(true.tr==pred.tr,rep(NA,(ncol(MM.acc)-length(true.tr))))
        riga.fomm<-c(true.tr==fomm.tr,rep(NA,(ncol(MM.acc)-length(true.tr))))

      }else{
        riga<-true.tr==pred.tr
        riga.fomm<-true.tr==fomm.tr
      }

      # for(ev in c(1:(length(true.tr-1)))){
       MM.acc[paz,]<<-riga
       MM.acc.fomm[paz,]<<-riga.fomm
    })

    n.cells<-(dim(MM.acc)[1]*dim(MM.acc)[2])-length(which(is.na(MM.acc)))
    performances<-list("fomm"=(sum(MM.acc.fomm,na.rm = T)/n.cells),"pred"=(sum(MM.acc,na.rm = T)/n.cells))


    # MM.acc.fomm<-matrix(0,nrow =length(lst.tr),ncol = max(lengths(lst.tr)))
    # rownames(MM.acc.fomm)<-names(lst.tr)
    # lapply(1:length(lst.tr),function(ind.paz){
    #   true.tr<-lst.tr[[ind.paz]]
    #   fomm.tr<-lst.tr.fomm[[ind.paz]]
    #   paz<-names(lst.tr)[ind.paz]
    #   print(paz)
    #   if(length(true.tr)<ncol(MM.acc)){
    #     riga<-c(true.tr==fomm.tr,rep(NA,(ncol(MM.acc)-length(true.tr))))
    #   }else{
    #     riga<-true.tr==fomm.tr
    #   }
    #
    #   # for(ev in c(1:(length(true.tr-1)))){
    #   MM.acc.fomm[paz,]<<-riga
    # })

    # lst.tr<-lapply(tmp.ID.traces, function(paz){return(paz$trace)})
    # lst.tr.pred<-lapply(tmp.ID.traces, function(paz){return(paz$trace.pred)})

    MM.trace<-matrix(0,nrow =length(unique(unlist(lst.tr))),ncol = length(unique(unlist(lst.tr))))
    colnames(MM.trace)<-unique(unlist(lst.tr))
    rownames(MM.trace)<-unique(unlist(lst.tr))

    MM.trace.pred<-MM.trace

    for(i in seq(1,nrow(MM.trace))){
     for(j in seq(1,ncol(MM.trace))){
       if(i!=j){
         # mat.val<-0
         start.tr<-rownames(MM.trace)[i]
         start.pred<-rownames(MM.trace.pred)[i]
         to.tr<-colnames(MM.trace)[j]
         to.pred<-colnames(MM.trace.pred)[j]

         for(id in seq(1,length(lst.tr))){

           ind.start.tr<-which(lst.tr[[id]]==start.tr)
           if(!identical(ind.start.tr,integer(0))){
             if(max(ind.start.tr)<=(length(lst.tr[[id]])-1)){
               next.ev<-lst.tr[[id]][ind.start.tr+1]
             }else{
               next.ev<-""
               }
             }else{
            next.ev<-""
            }

           ind.start.pred<-which(lst.tr.pred[[id]]==start.pred)
           if(!identical(ind.start.pred,integer(0))){
             if(max(ind.start.pred)<=(length(lst.tr.pred[[id]])-1)){
               next.ev.pred<-lst.tr.pred[[id]][ind.start.pred+1]
               }else{
               next.ev.pred<-""
               }
             }else{
               next.ev.pred<-""
             }

           # next.ev<-lst.tr[[id]][ind.start.tr+1]

           if(next.ev[1]==to.tr){MM.trace[i,j]<- MM.trace[i,j]+1}

           if(next.ev.pred[1]==to.pred){MM.trace.pred[i,j]<-MM.trace.pred[i,j]+1}

           }
         }
       }
    }

    MM.trace.perc<-MM.trace
    MM.trace.pred.perc<-MM.trace.pred
    MM.perc.tmp<-lapply(1:nrow(MM.trace), function(riga){
      MM.trace.perc[riga,which(MM.trace[riga,]!=0)]<<-MM.trace[riga,which(MM.trace[riga,]!=0)]/sum(MM.trace[riga,])
      MM.trace.pred.perc[riga,which(MM.trace.pred[riga,]!=0)]<<-MM.trace.pred[riga,which(MM.trace.pred[riga,]!=0)]/sum(MM.trace.pred[riga,])
    })

    if(on.global){
      val.graph<-build.graph.from.table(MM = obj.out$MMatrix.perc,threshold = 0.02,pred.graph = F,val.plot = T,MM.trace.perc = MM.trace.perc )
      val.graph.pred<-build.graph.from.table(MM = obj.out$MMatrix.perc,threshold = 0.02,pred.graph = F,val.plot = T,MM.trace.perc = MM.trace.pred.perc)
    }else{
      val.graph<-build.graph.from.table(MM =  MM.trace.perc,threshold = 0.02,pred.graph = F)
      val.graph.pred<-build.graph.from.table(MM =  MM.trace.pred.perc,threshold = 0.02,pred.graph = F)
    }




    return(c("script.trace"=val.graph,"script.prediction"=val.graph.pred))

  }

  #===========================================================
  # Plot cov vs Node
  #===========================================================

  covariate.heatmap<-function(lst.local.rep,eventNode,par.margin = c(2, 20, 8, 2),threshold.low = 0, threshold.hi = 0.5,cex = 0.5){
    lst.heatMat<-lapply(lst.local.rep, function(mat.node.from){

      if(!is.null(mat.node.from)){
        ind.node.mod<-which(mat.node.from[,6]!="")

        if(length(ind.node.mod)!=0){
          arr.cov<-unique(unlist(strsplit(mat.node.from[ind.node.mod,6],split = " ")))
          MM<-matrix(0,nrow = length(arr.cov),ncol = length(ind.node.mod) )
          colnames(MM)<-mat.node.from[ind.node.mod,2]
          rownames(MM)<-arr.cov

          for(i in seq(1,ncol(MM))){
            ev.goal<-colnames(MM)[i]
            cov<-unlist(strsplit(mat.node.from[which(mat.node.from[,2]==ev.goal),6]," "))
            pval<-as.numeric(unlist(strsplit(mat.node.from[which(mat.node.from[,2]==ev.goal),5]," ")))

            for (j in seq(1,length(cov))) {
              MM[which(rownames(MM)==cov[j]),i]<-pval[j]
            }

          }

        }else{
          MM<-NULL
        }

      }else{
       MM<-NULL
      }
      return(MM)
      })

    names(lst.heatMat)<-names(lst.local.rep)
    MM.Cross<-lst.heatMat[[eventNode]]

    if(!is.null(MM.Cross)){
      par(mar = par.margin)
      image(t(MM.Cross),col = heat.colors(256) , axes=FALSE,oldstyle = F)
      arr.posizioni <- (0.01:ncol(MM.Cross)/(ncol(MM.Cross)-1))
      if(nrow(MM.Cross)==1){
        arr.posizioni.row<-(0.05:nrow(MM.Cross))
        axis(3, arr.posizioni, labels=colnames(MM.Cross),las=2,cex=0.3)
        axis(2, arr.posizioni.row, labels=rownames(MM.Cross),las=2)
        for( riga in 1:nrow(t(MM.Cross))) {
          for( colonna in 1:ncol(t(MM.Cross))) {
            valore <- t(MM.Cross)[riga,colonna]
            if( valore >= threshold.low & valore <= threshold.hi ) {
              text((riga-1)/(nrow(t(MM.Cross))-1),(colonna-1)/(ncol(t(MM.Cross))),format(valore,digits = 2) , cex=cex )
            }
          }
        }
      }else{
        # arr.posizioni <- (0.01:ncol(MM.Cross)/(ncol(MM.Cross)-1))
        arr.posizioni.row <- (0.05:nrow(MM.Cross)/(nrow(MM.Cross)-1))
        axis(3, arr.posizioni, labels=colnames(MM.Cross),las=2,cex=0.3)
        axis(2, arr.posizioni.row, labels=rownames(MM.Cross),las=2)
        for( riga in 1:nrow(t(MM.Cross))) {
          for( colonna in 1:ncol(t(MM.Cross))) {
            valore <- t(MM.Cross)[riga,colonna]
            if( valore >= threshold.low & valore <= threshold.hi ) {
              text((riga-1)/(nrow(t(MM.Cross))-1),(colonna-1)/(ncol(t(MM.Cross))-1),format(valore,digits = 2) , cex=cex )
            }
          }
        }
      }


    }


    return(MM.Cross)


  }





  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list() , verboseMode) {
    MMatrix<<-''
    footPrint<<-''
    model.grViz<<-'';
    is.dataLoaded<<-FALSE
    parameters<<-parametersFromInput
    MMatrix.perc<<-NA
    MMatrix.perc.noLoop<<-NA
    MMatrix.mean.time<<-NA
    MMatrix.density.list<<-NA
    MM.den.list.high.det<<-NA
    MM.pat.process<<-NA
    MM.csv.parameters<<-list()
    istanceClass<<-list()
    obj.log<<-logHandler();
    setInstanceClass(className = "FOMM")
    global.personal.ID<<-paste( c(as.character(runif(1,1,100000)),as.character(runif(1,1,100000)),as.character(runif(1,1,100000))), collapse = '' )
    param.verbose <<- verbose.mode
  }
  #===========================================================
  costructor( parametersFromInput = parameters.list, verboseMode = verbose.mode);
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "getModel"=getModel,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "distanceFrom"=distanceFrom,
    "getClass"=getClass,
    "getInstanceClass"=getInstanceClass,
    "plot.delta.graph"=plot.delta.graph,
    "build.PWF"=build.PWF,
    "findReacheableNodes"=findReacheableNodes,
    "KaplanMeier"=KaplanMeier,
    "LogRankTest"=LogRankTest,
    "Predictive.model"=Predictive.model,
    "Pred.mod.node"=Pred.mod.node,
    "Predictive.PD"=Predictive.PD,
    "grad.desc.fun"=grad.desc.fun,
    "compute.gen.perf"=compute.gen.perf
  ) )
}


























