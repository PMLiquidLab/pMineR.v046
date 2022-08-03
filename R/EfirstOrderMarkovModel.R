#' A class to train First Order Markov Models
#' 
#' @description  This is an implementation of the First Order Markov Model (EfirstOrderMarkovModel) for Process Mining issues.
#'                This class provides a minimal set of methods to handle with the FOMM model:
#'                \itemize{
#'                \item \code{EfirstOrderMarkovModel( ) } is the costructor
#'                \item \code{loadDataset( ) } loads data taken from a dataLoader::getData() method, into a FOMM object
#'                \item \code{trainModel( ) } train a model using the previously loaded dataset
#'                \item \code{replay( ) } re-play a given event log on the internal FOMM model
#'                \item \code{play( ) } play the internal FOMM model a desired number of times, in order to simulate new event-logs. This methods can also, if desired, simulate event-logs which does not complies with the internal FOMM model.
#'                \item \code{plot( ) } plot the internal model
#'                \item \code{getModel( ) } return the trained internal FOMM model
#'                \item \code{getInstanceClass( ) } return the instance class Name and description (version, etc.)
#'                \item \code{findReacheableNodes( ) } and return the array containing the reacheable states, starting from the passed one.
#'                }
#'              In order to better undestand the use of such methods, please visit: www.pminer.info
#'              
#'              The consturctor admit the following parameters:
#' parameters.list a list containing possible parameters to tune the model. 
#' @param parameters.list a list containing the parameters. The possible ones are: 'considerAutoLoop' and 'threshold'. 'considerAutoLoop' is a boolean which indicates if the autoloops have to be admitted, while 'threshold' is the minimum value that a probability should have to do not be set to zero, in the transition matrix.
#' @import progress DiagrammeR
#' @export
EfirstOrderMarkovModel<-function( parameters.list = list() ) {
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
  istanceClass<-list()
  obj.log<-NA
  global.personal.ID<-NA
  EFPTs <- NA
  
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
    
    res <- calcolaEnhancedFootPrintTable.pat.process( dataList )
    # calcolaEnhancedFootPrintTable.pat.process( objDL$getData() )
    # browser()
    EFPTs <<- res
    
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
    aa<- MMatrix.perc; bb <- MMatrix
    aa[ which(aa<=threshold,arr.ind = T) ]<-0
    bb[ which(bb<=threshold,arr.ind = T) ]<-0
    for( i in seq( 1 , nrow(aa)) ) {if(sum(aa[i,])>0)  {aa[i,]<-aa[i,]/sum(aa[i,]);} } 
    MMatrix.perc<<-aa ; MMatrix<<-bb
    grafo<-build.graph.from.table( MM = MMatrix.perc, threshold  = threshold,withAlwaysAfter = TRUE)
    # grafo<-build.graph.from.table.always( MM = MMatrix.perc, threshold  = threshold)
    # ohohoh <- build.graph.v2(MM = MMatrix.perc, threshold  = threshold)
    
    model.grViz<<-grafo;
  }
  #===========================================================
  # replay
  #===========================================================
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
    

    # for(patId in names(dataList$pat.process)) { 
    #   parola <- unlist(dataList$pat.process[[patId]][[dataList$csv.EVENTName]])
    #   parola <- c("BEGIN",parola)
    #   success <- TRUE;
    #   cosa <- getEFPTs()
    #   for( caratt.i in seq(1,(length(parola)-1)) ) {
    #     # Verifica che dopo ci siano le parole che ci DEVONO essere
    #     for( car.dopo in seq(caratt.i+1,(length(parola)-1)) ) {
    #       # Verifica che dopo NON ci siano le parole che NON CI DEVONO ESSERE
    #       if( cosa$EFPT.hasNeverAfter[ caratt.i  , car.dopo  ] == "!>>" ) { success = FALSE; break; }
    #       # Verifica che ci siano le parole che CI DEVONO ESSERE
    #       nomi.da.trovare <- names(cosa$EFPT.hasAlwaysAfter[ caratt.i, cosa$EFPT.hasAlwaysAfter[ caratt.i  , ] == ">>"])
    #       conta <- sum(nomi.da.trovare %in% c(parola[(caratt.i+1):length(parola)] ,"END"))
    #       if(conta<length(nomi.da.trovare)) { success = FALSE; break; }
    #     }
    #     if(success == FALSE ) break;
    #   }
    # }
        
    cosa <- getEFPTs()
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
        
        # browser()
        # Verifica che dopo ci siano le parole che ci DEVONO essere
        for( car.dopo in seq(caratt.i+1,(length(parola)-1)) ) {
          
          # cat("\n",caratt.i,"   -  ",car.dopo,"  CAR:",caratt.s," in  PAROLA=",paste(parola,collapse = ","))
          
          # if(caratt.i == 1 & car.dopo == 20 ) browser()
          # Verifica che dopo NON ci siano le parole che NON CI DEVONO ESSERE
          if( cosa$EFPT.hasNeverAfter[ parola[caratt.i]  , parola[car.dopo]  ] == "!>>" ) { browser(); success = FALSE; break; }
          # Verifica che ci siano le parole che CI DEVONO ESSERE
          nomi.da.trovare <- names(cosa$EFPT.hasAlwaysAfter[ caratt.i, cosa$EFPT.hasAlwaysAfter[ caratt.i  , ] == ">>"])
          conta <- sum(nomi.da.trovare %in% c(parola[(caratt.i+1):length(parola)] ,"END"))
          if(conta<length(nomi.da.trovare)) { browser(); success = FALSE; break; }
          
        }
        cat("\n-----------------------------")
        if(success == FALSE ) break;        
        # 
        # cosa <- getEFPTs()
        # cosa$EFPT.hasAlwaysAfter
        # cosa$EFPT.hasNeverAfter
        # caratt.s %in% colnames(MMatrix.perc)
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
  findReacheableNodes<-function( startingNode = 'BEGIN'  ) {
    nodoDiPatenza <- startingNode
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
    if(kindOfOutput=="EFPTs") return( EFPTs )

    obj.log$sendLog( msg = c("The requested model is not available yet! Only 'grViz', 'MMatrix' and 'MMatrix.perc' are admitted. ")  ,type="ERR");
  }
  #===========================================================
  # plot
  #===========================================================
  plot<-function(){
    grViz( getModel(kindOfOutput = "grViz" ) )
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
  # build.graph.v2
  #===========================================================
  build.graph.v2<-function(MM, threshold ) {
    
    # https://cran.r-project.org/web/packages/visNetwork/vignettes/Introduction-to-visNetwork.html
    
    possibiliEventi <- colnames(MM)
    
    nodeDF <- data.frame( seq(1,length(possibiliEventi) ) , possibiliEventi )

    df.nodi <- data.frame()
    for( evento in possibiliEventi ) {
      id.evento <- which( possibiliEventi == evento )
      arr.Eventi <- colnames(MM)[   which(MM[ evento, ] >0) ]
      if(length(arr.Eventi)>0) {
        id.nodi.dest <- unlist(lapply(arr.Eventi,function(x){ which(possibiliEventi==x) }))
        df.nodi <- rbind(df.nodi,data.frame(rep(id.evento,length(arr.Eventi)),id.nodi.dest))
      }
    }
    colnames(nodeDF) <- c("id","label")
    colnames(df.nodi) <- c("from","to")
    browser()
    
  }
  #===========================================================
  # build.graph.from.table
  #===========================================================
  build.graph.from.table<-function(MM, threshold, second.MM = NA, threshold.second.MM=.2 , type.of.graph= "delta", withNormalArcs = TRUE, withAlwaysAfter = FALSE, withNeverAfter = FALSE) {
    
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

          peso<-round(as.numeric(MM[i, listaNodiRiga[ct]]),digits = 2)
          penwidth<- peso*3 + 0.01
          if(penwidth<0.4) penwidth=0.4
          fontSize = 5+peso*9
          colore = as.integer(100-(30+peso*70))
          if( type.of.graph == "overlapped") {
            second.peso<-round(as.numeric(second.MM[i, listaNodiRiga[ct]]),digits = 2)
            if( abs(peso - second.peso) >= threshold.second.MM ) {
              delta.peso<-round(as.numeric((peso - second.peso)),digits = 2)
              penwidth<- max(peso,abs(delta.peso))*3 + 0.01
              fontSize = 5+max(peso,abs(delta.peso))*9
              if(delta.peso>0) colore.delta.peso<-"Red"
              else colore.delta.peso<-"Green"
               if(peso > threshold | second.peso > threshold) {
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"/",second.peso,"', style='dashed', fontcolor='",colore.delta.peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = ",colore.delta.peso,"]\n"), collapse = '')   
                arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
               }
            } else{
               if(peso > threshold) {
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
                arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )                
               }
            }
          } else {
             if(peso > threshold) {
              stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
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
    
    
    if( withAlwaysAfter == TRUE ) {
      cosa <- getEFPTs()
      for(nomeRiga in rownames(cosa$EFPT.hasAlwaysAfter) ) {
        for(nomeColonna in colnames(cosa$EFPT.hasAlwaysAfter) ) {
          if( cosa$EFPT.hasAlwaysAfter[ nomeRiga , nomeColonna ] == ">>") {
            stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",nomeRiga,"'->'",nomeColonna,"' [ label='",0.7,"', penwidth='",1,"', color = Blue]\n"), collapse = '')       
          }
        }
      }
      # browser()
    }
    
    # now plot it
    a<-paste(c("digraph boxes_and_circles {
             
             # a 'graph' statement
             graph [overlap = true, fontsize = 10]
             
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
  getClass<-function(){
    return(list(
      "class"="EfirstOrderMarkovModel",
      "obj.ID"=global.personal.ID,
      "version"="0.41"      
    ))
  }    
  #===========================================================
  # build.graph.from.table
  #===========================================================
  EPlot<-function(threshold = 0 , withRegularNodes = TRUE, withAlwaysAfter = FALSE, withNeverAfter = FALSE) {
    
    if(!is.null(parameters$threshold)) threshold<-parameters$threshold
    else threshold<-0
    MM <- MMatrix.perc
    
    second.MM <- NA
    threshold.second.MM <- .2
    type.of.graph <- "delta"
    withNormalArcs <- TRUE
    
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
      # if( !(rownames(MM)[i] %in% c("BEGIN","END") )) {
        listaNodiRiga<-listaNodi[which(MM[i,]>=threshold)]
        if(length(listaNodiRiga)>0) {
          for( ct in seq(1,length(listaNodiRiga))) {
            
            peso<-round(as.numeric(MM[i, listaNodiRiga[ct]]),digits = 2)
            penwidth<- peso*3 + 0.01
            if(penwidth<0.4) penwidth=0.4
            fontSize = 5+peso*9
            colore = as.integer(100-(30+peso*70))
            if( type.of.graph == "overlapped") {
              second.peso<-round(as.numeric(second.MM[i, listaNodiRiga[ct]]),digits = 2)
              if( abs(peso - second.peso) >= threshold.second.MM ) {
                delta.peso<-round(as.numeric((peso - second.peso)),digits = 2)
                penwidth<- max(peso,abs(delta.peso))*3 + 0.01
                fontSize = 5+max(peso,abs(delta.peso))*9
                if(delta.peso>0) colore.delta.peso<-"Red"
                else colore.delta.peso<-"Green"
                if(peso > threshold | second.peso > threshold) {
                  stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"/",second.peso,"', style='dashed', fontcolor='",colore.delta.peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = ",colore.delta.peso,"]\n"), collapse = '')   
                  arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
                }
              } else{
                if(peso > threshold) {
                  stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
                  arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )                
                }
              }
            } else {
              if(peso > threshold) {
                stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",peso,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"', color = Gray",colore,"]\n"), collapse = '')   
                arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )              
              }
            }
          }
        }        
      # }

      
    }
    
    listaNodiToPrint<-''
    arr.nodi.con.archi <- unique(arr.nodi.con.archi)
    
    listaNodi <- listaNodi[ listaNodi %in% arr.nodi.con.archi ]
    
    for(i in seq(1,length(listaNodi))) {
      if(i<length(listaNodi)) listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"';"), collapse=''    )
      else listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"'"), collapse=''    )
    }
    
    if(  withRegularNodes == FALSE) {
      stringaNodiComplessi<-""
    }
      
    # fontSize = 5+1*9
    cosa <- getEFPTs()
    if( withAlwaysAfter == TRUE ) {
      for(nomeRiga in rownames(cosa$EFPT.hasAlwaysAfter) ) {
        for(nomeColonna in colnames(cosa$EFPT.hasAlwaysAfter) ) {
          if( cosa$EFPT.hasAlwaysAfter[ nomeRiga , nomeColonna ] == ">>") {
            if( !(nomeColonna %in% c("BEGIN","END")) &  !(nomeRiga %in% c("BEGIN","END")) ) {
              stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",nomeRiga,"'->'",nomeColonna,"' [ label='>>', fontsize = '13', penwidth='",1,"', color = Blue]\n"), collapse = '')             
            }
          }
        }
      }
    }
    # browser()
    if( withNeverAfter == TRUE ) {
      for(nomeRiga in rownames(cosa$EFPT.hasNeverAfter) ) {
        for(nomeColonna in colnames(cosa$EFPT.hasNeverAfter) ) {
          if( cosa$EFPT.hasNeverAfter[ nomeRiga , nomeColonna ] == "!>>") {
            if( !(nomeColonna %in% c("BEGIN","END")) &  !(nomeRiga %in% c("BEGIN","END")) ) {
              stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",nomeRiga,"'->'",nomeColonna,"' [ label='!>>', fontsize = '13', penwidth='",1,"', color = Red]\n"), collapse = '')        
            }
          }
        }
      }
    }    
    
    # now plot it
    a<-paste(c("digraph boxes_and_circles {
               
               # a 'graph' statement
               graph [overlap = true, fontsize = 10]
               
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
  getClass<-function(){
    return(list(
      "class"="EfirstOrderMarkovModel",
      "obj.ID"=global.personal.ID,
      "version"="0.41"      
    ))
  }    
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( parametersFromInput = list() ) {
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
    EFPTs <<- NA
    istanceClass<<-list()
    obj.log<<-logHandler();
    setInstanceClass(className = "EfirstOrderMarkovModel")
    global.personal.ID<<-paste( c(as.character(runif(1,1,100000)),as.character(runif(1,1,100000)),as.character(runif(1,1,100000))), collapse = '' )
  }
  getEFPTs <- function()  {
    return(EFPTs) 
  }
    
  #===========================================================
  costructor( parametersFromInput = parameters.list);
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "trainModel"=trainModel,
    "getModel"=getModel,
    "replay"=replay,
    "play"=play,
    "plot"=plot,
    "getClass"=getClass,
    "getInstanceClass"=getInstanceClass,
    "findReacheableNodes"=findReacheableNodes,
    "getEFPTs"=getEFPTs,
    "EPlot"=EPlot
  ) )  
}
