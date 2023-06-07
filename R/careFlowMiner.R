#' A class for a revisited Careflow Mining
#'
#' @description  This is an implementation of the Care Flow Mining algorithm, a bit revisited
#' @import progress 
#' @export


careFlowMiner <- function( verbose.mode = FALSE ) {
  lst.nodi <- list()
  MM <- c()
  IDD <- 0
  attr.dateToFormat <- ""
  attr.date.format <- ""
  param.verbose<-''
  intGraph <- ""
  cmpStr <- list()
  loadedDataset <- list()
  
  # ---------------------------------------------------------------
  # add a node to the tree
  # ---------------------------------------------------------------
  add.node <- function( father = NA , evento ) {
    id <- as.character(IDD)
    IDD <<- IDD + 1
    
    old.colNames <- colnames(MM)
    old.rowNames <- rownames(MM)
    
    MM <<- cbind(MM , rep("",nrow(MM)))
    MM <<- rbind(MM , rep("",ncol(MM)))
    colnames(MM) <<- c(old.colNames,id)
    rownames(MM) <<- c(old.rowNames,id)
    
    if( is.na(father) ) {
      MM[ "root", id] <<- evento; 
    } else {
      MM[ father, id] <<- evento; 
    }
    return(id)
  }
  # ---------------------------------------------------------------
  # ritorna l'ID di un elemento di una sequenza
  # ---------------------------------------------------------------
  get.id <- function(sequenza, i, fromID, debug = FALSE  ) {
    if( debug == TRUE ) browser()
    
    if(length(which(MM[fromID,] == sequenza[i])) == 0) return( NA )
    if(length(which(MM[fromID,] == sequenza[i])) > 1 ) browser()
    
    col.id <- colnames(MM)[which(MM[fromID,] == sequenza[i])]
    return( col.id )
  }
  # ---------------------------------------------------------------
  # aggiungi un path all'albero
  # ---------------------------------------------------------------
  add.path <- function( sequenza, debug = FALSE, col.dateFrom=c(), col.dateTo=c() , ID="", UM="days" ,IPP="", col.pMineR.deltaDate = c()) {
    old.id <- "root"
    for( i in 1:length(sequenza)) {
      id <- get.id( sequenza, i, fromID = old.id , debug = debug )
      if( is.na(id) ) {
        if( i == 1) {
          id <- add.node(evento = sequenza[i])
        } else {
          id <- add.node(evento = sequenza[i], father = old.id)
        }
        lst.nodi[[ id ]] <<- list("evento" = sequenza[i], "hits" = 1, "depth" = i, "IPP"=IPP)
      } else {
        lst.nodi[[id]]$hits <<- lst.nodi[[id]]$hits + 1
      }
      
      if(length(col.dateFrom)>0) {
        lst.nodi[[id]]$activationDates <<- c( lst.nodi[[id]]$activationDates , col.dateFrom[i])
        lst.nodi[[id]]$pMineR.deltaDate <<- c( lst.nodi[[id]]$pMineR.deltaDate , col.pMineR.deltaDate[i]) 
        lst.nodi[[id]]$IPP <<- unique(c( lst.nodi[[id]]$IPP , IPP ))
        if(length(col.dateTo)>0) {
          deltaTime <- difftime(as.Date(col.dateTo[i],format = attr.dateToFormat),as.Date(col.dateFrom[i],format = attr.date.format),units = "days")
          lst.nodi[[id]]$duration <<- c(lst.nodi[[id]]$duration , deltaTime)
        }  
      } 
      # browser()
      old.id <- id
    }
  }
  # ---------------------------------------------------------------
  # canonical function to load the data.
  # ---------------------------------------------------------------  
  loadDataset <- function( inputData , dateToColumnName=NA , dateToFormat = "") {
    DLS <- inputData
    attr.date.format <<- DLS$csv.date.format
    attr.dateToFormat <<- dateToFormat
    lst.nodi[[ "root" ]] <<- list("evento" = "root", "hits" = length(DLS$pat.process), "depth" = 0, "duration"=c())
    loadedDataset <<-  DLS
    
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(names(DLS$pat.process)), style = 3)
    pb.ct <- 0
    
    for( ID in names(DLS$pat.process) ) {
      pb.ct <- pb.ct + 1;
      if(param.verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      
      sequenza <- DLS$pat.process[[ID]][,DLS$csv.EVENTName]
      col.dateFrom <- DLS$pat.process[[ID]][,DLS$csv.dateColumnName]
      col.pMineR.deltaDate <- DLS$pat.process[[ID]][,"pMineR.deltaDate"]
      if(!is.na(dateToColumnName)) {
        col.dateTo <- DLS$pat.process[[ID]][,dateToColumnName]  
      } else { col.dateTo <- c() }
      
      # cat("\n ID: (",ID,")",sequenza)
      add.path(sequenza = sequenza, col.dateFrom = col.dateFrom, col.dateTo = col.dateTo, IPP = ID, col.pMineR.deltaDate = col.pMineR.deltaDate )
      # browser()
      lst.nodi[[ "root" ]]$hits <-  lst.nodi[[ "root" ]]$hits + 1
    }
    for( nodeName in names(lst.nodi) ) {
      if( nodeName != "root"){
        arr.eventi <- DLS$pat.process[[ lst.nodi[[nodeName]]$IPP[1] ]][[ DLS$csv.EVENTName ]][1 : lst.nodi[[nodeName]]$depth] 
        lst.nodi[[nodeName]]$arr.evt <<- arr.eventi
      }
    }
    
  }
  # ---------------------------------------------------------------
  # retrieve the data structure
  # ---------------------------------------------------------------   
  getDataStructure <- function() {
    
    arr.duration <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$duration} ))
    arr.pMineR.deltaDate <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$pMineR.deltaDate} ))
    arr.depth <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$depth} ))
    arr.freq <- unlist(lapply(1:length(lst.nodi),function(i) {lst.nodi[[i]]$hits} ))
    arr.mean.duration <- unlist(lapply(1:length(lst.nodi),function(i) { if(length(lst.nodi[[i]]$duration)>0) {return(mean(lst.nodi[[i]]$duration))} else{return(0)}   } ))
    nomi <- as.character(unlist(lapply(1:length(lst.nodi),function(i) { names(lst.nodi)[i]} )))
    mtr.res <- cbind( nomi , arr.freq , arr.mean.duration, arr.depth )
    colnames(mtr.res) <- c("ID","freq","mean.duration","depth")
    rownames(mtr.res) <- nomi
    
    return(
      list("MM"=MM,
           "lst.nodi"=lst.nodi,
           "mtr.res"=mtr.res)
    )
  }
  
  # ---------------------------------------------------------------
  # retrieve the data structure
  # ---------------------------------------------------------------   
  plotCFGraph <- function(  depth= 2 , starting.ID = "root", currentLevel = 0, total.hits = 0,
                            kindOfGraph = "dot", GraphFontsize = "9" , 
                            withPercentages = TRUE,
                            relative.percentages = FALSE, 
                            proportionalPenwidth=TRUE , default.arcColor = "Black", proportionalPenwidth.k.thickness = 5,
                            arr.States.color=c(), arr.States.color.shades = FALSE, arr.States.color.shades.thresholds = c(25,50,75),
                            predictive.model = FALSE, predictive.model.outcome = "", predictive.model.skipNodeLabel = c(),
                            predictive.model.engine.type = "default", predictive.model.engine.parameter = list(),
                            preserve.topology = FALSE, set.to.gray = FALSE, set.to.gray.color= "WhiteSmoke" , debug.it = FALSE,
                            show.far.leaf = FALSE, 
                            show.median.time.from.root = FALSE, heatmap.based.on.median.time = FALSE , 
                            heatmap.base.color = "Khaki", abs.threshold = NA , nodeShape = "square",
                            printNodeID = FALSE) {
    # withPercentages <- TRUE; 
    if( predictive.model.engine.type != "default" ) {
      predictive.model <- TRUE;
      predictive.model.outcome <- predictive.model.engine.parameter$outcome$eventName[1]
    }
    # relative.percentages <- FALSE
    if( starting.ID != "root") {
      if( lst.nodi[[starting.ID]]$depth == depth | 
          ( predictive.model==TRUE & lst.nodi[[starting.ID]]$evento == predictive.model.outcome & preserve.topology == FALSE  ) ) {
        if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
          num.outcome <- lst.nodi[[starting.ID]]$hits
        } else {
          num.outcome <- 0
        }
        return(list("stringa.nodi"=c(),"stringa.archi"=c(),"script"="", "num.outcome" = num.outcome, "sonHits" = lst.nodi[[starting.ID]]$hits))
      }
    }
    if( predictive.model==TRUE & lst.nodi[[starting.ID]]$evento == predictive.model.outcome & preserve.topology == TRUE) {
      set.to.gray <- TRUE
      default.arcColor <- set.to.gray.color
    }
    if(debug.it==TRUE) browser()

    arrId2Jump <- names(which(MM[starting.ID,]!=""))
    if( length(arrId2Jump) > 1 ) {
      arrId2Jump <- arrId2Jump[order(unlist(lapply(arrId2Jump,function(i) { lst.nodi[[i]]$evento })))]  
    }
    
    if(currentLevel == 0 ) { total.hits <- lst.nodi[[starting.ID]]$hits }
    arr.nodi <- c()
    arr.archi <- c()
    script <- ""
    
    if( currentLevel == 0) {
      arr.nodi <- c(paste( c("'",starting.ID,"' [ label='",lst.nodi[[starting.ID]]$evento,"\n(",total.hits,")', penwidth=3]"),collapse = "" ))
    }

    num.outcome <- 0
    totaleSonHits <- 0
    totale <- lst.nodi[[starting.ID]]$hits
    totale <- length(lst.nodi[[starting.ID]]$IPP)

    if( length(arrId2Jump) > 0 ) {
      totale <- sum(unlist(lapply( arrId2Jump, function(x) {lst.nodi[[x]]$hits} )))
      num.outcome <- 0
      
      for( son in arrId2Jump) {
        sonLabel <- lst.nodi[[son]]$evento
        sonHits <- lst.nodi[[son]]$hits
        totaleSonHits <- totaleSonHits + sonHits
        
        arc.fontsize <- "1"
        penwidth <- "1"
        arcColor <- default.arcColor
        arcLabel <- sonHits
        fillColor <- "White"
        
        if( withPercentages == TRUE ) {
          if(relative.percentages == TRUE) {
            percentuale <- as.integer((arcLabel/totale)*100)  
          } else {
            percentuale <- as.integer((arcLabel/total.hits)*100)  
          }
          edge.perc.father <- as.integer((arcLabel/totale)*100)  
          edge.perc.root <- as.integer((arcLabel/total.hits)*100)  
          edge.sonHits <- sonHits
          if( predictive.model == FALSE) {
            # -im 
            # arcLabel <- paste( c(percentuale,"%"),collapse =  '')
            # arcLabel.value = percentuale
            arcLabel <- paste( c(edge.perc.father,"%\n",edge.perc.root,"%"),collapse =  '')
            arcLabel.value = edge.perc.root
            # -fm
            
          } else {
            percentuale <- as.integer((arcLabel/totale)*100)
            arcLabel <- paste( c(as.integer((arcLabel/totale)*100) ,"%"),collapse =  '')  
            arcLabel.value = percentuale
          }
          arc.fontsize <- "8.5"
        }
        if( proportionalPenwidth == TRUE ) {
          penwidth <- proportionalPenwidth.k.thickness*(percentuale/100)+0.2
        }
        if(length(arr.States.color) > 0) {
          if( lst.nodi[[son]]$evento %in% names(arr.States.color)) {
            fillColor <- arr.States.color[ which(names(arr.States.color) == lst.nodi[[son]]$evento)  ]
            if( set.to.gray == TRUE) { fillColor <- set.to.gray.color } 
          }
        }

        res <- plotCFGraph( depth = depth , starting.ID = son , currentLevel = currentLevel + 1, total.hits = total.hits,
                            default.arcColor = default.arcColor, arr.States.color = arr.States.color, proportionalPenwidth.k.thickness = proportionalPenwidth.k.thickness,
                            predictive.model = predictive.model, predictive.model.outcome = predictive.model.outcome, 
                            predictive.model.engine.type = predictive.model.engine.type,
                            predictive.model.engine.parameter = predictive.model.engine.parameter,
                            predictive.model.skipNodeLabel = predictive.model.skipNodeLabel,
                            preserve.topology = preserve.topology, set.to.gray = set.to.gray,
                            set.to.gray.color = set.to.gray.color , debug.it = debug.it,
                            show.far.leaf = show.far.leaf,
                            withPercentages = withPercentages,
                            relative.percentages = relative.percentages,
                            show.median.time.from.root = show.median.time.from.root,
                            heatmap.based.on.median.time = heatmap.based.on.median.time,
                            heatmap.base.color = heatmap.base.color, 
                            abs.threshold = abs.threshold , nodeShape = nodeShape,
                            arr.States.color.shades = arr.States.color.shades, arr.States.color.shades.thresholds = arr.States.color.shades.thresholds,
                            printNodeID = printNodeID
        )
        
        nodo.partenza <- lst.nodi[[son]]$depth
        quanti.eventi.finali <- sum(unlist(lapply(unique(lst.nodi[[son]]$IPP) , function(IPP) {
          sequenza <- c()
          if( length(loadedDataset$wordSequence.raw[[IPP]]) >= nodo.partenza) {
            sequenza <- loadedDataset$wordSequence.raw[[IPP]][nodo.partenza:length(loadedDataset$wordSequence.raw[[IPP]])] 
          }
          if( predictive.model.outcome %in% sequenza) return(1)
          return(0)
        })))   
        if( lst.nodi[[son]]$evento == predictive.model.outcome ) quanti.eventi.finali <- lst.nodi[[son]]$hits
        
        if( predictive.model == FALSE) {
          
          if( sonLabel %in% arr.States.color.shades ) {
            if(arcLabel.value >= 0 &  arcLabel.value < arr.States.color.shades.thresholds[1]) fillColor <- paste(c(fillColor,2),collapse = "")
            if(arcLabel.value >= arr.States.color.shades.thresholds[1]  &  arcLabel.value < arr.States.color.shades.thresholds[2]) fillColor <- paste(c(fillColor,2),collapse = "")
            if(arcLabel.value >= arr.States.color.shades.thresholds[2]  &  arcLabel.value < arr.States.color.shades.thresholds[3]) fillColor <- paste(c(fillColor,3),collapse = "")
            if(arcLabel.value >= arr.States.color.shades.thresholds[3] ) fillColor <- paste(c(fillColor,4),collapse = "")
          }
          
          stringa.tempi <- ""
          if( show.median.time.from.root  == TRUE) {
            tmp.tempi <- unlist(lapply(lst.nodi[[son]]$IPP, function(tmpIPP) { loadedDataset$pat.process[[tmpIPP]][lst.nodi[[son]]$depth,"pMineR.deltaDate"] }))
            if( length(tmp.tempi) > 0) {
              tmp.tempi <- as.numeric(unlist(lapply(tmp.tempi,function(x){  format((x/(24*60)),digits=3) })))
              stringa.tempi <- paste( c( "\n",min(tmp.tempi)," - ",median(tmp.tempi)," - ",max(tmp.tempi)  ), collapse = '')
              if( length(heatmap.based.on.median.time) > 0 ) {
                arr.numeri.colore <- heatmap.based.on.median.time
                paletteColorNumber <- which(c(arr.numeri.colore,Inf) - median(tmp.tempi) >= 0)[1]
                fillColor <-  paste(c(heatmap.base.color,paletteColorNumber),collapse = '')
              }
            }
          }
          if(printNodeID==TRUE) {idName <- paste(c("#",son,"\n"),collapse = '')
          } else {idName <- ""}
          riga.nodi <- paste( c("'",son,"' [ label='",idName,sonLabel,"\n(",sonHits,")",stringa.tempi,"' ,  fillcolor = '",fillColor,"' , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        } else{
          # Sono qui se si deve analizzare un predictive.mode
          if( predictive.model.engine.type == "default") {
            if(printNodeID==TRUE) {idName <- paste(c("#",son,"\n"),collapse = '')
            } else {idName <- ""}            
            
            if(sonLabel %in% predictive.model.skipNodeLabel) {
              totale.outcome <- res$num.outcome
              totale.outcome <- quanti.eventi.finali
              riga.nodi <- paste( c("'",son,"' [ label='",idName,sonLabel,"\n(",totale.outcome,")' , color=",default.arcColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
            } else {
              totale.outcome <- res$num.outcome
              totale.outcome <- quanti.eventi.finali
              
              denominatore <- lst.nodi[[son]]$hits
              percentuale <- as.integer((totale.outcome/denominatore)*100)
              
              riga.nodi <- paste( c("'",son,"' [ label='",idName,sonLabel,"\n(",totale.outcome,"/",denominatore,": ",percentuale,"%)' , color=",default.arcColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )            
            }
            riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
          }
          
          if( predictive.model.engine.type == "kNN") {
            
            # # predictive.model.engine.parameter$variables e' la lista delle covariate, specificate con i relativi attributi
            # # predictive.model.engine.parameter$outcome  contiene i dati su come elaborare l'outcome
            arr.variabili <- names(predictive.model.engine.parameter$variables)
            mtr.res <- c()
            for(variabile.label in arr.variabili ) {
            #   tmp.elemento <- predictive.model.engine.parameter$variables[[ variabile.label  ]]
            #   if( tmp.elemento$type == "timeFromRoot" ) {
            #     arr.IPP.2.explore <- lst.nodi[[son]]$IPP
            #     for(tmp.IPP in arr.IPP.2.explore) {
            #       tp.tempo <- loadedDataset$pat.process[[ tmp.IPP ]][lst.nodi[[son]]$depth, "pMineR.deltaDate"]
            #       tp.next.events <- loadedDataset$pat.process[[ tmp.IPP ]][lst.nodi[[son]]$depth:nrow(loadedDataset$pat.process[[ tmp.IPP ]]), c(loadedDataset$csv.EVENTName, "pMineR.deltaDate")]
            #       
            #       tToReachOutcome <- NA
            #       if( predictive.model.outcome %in% tp.next.events[[loadedDataset$csv.EVENTName]] ) {
            #         browser()
            #       } else { tToReachOutcome <- Inf }
            #       mtr.res <- rbind( mtr.res , c(  "IPP"=tmp.IPP,"tempo"=tp.tempo , "t2Outcome"=tToReachOutcome ) )
            #       
            #     }
            #   }
            #   browser()
            }
            browser()
          }
          
        }
        
        if( show.far.leaf & (lst.nodi[[son]]$depth == depth) & (lst.nodi[[son]]$evento != predictive.model.outcome) ) {
          arr.ultimi <- unlist(lapply( lst.nodi[[son]]$IPP, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
 
          if( length(arr.ultimi) > 0) {
            
            colore.arco <- "grey"
            if( set.to.gray == TRUE ) colore.arco <- set.to.gray.color
            colore.arco <- "grey"
            colore.nodo <- colore.arco
            
            
            tabella.ultimi <- table(arr.ultimi)
            names(tabella.ultimi) <- unlist(lapply( 1:nrow(tabella.ultimi), function(i) { paste(c( names(tabella.ultimi)[i] ,son),collapse = "_") }))
            for(i in 1:length(tabella.ultimi)) {
              fillColor <- "White"
              if(length(arr.States.color) > 0) {
                if( names(table(arr.ultimi))[i] %in% names(arr.States.color)) {
                  fillColor <- arr.States.color[ which(names(arr.States.color) == names(table(arr.ultimi))[i])  ]
                  if( set.to.gray == TRUE) { fillColor <- set.to.gray.color } 
                }
              }
              
              tmp.str.arco <- paste( c("'",son,"'->'",names(tabella.ultimi)[i],"' [style='dashed', label='', color = '",colore.arco <- "grey","', penwidth = 0.8, arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
              tmp.str.nodo <- paste( c("'",names(tabella.ultimi)[i],"' [ label='",names(table(arr.ultimi))[i],"\n(",tabella.ultimi[i],"/",res$sonHits,")' , color='",colore.nodo,"', fillcolor = '",fillColor,"' , style = filled]"),collapse = "" )
              if(is.na(abs.threshold) | sonHits >= abs.threshold) {
                arr.archi <- c( arr.archi , tmp.str.arco)
                arr.nodi <- c( arr.nodi , tmp.str.nodo )
              }

            }
          }
        }
        
        if(is.na(abs.threshold) | sonHits >= abs.threshold) {
          arr.nodi <- c( arr.nodi , riga.nodi )
          arr.archi <- c( arr.archi , riga.archi)
        }

        if( res$num.outcome > 0 ) num.outcome <- num.outcome +  res$num.outcome
        altri.nodi <- res$arr.nodi
        altri.archi <- res$arr.archi 
        
        arr.nodi <- c( arr.nodi , altri.nodi)
        arr.archi <- c( arr.archi , altri.archi)
      }
      
    }
    
    if( currentLevel == 0 ) {
      script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = ##GraphFontsize##, layout = ##kindOfGraph##]
          
            # several 'node' statements
            node [shape = ##nodeShape##, fontname = Helvetica , fontsize = 9]
            ##NodesPlaceholder##
          
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##ArcsPlaceholder##
          }" 
      NodesPlaceholder <- paste(arr.nodi,collapse = "\n")
      ArcsPlaceholder <- paste(arr.archi,collapse = "\n")
      script <- str_replace_all( script , "##NodesPlaceholder##", NodesPlaceholder )
      script <- str_replace_all( script , "##ArcsPlaceholder##", ArcsPlaceholder )
      script <- str_replace_all( script , "##kindOfGraph##", kindOfGraph )
      script <- str_replace_all( script , "##GraphFontsize##", GraphFontsize )
      script <- str_replace_all( script , "##nodeShape##", nodeShape )
    }
    if(  length(arrId2Jump) == 0 ) {
      if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
        num.outcome <- lst.nodi[[starting.ID]]$hits
      }
    }
    return(list( "arr.nodi"=arr.nodi,
                 "arr.archi"=arr.archi, 
                 "script"=script,
                 "num.outcome" = num.outcome, 
                 "sonHits"= totaleSonHits,
                 "heatmap.based.on.median.time" = heatmap.based.on.median.time))
  }
  
  plotCFGraphComparison <- function( stratifyFor , stratificationValues=c(), stratificationThreshold = NA,
                                     arr.stratificationValues.A = c(), arr.stratificationValues.B = c(),
                                     depth = 4, fisher.threshold = 0.5,
                                     checkDurationFromRoot = FALSE, 
                                     hitsMeansReachAGivenFinalState = FALSE, finalStateForHits = c(),
                                     arr.States.color=c("Deces"="Red","intensive care"="Orange","Recovered"="YellowGreen"), 
                                     debug.it = F, show.far.leaf = FALSE , abs.threshold = NA , kindOfGraph = "neato",
                                     nodeShape = "oval" , UM = "days") {
    
    # stratificationValues <- c(1,2)
    b <- plot.comparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, 
                          stratificationThreshold = stratificationThreshold,
                          arr.stratificationValues.A = arr.stratificationValues.A, arr.stratificationValues.B = arr.stratificationValues.B,
                          depth = depth,
                          fisher.threshold = fisher.threshold, checkDurationFromRoot = checkDurationFromRoot,
                          hitsMeansReachAGivenFinalState = hitsMeansReachAGivenFinalState, finalStateForHits = finalStateForHits,
                          arr.States.color=arr.States.color, set.to.gray = FALSE , set.to.gray.color= "WhiteSmoke",
                          debug.it = debug.it, show.far.leaf = show.far.leaf, abs.threshold = abs.threshold,
                          kindOfGraph = kindOfGraph, nodeShape = nodeShape, UM = UM)
    return(b)
  }
  compare.array <- function( a, b ) {
    if( length( a ) != length( b ) ) return( FALSE )
    tmp <- unlist(lapply( 1:length(a), function(x) { if(a[x]!=b[x])  return(TRUE); return(FALSE) } ))
    if(sum(tmp)>0) return( FALSE )
    return( TRUE )
  }
  getPatientWithSpecificedPath <- function( sequenza ){
    IDName <- loadedDataset$csv.IDName; EventName <- loadedDataset$csv.EVENTName
    arr.ID <- unlist(lapply( names(loadedDataset$pat.process), function(ID) {
      if(compare.array( loadedDataset$pat.process[[ID]][[EventName]][1:min(length(loadedDataset$pat.process[[ID]][[EventName]]),
                                                                           length(sequenza))] , sequenza) ==TRUE  ) return(ID)
      return(NA)
    }))
    arr.ID <- arr.ID[which(!is.na(arr.ID))]
    return(arr.ID)
  }
  
  plot.comparison <- function( stratifyFor, stratificationValues, stratificationThreshold = NA,
                               arr.stratificationValues.A = c(), arr.stratificationValues.B = c(),
                               fisher.threshold = 0.05, checkDurationFromRoot = FALSE,
                               hitsMeansReachAGivenFinalState = FALSE, finalStateForHits = c(),
                               starting.ID = "root", sequenza =c("root") , currentLevel = 0, 
                               depth = 4, arr.States.color=c(), GraphFontsize = "9" ,
                               set.to.gray = FALSE , set.to.gray.color= "WhiteSmoke", 
                               debug.it = F, show.far.leaf = FALSE , abs.threshold = NA,
                               kindOfGraph = "neato", nodeShape = "oval", UM = "days") {
    
    IDName <- loadedDataset$csv.IDName; EventName <- loadedDataset$csv.EVENTName
    decoded.seq <- sequenza[ which(sequenza!="root")]
    if( length(decoded.seq) > 0 ) {
      decoded.seq <- unlist(lapply( 1:length(decoded.seq), function(i) { lst.nodi[[decoded.seq[[i]]]]$evento } ))      
    }
    
    # -im rg - stratificationValues
    if( length(stratificationValues) ) {
      arr.stratificationValues.A <- stratificationValues[1]
      arr.stratificationValues.B <- stratificationValues[2]
    }
    # -fm rg - stratificationValues
    
    if( lst.nodi[[starting.ID]]$depth == depth ) {
      # browser()
      if( debug.it == TRUE)  browser()
      # -im rg - stratificationValues
      
      if( !is.na(stratificationThreshold) ) {
        first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] <= stratificationThreshold ) ])
        second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] > stratificationThreshold ) ])
      } else {
        first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] %in% arr.stratificationValues.A ) ])
        second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] %in% arr.stratificationValues.B ) ])
      }
      # first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
      # second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
      # -fm rg - stratificationValues
      
      ID <- getPatientWithSpecificedPath( decoded.seq )
      
      quanti.first <- sum( ID %in% first.ID)
      quanti.second <- sum( ID %in% second.ID)
      
      validi.first.ID <- ID[ID %in% first.ID]
      validi.second.ID <- ID[ID %in% second.ID] 
      
      return(list("stringa.nodi"=c(),"stringa.archi"=c(),"script"="", "sonHits" = lst.nodi[[starting.ID]]$hits,
                  "first.hits" = quanti.first, "second.hits" = quanti.second ,
                  "first.ID" = validi.first.ID, "second.ID" = validi.second.ID, 
                  "first.missed"= (length(first.ID)-quanti.first), 
                  "second.missed"=(length(second.ID)-quanti.second)
      ))
    }
    
    arrId2Jump <- names(which(MM[starting.ID,]!=""))
    if( length(arrId2Jump) > 1 ) {
      arrId2Jump <- arrId2Jump[order(unlist(lapply(arrId2Jump,function(i) { lst.nodi[[i]]$evento })))]  
    }    
    arr.nodi <- c()
    arr.archi <- c()
    script <- ""
    fillColor <- "White"
    arcLabel <- ""
    arcColor <- "Black"
    penwidth <- 0.5
    arc.fontsize <- 10
    if( UM == "minutes") divisore <- 60 
    if( UM == "hours") divisore <- 60 
    if( UM == "days") divisore <- 60 * 24
    if( UM == "weeks") divisore <- 60 * 24 * 7
    if( UM == "months") divisore <- 60 * 24 * 30
    if( UM == "years") divisore <- 60 * 24 * 365
    if( set.to.gray == TRUE) { fillColor <- set.to.gray.color;  }
    
    if( currentLevel == 0) {
      arr.nodi <- c(paste( c("'",starting.ID,"' [ label='",lst.nodi[[starting.ID]]$evento,"', penwidth=3]"),collapse = "" ))
    }    
    
    num.outcome <- 0
    totaleSonHits <- 0
    # totale <- lst.nodi[[starting.ID]]$hits
    
    if( length(arrId2Jump) > 0 ) {
      # totale <- sum(unlist(lapply( arrId2Jump, function(x) {lst.nodi[[x]]$hits} )))
      # browser()
      num.outcome <- 0
      
      for( son in arrId2Jump) {
        sonLabel <- lst.nodi[[son]]$evento
        sonHits <- lst.nodi[[son]]$hits
        totaleSonHits <- totaleSonHits + sonHits
        
        if(!is.na(abs.threshold) & sonHits < abs.threshold) next;
        
        # if( currentLevel == 1) browser()
        
        # percentuale <- as.integer((sonHits/totale)*100)
        # penwidth <- 5*(percentuale/100)+0.2
        
        res <- plot.comparison( stratifyFor = stratifyFor, stratificationValues = stratificationValues, 
                                stratificationThreshold = stratificationThreshold,
                                arr.stratificationValues.A = arr.stratificationValues.A, 
                                arr.stratificationValues.B = arr.stratificationValues.B,
                                starting.ID = son, currentLevel = currentLevel + 1, sequenza = c(sequenza,son),
                                GraphFontsize = GraphFontsize, fisher.threshold = fisher.threshold,
                                checkDurationFromRoot = checkDurationFromRoot,
                                hitsMeansReachAGivenFinalState = hitsMeansReachAGivenFinalState, finalStateForHits = finalStateForHits,
                                depth = depth, arr.States.color = arr.States.color,
                                set.to.gray = set.to.gray, show.far.leaf = show.far.leaf,
                                abs.threshold = abs.threshold, kindOfGraph = kindOfGraph, nodeShape = nodeShape, UM = UM) 
        # if(son=="0") browser() 
        # browser()
        matriceFisher <- matrix( c(res$first.hits, res$first.missed , res$second.hits , res$second.missed), byrow = F, ncol=2 )
        wilcoxTest.p <- NA
        if(checkDurationFromRoot == TRUE) {
          if( length(res$first.ID) > 0 & length(res$second.ID) > 0 & starting.ID!="root") {
            wilcoxTest.p <- suppressWarnings(wilcox.test( paired = FALSE,
                                                          unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })) / divisore,
                                                          unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) })) / divisore)$p.value)
            wilcoxTest.p <- as.numeric(format(wilcoxTest.p, digits = 3))
          } else {
            wilcoxTest.p <- 1
          }
        }
        if( is.na(wilcoxTest.p) ) wilcoxTest.p <- 1
        # cat("\n", son)
        # if( son %in% c("0","1","9","37","2") ) browser()
        # cat("\n FT: ",sum(matriceFisher[1,])," - ", ((sum(matriceFisher[2,])*fisher.threshold)) )
        if(checkDurationFromRoot == FALSE) {
          # if( sum(matriceFisher[1,]) > ((sum(matriceFisher[2,])*fisher.threshold)) ) {
          if( sum(matriceFisher)> 20 ) { 
            
            # browser()
            p.value <- as.numeric(format(fisher.test(matriceFisher)$p.value,digits = 3))
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( p.value < fisher.threshold) fillColor <- "Yellow";
            if( p.value < fisher.threshold) fillColor <- "Yellow";
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        } else {
          if( ((length(res$first.ID)+length(res$second.ID)) > 7) & length(res$first.ID)>3 & length(res$second.ID)>3 ) { 
            # browser()
            fillColor <- "White"; 
            borderColor <- "Black"
            fontColor <- "Black"
            arcColor <- "Black"
            
            if( wilcoxTest.p < 0.05) fillColor <- "Yellow";
            if( wilcoxTest.p < 0.01) fillColor <- "Yellow";
            p.value <- wilcoxTest.p
          } else {
            p.value = "NA"
            set.to.gray <- TRUE;
            fillColor <- set.to.gray.color; 
            borderColor <- "Gray"
            fontColor <- "Gray"
            arcColor <- "Gray"
          }
        }
        Stringa.Totali.Originali<-""
        if(checkDurationFromRoot == FALSE) {
          if( hitsMeansReachAGivenFinalState == TRUE ) {
            # browser()
            fillColor <- "White";
            
            morti.first <- sum(unlist(lapply( res$first.ID , function(IPP) {
              return(finalStateForHits %in% loadedDataset$pat.process[[IPP]][  , loadedDataset$csv.EVENTName ])
            })))
            morti.second <- sum(unlist(lapply( res$second.ID , function(IPP) {
              return(finalStateForHits %in% loadedDataset$pat.process[[IPP]][  , loadedDataset$csv.EVENTName ])
            })))            
            totali.first <- length(res$first.ID)
            totali.second <- length(res$second.ID) 
            # bbb <- matrix( c(morti.first, (totali.first-totali.first), morti.second , (totali.second-morti.second) ), nrow=2)
            bbb <- matrix( c( (totali.first-morti.first),morti.first, (totali.second-morti.second),morti.second  ), nrow=2)
            p.value.fisher <- fisher.test(bbb)$p.value
            
            # -im RG
            # res$first.hits <- as.numeric(format(morti.first / totali.first,digits = 2))
            # res$second.hits <- as.numeric(format(morti.second / totali.second,digits = 2))
            tmp.res.first.hits <- as.numeric(format(morti.first / totali.first,digits = 2))
            tmp.res.second.hits <- as.numeric(format(morti.second / totali.second,digits = 2))
            
            # -fm RG
            
            # Stringa.Totali.Originali <- paste(c("\n",morti.first,"/",totali.first," vs ",morti.second,"/",totali.second),collapse = '')
            
            p.value <- p.value.fisher
            p.value <- as.numeric(format(p.value,digits = 3))
            
            if( p.value < fisher.threshold) fillColor <- "Yellow";
            
            if( (morti.first + morti.second) < 10  ) {
              p.value = "NA"
              set.to.gray <- TRUE;
              fillColor <- set.to.gray.color; 
              borderColor <- "Gray"
              fontColor <- "Gray"
              arcColor <- "Gray"
            }
            # browser()
            ratio.hits <- format( (tmp.res.first.hits / tmp.res.second.hits) , digits = 2)
            orig.ratio.hits <- res$first.hits / res$second.hits
            Stringa.Totali.Originali <- paste(c("\n",morti.first,"/",totali.first," vs ",morti.second,"/",totali.second),collapse = '')
            if( hitsMeansReachAGivenFinalState == FALSE ) {
              Stringa.sotto <- paste(c("\n",tmp.res.first.hits,"/",tmp.res.second.hits,"(ratio ",ratio.hits," : ",format(((as.numeric(orig.ratio.hits) - (totali.first / totali.second))/(totali.first / totali.second))*100,digits = 4),"%)","\n p = ",p.value),collapse = '')
            } else {
              strano <- format( ((morti.first / morti.second) - (totali.first / totali.second))/(totali.first / totali.second)*100,digits = 4 ) 
              Stringa.sotto <- paste(c("\n",tmp.res.first.hits,"/",tmp.res.second.hits,", ratio ",ratio.hits," \n ",strano,"%","\n p = ",p.value),collapse = '')
              # Stringa.sotto <- paste(c("\n",tmp.res.first.hits,"/",tmp.res.second.hits,"\n p = ",p.value),collapse = '')  
            }
            
            
          } else {
            # -im RG
            # browser()
            totali.first <- res$first.missed + res$first.hits
            totali.second <- res$second.missed + res$second.hits             
            ratio.hits <- format( (res$first.hits / res$second.hits) , digits = 2)
            primo.oo <- format( res$first.hit , digits = 2); secondo.oo <- format( res$second.hits , digits = 2)
            orig.ratio.hits <- res$first.hits / res$second.hits
            Stringa.Totali.Originali <- paste(c("\n",res$first.hits,"/",totali.first," vs ",res$second.hits,"/",totali.second),collapse = '')
            # Stringa.sotto <- paste(c("\n","(ratio ",ratio.hits," : ",format(((as.numeric(orig.ratio.hits) - (totali.first / totali.second))/(totali.first / totali.second))*100,digits = 4),"%)","\n p = ",p.value),collapse = '')
            Stringa.sotto <- paste(c("\n",primo.oo,"/",secondo.oo,", ratio ",ratio.hits," \n ",format(((as.numeric(orig.ratio.hits) - (totali.first / totali.second))/(totali.first / totali.second))*100,digits = 4),"%","\n p = ",p.value),collapse = '')
            # -fm RG
          }
          
          
          # -im RG
          # ratio.hits <- format( (res$first.hits / res$second.hits) , digits = 2)
          # riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,Stringa.Totali.Originali,"\n",res$first.hits,"/",res$second.hits,"(",ratio.hits,")","\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,Stringa.Totali.Originali,Stringa.sotto,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          # -fm RG
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
        } else {
          a <- unlist(lapply( res$first.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          b <- unlist(lapply( res$second.ID, function(IPP) { return( loadedDataset$pat.process[[IPP]]$pMineR.deltaDate[currentLevel+1] ) }))
          
          a <- as.integer(mean(a)/divisore)
          b <- as.integer(mean(b)/divisore)
          riga.nodi <- paste( c("'",son,"' [ label='",sonLabel,"\n mdn:",a," vs ",b,"\n p = ",p.value,"' ,  fontcolor = ",fontColor,", color = ",borderColor,", fillcolor = ",fillColor," , style = filled]"),collapse = "" )
          riga.archi <- paste( c("'",starting.ID,"'->'",son,"' [label='",arcLabel,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
          
        }
        
        # -RG
        # browser()
        
        if( show.far.leaf & (lst.nodi[[son]]$depth == depth ) &
            ( hitsMeansReachAGivenFinalState == FALSE ) &
            ( checkDurationFromRoot == FALSE )) {
          arr.ultimi <- unlist(lapply( lst.nodi[[son]]$IPP, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
          
          arr.ultimi.first <- unlist(lapply( res$first.ID, function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )
          arr.ultimi.second <- unlist(lapply( res$second.ID , function(IPP) {
            return(tail(loadedDataset$pat.process[[IPP]][[ loadedDataset$csv.EVENTName ]],n=1))
          } ) )          
          
          colore.arco <- "grey"
          if( set.to.gray == TRUE ) colore.arco <- set.to.gray.color
          colore.arco <- "grey"
          colore.nodo <- colore.arco
          
          tabella.ultimi.first <- table(arr.ultimi.first)
          tabella.ultimi.second <- table(arr.ultimi.second)
          
          arr.possibili.stati <- unique( c( names(tabella.ultimi.first),names(tabella.ultimi.second) ))
          
          for(i in 1:length(arr.possibili.stati)) {
            
            if(  arr.possibili.stati[i] %in% names(tabella.ultimi.first) ) {
              quanti.first <- tabella.ultimi.first[ arr.possibili.stati[i] ]
            } else {
              quanti.first <- 0
            }
            if(  arr.possibili.stati[i] %in% names(tabella.ultimi.second) ) {
              quanti.second <- tabella.ultimi.second[ arr.possibili.stati[i] ]
            } else {
              quanti.second <- 0
            }
            # -RG
            cat("\n verificare la seguente matrice x il Fisher")
            # browser()
            matriceFisher.leaf <- matrix( c(quanti.first, res$first.missed , quanti.second , res$second.missed), byrow = F, ncol=2 )
            p.value <- "NA"
            fillColor <- "White";
            if(checkDurationFromRoot == FALSE) {
              if( sum(matriceFisher.leaf[1,]) > ((sum(matriceFisher.leaf[2,])*fisher.threshold)) ) { 
                # browser()
                p.value <- as.numeric(format(fisher.test(matriceFisher.leaf)$p.value,digits = 3))
                
                if( p.value < fisher.threshold) fillColor <- "Yellow";
                if( p.value < fisher.threshold) fillColor <- "Yellow";
              } else {
                p.value = "NA"
                set.to.gray <- TRUE;
              }
            }
            
            nome.nodo.tmp <- paste(c(arr.possibili.stati[i],"_",son),collapse='')
            
            tmp.str <- paste( c("'",son,"'->'",nome.nodo.tmp,"' [style='dashed', label='', color = '",colore.arco <- "grey","', penwidth = 0.8, arrowsize=0.8, fontsize = ",arc.fontsize,"]"),collapse = "" )
            arr.archi <- c( arr.archi , tmp.str)
            tmp.str <- paste( c("'",nome.nodo.tmp,"' [ label='",arr.possibili.stati[i],"\n(",quanti.first ,"/",quanti.second,")\n p = ",p.value,"' , color='",colore.nodo,"', fillcolor = '",fillColor,"' , style = filled]"),collapse = "" )            
            arr.nodi <- c( arr.nodi , tmp.str )
          }
          
        }        
        
        arr.nodi <- c( arr.nodi , riga.nodi )
        arr.archi <- c( arr.archi , riga.archi)
        
        # if( res$num.outcome > 0 ) num.outcome <- num.outcome +  res$num.outcome
        altri.nodi <- res$arr.nodi
        altri.archi <- res$arr.archi 
        
        arr.nodi <- c( arr.nodi , altri.nodi)
        arr.archi <- c( arr.archi , altri.archi)   
        
      }
    }
    # browser()
    if( currentLevel == 0 ) {
      script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = ##GraphFontsize##, layout = ##kindOfGraph##]
          
            # several 'node' statements
            node [shape = ##nodeShape##, fontname = Helvetica , fontsize = 9]
            ##NodesPlaceholder##
          
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##ArcsPlaceholder##
          }" 
      NodesPlaceholder <- paste(arr.nodi,collapse = "\n")
      ArcsPlaceholder <- paste(arr.archi,collapse = "\n")
      script <- str_replace_all( script , "##NodesPlaceholder##", NodesPlaceholder )
      script <- str_replace_all( script , "##ArcsPlaceholder##", ArcsPlaceholder )
      script <- str_replace_all( script , "##GraphFontsize##", GraphFontsize )
      script <- str_replace_all( script , "##kindOfGraph##", kindOfGraph )
      script <- str_replace_all( script , "##nodeShape##", nodeShape )
      
      
    }
    # if(  length(arrId2Jump) == 0 ) {
    #   if(lst.nodi[[starting.ID]]$evento == predictive.model.outcome) {
    #     num.outcome <- lst.nodi[[starting.ID]]$hits
    #   }
    # }
    # browser()
    # -im rg - stratificationValues
    if( !is.na(stratificationThreshold) ) {
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] <= stratificationThreshold ) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] > stratificationThreshold ) ])
    } else {
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] %in% arr.stratificationValues.A ) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]] %in% arr.stratificationValues.B ) ])
    }
    # first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
    # second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
    # -fm rg - stratificationValues
    
    ID <- getPatientWithSpecificedPath( decoded.seq )
    
    quanti.first <- sum( ID %in% first.ID)
    quanti.second <- sum( ID %in% second.ID)
    # browser()
    # cat("\n ", length(quanti.first) , " - ", length(quanti.second)  )
    
    validi.first.ID <- ID[ID %in% first.ID]
    validi.second.ID <- ID[ID %in% second.ID]
    # browser()
    return(list("arr.nodi"=arr.nodi,"arr.archi"=arr.archi, "script"=script,"num.outcome" = num.outcome, 
                "first.hits" = quanti.first, "second.hits" = quanti.second ,
                "first.missed"= (length(first.ID)-quanti.first), "second.missed"=(length(second.ID)-quanti.second),
                "first.ID" = validi.first.ID, "second.ID" = validi.second.ID,  
                "sonHits"= totaleSonHits))
  }

  pathBeetweenStackedNodes <- function( fromState , toState, stratifyFor = "" , minPath = FALSE, stratificationValues , fisher.threshold = 0.05,
                                        kindOfGraph = "dot", arcColor = "black", arc.fontsize = 10, arc.fontcolor = "red",
                                        arr.States.color=c(), set.to.gray.color= "WhiteSmoke", p.value.threshold = 0.05,
                                        giveBackMatrix = FALSE ) {
    
    stratify <- FALSE
    parameter.arcColor <- arcColor
    parameter.arc.fontcolor <- arc.fontcolor
    if( stratifyFor != "" ) stratify <- TRUE
    # browser()
    # get all the paths with at least one occurrence
    a <- unlist(lapply(1:length(loadedDataset$wordSequence.raw), function(i) {  
      a <- which(loadedDataset$wordSequence.raw[[i]]==fromState)
      b <- which(loadedDataset$wordSequence.raw[[i]]==toState)
      if( fromState == "BEGIN") { a <- 1 }
      if( toState == "END")   { b <- length(loadedDataset$wordSequence.raw[[i]]) }
      if(length(a)==0 | length(b)==0) return(FALSE)
      if( min(a) < min(b) ) return(TRUE) 
      return(FALSE)  
    }  ))
    IPP.all <- names(loadedDataset$wordSequence.raw)[a]
    if( length(IPP.all) == 0 ) return()
    # browser()
    # extract the possible occurencies
    lst.path <- lapply( IPP.all , function(IPP) {
      a <- which(loadedDataset$wordSequence.raw[[IPP]]==fromState)
      b <- which(loadedDataset$wordSequence.raw[[IPP]]==toState)   
      if( fromState == "BEGIN") { a <- 1 }
      if( toState == "END")   { b <- length(loadedDataset$wordSequence.raw[[IPP]]) }
      if( minPath == TRUE ) { inizio <- max(a[a < b])
      } else {  inizio <- min(a) }
      fine <- min(b[b>inizio])  
      cosa.ritornare <- loadedDataset$wordSequence.raw[[IPP]][inizio:fine]
      if( fromState == "BEGIN") cosa.ritornare <- c( "BEGIN", cosa.ritornare )
      if( toState == "END") cosa.ritornare <- c( cosa.ritornare , "END")
      return( cosa.ritornare )
    })
    # browser()
    str.path <- unlist(lapply( lst.path, function(x) {paste( x, collapse = "->" )}))
    # if( fromState == "BEGIN") lst.path <- unlist(lapply(1:length(str.path),function(i){ paste( c("BEGIN->",str_trim(str.path[[i]])),collapse = '') }))
    # if( toState == "END") lst.path <- unlist(lapply(1:length(str.path),function(i){ paste( c(str_trim(str.path[[i]]),"->END"),collapse = '') }))
    
    kind.of.path <- table(str.path)
    numero.in.From <- sum(kind.of.path); numero.in.To <- sum(kind.of.path)
    
    if( stratify == TRUE ) {
      IDName <- loadedDataset$csv.IDName;
      first.ID <- unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[1]) ])
      second.ID <-unique(loadedDataset$original.CSV[[IDName]][  which(loadedDataset$original.CSV[[stratifyFor]]==stratificationValues[2]) ])
      first.ID <- first.ID[which( first.ID %in% IPP.all)]
      second.ID <- second.ID[which( second.ID %in% IPP.all)]  
      first.kind.of.path <- table(str.path[which(IPP.all %in% first.ID)])
      second.kind.of.path <- table(str.path[which(IPP.all %in% second.ID)])
    }
    
    nodeColor <- "White"
    if( fromState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==fromState)]
    if( stratify == FALSE ) {
      nodo.inizio <- paste( c("'fromState' [ label='",fromState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')  
    } else {
      nodo.inizio <- paste( c("'fromState' [ label='",fromState,"\n(",length(first.ID),"/",length(second.ID),")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
    }
    
    nodo.fine <- paste( c("'toState' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = White , style = filled]"),collapse = '')
    
    str.fatte <- c()
    ct <- 1
    arr.nuovi.nodi <- c(); arr.nuovi.archi <- c()
    for( i in 1:length(lst.path) ) {
      
      stringa.sequenza <- paste( lst.path[[i]], collapse = "->" )
      
      numero.ricorrenze <- as.numeric(kind.of.path[which( names(kind.of.path) == stringa.sequenza)])
      if( stratify == TRUE ) {
        first.ricorrenza <- as.numeric(first.kind.of.path[which( names(first.kind.of.path) == stringa.sequenza)])
        second.ricorrenza <- as.numeric(second.kind.of.path[which( names(second.kind.of.path) == stringa.sequenza)])
        if(length(first.ricorrenza)==0) first.ricorrenza <- 0
        if(length(second.ricorrenza)==0) second.ricorrenza <- 0
        first.totale <- sum(first.kind.of.path)
        second.totale <- sum(second.kind.of.path)
        
        piccolaM <- matrix(  c( first.ricorrenza , second.ricorrenza , (first.totale-first.ricorrenza) , (second.totale-second.ricorrenza) ), nrow=2 , byrow = T )
        p.value <- fisher.test(piccolaM)$p.value
        p.value <- as.numeric(format(p.value,digits = 3))
        
        aaa <- ((sum(piccolaM[2,])*fisher.threshold))
        bbb <-  sum(piccolaM[1,])
        if( bbb > aaa ) Fisher.valido <- TRUE
        else Fisher.valido <- FALSE
      }
      
      penwidth = 0.3 + 3 * (numero.ricorrenze / numero.in.From)
      
      if( !(stringa.sequenza %in% str.fatte) ) { 
        
        for( pos in 2:length(lst.path[[i]]) ) {
          
          nodeColor <- "White"
          nodeBorderColor <- "Black"
          nodeFontColor <- "Black"
          nomeNodo <- as.character(ct)
          nomeNodoPrecedente <- as.character(ct - 1)
          labelNuovoNodo <- lst.path[[i]][[pos]]
          
          # se lunghezza e' due o se e' il primo
          if( pos == 2 | length(lst.path[[i]])==2) {
            # se lunghezzza e' 2'
            if( length(lst.path[[i]]) == 2 ) {
              nuovoNodo <- ""
              if( stratify == FALSE ) {
                nuovoArco <- paste( c("'fromState'->'",toState,"' [label='",numero.ricorrenze,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              } else {
                if( Fisher.valido == TRUE) {
                  if( p.value < p.value.threshold ) {
                    arcColor <- "Red"
                    arc.fontcolor <- parameter.arc.fontcolor                      
                  } else {
                    arcColor <- parameter.arcColor
                    arc.fontcolor <- "Black"
                  }
                } else {
                  arcColor <- set.to.gray.color
                  arc.fontcolor <- set.to.gray.color
                }
                nuovoArco <- paste( c("'fromState'->'",toState,"' [label='",first.ricorrenza,"/",second.ricorrenza," \np=",p.value,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor," ]"),collapse = "" )                  
              }
            } else {
              if( stratify == FALSE ) {
                # See e' il primo
                if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                nuovoArco <- paste( c("'fromState'->'",nomeNodo,"' [label='",numero.ricorrenze,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize," , fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              } else {
                if( Fisher.valido == TRUE) {
                  arcColor <- parameter.arcColor
                  arc.fontcolor <- "Black"
                  nodeColor <- "White"
                  if(p.value < p.value.threshold ) nodeColor <- "Yellow"
                  if(p.value < p.value.threshold ) arcColor <- "Red"
                  if(p.value < p.value.threshold ) arc.fontcolor <- parameter.arc.fontcolor
                  # if(p.value < p.value.threshold ) arcColor <- "Red"
                } else {
                  arcColor <- "Gray"
                  arc.fontcolor <- "Gray"
                  nodeColor <- set.to.gray.color
                  nodeBorderColor <- "Gray"
                  nodeFontColor <- "Gray"
                  p.value <- NA
                }
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = ",nodeFontColor,", color = ",nodeBorderColor,", fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                nuovoArco <- paste( c("'fromState'->'",nomeNodo,"' [label='",first.ricorrenza,"/",second.ricorrenza," \np=",p.value,"', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize," , fontcolor = ",arc.fontcolor," ]"),collapse = "" )
              }
            }
          }  else { 
            # Sono negli altri (fosse anche l'ultimo)
            if( pos == length(lst.path[[i]]) ) nomeNodo <- toState
            # E' l'utimo
            if( nomeNodo == toState)
            {
              if( stratify == FALSE ) {
                if( toState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==toState)]
                nuovoNodo <- paste( c("'",toState,"' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
              } else {
                if( toState %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==toState)]
                nuovoNodo <- paste( c("'",toState,"' [ label='",toState,"\n(",numero.in.From,")' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')
              }
            }
            else
            {
              if( stratify == FALSE ) {
                # Uno dei tanti
                if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')                
              } else {
                # Uno dei tanti
                if( Fisher.valido == TRUE) {
                  # arcColor <- parameter.arcColor
                  arc.fontcolor <- parameter.arc.fontcolor
                  nodeColor <- "White"
                  arc.fontcolor <- "Black"
                  if(p.value < p.value.threshold ) nodeColor <- "Yellow"
                  if(p.value < p.value.threshold ) arcColor <- "Red"
                  if(p.value < p.value.threshold ) arc.fontcolor <- parameter.arc.fontcolor
                } else {
                  arcColor <- "Gray"
                  arc.fontcolor <- "Gray"
                  nodeColor <- set.to.gray.color
                  nodeBorderColor <- "Gray"
                  nodeFontColor <- "Gray"
                  p.value <- NA
                }
                # if( labelNuovoNodo %in% names(arr.States.color) ) nodeColor <- arr.States.color[which(names(arr.States.color)==labelNuovoNodo)]
                nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = ",nodeFontColor,", color = ",nodeBorderColor,", fillcolor = ",nodeColor," , style = filled]"),collapse = '')
                # nuovoNodo <- paste( c("'",nomeNodo,"' [ label='",labelNuovoNodo,"' ,  fontcolor = Black, color = Black, fillcolor = ",nodeColor," , style = filled]"),collapse = '')                
              }
            }
            if( stratify == FALSE ) {
              nuovoArco <- paste( c("'",nomeNodoPrecedente,"'->'",nomeNodo,"' [label='', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor,"]"),collapse = "" )
            } else {
              nuovoArco <- paste( c("'",nomeNodoPrecedente,"'->'",nomeNodo,"' [label='', color = ",arcColor,", penwidth = ",penwidth,", arrowsize=0.8, fontsize = ",arc.fontsize,", fontcolor = ",arc.fontcolor,"]"),collapse = "" )
            }
          }
          arr.nuovi.nodi <- c( arr.nuovi.nodi , nuovoNodo )
          arr.nuovi.archi <- c ( arr.nuovi.archi, nuovoArco )
          
          ct <- ct + 1
        }
      }
      str.fatte <- c( str.fatte , stringa.sequenza )
    }
    
    str.nuovi.nodi <- paste( arr.nuovi.nodi, collapse = "\n")
    str.nuovi.archi <- paste( arr.nuovi.archi, collapse = "\n")
    
    script <- "
          digraph boxes_and_circles {
            graph [overlap = false, fontsize = 1, layout = ##kindOfGraph##]
          
            # several 'node' statements
            node [shape = oval, fontname = Helvetica , fontsize = 9]
            ##nodo.inizio##
            ##str.nuovi.nodi##
            
            # several 'edge' statements
            edge [ fontname = Helvetica]
            ##str.nuovi.archi##
          }"
    
    script <- str_replace_all( script , "##nodo.inizio##", nodo.inizio )
    script <- str_replace_all( script , "##str.nuovi.nodi##", str.nuovi.nodi )
    script <- str_replace_all( script , "##str.nuovi.archi##", str.nuovi.archi )
    script <- str_replace_all( script , "##kindOfGraph##", kindOfGraph )
    
    if( giveBackMatrix == TRUE ) {
      script <- build.result.table.pathBeetweenStackedNodes( lst.path = lst.path , fromState = fromState , 
                                                             toState = toState, stratifyFor = stratifyFor, 
                                                             minPath = minPath, stratificationValues = stratificationValues, 
                                                             fisher.threshold = fisher.threshold ) 
    }
    
    return(script)
  }
  
  build.result.table.pathBeetweenStackedNodes <- function( lst.path , fromState , toState, stratifyFor, 
                                                           minPath, stratificationValues, fisher.threshold) {
    browser()
    stringa.sequenza <- paste( lst.path[[i]], collapse = "->" )
    
    kind.of.path <- table(stringa.sequenza)
    numero.in.From <- sum(kind.of.path); numero.in.To <- sum(kind.of.path)
    
    numero.ricorrenze <- as.numeric(kind.of.path[which( names(kind.of.path) == stringa.sequenza)])
    if( stratify == TRUE ) {
      first.ricorrenza <- as.numeric(first.kind.of.path[which( names(first.kind.of.path) == stringa.sequenza)])
      second.ricorrenza <- as.numeric(second.kind.of.path[which( names(second.kind.of.path) == stringa.sequenza)])
      if(length(first.ricorrenza)==0) first.ricorrenza <- 0
      if(length(second.ricorrenza)==0) second.ricorrenza <- 0
      first.totale <- sum(first.kind.of.path)
      second.totale <- sum(second.kind.of.path)
      
      piccolaM <- matrix(  c( first.ricorrenza , second.ricorrenza , (first.totale-first.ricorrenza) , (second.totale-second.ricorrenza) ), nrow=2 , byrow = T )
      p.value <- fisher.test(piccolaM)$p.value
      p.value <- as.numeric(format(p.value,digits = 3))
      
      aaa <- ((sum(piccolaM[2,])*fisher.threshold))
      bbb <-  sum(piccolaM[1,])
      if( bbb > aaa ) Fisher.valido <- TRUE
      else Fisher.valido <- FALSE
    }
    
  }
  
  #=================================================================================
  # kaplanMeierCurves
  #================================================================================= 
  
  kaplanMeierCurves <- function( id.start, id.end, cens.leaf = TRUE,  id.cens = c(), 
                                UM = "days", plotIt = TRUE ){
    
    s <- getDataStructure()
    out <- loadedDataset
    autocens <- TRUE

    # Verifica se ci sono ID sui nodi censored    
    if( !identical(is.na(id.cens),logical(0)) ) {
      
      autocens <- FALSE
      # costruisci la lista che per ogni nodo cens contiene una matrice con id e rispettivo nodo cens
      paz.list<-lapply(id.cens, function(nodo.cens){
        id.paz<-s$lst.nodi[[as.character(nodo.cens)]]$IPP
        paz<-cbind(id.paz, nodo.cens)
        return(paz)
      })
      
      names(paz.list) <- id.cens
      paz.mat<-c()
      for (i in c(1:length(paz.list))) {
        paz.mat<-rbind(paz.mat,paz.list[[i]])
      }
    }
    
    #seleziono coorte: tutti gli id che transitano in nodo start
    coorte <- s$lst.nodi[[as.character(id.start)]]$IPP
    
    #controllo che nodo end sia nel sottoalbero che inizia con nodo start
    check <- which( s$lst.nodi[[ as.character(id.start) ]]$IPP %in% s$lst.nodi[[ as.character(id.end) ]]$IPP)
    if( identical(check, integer(0)) ){
      to_ret <- NULL
      KM0 <- NA
    } else {

      #ciclo su tutti gli id di coorte e calcolo deltaT come:
      #delta date al nodo evento - delta date al nodo start
      tmp<-lapply(coorte, function(id){
        time.start <- s$lst.nodi[[ as.character(id.start) ]]$pMineR.deltaDate[ which( s$lst.nodi[[ as.character(id.start) ]]$IPP == id ) ]
        
        #possono esserci piu' nodi di end
        time.end <- array()
        for (i in c(1:length(id.end))) {
          dt <- s$lst.nodi[[ as.character( id.end[i] ) ]]$pMineR.deltaDate[ which( s$lst.nodi[[ as.character(id.end[i])]]$IPP == id ) ]
          if(identical(dt,numeric(0))){
            time.end[i] <- NA
          } else{
            time.end[i] <- dt
          }
        }
        
        if(length(time.end) == 1) {
          deltaT <- NA
          if(!is.na(time.end)){
            deltaT<-time.end-time.start
          }
          # puo' accadere che i due nodi di end siano sullo stesso ramo e che lo stesso paziente li sperimenti entrambi---> E' giusto???
          # in questo caso prendo come data end quella relativa all'evento piu' lontano nel tempo????
        } else {
          if( identical( which(!is.na(time.end) ),integer(0)) ){
            deltaT <- NA
          }else{
            # metto min se la regola diventa che per pazienti su stesso ramo prendo come delta quello relativo al primo evento
            # (forse min ha piu' senso...)
            deltaT <- max( time.end,na.rm = TRUE) - time.start
          }
        }
        return( list( "id" = id, "deltaT" = deltaT ) )
      })
      
      matrice.KM <- c()
      
      for( i in c(1:length(tmp)) ) {
        if(is.na(tmp[[i]]$deltaT)){
          
          # SONO QUI SE tmp$ID[i] e' un CENS-->se deltaT e' NA vuol dire che l'id i-esimo che sto considerando non ha transitato nel nodo di end quindi cens
          # bisogna controllare due flag: autocens e cens.leaf:
          
          # caso 1: F & F --> ho id nodi cens e voglio che solo quelli siano i cens
          # caso 2: F & T --> ho id nodi cens e voglio anche usare i paz ai leaf
          # caso 3: T & T --> non ho id nodi cens e uso come cens i paz ai leaf
          # caso 4: T & F --> non ho nodi cens e non uso come cens i paz ai leaf --> NON POSSO TORNO A CASO 1
          
          if(autocens== FALSE){
            if(tmp[[i]]$id %in% paz.mat[,1]){
              
              # sia per caso 1 che per caso 2 tratto i paz che passano nei nodi cens nello stesso modo:
              # devo calcolare il deltat come time.start - time in cui arrivano al nodo cens
              t.start <- s$lst.nodi[[as.character(id.start)]]$pMineR.deltaDate[which(s$lst.nodi[[as.character(id.start)]]$IPP==tmp[[i]]$id)]
              t.end <- s$lst.nodi[[paz.mat[which(paz.mat[,1]==tmp[[i]]$id),2]]]$pMineR.deltaDate[which(s$lst.nodi[[paz.mat[which(paz.mat[,1]==tmp[[i]]$id),2]]]$IPP==tmp[[i]]$id)]
              delta <- t.end-t.start
              matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
            } else {
              if(cens.leaf== FALSE){
                # CASO 1:  uso come cens SOLO i pazi che transitano nei nodi inseriti come nodi cens
                matrice.KM <- matrice.KM
              } else {
                # CASO 2:  uso come cens sia i paz che transitano nei nodi cens che i paz che vanno nelle leaf
                #         per questi calcolo delta t come t.ultimo evento della traccia - t.start
                delta <- out$pat.process[[tmp[[i]]$id]]$pMineR.deltaDate[nrow(out$pat.process[[tmp[[i]]$id]])]
                matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
              }
            }
          }else{
            # restano fuori caso 3 e 4 --> in entrambi questi casi rientro nella tecnica: non ho nodi cens uso come
            #paz cens quelli delle leaf
            delta <- out$pat.process[[tmp[[i]]$id]]$pMineR.deltaDate[nrow(out$pat.process[[tmp[[i]]$id]])]
            matrice.KM <- rbind(matrice.KM, c(tmp[[i]]$id, delta,"0"))
          }
        } else {
          matrice.KM <- rbind( matrice.KM, c(tmp[[i]]$id, tmp[[i]]$deltaT, "1"))
        }
      }
      
      colnames(matrice.KM) <- c("ID","time","outcome")
      matrice.KM <- as.data.frame(matrice.KM)

      if(class(matrice.KM$outcome)=="factor"){
        matrice.KM$outcome <- as.numeric(levels(matrice.KM$outcome))[matrice.KM$outcome]
      } else {
        matrice.KM$outcome <- as.numeric(matrice.KM$outcome)
      }
      
      if(class(matrice.KM$time)=="factor"){
        matrice.KM$time <- as.numeric(levels(matrice.KM$time))[matrice.KM$time]
      } else {
        matrice.KM$time <- as.numeric(matrice.KM$time)
      }

      if( UM == "days") matrice.KM$time <- matrice.KM$time / 1440
      if( UM == "hours") matrice.KM$time <- matrice.KM$time / 60
      if( UM == "weeks") matrice.KM$time <- matrice.KM$time / (1440 * 7)
      if( UM == "months") matrice.KM$time <- matrice.KM$time / (43800)
      
      KM0 <- survfit(Surv(time, outcome)~1,   data=matrice.KM)
      
      to_ret <- NA
      if( plotIt == TRUE ) {
        titolo <- paste0(s$lst.nodi[[as.character(id.start)]]$event, "->", unlist(lapply(id.end, function(nodo){ s$lst.nodi[[as.character(nodo)]]$event})) )
        to_ret <- plot(KM0,
                       main = titolo,xlab = UM, ylab = "p", mark.time = TRUE )
      }
    }

    return( list("KM0" = KM0, "to_ret" = to_ret) )
  }
  
  #=================================================================================
  # plotNodeStats
  #=================================================================================  
  
  plotNodeStats <- function(  arr.nodes.from, arr.nodes.to = c(), abs.threshold = NA,
                              baseline.cov = FALSE, covariate, is.numerical = FALSE,  plot.points = TRUE, plot.RegressionLine = FALSE,
                              xlim = 30, givebackNumber = FALSE, plotIT = TRUE, covariate.type = 'attribute', covariate.value.column = NA) {
    
    CFMstructure <- getDataStructure()
    # EL <- loadedDataset
    
    # EL_pMiner <- EL$getData()
    EL_pMiner <- loadedDataset
    
    Event <- EL_pMiner$csv.EVENTName
    Date <- EL_pMiner$csv.dateColumnName
    ID <- EL_pMiner$csv.IDName
    arr.nodes.from <- gsub('BEGIN', 'root', arr.nodes.from)
    
    if(!is.null(arr.nodes.to) & (length(arr.nodes.from) != length(arr.nodes.to))){
      stop('Il numero di nodi di input deve essere uguale al numero di nodi di output')
    }
    
    #Controllo che il numero di pazienti nel nodo START sia > soglia
    if(!is.na(abs.threshold)){
      arr.nodes.from.new <- c()
      arr.nodes.to.new <- c()
      n <- 1
      for(i in 1:length(arr.nodes.from)){
        nodo <- arr.nodes.from[i]
        if(CFMstructure$lst.nodi[[nodo]]$hits >= abs.threshold){
          if(is.null(arr.nodes.to)){
            arr.nodes.from.new[n] <- nodo
            n <- n+1
          }else{
            arr.nodes.from.new[n] <- nodo
            arr.nodes.to.new[n] <- arr.nodes.to[i]
            n <- n+1
          }
        }
      }
    }else{ 
      arr.nodes.from.new <- arr.nodes.from
      arr.nodes.to.new <- arr.nodes.to
    }
    
    ############ STRUTTURA DATI: Preprocessing dei dati ############################### 
    if(is.null(arr.nodes.to)){  
      #Struttura dati per grafici puntuali
      df_tot <- lapply(arr.nodes.from.new, function(nodo){
        mat_cov <- lapply(CFMstructure$lst.nodi[[nodo]]$IPP, function(ID){
          if(covariate.type == 'attribute'){
            cov <- ifelse((baseline.cov | nodo == 'root'), EL_pMiner$pat.process[[ID]][1,covariate], EL_pMiner$pat.process[[ID]][CFMstructure$lst.nodi[[nodo]]$depth,covariate])
            id_paz <- ID
            a <- data.frame(cbind(id_paz, cov))
            return(a) 
          }else{
            cov <- ifelse((baseline.cov | nodo == 'root'), EL_pMiner$pat.process[[ID]][which(EL$pat.process[[ID]][[Event]] == covariate)[1],covariate.value.column], EL_pMiner[[ID]]$pat.process[CFMstructure$lst.nodi[[nodo]]$depth,covariate.value.column])
            id_paz <- ID
            a <- data.frame(cbind(id_paz, cov))
            return(a)
          }  
        })
        mat_new <- as.data.frame(do.call('rbind', mat_cov))
        mat_new$nodo <- nodo
        return(mat_new)
      })
      df_tot <- do.call('rbind', df_tot)
      df_tot$nome_nodo <- unlist(lapply(1:nrow(df_tot), function(x){ CFMstructure$lst.nodi[[df_tot[x, 'nodo']]]$evento } ))
      df_tot$nome_nodo <- paste(df_tot$nodo, df_tot$nome_nodo, sep = '-' )
      df_tot <- df_tot[which(!is.na(df_tot$cov)),]
      df_tot$baseline <- baseline.cov
      colnames(df_tot) <- c('ID','covariate', 'nodo', 'nome_nodo', 'baseline')
    } else if(!is.null(arr.nodes.to.new) & is.numerical == TRUE){
      #Struttura dati per grafici andamentali
      df_tot <- lapply(1:length(arr.nodes.from.new), function(i){
        nodo_IN <- arr.nodes.from.new[i]
        nodo_OUT <- arr.nodes.to.new[i]
        aaa <- EL
        depth_in <- ifelse(nodo_IN == 'root', 1, CFMstructure$lst.nodi[[nodo_IN]]$depth)
        if(nodo_OUT != 'END'){
          tmp <- aaa$applyFilter(array.pazienti.to.keep = CFMstructure$lst.nodi[[nodo_OUT]]$IPP, whatToReturn = 'dataLoader')
          paz_nodo_1 <- tmp$getData()
          mat_cov <- lapply(names(paz_nodo_1$pat.process), function(ID){
            if(covariate.type == 'attribute'){
              a <- paz_nodo_1$pat.process[[ID]][depth_in:CFMstructure$lst.nodi[[nodo_OUT]]$depth,]
              a$time <- (a$pMineR.deltaDate - a[1, 'pMineR.deltaDate'])/1440
              a <- a[which(!is.na(a[[covariate]])), c(paz_nodo_1$csv.IDName, 'time', covariate)]
              return(a) 
            }else{
              a <- paz_nodo_1$pat.process[[ID]][depth_in:CFMstructure$lst.nodi[[nodo_OUT]]$depth,]
              a <- a[which(a[[Event]] == covariate), ]
              a$time <- (a$pMineR.deltaDate - a[1, 'pMineR.deltaDate'])/1440
              a <- a[, c(paz_nodo_1$csv.IDName, 'time', covariate.value.column)]
              colnames(a) = c('ID', 'time', covariate)
              return(a) 
            }
          })
        }else{
          tmp <- aaa$applyFilter(array.pazienti.to.keep = CFMstructure$lst.nodi[[nodo_IN]]$IPP, whatToReturn = 'dataLoader')
          paz_nodo_1 <- tmp$getData()
          mat_cov <- lapply(names(paz_nodo_1$pat.process), function(ID){
            if(covariate.type == 'attribute'){
              a <- paz_nodo_1$pat.process[[ID]][depth_in:nrow(paz_nodo_1$pat.process[[ID]]),]
              a$time <- (a$pMineR.deltaDate - a[1, 'pMineR.deltaDate'])/1440
              a <- a[which(!is.na(a[[covariate]])), c(paz_nodo_1$csv.IDName, 'time', covariate)]
              return(a) 
            }else{
              a <- paz_nodo_1$pat.process[[ID]][depth_in:nrow(paz_nodo_1$pat.process[[ID]]),]
              a <- a[which(a[[Event]] == covariate), ]
              a$time <- (a$pMineR.deltaDate - a[1, 'pMineR.deltaDate'])/1440
              a <- a[, c(paz_nodo_1$csv.IDName, 'time', covariate.value.column)]
              colnames(a) <- c('ID', 'time', covariate)
              return(a)
            }
          })
        }
        mat_new <- as.data.frame(do.call('rbind', mat_cov))
        mat_new$nodo_IN <- nodo_IN 
        mat_new$nodo_OUT <- nodo_OUT
        return(mat_new)
      })
      df_tot <- do.call('rbind', df_tot)
      df_tot <- df_tot[which(df_tot$time <= xlim),]
      df_tot[[covariate]] <- as.numeric(df_tot[[covariate]])
      colnames(df_tot) <- c('ID', 'time', 'covariate', 'nodo_IN', 'nodo_OUT')
    } else {
      stop("Non è possibile visualizzare l'andamento nel tempo di una variabile categorica")
    }
    
    ############### PLOT dei grafici ##################################################
    
    if(plotIT == TRUE){
      #Plot dei grafici
      if(is.null(arr.nodes.to.new)){
        if (is.numerical == TRUE){
          # Grafici puntuali variabili numeriche
          arr.colore <- rainbow(n = length(unique(df_tot$nodo))) 
          names(arr.colore) <-  unique(df_tot$nome_nodo)
          df_tot$covariate <- as.numeric(df_tot$covariate)
          d <- lapply(unique(df_tot$nodo), function(i){
            a.1 <- df_tot[which(df_tot$nodo == i), ]
            return(density(a.1$covariate, na.rm = T)$y)
          })
          max_lim <- max(unlist(d))
          matx <- matrix(c(1,2,3,3), byrow = T, nrow = 2)
          layout(matx, heights = c(0.8, 0.2))
          for(i in 1:length(unique(df_tot$nodo))){
            nodo <- unique(df_tot$nodo)[i]
            nome_nodo <- df_tot[which(df_tot$nodo == nodo)[1], 'nome_nodo']
            if (i == 1){
              plot(density(df_tot[which(df_tot$nodo == nodo),'covariate']), lwd = 2, col = arr.colore[nome_nodo],ylim = c(0,max_lim),main = "" )#main = paste('KDE della variabile:', covariata) 
              
            }else{
              lines(density(df_tot[which(df_tot$nodo == nodo), 'covariate']), lwd = 2, col = arr.colore[nome_nodo])
            }
          }
          df_tot$nome_nodo <- factor(df_tot$nome_nodo, levels = names(arr.colore))
          aaa_p <- boxplot(covariate ~ nome_nodo, data = df_tot, ylab = covariate,xlab = "",  col = arr.colore, xaxt = "n")#main = paste('Boxplot della variabile:', covariata),
          text(x = 1:length(unique(df_tot$nome_nodo)),y = par("usr")[3] - 0.45,labels = aaa_p$names,xpd = NA,srt = 25,cex = 0.8,adj = 0.965)
          plot(1, type = "n", axes = F, xlab = "", ylab = "")
          # legend('top',inset = 0,  legend = names(arr.colore), col=arr.colore, lwd = 2, horiz = T,cex=0.7)
          # mtext(paste('Variable distribution:',covariate), side=3, line= 45, cex = 1.4) 
        }else{
          #Grafici puntuali variabili categoriche
          counts <- table(df_tot$covariate, df_tot$nome_nodo)
          arr.colore <- rainbow(n = length(rownames(counts)))
          names(arr.colore) <-  rownames(counts)
          x <- barplot(counts, main = paste("Variable distribution:", covariate), xlab="Nodes", col=arr.colore,legend = names(arr.colore), beside = F, xaxt = "n" )
          text(x, par("usr")[3]-0.25, srt = 15, adj = 1, xpd = TRUE,labels = paste(colnames(counts)), cex = 0.8) 
        }
      }else {
        #Andamento temporale variabile numerica
        colore <- rainbow(n = length(arr.nodes.from.new)) 
        arr.colore <- data.frame(cbind(colore, arr.nodes.from.new, arr.nodes.to.new))
        tipo_punti <- ifelse(plot.points, 'p', 'n')
        tipo_linea <- ifelse(plot.RegressionLine, 1, 0)
        tipo_ic <- ifelse(plot.RegressionLine, 2, 0)
        for (i in 1:length(arr.nodes.from.new)){
          dati <- df_tot[which(df_tot$nodo_IN == arr.nodes.from.new[i] & df_tot$nodo_OUT == arr.nodes.to.new[i]),]
          if(plot.RegressionLine){
            model <- glm(covariate ~ time, data = dati, family = 'gaussian') 
            #newx <- seq(min(dati$time), xlim, length.out= xlim)
            #preds <- predict(model, newdata = data.frame(time = newx), interval = 'confidence')
          }
          if(i == 1){
            plot(covariate ~ time, data = dati, type = tipo_punti,pch = 20, xlim = c(0,xlim), xlab = 'Days from input node',  
                 col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'] )
            if(plot.RegressionLine){
              abline(model, col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'], lwd = 2, lty = tipo_linea)
            }
          }else{
            points(covariate ~ time, data = dati, type = tipo_punti, pch = 20, 
                   col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'] )
            if(plot.RegressionLine){
              abline(model, col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'], lwd = 2, lty = tipo_linea)
            }
          }
          if(plot.RegressionLine){
            browser()
            #lines(newx, preds[ ,3],  col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'], lty = tipo_ic)
            #lines(newx, preds[ ,2],  col = arr.colore[which(arr.colore$arr.nodes.from == arr.nodes.from.new[i] & arr.colore$arr.nodes.to == arr.nodes.to.new[i]),'colore'], lty = tipo_ic)
          }
        }  
        # arr.colore$nomi_nodi_in <- unlist(lapply(arr.colore$arr.nodes.from, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
        # arr.colore$nomi_nodi_in <- gsub('root-root', 'BEGIN', arr.colore$nomi_nodi_in)
        # arr.colore$nomi_nodi_out <- unlist(lapply(arr.colore$arr.nodes.to, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
        # arr.colore$nomi_nodi_out <- gsub('END-', 'END', arr.colore$nomi_nodi_out)#quando cambio label in() lo devo modificare
        # arr.colore$Nome_coppie_nodi <- paste('From:', arr.colore$nomi_nodi_in, 'to:', arr.colore$nomi_nodi_out)
        # legend('topleft', legend = arr.colore$Nome_coppie_nodi, col=arr.colore[1:nrow(arr.colore),'colore'], 
        #        lwd = 2, cex=0.8, y.intersp = 0.8, bty = 'o')
      }
    }
    
    ############ DATI IN USCITA #######################################################
    
    if(givebackNumber == T){
      # In uscita ho lo struttura dati utilizzata per costruire i grafici
      df_tot$nodo <- NULL
      if(!is.null(arr.nodes.to)){
        df_tot$NODO_IN <-  unlist(lapply(df_tot$nodo_IN, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
        df_tot$NODO_OUT <-  unlist(lapply(df_tot$nodo_OUT, function(x){paste(x, CFMstructure$lst.nodi[[x]]$evento, sep = '-')} ))
        df_tot$nodo_IN <- NULL
        df_tot$nodo_OUT <-  NULL
        df_tot$NODO_IN <- gsub('root-root', 'BEGIN', df_tot$NODO_IN)
        df_tot$NODO_OUT <- gsub('END-', 'END', df_tot$NODO_OUT)#quando cambio label in() lo devo modificare
      }
      return(df_tot)
    }
    
  }    
  
  # ==========================================================================
  # selectSubCohorts
  #   restituisce i dei sub-pat.processes dove la prima riga e l'ultima corrispondono
  #   alla selezione desiderata
  # ==========================================================================
  selectSubCohorts <- function( arr.from, lst.to  ) {
    objCFM.DS <- getDataStructure()
    objDL.out <- loadedDataset
    colonnaEvento <- objDL.out$csv.EVENTName
    lst.res <- list()
    for( i in 1:length(arr.from) ) {
      
      # Prendi gli IPP dei nodi di destinazione (sono sicuramente anche quelli di partenza)
      arr.IPP.to <- unique(unlist(lapply(lst.to[[i]], function(nodo){ objCFM.DS$lst.nodi[[ nodo ]]$IPP  } )))
      
      # Prendi i dati utli per il from
      profondita.from <- objCFM.DS$lst.nodi[[ arr.from[i]  ]]$depth
      evento.profondita.from <- objCFM.DS$lst.nodi[[ arr.from[i]  ]]$evento
      
      # per ogni paziente, estrai la riga del pat.process dove si trova i target
      arr.depth <- unlist(lapply( arr.IPP.to, function(IPP) { 
        subMM <- objDL.out$pat.process[[IPP]]
        
        # Vediamo quale "to" fa match
        quali.match <- matrix(unlist(lapply(lst.to[[i]], function(nodo){ 
          c( (IPP %in%  objCFM.DS$lst.nodi[[ nodo ]]$IPP) , objCFM.DS$lst.nodi[[ nodo ]]$depth ) } 
        )),ncol=2,byrow = T)
        
        # retituisci il primo, cronologicamente, fra i possibili
        quali.match <- matrix(quali.match[which(quali.match[,1] == 1),],ncol=2,byrow = T)
        best.dept <- quali.match[order(quali.match[,2],decreasing = FALSE),2]
        return(max(best.dept))
      }))
      
      # Per ognuno di essi ora estrai cio' che c'e' nel mezzo (ed allinea il pminer.delta.date)
      lst.inTheMiddle <- lapply(1:length(arr.IPP.to), function(counter) {
        subMM <- objDL.out$pat.process[[arr.IPP.to[counter]]][  profondita.from:arr.depth[counter] ,   ]
        subMM$pMineR.deltaDate <- subMM$pMineR.deltaDate - subMM$pMineR.deltaDate[1]
        return(subMM)
      })
      names(lst.inTheMiddle) <- arr.IPP.to
      lst.res[[ arr.from[i] ]] <- lst.inTheMiddle
    }
    return( lst.res )
  }
  
  #=================================================================================
  # findReacheableNodes
  #================================================================================= 
  
  findReacheableNodes <- function( id ) {
    id <- as.character(id)
    res <- findReacheableNodes.ll( id = id  )$subMM
    if( id %in% res[1,]) {
      res <- res[ , -which( res[1,] == id )  ]
    }
    if(ncol(res)==0) res <- c()
    return(res)    
  }
  findReacheableNodes.ll <- function( id, subMM = c() ) {
    # casta a character, altrimenti non pesco l'indice ma la posizione
    id <- as.character(id)
    objCFM.DS <- getDataStructure()
    # Cerca se ci sono figli
    notBlank <- which(objCFM.DS$MM[id,]!="")
    # Se non ci sono, ritorna (sono in una foglia)
    if( length(notBlank) == 0) {
      n.pazienti <- length(objCFM.DS$lst.nodi[[id]]$IPP)
      depth <- objCFM.DS$lst.nodi[[id]]$depth
      subMM <- cbind( subMM , c(  id , n.pazienti , depth  ) )
      return( list("leaf"=TRUE, "subMM" = subMM) )
    }
    
    # se sono qui e' perche' invece ci sono: loopa su ognuno di essi richiamando
    # ricorsivamente la funzione.
    arr.id.son <- colnames(objCFM.DS$MM)[notBlank]
    for( id.son in arr.id.son ) {
      lst.res <- findReacheableNodes.ll( id = id.son, subMM = subMM  )
      subMM <- lst.res$subMM
      if( lst.res$leaf == FALSE ) {
        n.pazienti <- length(objCFM.DS$lst.nodi[[id.son]]$IPP)
        depth <- objCFM.DS$lst.nodi[[id.son]]$depth
        subMM <- cbind( subMM , c(  id.son , n.pazienti , depth  ) )
      }
    }
    rownames(subMM) <- c("id","Npatients","depth")
    return( list("leaf"=FALSE, "subMM" = subMM) )    
  }  
  
  #=================================================================================
  # play
  #=================================================================================   
  play <- function( n ) {
    BigMM <- c()
    for( i in 1:n ) {
      sumMM <- genTrace()
      sumMM <- cbind( "ID"=rep(i,nrow(sumMM)) ,   sumMM )
      BigMM <- rbind( BigMM , sumMM )
    }
    return( BigMM )
  }
  genTrace <- function( ) {
    fine <- FALSE
    i <- 0
    MM <- matrix(0,nrow=0,ncol=2)
    colnames(MM) <- c("date","event")
    
    while( fine == FALSE )  {
      if( i == 0 ) {
        riga <- getNextEvent()  
      } else{
        riga <- getNextEvent(ID = riga$ID , offsetDate = MM[nrow(MM),"date"])  
      }
      if( riga$end == TRUE ) return( MM )
      MM <- rbind(MM , riga$row)
      i <- i + 1
    }
  }
  getNextEvent <- function( ID = "root", offsetDate = "" ) {
    Struttura.Dati <- getDataStructure()
    ID.possibiliFigli <- colnames(Struttura.Dati$MM)[which(Struttura.Dati$MM[ID,]!="")]
    # browser()
    if( length(ID.possibiliFigli) == 0 ) {
      return( list("row" = c(), "end" = TRUE, "event" = NA , "ID" = NA) )
    }
    if( length(ID.possibiliFigli) == 1 ) {
      winner.pos <- 1; 
      winner.ID <- ID.possibiliFigli[ winner.pos ]
      winner.event <- Struttura.Dati$lst.nodi[[ winner.ID ]]$evento    
    }
    if( length(ID.possibiliFigli) > 1 ) {
      arr.possibili.figli <- unlist(lapply( 1:length(ID.possibiliFigli) , function(i.tmpID){ 
        rep(  ID.possibiliFigli[i.tmpID], Struttura.Dati$lst.nodi[[ i.tmpID ]]$hits )
      }))
      winner.ID <- arr.possibili.figli[round(as.numeric(runif(n = 1,min = 1,max = length(arr.possibili.figli))))]
      winner.event <- Struttura.Dati$lst.nodi[[ winner.ID ]]$evento
    }
    
    if( ID == "root")  {
      startingDate <- sample(seq(as.Date('2021/01/01'), as.Date('2022/01/01'), by="day"), 1)
      startingDate <- as.character(format(startingDate,"%d/%m%/%Y"))
      startingDate <- paste(c(startingDate," 00:00:00"),collapse = '')
    } else {
      offsetDate <- str_trim(offsetDate)
      if( str_length(offsetDate) == 10 )  offsetDate <- paste(c(offsetDate," 00:00:00"),collapse = '')
      winner.IPP <- Struttura.Dati$lst.nodi[[ winner.ID ]]$IPP
      date.from <- Struttura.Dati$lst.nodi[[ID]]$activationDates[  which(Struttura.Dati$lst.nodi[[ID]]$IPP  %in% winner.IPP )]
      date.to <- Struttura.Dati$lst.nodi[[winner.ID]]$activationDates
      arr.secs <- unlist(lapply( 1:length(date.to), function(cur) {
        deltaSec <- as.numeric(strptime(  date.to[cur] , "%d/%m/%Y %H:%M:%S"   ) - strptime(  date.from[cur] , "%d/%m/%Y %H:%M:%S"   ),units="secs")
        return(deltaSec)
      }))
      if( length(arr.secs) > 3 ) {
        dens <- density(arr.secs)
        newValue <- as.integer(sample(x = dens$x , 1, prob = dens$y) + rnorm(1,0,dens$bw ))
      } else {
        newValue <- mean(arr.secs)
      }
      startingDate <- substr(as.character(as.POSIXct( offsetDate , format =  "%d/%m/%Y %H:%M:%S" ) + newValue),1,19)
      if( is.na(startingDate)) browser()
      startingDate <- str_replace_all(string = startingDate, pattern = "-",replacement = "/")
      startingDate <- paste(c(substr(startingDate,9,10),"/",substr(startingDate,6,7),"/",substr(startingDate,1,4)," ",substr(startingDate,12,19) ),collapse = '')
    }
    evento <- Struttura.Dati$lst.nodi[[ winner.ID ]]$evento
    riga <- c(  "date" = startingDate, "event" = evento ) 
    return( list("row" = riga, "end" = FALSE, "event" = winner.event , "ID" = winner.ID) )  
  }
  
  #=================================================================================
  # constructor
  #=================================================================================  
  
  constructor <- function( verboseMode  ) {
    MM <<- matrix("",ncol=1, nrow=1)
    colnames(MM) <<- c("root")
    rownames(MM) <<- c("root")
    lst.nodi <<- list()
    attr.date.format <<- ""
    attr.dateToFormat <<- ""
    param.verbose <<- verbose.mode
    intGraph <<- create_graph()
    cmpStr <<- list()
    loadedDataset <<- list()
  }
  constructor(verboseMode = verbose.mode)
  return(list(
    "add.node" = add.node,
    "add.path"= add.path,
    "get.id" = get.id,
    "loadDataset" = loadDataset,
    "getDataStructure" = getDataStructure,
    "getPatientWithSpecificedPath" = getPatientWithSpecificedPath,
    "plotCFGraph" = plotCFGraph,
    "plotNodeStats" = plotNodeStats,
    "plotCFGraphComparison" = plotCFGraphComparison,
    "pathBeetweenStackedNodes" = pathBeetweenStackedNodes,
    "kaplanMeierCurves" = kaplanMeierCurves,
    "selectSubCohorts" = selectSubCohorts,
    "findReacheableNodes" = findReacheableNodes,
    "play" = play
  ))
}
