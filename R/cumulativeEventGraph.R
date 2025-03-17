#' A class for a revisited Careflow Mining
#'
#' @description  This is an implementation of the Care Flow Mining algorithm, a bit revisited
#' @import progress 
#' @export


cumulativeEvent <- function( verbose.mode = FALSE ) {
  loadedDataset <- list()
  objDL.v3.out <- list()
  param.verbose <- FALSE
  

  loadDataset<-function( dataList ) {
    param.verbose <<- verbose.mode
    loadedDataset <<- dataList  
    objDL.v3.out <<- list()
    # train( objDL.v2.out = dataList)
  }
  
  train <- function(  
                    arr.time.points = c(5,10,15), 
                    UM = "years", 
                    abs.min.threshold.4.edges = 10) {
    
    objDL.v2.out <- loadedDataset
    arr.atm.evt <- objDL.v2.out$arrayAssociativo
    
    # Comincio
    charToPaste <- "|"
    arr.time.points <- arr.time.points[which(arr.time.points > 0)]
    arr.time.points <- c(0,arr.time.points,Inf)
    if(UM=="years") arr.time.points <- arr.time.points * 365 * 24 * 60
    if(UM=="months") arr.time.points <- arr.time.points * 30 * 24 * 60
    if(UM=="weeks") arr.time.points <- arr.time.points * 7 * 24 * 60
    if(UM=="days") arr.time.points <- arr.time.points * 7 * 24 * 60
    
    csv.EVENTName <- objDL.v2.out$csv.EVENTName
    csv.dateColumnName <- objDL.v2.out$csv.dateColumnName
    newMM <- c()
    tmp <- lapply( names(objDL.v2.out$pat.process), function(ID){
      cumulativo.eventi <- c()
      MM <- objDL.v2.out$pat.process[[ID]]
      for( ct in 1:(length(arr.time.points)-1) ) {
        lower <- arr.time.points[ct]
        higher <- arr.time.points[ct+1]
        quali <- which( MM$pMineR.deltaDate >= lower & MM$pMineR.deltaDate <= higher   ) 
        if( length(quali) > 0) {
          cumulativo.eventi <- unique(c( cumulativo.eventi  , MM[quali,csv.EVENTName] ))
          cumulativo.eventi <- cumulativo.eventi[order(cumulativo.eventi)]
          nuovo.evento <- paste(cumulativo.eventi,collapse = charToPaste)      
          # if( nuovo.evento == "0|1|1") browser()
          nuova.riga <- MM[quali[1],]
          # set the cluster in the date colums
          nuova.riga[,csv.dateColumnName] <- ct
          nuova.riga[,csv.EVENTName] <- nuovo.evento      
          newMM <<- rbind( newMM , nuova.riga)
        } else { 
          nuova.riga <- newMM[ nrow(newMM),  ]
          nuova.riga["data"] <- ct
          newMM <<- rbind( newMM , nuova.riga)
        }
      }
    } )
    
    aaa <- newMM
    tmp <- unlist(lapply(1:nrow(aaa),function(riga){ 
      paste(c(aaa[riga,"evento"]," (lev.",aaa[riga,"data"],")"),collapse = '')
    }))
    aaa <- cbind( aaa , "newEvent"=tmp)
    startingDate <- "2000-01-01 00:00:00"
    arr.Date.Cluster <- unique(newMM[,"data"])
    arr.Date <- unique(arr.Date.Cluster) + as.Date(startingDate)
    names(arr.Date) <- arr.Date.Cluster
    for( i in arr.Date.Cluster ){
      aaa[which( aaa[,"data"]== i ), "data"] <- as.character(arr.Date[i])
    }
    newMM <- aaa
    
    objDL.v3 <- dataLoader(verbose.mode = FALSE)
    objDL.v3$load.data.frame( mydata =  newMM,IDName = "ID",EVENTName = "newEvent",dateColumnName = "data",
                              format.column.date = "%Y-%m-%d")
    objDL.v3.out <<- objDL.v3$getData()

  }
  
  plot.p.time.evolution <- function( time.from, time.to, time.step, UM, p.value.threshold = 0.01,
                                     stratifyForVariableName = "sesso",
                                     stratificationvaueles.arr.clusterA = c("0"),
                                     stratificationvaueles.arr.clusterB = c("1"),                                     
                                     abs.min.threshold.4.edges = 10,threshold = 10,
                                     verbose = TRUE) {
    
    arr.tempi <- seq( time.from, time.to, by = time.step )
    lst.res <- list()
    if(verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(arr.tempi), style = 3)
    pb.ct <- 0
    for( tempo in arr.tempi ) {
      pb.ct <- pb.ct + 1;
      if(verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      a <- objCEG$plotCumulativeEvent(arr.time.points = c(tempo),UM = UM,
                                      abs.min.threshold.4.edges = abs.min.threshold.4.edges, 
                                      threshold = threshold,
                                      stratifyForVariableName = stratifyForVariableName,
                                      stratificationvaueles.arr.clusterA = stratificationvaueles.arr.clusterA,
                                      stratificationvaueles.arr.clusterB = stratificationvaueles.arr.clusterB,
                                      p.value.threshold = p.value.threshold) 
      lst.res[[ as.character(tempo) ]] <- a$data
    }
    if(verbose == TRUE) close(pb)

    arr.eventi <- unique(unlist(lapply(names(lst.res),function(tempo){ lapply( names(lst.res[[tempo]]), function(bomba){ return(bomba)  } )})))
    
    mtr <- c()
    
    for(tempo in names(lst.res)) {
      for( evento in arr.eventi ) {
        se <- length(lst.res[[tempo]][[evento]] )
        if(se > 0 ) {
          riga <- c(   tempo, evento,     lst.res[[tempo]][[evento]]$p.value )
        } else {
          riga <- c(   tempo, evento,     1 )
        }
        mtr <- rbind( mtr  , riga )
      }  
    }
    colnames(mtr) <- c("time","event","p.value")
    mtr <- mtr[order(as.numeric(mtr[,"p.value"])),]
    return(mtr)
  }
  
  plotCumulativeEvent <- function( 
                          arr.time.points = c(5,10,15), 
                          UM = "years", 
                          threshold = 0, 
                          defaultNodeColor = "Gold", 
                          arr.node.col.threshold=c(0.25,0.5,0.75),
                          abs.min.threshold.4.edges = 0,
                          stratifyForVariableName = NA,
                          stratificationvaueles.arr.clusterA = c(),
                          stratificationvaueles.arr.clusterB = c() ,
                          p.value.threshold = 0.01
                          ) {
    lst.data = list()
    train(arr.time.points = arr.time.points, UM = UM)
    MM <- objDL.v3.out$MMatrix 
    # prendi la lista dei nomi
    listaNodi<-colnames(MM)
    # la lista dei nodi raggiungibili da BEGIN
    listaNodiFromBegin<-listaNodi[which(MM["BEGIN",]>threshold)]
    # la lista dei nodi che vanno a END
    listaNodiToEnd<-listaNodi[which(MM[,"END"]>threshold)]
    
    if( !is.na(stratifyForVariableName) ) {
      strat.ID.A <- unique(toChar(objDL.v3.out$original.CSV[[objDL.v3.out$csv.IDName]])[which( toChar(objDL.v3.out$original.CSV[[stratifyForVariableName]]) %in% stratificationvaueles.arr.clusterA )])
      strat.ID.B <- unique(toChar(objDL.v3.out$original.CSV[[objDL.v3.out$csv.IDName]])[which( toChar(objDL.v3.out$original.CSV[[stratifyForVariableName]]) %in% stratificationvaueles.arr.clusterB )])
    }    

    rigaBEGIN<-''; rigaEND<-''
    for( i in listaNodiFromBegin) { rigaBEGIN<-paste(   c(rigaBEGIN, "'BEGIN'->'",i,"' "), collapse = '') }
    for( i in listaNodiToEnd) { rigaEND<-paste(   c(rigaEND, "'",i,"'->'END' "), collapse = '') }  
    
    arr.nodi.con.archi <- c();  stringaNodiComplessi<-''
    maxEdgeWeight = max(MM)
    min.penwidth <- 0.8; max.penwidth <- 10
    min.color <- 20; max.color <- 99
    # penwidth <- 1
    fontSize <- 12
    colore <- 50
    for(i in seq(1,nrow(MM))) {
      listaNodiRiga<-listaNodi[which(MM[i,]>threshold)]
      if(length(listaNodiRiga)>0) {
        for( ct in seq(1,length(listaNodiRiga))) {
          penwidth <- min.penwidth
          color <- min.color
          
          denominatore.abs <- sum(MM[i,])
          numeratore.abs <- MM[i,listaNodiRiga[ct]]
          
          proporzione <- numeratore.abs / maxEdgeWeight
          penwidth <- penwidth + (max.penwidth-min.penwidth)*proporzione
          colore <- as.integer((100-max.color) + (max.color-min.color)*(1-proporzione))
          colore <- paste(c("Gray",colore),collapse = '')
          
          stringa.arco <- paste(c( "(",numeratore.abs,"/",denominatore.abs,")"),collapse = '')
          
          if( numeratore.abs < abs.min.threshold.4.edges ) { 
            colore <- "White"
          }
          
          stringaNodiComplessi<-paste(   c(stringaNodiComplessi, "'",listaNodi[i],"'->'",listaNodiRiga[ct],"' [ label='",stringa.arco,"', penwidth='",penwidth,"' ,fontsize = '",fontSize,"',  fontcolor=",colore,", color = ",colore," ]\n"), collapse = '')  
          arr.nodi.con.archi<-c(arr.nodi.con.archi,listaNodi[i],listaNodiRiga[ct] )
        }
      }
    }
    
    listaNodiToPrint<-''
    arr.nodi.con.archi <- unique(arr.nodi.con.archi)
    
    listaNodi <- listaNodi[ listaNodi %in% arr.nodi.con.archi ]  
    # browser()
    quanti.pazienti <- sum(MM["BEGIN",])
    for(i in seq(1,length(listaNodi))) {
      
      
      quanti.passano.per.il.nodo <- sum(MM[,listaNodi[i]])
      totalePerNodo <- sum(MM[,listaNodi[i]])
      if( listaNodi[i] == "BEGIN") quanti.passano.per.il.nodo <- quanti.pazienti
      # browser()
      ratio <- totalePerNodo / quanti.pazienti
      fillColor <- defaultNodeColor
      if(ratio < arr.node.col.threshold[1]) { fillColor <- paste(c(defaultNodeColor,"1"),collapse = ''); fontColor <- "Gray1" }
      if(ratio >= arr.node.col.threshold[1] & ratio <arr.node.col.threshold[2]) { fillColor <- paste(c(defaultNodeColor,"2"),collapse = ''); fontColor <- "Gray1"  }
      if(ratio >= arr.node.col.threshold[2] & ratio <arr.node.col.threshold[3]) { fillColor <- paste(c(defaultNodeColor,"3"),collapse = ''); fontColor <- "Gray1"  }
      if(ratio >= arr.node.col.threshold[3]) { fillColor <- paste(c(defaultNodeColor,"4"),collapse = ''); fontColor <- "Gray99"  }
      
      if( !is.na(stratifyForVariableName) ) {
        orig.A <- length(strat.ID.A)
        orig.B <- length(strat.ID.B)
      }

      label <- paste(c(listaNodi[i],"\n n=",quanti.passano.per.il.nodo),collapse = '')
      lst.data[[ listaNodi[i] ]] <- list()
      
      # Se e' stata richiesta una stratificazione 
      if( !is.na(stratifyForVariableName) & !(listaNodi[i] %in% c("BEGIN","END")) ) {
        # orig.A <- length(strat.ID.A)
        # orig.B <- length(strat.ID.B)
        targ.A <- which(objDL.v3.out$original.CSV[[ objDL.v3.out$csv.IDName  ]] %in% strat.ID.A & objDL.v3.out$original.CSV[[ objDL.v3.out$csv.EVENTName  ]] == listaNodi[i] )
        targ.B <- which(objDL.v3.out$original.CSV[[ objDL.v3.out$csv.IDName  ]] %in% strat.ID.B & objDL.v3.out$original.CSV[[ objDL.v3.out$csv.EVENTName  ]] == listaNodi[i] )
        if( length(targ.A) > 0 ) {
          qta.A <- length(unique(objDL.v3.out$original.CSV[targ.A, objDL.v3.out$csv.IDName ]))
        } else {
          qta.A <- 0
        }
        if( length(targ.B) > 0 ) {
          qta.B <- length(unique(objDL.v3.out$original.CSV[targ.B, objDL.v3.out$csv.IDName ]))  
        } else {
          qta.B <- 0
        }
        matrice <- matrix( c(orig.A,orig.B,qta.A,qta.B) , byrow = TRUE, ncol = 2 )
        p.value <- NA
        if( sum(matrice) > 30 ) {
          if( sum(matrice) > 100 ) {
            p.value <- chisq.test( matrice )$p.value 
          } else { 
            p.value <- fisher.test( matrice )$p.value 
          }
        }
        
        if(is.na(p.value)) p.value <- "NA"
        if( p.value!="NA" ) {
          label <- paste(c(listaNodi[i],"\n ",qta.A,"/",orig.A," vs ",qta.B,"/",orig.B,"\np=",round(p.value,digits = 4)),collapse = '')
          if( p.value <= p.value.threshold ) {
            fillColor <- "Yellow";
            fontColor <- "Black"
          }
        } else {
          label <- paste(c(listaNodi[i],"\n ",qta.A,"/",orig.A," vs ",qta.B,"/",orig.B,"\np=NA"),collapse = '')
        }
        if( listaNodi[i] %in% c("BEGIN","END") ) {
          label <- paste(c(listaNodi[i],"\n ",orig.A,"/",orig.B),collapse = '')  
        }
        lst.data[[ listaNodi[i] ]] <- list( "qta.A" = qta.A, "qta.B" = qta.B, "p.value"=p.value )
      }
      if( listaNodi[i] %in% c("BEGIN","END") & !is.na(stratifyForVariableName)  ) {
        label <- paste(c(listaNodi[i],"\n ",orig.A,"/",orig.B),collapse = '')
      }
      
      
      if(i<length(listaNodi)) {
        # listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"' [ label='",listaNodi[i],"\n n=",quanti.passano.per.il.nodo,"' , shape = 'box' , fillcolor = '",fillColor,"', fontcolor = '",fontColor,"' ] \n"), collapse=''    )
        listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"' [ label='",label,"' , shape = 'box' , fillcolor = '",fillColor,"', fontcolor = '",fontColor,"' ] \n"), collapse=''    )            
      } 
      else {
        listaNodiToPrint <- paste( c(listaNodiToPrint," '",listaNodi[i],"' [ shape = 'box' , fillcolor = '",fillColor,"', fontcolor = '",fontColor,"' ]"), collapse=''    ) 
      }
    }  
    
    
    # now plot it
    a<-paste(c("digraph boxes_and_circles {

               # a 'graph' statement
               graph [overlap = true, fontsize = 10, layout = dot ]

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
    return( list("script" = a , "data" = lst.data ) ) 
    
  }
  
  
  #=================================================================================
  # constructor
  #=================================================================================  
  
  constructor <- function( verboseMode  ) {
    loadedDataset <<- list()
  }
  constructor(verboseMode = verbose.mode)
  return(list(
    "loadDataset" = loadDataset,
    "plotCumulativeEvent" = plotCumulativeEvent,
    "plot.p.time.evolution" = plot.p.time.evolution
  ))
}
