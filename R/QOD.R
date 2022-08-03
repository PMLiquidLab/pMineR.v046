#' A quality of data inspector, second Version
#'
#' @description   a QoD inspector class
#' @export
QOD <- function( UM = "" ) {

  global.dataLoader <- c();
  global.UM <- ""
  
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {
    # Clear some possible previously inserted attribute
    clearAttributes()
    # set the new attributes
    EFOMM <- EfirstOrderMarkovModel()
    EFOMM$loadDataset( dataList )
    global.EFPTs <<- EFOMM$getEFPTs()
    # set the new attributes
    global.dataLoader <<- dataList
  }
  #=================================================================================
  # query
  #=================================================================================
  query <- function( from , to , complement = FALSE, time.range=c(0,Inf), step.range = c(1,Inf) , UM = NA, 
                     arr.passingThrough = c(), arr.NOTpassingThrough = c(),
                     returnCompleteMatrix = FALSE) {
    forceCheck <- TRUE
    EventName <- global.dataLoader$csv.EVENTName
    if( (!(from %in% global.dataLoader$arrayAssociativo) | (!(to %in% global.dataLoader$arrayAssociativo) & to!="*") ) & forceCheck == TRUE ) {
      stop("Error: from or to not available as events in the Event Log")
    }
    
    if(is.na(UM)) UM <- global.UM
    mainMM <- c()
    tmp.res <- unlist(lapply( names( global.dataLoader$pat.process ), function(ID) {
      subMM <- global.dataLoader$pat.process[[ID]]
      
      if( UM == "hours" ) subMM$pMineR.deltaDate <- subMM$pMineR.deltaDate/60
      if( UM == "days" ) subMM$pMineR.deltaDate <- subMM$pMineR.deltaDate/(60*24)
      if( UM == "weeks" ) subMM$pMineR.deltaDate <- subMM$pMineR.deltaDate/(60*24*7)
      if( UM == "months" ) subMM$pMineR.deltaDate <- subMM$pMineR.deltaDate/(60*24*7*30)
      
      begin.line <- subMM[1,]; begin.line[ EventName ] <- "BEGIN"
      end.line <- subMM[nrow(subMM),]; end.line[ EventName ] <- "END"
      subMM <- rbind( begin.line, subMM ); subMM <- rbind( subMM, end.line )

      arr.from <- which(subMM[[EventName]] == from)
      arr.to <- which(subMM[[EventName]] == to)
      
      if( to == "*" & length(arr.from)>0) arr.to <- arr.from + 1
      
      if( length(arr.from) == 0 | length(arr.to) == 0 ) return( FALSE )
      
      MM <- expand.grid.unique(arr.from,arr.to)
      MM <- cbind(MM , rep(0,nrow(MM))); MM <- cbind(MM , rep(0,nrow(MM))); MM <- cbind(MM , rep(0,nrow(MM)))
      tmp <- lapply(1:nrow(MM),function(riga) { 
        MM[riga,3] <<- MM[riga,2] - MM[riga,1]
        MM[riga,4] <<- subMM$pMineR.deltaDate[ MM[riga,2] ] - subMM$pMineR.deltaDate[ MM[riga,1] ]
      })
      colnames(MM) <- c("from","to","step","time","valid")
      
      for( riga in 1:nrow(MM)) {
        valido <- TRUE
        if(! (MM[riga,"step"] >= step.range[1] & MM[riga,"step"] <= step.range[2]) ) {
          valido <- FALSE
        }
        if(! (MM[riga,"time"] >= time.range[1] & MM[riga,"time"] <= time.range[2]) ) {
          valido <- FALSE
        }
        if( sum(arr.passingThrough %in% subMM[[EventName]][ MM[riga,1]:MM[riga,2] ]) != length(arr.passingThrough) )  {
          valido <- FALSE
        }
        if( sum(arr.NOTpassingThrough %in% subMM[[EventName]][ MM[riga,1]:MM[riga,2] ]) >0 )  {
          valido <- FALSE
        }        
        # browser()
        MM[riga,"valid"] <- valido
      }
      mainMM <<- rbind(mainMM, cbind( rep(ID,nrow(MM)), MM ) )
      return(sum(MM[,"valid"]) > 0)
    } ))
    # browser()
    # mainMM <- mainMM[ which(mainMM[,"valid"] == 1),]
    nomiColonne <- colnames(mainMM)
    mainMM <- matrix(mainMM[ which(mainMM[,"valid"] == 1),], ncol=ncol(mainMM))
    if(length(mainMM) == 0) return(NA)
    colnames(mainMM) <- nomiColonne
    mainMM[,2] <- as.numeric(mainMM[,2])-1
    mainMM[,3] <- as.numeric(mainMM[,3])-1
    if( returnCompleteMatrix == TRUE ) return(mainMM)
    
    res <- names( global.dataLoader$pat.process )[which(tmp.res==TRUE)]
    
    if( complement == TRUE ) { 
      res <- names(global.dataLoader$pat.process)[which( !(names(global.dataLoader$pat.process) %in% res))]
    }
    
    return(res)
  } 
  #=================================================================================
  # path.count
  #=================================================================================  
  path.count <- function( evtSequenceLength = 1, notMoreThanOnePerPatient = FALSE , fromBegin = TRUE ) {
    browser()
  }
  #=================================================================================
  # eventHeatmap
  #=================================================================================
  eventHeatmap <- function( cex = 0.5 , threshold.low = 0.5, threshold.hi = 1, show.diagonal = TRUE, par.margin = c(4, 10, 10, 2)) {
    
    objDL.new.export <- global.dataLoader
    
    arr.eventi <- objDL.new.export$arrayAssociativo[!(objDL.new.export$arrayAssociativo %in% c("BEGIN","END"))]
    MM.Cross <- matrix( 0,nrow = length(arr.eventi), ncol = length(arr.eventi) )
    colnames(MM.Cross) <- arr.eventi; rownames(MM.Cross) <- arr.eventi;
    tmp.1 <- lapply( rownames(MM.Cross) , function(event.C) {
      tmp.2 <- lapply(colnames(MM.Cross), function(event.R) {
        tmp.3 <- lapply( names(objDL.new.export$pat.process) , function(patID) {
          arr.evt.to.chech <- objDL.new.export$pat.process[[patID]][[objDL.new.export$csv.EVENTName]]
          if( event.C %in% arr.evt.to.chech & event.R %in% arr.evt.to.chech) {
            MM.Cross[ event.R , event.C ] <<- MM.Cross[ event.R , event.C ] + 1
          }
        })
      } )
    })
    aaa <- MM.Cross
    tmp.1 <- lapply( 1:nrow(MM.Cross) , function(riga) { 
      MM.Cross[riga,] <<- MM.Cross[riga,] / MM.Cross[riga,riga]
    })
    
    
    par(mar = par.margin) 
    image(t(MM.Cross[nrow(MM.Cross):1,]),col = heat.colors(256) , axes=FALSE )
    arr.posizioni <- (0.1:ncol(MM.Cross)/(ncol(MM.Cross)-1))
    axis(2, arr.posizioni, labels=rownames(MM.Cross)[length(rownames(MM.Cross)):1],las=2)
    axis(3, arr.posizioni, labels=rownames(MM.Cross),las=2)
    for( riga in 1:nrow(MM.Cross)) {
      for( colonna in 1:ncol(MM.Cross)) {
        valore <- t(MM.Cross[ncol(MM.Cross)-colonna+1,riga])
        if( valore >= threshold.low & valore <= threshold.hi ) {
          text((riga-1)/(nrow(MM.Cross)-1),(colonna-1)/(ncol(MM.Cross)-1),format(valore,digits = 2) , cex=cex ) 
        }
      }
    }
  }  
  #=================================================================================
  # plotTimeline
  #=================================================================================  
  plotTimeline <- function(  objDL.obj=c(), arr.ID = c(), max.time = Inf , UM = "days", arr.events = c(), 
                             arr.evt.pch = c(), evt.pch.default.value = 3,
                             ID.on.y.label  = TRUE, y.label.cex = 0.7,
                             Time.on.x.label = TRUE, x.label.cex = 0.7,
                             plot.legend = TRUE, legend.Pos = "topright", legend.cex = 0.7,
                             ID.ordering = TRUE, ID.ordering.desc = TRUE
  ) {
    if( length(objDL.obj) == 0 )  objDL.obj <- global.dataLoader
    objDL.out <- objDL.obj
    max.time.window <- max.time
    if( UM == "hours" ) { max.time.window <- max.time.window * 60 }
    if( UM == "days" ) { max.time.window <- max.time.window * 60*24  }
    if( UM == "weeks" ) { max.time.window <- max.time.window *  60*24*7  }
    if( UM == "months" ) { max.time.window <- max.time.window * 60*24*30  }
    
    if( length(arr.ID) == 0 ) arr.ID <- names(objDL.out$pat.process)
    evtName <- objDL.out$csv.EVENTName
    
    bigM <- do.call(rbind,objDL.out$pat.process[arr.ID])
    time.range <- range(bigM$pMineR.deltaDate)
    maxTime <- min(max(bigM$pMineR.deltaDate),max.time.window)
    max.x <- maxTime
    
    
    if( ID.ordering == TRUE) {
      arr.ID <- arr.ID[order(unlist(lapply(arr.ID, function(ID)  {  max(objDL.out$pat.process[[ID]]$pMineR.deltaDate)   } )),decreasing = ID.ordering.desc)]  
    }
    
    x.offset <- 0; y.offset <- 0
    if(ID.on.y.label==TRUE) { x.offset <- 0.1 }
    if(Time.on.x.label==TRUE) { y.offset <- 0.1 }
    
    
    n.patients <- length(arr.ID)
    if( length(arr.events) == 0 ) { arr.events <- unlist(unique(bigM[ evtName ])) }
    arr.col <- rainbow(n = length(arr.events)); names(arr.col) <- arr.events
    # browser()
    # set the array of pch
    add.arr.evt.pch <- c()
    tmp.n <- unlist(lapply( arr.events, function(i) {   if( !(i %in% names(arr.evt.pch)) ) return(i) }))
    tmp.v <- rep(evt.pch.default.value,length(tmp.n)); names(tmp.v) <- tmp.n
    arr.evt.pch <- c(arr.evt.pch ,tmp.v )
    
    minThickness <- 5
    
    frameIt <- function( x , y ) {
      abs.x.offset <- max.x * x.offset;  
      m <- (max.x - abs.x.offset) / max.x
      x <- m * x + abs.x.offset
      return( c(x,y) )
    }
    
    par(mar=c(1,1,1,1))
    plot( 0 , 0 , xlim = c( 0 , max.x ) , ylim = c( 0, (n.patients+1) * minThickness ), axes = FALSE, xlab = UM , ylab="" , col="white")
    if( Time.on.x.label == TRUE ) {
      xy.1 <- frameIt( 0, 0 ); xy.2 <- frameIt( maxTime, 0 )
      points( x = c(xy.1[1],xy.2[1]), y = c(xy.1[2],xy.1[2]), col="black", type='l')
      if( UM == "mins" ) { x.sequenza <- seq( 0 , maxTime ) }
      if( UM == "hours" ) { x.sequenza <- seq( 0 , maxTime , by = 60 ) }
      if( UM == "days" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24 ) }
      if( UM == "weeks" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24*7 ) }
      if( UM == "months" ) { x.sequenza <- seq( 0 , maxTime , by = 60*24*30 ) }
      x.sequenza <- x.sequenza[1:(length(x.sequenza)-1)]
      # browser()
      for( i in 1:length(x.sequenza) ) {
        xy <- frameIt( x.sequenza[i], 0 );
        points( x = xy[1], y = xy[2], col="black", pch=3)
        label <- x.sequenza[i]
        if( UM == "hours" ) { label <- label/60  }
        if( UM == "days" ) { label <- label/ (60*24)  }
        if( UM == "weeks" ) { label <- label/ (60*24*7)  }
        if( UM == "months" ) { label <- label/ (60*24*30)  }
        
        text(xy[1],xy[2],label,pos = 1 ,cex = x.label.cex )
      }
    }
    
    mtrPointsToPlot <- c()
    tmp <- lapply(1:length( arr.ID ),function( riga ) {
      ID <- arr.ID[riga]
      subMM <- objDL.out$pat.process[[ID]]
      maxTime <- max(subMM$pMineR.deltaDate)
      
      xy.1 <- frameIt( 0, (riga*minThickness) ); xy.2 <- frameIt( maxTime, (riga*minThickness) )
      points( x = c(xy.1[1],xy.2[1]), y = c(xy.1[2],xy.1[2]), col="lightgrey", type='l')
      
      if( ID.on.y.label == TRUE ) { 
        text( (max.x / 100 ), (riga*minThickness) , ID, cex = y.label.cex  )
      }
      
      for( linea in 1:nrow( subMM )) {
        if( subMM[[ evtName ]][linea] %in%  arr.events ) {
          pch.val <- arr.evt.pch[subMM[[ evtName ]][linea]]
          if( grepl("[0-9]+",pch.val) ) pch.val <- as.numeric(pch.val)
          xy <- frameIt( subMM$pMineR.deltaDate[linea]  ,  (riga*minThickness) )
          mtrPointsToPlot <<- rbind( mtrPointsToPlot , c(xy[1],xy[2],pch.val , arr.col[subMM[[ evtName ]][linea]] ))
        }
      }
    })
    tmp <- lapply(1:nrow(mtrPointsToPlot),function(i)  { 
      points( x = mtrPointsToPlot[i,1] ,  y = mtrPointsToPlot[i,2] , pch = as.numeric(mtrPointsToPlot[i,3]), lwd=1, 
              col=mtrPointsToPlot[i,4]  )   
    })
    if( plot.legend == TRUE) {
      legend( legend.Pos, legend = arr.events, col =  arr.col, lty=1, lwd=2 , cex = legend.cex)
    }
    
  }
  
  #=================================================================================
  # plotTraceEvolution
  #=================================================================================
  plotTraceEvolution <- function( objDL.out, holdEvts = FALSE , UM = "days" , max.t = Inf, 
                             legend.Pos = "topright", plot.legend = TRUE, legend.cex = 0.8,
                             arr.ID = c(), cumulative = FALSE, arr.events = c()) {
    
    get.during.time <- function( loadedData , max.t , UM , holdEvts , arr.events) {
      
      tmpDLS <- loadedData
      
      if( UM == "mins") convUM <- 1
      if( UM == "hours") convUM <- 60
      if( UM == "days") convUM <- 60*24
      if( UM == "weeks") convUM <- 60*24*7
      if( UM == "months") convUM <- 60*24*30
      
      new_t <- do.call(rbind, tmpDLS$pat.process )
      new_t$pMineR.deltaDate <-  as.integer(new_t$pMineR.deltaDate/convUM)
      
      arr.eventi <- arr.events
      if( length(arr.eventi)==0 ) {
        arr.eventi <- unique(new_t[[tmpDLS$csv.EVENTName]])  
      }
      
      max.t <- min(max(new_t$pMineR.deltaDate) , max.t)
      
      lst.valori <- list()  
      for(t in 1:max.t) {
        if( length(arr.ID) == 0 ) arr.ID <- names(tmpDLS$pat.process)
        eventi.t <- unlist(lapply( arr.ID, function(IPP) { 
          if( holdEvts == TRUE ) {
            quale <- which(tmpDLS$pat.process[[IPP]]$pMineR.deltaDate <= (t*convUM)   )  
          } else {
            quale <- which(tmpDLS$pat.process[[IPP]]$pMineR.deltaDate >= ((t-1)*convUM) & tmpDLS$pat.process[[IPP]]$pMineR.deltaDate <= ((t)*convUM)   )
          }
          quale <- quale[ length(quale) ]
          tmpDLS$pat.process[[IPP]][[tmpDLS$csv.EVENTName]][quale]
          
        }  ))
        
        tbl.eventi.t <- table(eventi.t)  
        
        for(evento in arr.eventi) {
          if(evento %in% names(tbl.eventi.t)) {
            lst.valori[[as.character(t)]][[evento]] <- tbl.eventi.t[evento]  
          } else  {
            lst.valori[[as.character(t)]][[evento]] <- 0
          }
          if( cumulative == TRUE & holdEvts==FALSE & t>1) {
            lst.valori[[as.character(t)]][[evento]] <- lst.valori[[as.character(t)]][[evento]] + lst.valori[[as.character(t-1)]][[evento]]
          }
        }
      }
      return(list("lst.valori"=lst.valori,"arr.eventi"=arr.eventi))
    }
    
    
    if( holdEvts == TRUE ) {
      WTF<- get.during.time(loadedData = objDL.out, UM = UM, max.t = max.t, holdEvts = holdEvts, arr.events = arr.events)
    } else {
      WTF<- get.during.time(loadedData = objDL.out, UM = UM, max.t = max.t, holdEvts = holdEvts, arr.events = arr.events)  
    }
    
    lst.valori.1 <- WTF$lst.valori
    arr.eventi.1 <- WTF$arr.eventi
    
    arr.colore <- rainbow(n = length(arr.eventi.1)); names(arr.colore) <- arr.eventi.1
    for(i in 1:length(arr.eventi.1)) {
      evento <- arr.eventi.1[i]
      y <- unlist(lapply(names(lst.valori.1), function(t) {  
        lst.valori.1[[t]][[evento]] 
      } ))

      if( holdEvts == TRUE ) {
        y.max.val <- sum(unlist(lst.valori.1[[1]]))
        y.vals <- y/y.max.val; ylim <- c(0,1);ylab <- "%"
      } else {
        y.max.val <- max(unlist(lst.valori.1))
        y.vals <- y; ylim <- c(0,y.max.val); ylab <- "abs"
      }
      if( i == 1) {
        plot( as.numeric(names(lst.valori.1)) ,y.vals, type='l', lwd=2,col=arr.colore[i], 
              xlab=UM, ylab=ylab, ylim=ylim, main="traces evolution")  
      } else {
        points( as.numeric(names(lst.valori.1)) , y.vals, type='l', lwd=2, col=arr.colore[i])  
      }
    }
    if( plot.legend == TRUE ) {
      legend(legend.Pos, legend=arr.eventi.1,
             col=arr.colore[1:length(arr.eventi.1)], lty=1, cex=legend.cex)
    }
    
  }  

  #=================================================================================
  # clearAttributes
  #=================================================================================
  clearAttributes<-function() {
    costructor( UM = UM );
  }  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( UM ) {
    global.dataLoader <<- ''
    global.EFPTs <<- c()
    global.UM <<- UM
    if( global.UM == "" ) global.UM <<- "days"
  }
  #===========================================================
  costructor( UM = UM );
  #===========================================================
  return( list(
    "loadDataset"=loadDataset,
    "query"=query,
    "eventHeatmap"=eventHeatmap,
    "plotTimeline"=plotTimeline,
    "plotTraceEvolution"=plotTraceEvolution
    # "path.count"=path.count
  ) )
}
