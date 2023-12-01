#' Cascade Of arroW : a class to path visualization
#' @import survival
#' @export

COW<-function(parameters.list = list()) {
  
  arr.eventi <- NA
  n.arr.eventi <- NA
  wordSequence.raw <- NA
  # pat.process<-NA
  plot.struct<-NA
  parameters<-list()
  
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( out.objDL ) {
    
    arr.eventi <<- out.objDL$arrayAssociativo[which(!(out.objDL$arrayAssociativo %in% c("BEGIN","END")))]
    n.arr.eventi <<- length(arr.eventi)
    wordSequence.raw <<- out.objDL$wordSequence.raw
    # pat.process<<-out.objDL$pat.process
    
    # # dichiara che i dati sono stati caricati
    # is.dataLoaded<<-TRUE
  }
  
  #===========================================================
  # pre.processing function: ROBERTO
  #===========================================================
  pre.processing <- function(  wordSequence.raw ) {
    lst.res <- prendi.storie.omologhe( wordSequence.raw = wordSequence.raw)
    gruppi <- lst.res$gruppi
    su.quali.gruppi.agire <- which(unlist(lapply(gruppi,length))>1)
    lst.nuove.parole <- wordSequence.raw
    for(g in su.quali.gruppi.agire) {
      righe <- gruppi[[ g ]]
      MM <- c()
      for(j in righe){
        p.1 <- wordSequence.raw[[ j ]]
        esito <- c(unlist(lapply(1:(length(p.1)-1) , function(k){ if( p.1[k]==p.1[k+1]  ) {return(TRUE)} else{return(FALSE)}   })),FALSE)
        seq.compressa <- lst.res$sequence.no.rep[[j]]
        
        arr.ripetizioni <- rep(1,length(seq.compressa))
        hook.sq.Non.Rip <- 1
        for(i in 1:length(p.1)) {
          if( p.1[i]==seq.compressa[hook.sq.Non.Rip]  ) {
            arr.ripetizioni[hook.sq.Non.Rip] <- arr.ripetizioni[hook.sq.Non.Rip] + 1
          } else {
            hook.sq.Non.Rip <- hook.sq.Non.Rip + 1
          }
        }
        arr.ripetizioni[1] <- arr.ripetizioni[1] - 1
        MM <- rbind(MM, arr.ripetizioni )
      }
      max.repetitions <- apply(MM,MARGIN = 2,max)
      # ora rigenera la nuova parola
      nuova.parola <- unlist(lapply(1:length(seq.compressa),function(kk) { rep(seq.compressa[kk],max.repetitions[kk])   }))
      lst.nuove.parole[[j]] <- nuova.parola
    }
    return(list("new.words"=lst.nuove.parole))
  }
  
  #==========================================================================
  # prendi.storie.omologhe function:
  #
  # INPUT  wordSequence.raw = lista percorsi
  #        arr.n.stories    = array storie da plottare
  #
  # OUTPUT gruppi           = lista percorsi unici. ogni elemento-i
  #                           della lista è un path unico e contiene
  #                           gli indici di arr.n.stories relativi a
  #                           quell' i-esimo path
  #        sequence.no.rep  = per ogni elemento di arr.n.stories viene
  #                          calcolata la traccia senza loop
  #=========================================================================
  
  prendi.storie.omologhe <- function( wordSequence.raw , arr.n.stories=c() ) {
    if( is.null(arr.n.stories) ) arr.n.stories <- 1:length(wordSequence.raw)
    # fai una prima soppressione delle ripetizioni
    sequence.no.rep <- lapply( arr.n.stories , function( i ) {
      sequenza <- as.character(unlist(wordSequence.raw[[i]]))
      what2keep <- unlist(lapply(2:length(sequenza), function(j){ if(sequenza[j]!=sequenza[j-1] ) {return(TRUE)} else{ return(FALSE)}}))
      sequenza <- sequenza[c(TRUE,what2keep)]
      return(sequenza)
    })
    # fai una prima soppressione delle ripetizioni
    sequence.imploded <- as.character(unlist(lapply( 1:length(sequence.no.rep) , function( i ) {
      sequenza <- paste(as.character(unlist(sequence.no.rep[[i]])),collapse = "|&|&|&|")
      return(sequenza)
    })))
    tabella.seq <- table(sequence.imploded)
    
    gruppi <- lapply(names(tabella.seq), function(gruppo) { which( sequence.imploded == gruppo) }  )
    names(gruppi)<-names(tabella.seq)
    return(
      list(
        "gruppi"=gruppi,
        "sequence.no.rep"=sequence.no.rep
      ))
  }
  
  #=================================================================================
  # loop. compute function :
  # INPUT  wordSequence.raw = lista percorsi
  #        arr.n.stories    = array storie da plottare
  #
  # OUTPUT lst.MM.loop      = lista in cui ogni elemento rappresenta un path unico.
  #                           af ogni evento del path viene associato 1/0 se
  #                           l'evento si ripete (1) oppure se è unico (0)
  #        seq.no.rep       = lista in cui ogni elemento rapresenta un path unico
  #                           (!= da sequence.no.rep,output di prendi.storie.omologhe()
  #                           che mostra i path per ogni paz senza eventi ripetuti)
  #=================================================================================
  
  loop.compute<-function(wordSequence.raw,arr.n.stories = arr.n.stories){
    out.storie.omologhe<-prendi.storie.omologhe(wordSequence.raw, arr.n.stories=arr.n.stories)
    sequence.no.rep<-out.storie.omologhe$sequence.no.rep
    gruppi<-out.storie.omologhe$gruppi
    
    #calcolo lista path per ogni paz
    seq.rep<-lapply(arr.n.stories, function(n.stories){return(wordSequence.raw[[n.stories]])})
    #calcolo path non ripetuti
    seq.no.rep<-lapply(names(gruppi), function(gr.name){return(unlist(strsplit(gr.name,split = "|&|&|&|",fixed = T)))})
    
    #per ogni path unico creo struttura di check per i loop
    lst.MM.loop<-lapply(seq.no.rep, function(sequ){
      paths<-gruppi[[paste(as.character(unlist(sequ)),collapse = "|&|&|&|")]]
      MM.loop<-matrix(0,nrow = length(paths),ncol = length(sequ))
      rownames(MM.loop)<-paths
      colnames(MM.loop)<-sequ
      tmp<-lapply(paths,function(id.path){
        sequenza<-seq.rep[[id.path]]; n.sequenza<-length(sequenza)
        seq.loop<-rep(0,n.sequenza)
        what2keep <- c(TRUE,unlist(lapply(2:length(sequenza), function(j){ if(sequenza[j]!=sequenza[j-1] ) {return(TRUE)} else{return(FALSE)}})))
        seq.loop[which(!what2keep)]<-1
        names(seq.loop)<-sequenza
        if(length(which(seq.loop==1))>0){
          id.rep<-which(seq.loop>0)
          id.mod<-c()
          for (id in id.rep) {
            sub.seq<-seq.loop[1:id]
            id.cand<-which(sub.seq==0 & names(sub.seq)==names(sub.seq)[id])
            id.mod<-c(id.mod,id.cand[which((id.cand-id)==max(id.cand-id))])
          }
          seq.loop[id.mod]<-2
          seq.loop.fin<-seq.loop[which(seq.loop==2| seq.loop==0)]
          seq.loop.fin[which(seq.loop.fin==2)]<-1
          MM.loop[as.character(id.path),]<<-seq.loop.fin
        }else{
          MM.loop[as.character(id.path),]<<-seq.loop
        }
      })
      return(apply(MM.loop, MARGIN=2, sum))
    })
    return(list("lst.MM.loop"=lst.MM.loop,"seq.no.rep"=seq.no.rep))
  }
  
  #=================================================================================
  # segment.implode function :
  # INPUT  gruppi           = output prendi.storie.omologhe() fun
  #        lst.MM.gruppi    = output loop compute funtion
  #
  # OUTPUT lst.MM.gruppi    = lista che contiene: - lst.MM.loop
  #                                               - seq.no.rep
  #        lst.MM.seg       = i cui elementi contengono il numero di pas
  #                           lungo ciascun segmento di un path
  #        gruppi           = lista gruppi unici post implosione
  #=================================================================================
  
  segment.implode.fun<-function(gruppi,lst.MM.gruppi){
    seq.no.rep<-lst.MM.gruppi$seq.no.rep
    tmp<-lapply(1:length(seq.no.rep), function(id.seq){
      to.ret<-NULL
      sequenza<-seq.no.rep[[id.seq]]
      seq.compare<-seq.no.rep[-id.seq]
      tmp2<-lapply(seq.compare, function(sequenza.comp){
        ret.val<-NULL
        if(length(sequenza)<=length(sequenza.comp)){
          if(length(which(sequenza.comp[1:length(sequenza)]==sequenza))==length(sequenza)){ret.val<-TRUE}
        }
      })
      
      if(length(which(!unlist(lapply(tmp2, is.null))))<=1){
        names(tmp2)<-unlist(lapply(seq.compare,function(sequ){return(paste0(sequ,collapse = "|&|&|&|"))}))
        str.sub<-names(unlist(tmp2))
        if(!is.null(str.sub)) to.ret<-str.sub
      }
      return(to.ret)
    })
    
    names(tmp)=unlist(lapply(seq.no.rep,function(sequ){return(paste0(sequ,collapse = "|&|&|&|"))}))
    names(seq.no.rep)<-names(tmp)
    seq.sub<-names(unlist(tmp))
    lst.MM.seg<-lst.MM.gruppi$lst.MM.loop
    lst.MM.seg<-lapply(lst.MM.gruppi$lst.MM.loop, function(segmenti){
      group.name<-paste0(names(segmenti),collapse = "|&|&|&|")
      arr<-segmenti
      arr[1:length(segmenti)]<-length(gruppi[[group.name]])
      return(arr)
    })
    
    id.del<-c()
    id.loop<-c()
    tmp3<-lapply(seq.sub, function(seq.ricolloca){
      id.gr.sub<-gruppi[[seq.ricolloca]]
      n.cont.add<-length(id.gr.sub)
      gruppi[[seq.ricolloca]]<<-NULL
      gruppi[[tmp[[seq.ricolloca]]]]<<-c(gruppi[[tmp[[seq.ricolloca]]]],id.gr.sub)
      
      #modifico la lista seq.no.rep
      id.del<<-c(id.del,which(names(seq.no.rep)==seq.ricolloca))
      id.loop<<-c(id.loop,which(names(seq.no.rep)==tmp[[seq.ricolloca]]))
      # id.del<-which(names(seq.no.rep)==seq.ricolloca)
      # id.loop<-which(names(seq.no.rep)==tmp[[seq.ricolloca]])
      
      #modifico lista Loop
      sequenza.sub<-seq.no.rep[[which(names(seq.no.rep)==seq.ricolloca)]]
      lst.MM.seg[[which(names(seq.no.rep)==tmp[[seq.ricolloca]])]][1:(length(sequenza.sub)-1)]<<-n.cont.add+unique(lst.MM.seg[[which(names(seq.no.rep)==tmp[[seq.ricolloca]])]])
      # sequenza.sub<-seq.no.rep[[id.del]]
      # lst.MM.seg[[id.loop]][1:(length(sequenza.sub)-1)]<<-n.cont.add+unique(lst.MM.seg[[id.loop]])
      
      #elimino sequenza ripetuta
      # lst.MM.gruppi$lst.MM.loop<<-lst.MM.gruppi$lst.MM.loop[-id.del]
      # lst.MM.gruppi$seq.no.rep<<-lst.MM.gruppi$seq.no.rep[-id.del]
      # lst.MM.seg<<-lst.MM.seg[-id.del]
      return(lst.MM.seg)
    })
    
    if(!is.null(id.del)){
      lst.MM.gruppi$lst.MM.loop<-lst.MM.gruppi$lst.MM.loop[-id.del]
      lst.MM.gruppi$seq.no.rep<-lst.MM.gruppi$seq.no.rep[-id.del]
      lst.MM.seg<-lst.MM.seg[-id.del]
    }
    
    names(lst.MM.gruppi$seq.no.rep)<-as.character(seq(1:length(lst.MM.gruppi$seq.no.rep)))
    names(lst.MM.seg)<-as.character(seq(1:length(lst.MM.seg)))
    
    return(list("lst.MM.gruppi"=lst.MM.gruppi,"lst.MM.seg"=lst.MM.seg,"gruppi"=gruppi))
  }
  
  #=================================================================================
  # compute.struct.fun : funzione per creare la struttura per il plot
  #=================================================================================
  
  compute.struct.fun<-function(wordSequence.raw,arr.n.stories){
    lst.res<-prendi.storie.omologhe( wordSequence.raw = wordSequence.raw,arr.n.stories = arr.n.stories)
    lst.MM.gruppi<-loop.compute(wordSequence.raw =wordSequence.raw,arr.n.stories =  arr.n.stories)
    gruppi<-lst.res$gruppi
    seq.no.rep<-lst.MM.gruppi$seq.no.rep
    if(length(arr.n.stories)==1){
      lst.MM.seg<-lst.MM.gruppi$lst.MM.loop
      lst.MM.seg[[1]]<-rep(1,length(lst.MM.seg[[1]]))
      lst.seg.implode<-list("lst.MM.gruppi"=lst.MM.gruppi,"lst.MM.seg"=lst.MM.seg,"gruppi"=gruppi)
    }else{
      lst.seg.implode<-segment.implode.fun(gruppi,lst.MM.gruppi)
    }
    
    return(lst.seg.implode)
  }
  
  
  #=================================================================================
  # plotSequence.interna
  #=================================================================================
  
  plotSequence.interna <- function(arr.n.stories =c() , legend.on.top = FALSE , plot.distinct=TRUE, numbersOfItems=FALSE,joinLoops=T,
                                   maxThickness=4,showId=T) {
    struct<-plot.struct
    if(is.null(arr.n.stories)) arr.n.stories <- 1:length(wordSequence.raw)
    
    if(plot.distinct){
      numbersOfItems=FALSE
      seq.no.rep<-lapply(arr.n.stories,function(id.seq){
        return(wordSequence.raw[[id.seq]])
      })
    }else{
      gruppi<-struct$gruppi
      seq.no.rep<-struct$lst.MM.gruppi$seq.no.rep
      lst.MM.gruppi<-struct$lst.MM.gruppi
      lst.MM.seg<-struct$lst.MM.seg
    }
    par(mar = c(0.1,0.1,0.1,0.1))
    
    
    # costruisci la griglia
    y.pos.txt <- 0 ; legend.position <- 4
    if( legend.on.top == TRUE) { y.pos.txt <- 100; legend.position <- 2}
    plot(c(),xlim=c(0,(n.arr.eventi+1)), ylim=c(0,100) , bty="n", xaxt='n', yaxt='n')
    for( i in 1:n.arr.eventi ) {
      points( c(i,i), c(0,100), type='l', col="grey")
      text( x = i, y = y.pos.txt, arr.eventi[i],srt=90,pos=legend.position)
    }
    
    # plotta le storie
    y.step <- 5; y.delta.between.stories <- 10
    lwd = maxThickness
    
    # fai una stima del y.delta.between.stories ottimale
    total.jump <- 0
    for(n.stories in c(1:length(seq.no.rep)) ) {
      arr.evt <- as.character(unlist(seq.no.rep[[n.stories]]))
      total.jump <- total.jump + length(arr.evt)
    }
    
    y.step <- 70 / total.jump
    y.delta.between.stories <- y.step * 2
    down.counter <- 0
    
    # fai una stima del y.delta.between.stories ottimale
    for(n.stories in c(1:length(seq.no.rep)) ) {
      sequenza <- as.character(unlist(seq.no.rep[[n.stories]])); n.sequenza <- length(sequenza)
      direction <- ""; old.direction <- ""
      for(i in 1:(n.sequenza-1)) {
        evento <- sequenza[i];  evento.next <- sequenza[i+1]
        if( which( arr.eventi == evento.next) > which( arr.eventi == evento)) direction <- "dx"
        if( which( arr.eventi == evento.next) < which( arr.eventi == evento)) direction <- "sx"
        if( i == 1 ) {
          if( evento.next == evento ) { next; }
          old.direction <- direction
          next;
        }
        if( evento.next == evento ) {    next;  }
        if( direction != old.direction & old.direction!="") { down.counter <- down.counter + 1 }
        old.direction <- direction
      }
    }
    
    
    
    # plotta le storie
    if( legend.on.top == FALSE) {
      y.start <- 100;
      y.min.bottom <- 20
    } else {
      y.start <- 80;
      y.min.bottom <- 00
    }
    y.current <- y.start
    lwd = maxThickness
    
    
    y.step <- (y.start-y.min.bottom) / (down.counter + 2 * length(seq.no.rep))
    y.delta.between.stories <- y.step * 2
    down.counter <- 0
    
    for(k in c(1:length(seq.no.rep)) ) {
      sequenza <- seq.no.rep[[k]]; n.sequenza <- length(sequenza)
      if(plot.distinct){
        n.paz<-0.25
      }else{
        if(max(lengths(gruppi))==1){
          n.paz<-0.25
        }else{
          n.paz<-length(gruppi[[paste(as.character(unlist(seq.no.rep[[k]])),collapse = "|&|&|&|")]])/max(lengths(gruppi))
        }
        
      }
      
      direction <- ""; old.direction <- ""
      for(i in 1:(n.sequenza-1)) {
        evento <- sequenza[i]
        evento.next <- sequenza[i+1]
        
        if( which( arr.eventi == evento.next) > which( arr.eventi == evento)) direction <- "dx"
        if( which( arr.eventi == evento.next) < which( arr.eventi == evento)) direction <- "sx"
        
        if( i == 1 ) {
          x.pos <- which(arr.eventi == evento);     y.pos <- y.current
          x.pos.target <- which(arr.eventi == evento.next);       y.pos.target <- y.current
          if(showId){text(x=1-0.3,y=y.pos,paste(k),cex = 0.7)}
          if(!plot.distinct){
            n.paz<-lst.MM.seg[[k]][i]/max(lengths(gruppi))
          }
          points(  c(x.pos,x.pos.target) , c(y.pos.target,y.pos.target) , type='l' , lwd = lwd*n.paz)
          points(  x.pos , y.pos.target ,pch=20 )
          points(  x.pos.target , y.pos.target ,pch=20 )
          if(!plot.distinct & joinLoops ){
            if(lst.MM.gruppi$lst.MM.loop[[k]][i]>0){
              points(  x.pos , y.pos.target , pch=1 , cex = 3)
              arrows( x0 = x.pos,x1 = x.pos.target , y0 = y.pos.target,y1 = y.pos.target, lwd = lwd*n.paz,length = 0.1)
              old.direction <- direction
              next;
            }
            
          }else{
            if( evento.next == evento ) {next;}
            arrows( x0 = x.pos,x1 = x.pos.target , y0 = y.pos.target,y1 = y.pos.target, lwd = lwd*n.paz,length = 0.1)
            old.direction <- direction
            next;
          }
        }
        
        
        if(!plot.distinct & joinLoops){
          if(lst.MM.gruppi$lst.MM.loop[[k]][i]>0){
            points(  x.pos.target , y.pos.target , pch=1 , cex = 3)
            # next;
          }
        }else{
          if( evento.next == evento & joinLoops ) {
            points(  x.pos.target , y.pos.target , pch=1 , cex = 3)
            next;
          }
        }
        
        if( direction != old.direction & old.direction!="") {
          y.pos.down <- y.current-y.step
          down.counter <- down.counter + 1
          if(!plot.distinct){
            n.paz<-lst.MM.seg[[k]][i]/max(lengths(gruppi))
          }
          # n.paz<-lst.MM.seg[[k]][i]/max(lengths(gruppi))
          points(  c(x.pos.target,x.pos.target) , c(y.current,y.pos.down) , type='l' , lwd = lwd*n.paz )
          points(  x.pos.target , y.pos.down , pch=20, lwd = lwd*n.paz )
          y.current <- y.pos.down
        }
        
        x.pos <- which(arr.eventi == evento);     y.pos <- y.current
        x.pos.target <- which(arr.eventi == evento.next);       y.pos.target <- y.current
        
        if(numbersOfItems & !plot.distinct){text(x=x.pos+(x.pos.target-x.pos)/2,y= y.pos + 1 ,lst.MM.seg[[k]][i],cex=0.8)}
        
        
        points(  c(x.pos,x.pos.target) , c(y.pos.target,y.pos.target) , type='l' , lwd = lwd*n.paz )
        points(  x.pos.target , y.pos.target ,pch=20 )
        arrows( x0 = x.pos,x1 = x.pos.target , y0 = y.pos.target,y1 = y.pos.target, lwd = lwd*n.paz,length = 0.1)
        
        old.direction <- direction
      }
      y.current <- y.current - y.delta.between.stories
    }
  }
  
  
  plotTimeEvolution <- function( DL.out ,  arr.events , outcome, markedEvent=c(), main="" , xlim = c(), arr.ID = c(),
                                 plotLegend = TRUE, 
                                 outcome.override = TRUE,
                                 lst.strat.cohorts = list(),
                                 plotCumulativeToOutcome = FALSE, colCumulativeToOutcome = "yellow",
                                 plotKaplanMeier = FALSE
  ) {
    if( length(lst.strat.cohorts) > 2) stop("max 2 cohorts are allowed, for stratification")
    if( length(lst.strat.cohorts) > 0) {  arr.ID <- c( lst.strat.cohorts[[1]] , lst.strat.cohorts[[2]] ) }
    if( length(arr.ID) == 0 ) arr.ID <- names(DL.out$pat.process)
    if( length(markedEvent)==0 ) markedEvent <- outcome
    objUtils <- utils()
    arr.escalation <- arr.events
    arr.col <- objUtils$get.IRON.Palette(alpha = 0.2, n = length(arr.escalation))
    arr.col <- arr.col[length(arr.col):1]
    arr.col <- arr.col[ 2:(length(arr.escalation)+2) ]
    max.time <- max(unlist(lapply( 1:length(arr.ID), function(i) { max(DL.out$pat.process[[ arr.ID[i] ]]$pMineR.deltaDate)} ) ))  
    max.time <- ceiling(max.time/(60*24))
    if(length(xlim)==0) xlim <- c(0,max.time)
    
    ylim <- c(0,length(arr.escalation))
    plot( c(), xlim = xlim, ylim = ylim ,xlab = "days", ylab = "" , yaxt="n", main= main)
    for(i in 1:(length(arr.col)-1)) { rect(xlim[1],i-1,xlim[2],i,col=arr.col[i], border=NA) }
    axis(2, at=(1:(length(arr.col)-1)-0.5), labels=arr.escalation, las=2) #, pos=, lty=, col=, las=, tck=, ...)
    # arr.outcome.cols <- c("red","blue","yellow","green","brown","violet","purple","orange","salmon")[1:length(outcome)]
    if( plotLegend == TRUE) {
      legend("topright", inset=.05, title="Patients",
             c(markedEvent, paste(c("NOT ",outcome),collapse = '')),fill=c("red","blue"), horiz=FALSE)
    }
    
    ID.con.evento <- arr.ID[which(unlist(lapply(arr.ID,function(ID){ markedEvent %in% DL.out$pat.process[[ID]][[ DL.out$csv.EVENTName  ]]     })))]
    ID.senza.evento <- arr.ID[which(!arr.ID %in% ID.con.evento)]
    tempi.con.evento <- c()
    tempi.senza.evento <- c() 
    if( outcome.override == TRUE ) {
      seq.ID <- c(ID.senza.evento , ID.con.evento)
    } else {
      seq.ID <- c(ID.con.evento , ID.senza.evento)    
    }
    divisore <- (60*24)
    # scorri prima quelli senza evento per non rischiare di sovrapporre i colori ed uccidere la visibilita' dell'evento
    arr.t.outcome.1 <- c()
    for( ID in seq.ID ) {    
      sequenza <- DL.out$pat.process[[ID]][[DL.out$csv.EVENTName]]
      evt.subset <- which(sequenza %in% arr.events)
      sequenza <- sequenza[evt.subset]
      tempi <- DL.out$pat.process[[ID]]$pMineR.deltaDate[evt.subset]/divisore
      int.sequenza <- unlist(lapply( 1:length(sequenza), function(i) { which(arr.escalation==sequenza[i])} ) )
      # cat("\n ",ID)
      # if( ID == "100981") browser()
      colore <- ""
      if( length(lst.strat.cohorts) > 1 ) {
        if( ID %in% lst.strat.cohorts[[1]] & markedEvent %in% sequenza) { colore <- "red"; lwd=2 }
        if( ID %in% lst.strat.cohorts[[2]] & markedEvent %in% sequenza) { colore <- "brown"; lwd=2 }  
        if( ID %in% lst.strat.cohorts[[1]] & !(markedEvent %in% sequenza) ) { colore <- "blue"; lwd=2 }
        if( ID %in% lst.strat.cohorts[[2]] & !(markedEvent %in% sequenza) ) { colore <- "deepskyblue"; lwd=2 }       
      } else {
        if( markedEvent %in% sequenza ) {colore <- "red"; lwd <- 2; 
        arr.t.outcome.1 <- c( arr.t.outcome.1 , DL.out$pat.process[[ID]]$pMineR.deltaDate[which( sequenza == markedEvent )[1]]/divisore   )
        }
        if( !(markedEvent %in% sequenza) ) {colore <- "blue"; lwd <- 1}
      }
      points( tempi, int.sequenza-0.5 , type = 'l', col=colore, lwd = lwd)
      points( tempi, int.sequenza-0.5, pch = 20, col=colore, lwd = lwd)
    }
    if(plotCumulativeToOutcome == TRUE) {
      dens <- density(arr.t.outcome.1,from= min(xlim), to=max(xlim))
      x <- dens$x
      y <- cumsum(dens$y) / max( cumsum(dens$y))
      y <- y * (max(ylim)-0.5)
      points(x,y,type='l', lwd=2, col = colCumulativeToOutcome)
    }
    if(plotKaplanMeier == TRUE) {
      stop("not yet implemented")
    }
  }
  
  jumpBackIndex <- function( objDL.out , arrEvt = c() ) {
    evtName <- objDL.out$csv.EVENTName 
    if( length(arrEvt) == 0 ) {
      arrEvt <- objDL.out$arrayAssociativo[which(!(objDL.out$arrayAssociativo %in% c("BEGIN","END")))]
    }
    mtr.evt <- matrix(0,nrow=length(arrEvt),ncol=3)
    colnames(mtr.evt) <- c("jback","count","Sjback")
    rownames(mtr.evt) <- arrEvt
    lst.ID.data <- list()
    for( ID in names(objDL.out$pat.process) ) {
      lst.ID.data[[ID]] <- list()
      sequenza <- objDL.out$pat.process[[ID]][[ evtName ]]
      arr.posizioni <- unlist(lapply(  sequenza , function(evento) {  which(  arrEvt == evento)  }   ))
      arr.delta <- unlist(lapply(1:(length(arr.posizioni)-1), function(i) {  arr.posizioni[i+1]-arr.posizioni[i]   }   ))
      arr.loopback <- !(arr.delta>=0)
      arr.pat.jback <- c(); arr.pat.count <- c(); arr.pat.Sjback <- c()
      if( TRUE %in% arr.loopback ) {
        arr.T <- table(sequenza[1:length(arr.loopback)][arr.loopback])
        for(toAdd in names(arr.T) ){
          mtr.evt[ toAdd , "jback"] <- mtr.evt[ toAdd , "jback"] + as.numeric( arr.T[toAdd] ) 
          arr.pat.jback <- c(arr.pat.jback , rep(toAdd , as.numeric( arr.T[toAdd] )) )
        }
      }
      arr.T <- table(sequenza)
      for(toAdd in names(arr.T) ){
        mtr.evt[ toAdd , "count"] <- mtr.evt[ toAdd , "count"] + as.numeric( arr.T[toAdd] ) 
        # arr.pat.count <- c(arr.pat.count , as.numeric( arr.T[toAdd] ) )
      } 
      arr.pat.count <- length(sequenza)
      # strongLBack
      # browser()
      if( TRUE %in% unlist(lapply(1:(length(arr.loopback)-1),function(i){ ( !arr.loopback[i] & arr.loopback[i+1])  }))) {
        arr.loopback <- c(!unlist(lapply(1:(length(arr.loopback)-1),function(i){ (arr.loopback[i] & arr.loopback[i+1])  })), FALSE ) & arr.loopback
        # browser()
        if( TRUE %in% arr.loopback ) {
          arr.T <- table(sequenza[1:length(arr.loopback)][arr.loopback])
          for(toAdd in names(arr.T) ){
            mtr.evt[ toAdd , "Sjback"] <- mtr.evt[ toAdd , "Sjback"] + as.numeric( arr.T[toAdd] )
            arr.pat.Sjback <- c( arr.pat.Sjback , rep( toAdd , as.numeric( arr.T[toAdd] )) )
          }
        }        
      }
      # if( length(arr.pat.jback) > length(arr.pat.Sjback)) browser()
      lst.ID.data[[ID]]$arr.pat.jback <- arr.pat.jback
      lst.ID.data[[ID]]$arr.pat.count <- arr.pat.count
      lst.ID.data[[ID]]$arr.pat.Sjback <- arr.pat.Sjback            
    }
    
    mtr.evt <- cbind(mtr.evt,"jbackIndex"=rep(0,nrow(mtr.evt)))
    mtr.evt <- cbind(mtr.evt,"SjbackIndex"=rep(0,nrow(mtr.evt)))
    mtr.evt <- cbind(mtr.evt,"jbackNormIndex"=rep(0,nrow(mtr.evt)))
    mtr.evt <- cbind(mtr.evt,"SjbackNormIndex"=rep(0,nrow(mtr.evt)))        
    for(riga in 1:nrow(mtr.evt)) {
      mtr.evt[riga,"jbackIndex"] <- mtr.evt[riga,"jback"] / mtr.evt[riga,"count"]
      mtr.evt[riga,"SjbackIndex"] <- mtr.evt[riga,"Sjback"] / mtr.evt[riga,"count"]      
      mtr.evt[riga,"jbackNormIndex"] <- mtr.evt[riga,"jback"] / sum(mtr.evt[,"count"])
      mtr.evt[riga,"SjbackNormIndex"] <- mtr.evt[riga,"Sjback"] / sum(mtr.evt[,"count"])      
    }
      
    return( list( "mtr.evt" = mtr.evt,
                  "sum.jbackNormIndex" = sum(mtr.evt[,"jbackNormIndex"]),
                  "sum.SjbackNormIndex" = sum(mtr.evt[,"SjbackNormIndex"]),
                  "lst.ID.data"=lst.ID.data
                  )
          )
  }
  
  #=================================================================================
  # plotSequence: funzione di plot
  #=================================================================================
  plotSequence<-function(arr.n.stories =c() , legend.on.top = FALSE , plot.distinct=TRUE, numbersOfItems=FALSE,joinLoops=T,showId=T,maxThickness=4,arr.eventi=c(),plotIt=F){
    if(!is.null(arr.eventi)) arr.eventi<<-arr.eventi
    #creazione struttura
    #struct va in input a plotinterna
    plot.struct<<-compute.struct.fun(wordSequence.raw = wordSequence.raw,arr.n.stories = arr.n.stories)
    # plot.struct<<-struct
    if(plotIt){
      plotSequence.interna(arr.n.stories = arr.n.stories, legend.on.top = legend.on.top ,showId=showId, plot.distinct=plot.distinct, numbersOfItems=numbersOfItems,joinLoops=joinLoops,
                           maxThickness=maxThickness)
    }
    return(plot.struct)
  }
  
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function(parametersFromInput=list()) {
    arr.eventi <<- NA
    n.arr.eventi <<- NA
    wordSequence.raw <<- NA
    parameters<<-parametersFromInput
  }
  costructor(parametersFromInput = parameters.list)
  #=================================================================================
  return(list(
    "loadDataset"=loadDataset,
    "plotSequence"=plotSequence,
    "plotTimeEvolution"=plotTimeEvolution,
    "jumpBackIndex"=jumpBackIndex
  ))
}
