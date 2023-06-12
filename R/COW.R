#' Cascade Of arroW : a class to path visualization
#' @import survival
#' @export

COW<-function(parameters.list = list()) {
  
  arr.eventi <- NA
  n.arr.eventi <- NA
  wordSequence.raw <- NA
  pat.process<-NA
  
  plot.struct<-NA
  
  #parameters$paient.id : TRUE -> viene specificato l'ID del paziente
  #                               a cui appartiene la traccia
  #                       FALSE -> viene specidicato l'indice della traccia
  parameters<-list()
  
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( out.objDL ) {
    
    arr.eventi <<- out.objDL$arrayAssociativo[which(!(out.objDL$arrayAssociativo %in% c("BEGIN","END")))]
    n.arr.eventi <<- length(arr.eventi)
    wordSequence.raw <<- out.objDL$wordSequence.raw
    pat.process<<-out.objDL$pat.process
    
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
  # plotSequence: funzione di plot
  #=================================================================================
  
  KaplanMaier<-function(arr.id.path,eventGoal,arr.n.stories,UM="days"){
    path1<-plot.struct$lst.MM.gruppi$seq.no.rep[[arr.id.path[1]]]
    path2<-plot.struct$lst.MM.gruppi$seq.no.rep[[arr.id.path[2]]]
    
    if(!(eventGoal %in% path1) | !(eventGoal %in% path2)){
      
    }
    if(!parameters$paient.id){
      id.paz1<-names(wordSequence.raw)[arr.n.stories[plot.struct$gruppi[[paste(path1,collapse = "|&|&|&|")]]]]
      id.paz2<-names(wordSequence.raw)[arr.n.stories[plot.struct$gruppi[[paste(path2,collapse = "|&|&|&|")]]]]
    }else{
      id.paz1<-arr.n.stories[plot.struct$gruppi[[paste(path1,collapse = "|&|&|&|")]]]
      id.paz2<-arr.n.stories[plot.struct$gruppi[[paste(path2,collapse = "|&|&|&|")]]]
      
    }
    
    cens1<-c()
    tmp1<-lapply(id.paz1, function(paz){
      row.goal<-which(pat.process[[paz]]$Event==eventGoal)
      if(length(row.goal)==0){
        cens1<<-c(cens1,0)
        delta.time1<-NA
      }else{
        cens1<<-c(cens1,1)
        #sto prendendo il primo match con eventGoal: che fare se ho più eventi di tipo eventGoal??
        delta.time1<-pat.process[[paz]]$pMineR.deltaDate[row.goal[1]]
      }
      return(delta.time1)
    })
    tempi1<-unlist(tmp1)
    KMM1<-cbind(id.paz1,tempi1,cens1,rep("1",length(id.paz1)))
    
    cens2<-c()
    tmp2<-lapply(id.paz2, function(paz){
      row.goal<-which(pat.process[[paz]]$Event==eventGoal)
      if(length(row.goal)==0){
        cens2<<-c(cens2,0)
        delta.time2<-NA
      }else{
        cens2<<-c(cens2,1)
        #sto prendendo il primo match con eventGoal: che fare se ho più eventi di tipo eventGoal??
        delta.time2<-pat.process[[paz]]$pMineR.deltaDate[row.goal[1]]
      }
      return(delta.time2)
    })
    tempi2<-unlist(tmp2)
    KMM2<-cbind(id.paz2,tempi2,cens2,rep("2",length(id.paz2)))
    
    KM.MM<-rbind(KMM1,KMM2)
    colnames(KM.MM)<-c("id","time","outcome","path")
    
    
    if(!is.null(KM.MM)){
      # browser()
      matrice.KM<-as.data.frame(KM.MM)
      
      
      
      ####################### INIZIO MODIFICHE #####################################
      if(class(matrice.KM$outcome)=="factor"){
        matrice.KM$outcome <- as.numeric(levels(matrice.KM$outcome))[matrice.KM$outcome]
      }else{
        matrice.KM$outcome <- as.numeric(matrice.KM$outcome)
      }
      
      if(class(matrice.KM$time)=="factor"){
        matrice.KM$time <- as.numeric(levels(matrice.KM$time))[matrice.KM$time]
      }else{
        matrice.KM$time <- as.numeric(matrice.KM$time)
      }
      ##################### FINE MODIFICHE ###############################################
      
      
      if( UM == "days") matrice.KM$time <- matrice.KM$time / 1440
      if( UM == "hours") matrice.KM$time <- matrice.KM$time / 60
      if( UM == "weeks") matrice.KM$time <- matrice.KM$time / (1440 * 7)
      if( UM == "months") matrice.KM$time <- matrice.KM$time / (43800)
      
      KM<-survfit(Surv(time, outcome) ~ path, data = matrice.KM)
      # plot.obj<-survminer::ggsurvplot(KM,
      #                                 data = matrice.KM,
      #                                 conf.int = TRUE,          # Add confidence interval
      #                                 risk.table = TRUE,        # Add risk table
      #                                 risk.table.height = 0.27,
      #                                 risk.table.col = "strata", # Risk table color by groups
      #                                 pval.method = T,
      #                                 pval = T,xlab=paste0("Time (",UM,")")
      # )
      
      test.out<-wilcox.test(matrice.KM[which(matrice.KM$outcome=="1" & matrice.KM$path=="1" ),"time"],
                            matrice.KM[which(matrice.KM$outcome=="1" & matrice.KM$path=="2" ),"time"],
                            paired = FALSE)
      
      
      
      # test.out<-wilcox.test(matrice.KM[which(matrice.KM$outcome=="1"),"time"] ~ matrice.KM[which(matrice.KM$outcome=="1"),"path"],
      #                       data=matrice.KM[which(matrice.KM$outcome=="1"),],
      #                       paired = FALSE)
      
      
      
      to_ret<-list("table"=matrice.KM, "KM"=KM, "ID"=matrice.KM$ID, "error"=0 , "plot"=plot.obj,"test.out"=test.out)
      
    }else{
      to_ret<-NULL
    }
    
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
    "plotSequence"=plotSequence
  ))
}
