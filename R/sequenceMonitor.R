# library(pMineR)
# 
# 
# objDataFarm <- syntheticDataCreator()
# EL <- objDataFarm$cohort.RT()
# EL[which(EL[,"Event"]=="MedicalVisit"), "Event"] <- "Medical Visit"
# EL <- data.frame(EL)
# EL$ID <- as.numeric(EL$ID)
# EL$Event <- as.character(EL$Event)
# EL$Date <- as.character(EL$Date)
# 
# objDL <- dataLoader()
# objDL$load.data.frame(mydata = EL,IDName = "ID",EVENTName = "Event",dateColumnName = "Date",format.column.date = "%Y-%m-%d")
# out.objDL <- objDL$getData()
# 
# arr.eventi <- out.objDL$arrayAssociativo[which(!(out.objDL$arrayAssociativo %in% c("BEGIN","END")))]
# n.arr.eventi <- length(arr.eventi)
# 
# arr.n.stories <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
# 
# #
# prendi.storie.omologhe <- function( wordSequence.raw , arr.n.stories=c() ) {
#   if( is.null(arr.n.stories) ) arr.n.stories <- 1:length(wordSequence.raw)
#   # fai una prima soppressione delle ripetizioni
#   sequence.no.rep <- lapply( arr.n.stories , function( i ) {
#     sequenza <- as.character(unlist(wordSequence.raw[[i]]))
#     what2keep <- unlist(lapply(2:length(sequenza), function(j){ if(  sequenza[j]!=sequenza[j-1] ) {return(TRUE)} else{ return(FALSE)}  }))
#     sequenza <- sequenza[c(TRUE,what2keep)]
#     return(sequenza)
#   })
#   # fai una prima soppressione delle ripetizioni
#   sequence.imploded <- as.character(unlist(lapply( 1:length(sequence.no.rep) , function( i ) {
#     sequenza <- paste(as.character(unlist(sequence.no.rep[[i]])),collapse = "|&|&|&|")
#     return(sequenza)
#   })))
#   tabella.seq <- table(sequence.imploded)
# 
#   gruppi <- lapply(names(tabella.seq), function(gruppo) { which( sequence.imploded == gruppo) }  )
#   return(
#     list(
#       "gruppi"=gruppi,
#       "sequence.no.rep"=sequence.no.rep
#     ))
# }
# 
# pre.processing <- function(  wordSequence.raw ) {
#   lst.res <- prendi.storie.omologhe( wordSequence.raw = wordSequence.raw)
#   gruppi <- lst.res$gruppi
#   su.quali.gruppi.agire <- which(unlist(lapply(gruppi,length))>1)
#   lst.nuove.parole <- wordSequence.raw
#   for(g in su.quali.gruppi.agire) {
#     righe <- gruppi[[ g ]]
#     MM <- c()
#     for(j in righe){
#       p.1 <- wordSequence.raw[[ j ]]
#       esito <- c(unlist(lapply(1:(length(p.1)-1) , function(k){ if( p.1[k]==p.1[k+1]  ) {return(TRUE)} else{return(FALSE)}   })),FALSE)
#       seq.compressa <- lst.res$sequence.no.rep[[j]]
# 
#       arr.ripetizioni <- rep(1,length(seq.compressa))
#       hook.sq.Non.Rip <- 1
#       for(i in 1:length(p.1)) {
#         if( p.1[i]==seq.compressa[hook.sq.Non.Rip]  ) {
#           arr.ripetizioni[hook.sq.Non.Rip] <- arr.ripetizioni[hook.sq.Non.Rip] + 1
#         } else {
#           hook.sq.Non.Rip <- hook.sq.Non.Rip + 1
#         }
#       }
#       arr.ripetizioni[1] <- arr.ripetizioni[1] - 1
#       MM <- rbind(MM, arr.ripetizioni )
#     }
#     max.repetitions <- apply(MM,MARGIN = 2,max)
#     # ora rigenera la nuova parola
#     nuova.parola <- unlist(lapply(1:length(seq.compressa),function(kk) { rep(seq.compressa[kk],max.repetitions[kk])   }))
#     lst.nuove.parole[[j]] <- nuova.parola
#   }
#   return(list("new.words"=lst.nuove.parole))
# }
# 
# wordSequence.raw <- out.objDL$wordSequence.raw
# a <- pre.processing(wordSequence.raw = wordSequence.raw)
# # stop("testare a")
# #
# plotSequence <- function(out.objDL , arr.n.stories =c() , legend.on.top = FALSE ) {
#   if(is.null(arr.n.stories)) arr.n.stories <- 1:length(out.objDL$wordSequence.raw)
#   par(mar = c(0.1,0.1,0.1,0.1))
# 
#   # costruisci la griglia
#   y.pos.txt <- 0 ; legend.position <- 4
#   if( legend.on.top == TRUE) { y.pos.txt <- 100; legend.position <- 2}
#   plot(c(),xlim=c(0,(n.arr.eventi+1)), ylim=c(0,100) , bty="n", xaxt='n', yaxt='n')
#   for( i in 1:n.arr.eventi ) {
#     points( c(i,i), c(0,100), type='l', col="grey")
#     text( x = i, y = y.pos.txt, arr.eventi[i],srt=90,pos=legend.position)
#   }
# 
#   # plotta le storie
#   y.step <- 5; y.delta.between.stories <- 10
#   lwd = 2
# 
#   # fai una stima del y.delta.between.stories ottimale
#   total.jump <- 0
#   for(n.stories in arr.n.stories ) {
#     arr.evt <- as.character(unlist(out.objDL$wordSequence.raw[n.stories]))
#     total.jump <- total.jump + length(arr.evt)
#   }
# 
#   y.step <- 70 / total.jump
#   y.delta.between.stories <- y.step * 2
#   down.counter <- 0
# 
#   # fai una stima del y.delta.between.stories ottimale
#   for(n.stories in arr.n.stories ) {
#     sequenza <- as.character(unlist(out.objDL$wordSequence.raw[n.stories])); n.sequenza <- length(sequenza)
#     direction <- ""; old.direction <- ""
#     for(i in 1:(n.sequenza-1)) {
#       evento <- sequenza[i];  evento.next <- sequenza[i+1]
#       if( which( arr.eventi == evento.next) > which( arr.eventi == evento)) direction <- "dx"
#       if( which( arr.eventi == evento.next) < which( arr.eventi == evento)) direction <- "sx"
#       if( i == 1 ) {
#         if( evento.next == evento ) { next; }
#         old.direction <- direction
#         next;
#       }
#       if( evento.next == evento ) {    next;  }
#       if( direction != old.direction & old.direction!="") { down.counter <- down.counter + 1 }
#       old.direction <- direction
#     }
#   }
# 
# 
#   # plotta le storie
#   if( legend.on.top == FALSE) {
#     y.start <- 100;
#     y.min.bottom <- 20
#   } else {
#     y.start <- 80;
#     y.min.bottom <- 00
#   }
#   y.current <- y.start
#   lwd = 2
# 
# 
#   y.step <- (y.start-y.min.bottom) / (down.counter + 2 * length(arr.n.stories))
#   y.delta.between.stories <- y.step * 2
#   down.counter <- 0
# 
#   for(n.stories in arr.n.stories ) {
# 
#     sequenza <- as.character(unlist(out.objDL$wordSequence.raw[n.stories])); n.sequenza <- length(sequenza)
#     direction <- ""; old.direction <- ""
#     for(i in 1:(n.sequenza-1)) {
#       evento <- sequenza[i]
#       evento.next <- sequenza[i+1]
# 
#       if( which( arr.eventi == evento.next) > which( arr.eventi == evento)) direction <- "dx"
#       if( which( arr.eventi == evento.next) < which( arr.eventi == evento)) direction <- "sx"
# 
#       if( i == 1 ) {
#         x.pos <- which(arr.eventi == evento);     y.pos <- y.current
#         x.pos.target <- which(arr.eventi == evento.next);       y.pos.target <- y.current
#         points(  c(x.pos,x.pos.target) , c(y.pos.target,y.pos.target) , type='l' , lwd = lwd )
#         points(  x.pos , y.pos.target ,pch=20 )
#         points(  x.pos.target , y.pos.target ,pch=20 )
#         if( evento.next == evento ) { next; }
#         arrows( x0 = x.pos,x1 = x.pos.target , y0 = y.pos.target,y1 = y.pos.target, lwd = lwd,length = 0.1)
#         old.direction <- direction
#         next;
#       }
# 
#       if( evento.next == evento ) {
#         points(  x.pos.target , y.pos.target , pch=1 , cex = 3)
#         next;
#       }
# 
#       if( direction != old.direction & old.direction!="") {
#         y.pos.down <- y.current-y.step
#         down.counter <- down.counter + 1
#         points(  c(x.pos.target,x.pos.target) , c(y.current,y.pos.down) , type='l' , lwd = lwd )
#         points(  x.pos.target , y.pos.down , pch=20, lwd = lwd )
#         y.current <- y.pos.down
#       }
# 
#       x.pos <- which(arr.eventi == evento);     y.pos <- y.current
#       x.pos.target <- which(arr.eventi == evento.next);       y.pos.target <- y.current
#       points(  c(x.pos,x.pos.target) , c(y.pos.target,y.pos.target) , type='l' , lwd = lwd )
#       points(  x.pos.target , y.pos.target ,pch=20 )
#       arrows( x0 = x.pos,x1 = x.pos.target , y0 = y.pos.target,y1 = y.pos.target, lwd = lwd,length = 0.1)
# 
#       old.direction <- direction
#     }
#     y.current <- y.current - y.delta.between.stories
#   }
# 
# }
# 
# 
# plotSequence(out.objDL = out.objDL,arr.n.stories = c(1,2,3,4,5,6,7,8,9,10))



