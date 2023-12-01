#' A classto build Synthetic Data
#'
#' @description  A class with many methods for synthetic data generation
#' @export
syntheticDataCreator<-function() {
  
  dado <- function( facce ) {
    return(as.integer(runif(1)*facce+1))
  }
  dado.time<-function(lower_bound,upper_bound){
    # Genera un numero casuale da una distribuzione uniforme
    random_number <- runif(1, min = lower_bound, max = upper_bound)
    return(as.integer(random_number))
  }  
  
  cohort.RT<-function(numOfPat = 100, starting.date = "01/01/2001", giveBack = "csv" ,
                      include.sex.attribute = FALSE, p.censoring = 0) {
    data.partenza <- starting.date

    arr.sex.attribute <- c()
    
    matrice <- c()
    for ( id in seq(1,numOfPat) ) {
      vecchia.data <- data.partenza
      morto <- FALSE
      resezioneCompleta <- FALSE
      CHT <- FALSE
      RT <- FALSE
      
      arr.sex.attribute[as.character(id)] <- as.integer(runif(1,min = 0,max = 2))
      
      pMorte <- 0.11
      morto <- FALSE
      
      # Prima visita medica e imaging
      for( ct in seq(dado(6))) {
        if(dado(2) == 1) {
          evento = "Medical Visit"
        }
        else  {
          evento = "Imaging"
        }
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(15)
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
      }
      
      if(runif(1)<pMorte) {
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(15)
        matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
        morto <- TRUE
        next
      }
      if(morto == TRUE) next
      if( runif(n = 1) <= p.censoring) next
      
      # Biopsy
      vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(10)
      matrice <- rbind( matrice, c(id , "Biopsy" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
      
      pMorte <- pMorte + 0.01
      if(runif(1)<pMorte) {
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(15)
        matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
        morto <- TRUE
        next
      }  
      if(morto == TRUE) next
      if( runif(n = 1) <= p.censoring) next
      
      # Surgery
      vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(20)
      if(dado(5)<=2) {
        matrice <- rbind( matrice, c(id , "total resection" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
        pMorte <- pMorte + 0.3
        resezioneCompleta <- TRUE
      }
      else  {
        matrice <- rbind( matrice, c(id , "partial resection" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
        pMorte <- pMorte + 0.2
        resezioneCompleta <- FALSE
      }
      if(runif(1)<pMorte) {
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(15)
        matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
        morto <- TRUE
        next
      }   
      if(morto == TRUE) next
      if( runif(n = 1) <= p.censoring) next
      
      if(resezioneCompleta==TRUE) pMorte <- pMorte <- 0.15
      
      # Altra terapia?
      vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(15)
      if(dado(5)<=3) {
        # CHT
        if(dado(10)>=4) {
          for(ct in seq(5,(dado(5)+5)) ) {
            vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(3)
            matrice <- rbind( matrice, c(id , "chemotherapy" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))      
          }
          if(runif(1)<(pMorte/2)) {
            vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(1)
            matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
            morto <- TRUE
            next
          }       
          pMorte <- pMorte - .02
          CHT <- TRUE
          if(morto == TRUE) next
          if( runif(n = 1) <= p.censoring) next
        }
        
        # RT
        if(dado(10)>=4) {
          for(ct in seq(5,(dado(15)+5)) ) {
            vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(3)
            matrice <- rbind( matrice, c(id , "radiotherapy" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))      
          }
          if(runif(1)<(pMorte/2)) {
            vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(1)
            matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
            morto <- TRUE
            next
          }
          pMorte <- pMorte - .03
          RT <- TRUE
          if(morto == TRUE) next
          if( runif(n = 1) <= p.censoring) next
        }
      }
      
      # FOLLOWUP
      for( ct in seq(1,dado(8))) {
        
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") +  (180 - dado(120))  
        matrice <- rbind( matrice, c(id , "Medical Visit" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))      
        
        pMorte <- pMorte + 0.1
        
        if(runif(1)<pMorte) {
          vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(1)
          matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
          morto <- TRUE
        }
        if(morto == TRUE) break
        if( runif(n = 1) <= p.censoring) break
        if( resezioneCompleta == TRUE ) pMorte <- pMorte - 0.03
        if( CHT == TRUE ) pMorte <- pMorte - 0.02
        if( RT == TRUE ) pMorte <- pMorte - 0.02
        
      }
    }

    colnames(matrice) <- c("ID","Event","Date")
    
    if( include.sex.attribute == TRUE) {
      arr.sex <- unlist(lapply(unique(matrice[,"ID"]), function(ID){
        return(rep( arr.sex.attribute[ID], length(which(matrice[,"ID"] == ID))))
      }))
      arr.sex[which(arr.sex==1)] <- "M"
      arr.sex[which(arr.sex==0)] <- "F"
      matrice <- cbind( matrice , "Sex" = arr.sex)
    }

    if( giveBack == "dataLoader" ) {
      objDL <- dataLoader(verbose.mode = FALSE)
      objDL$load.data.frame(mydata = data.frame(matrice),IDName = "ID",EVENTName = "Event",dateColumnName = "Date",format.column.date = "%Y-%m-%d")
      matrice <- objDL
    }
    
    return(matrice);
  }
  
  cohort.RT2<-function(numOfPat = 100, starting.date = "01/01/2001", giveBack = "csv" ,arr.linac=c(),arr.priorita=c()) {
    data.partenza <- starting.date
    
    matrice <- c()
    for ( id in seq(1,numOfPat) ) {
      sospeso<-FALSE
      sospeso.cl<-FALSE

      #probabilità base che il trattamento venga sospeso x clinici
      p.sosp.RT<-0.11
      
      #genero attributi degli eventi (gli attibuti non variano nel tempo):
      if(is.null(arr.priorita)){
        priorita<-as.character(dado(4))
      }else{
        priorita<-arr.priorita[id]
      }
      
      if(is.null(arr.linac)){
        lin.code<-as.character(dado(4))
        linac<-switch(lin.code,"1"="CY","2"="TO3","3"="TY","4"="VE")
      }else{
        linac<-arr.linac[id]
      }

      motivo<-NA
      
      sex<-dado(2)
      if(sex==2){
        sex<-"Female"
      }else{
        sex<-"Male"
      }
      
      age<-dado(80)
      
      while(age<=20){
        age<-dado(80)
      }
      
      osp<-as.character(dado(2))

      #incremento della probabilità di sospensione x motivi clinici dipende dalla gravità del paziente
      p.sosp.RT.cl<-switch(priorita,"1"=0.46,"2"=0.02,"3"=0.01,"4"=0.01)
      p.sosp.RT<-p.sosp.RT+p.sosp.RT.cl
      
      #probabilità di sospensione legata alla manutenzione dei macchinari:
      p.sosp.RT.man<-0.46
      
      p.sosp.RT.mac<-switch(linac,"CY"=0.8,"TO3"= 0.05,"TY"=0.5,"VE"=0.02)

      p.sosp.RT.man<-p.sosp.RT.man+p.sosp.RT.mac
      
      #Suppongo che alcuni macchinari di ospedale 1 sono soliti andare in manutenzione molto più frequentemente di osp 2
      if(osp=="1"){
        # p.sosp<-p.sosp*1.3
        p.sosp.RT.mac<-p.sosp.RT.mac*1.2
      }
      
      # EVENTO1: Prescrizione
      evento="Prescrizione"
      new.date<-as.Date(data.partenza,"%d/%m/%Y") + dado(5)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(new.date,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))

      #EVENTO2: Scheduling
      evento="Scheduling"
      
      #tempi di passaggio da Prescription a Scheduling sulla base del tipo di Priorità
      time.pr.sche<-switch(priorita,"1"=dado.time(1,2),"2"=dado.time(1,3),
                           "3"=dado.time(1,56),"4"=dado.time(1,28))
      
      data.new<-as.Date(new.date,"%d/%m/%Y") + time.pr.sche
      
      #se sono in osp "2" aggiungo un'ulteriore latenza
      if(osp=="2") data.new<-data.new + dado(3)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y") ),priorita,linac,sex,age,osp,motivo))

      #EVENTO3: CT Simulation
      evento="CT Simulation"
      
      #tempi di attesa tra Scheduling e prescription dipendono dal macchinario usato:
      time.sche.sim<-switch(linac,
                            "CY"=dado.time(9,14),
                            "TO3"= dado.time(1,13),
                            "TY"=dado.time(1,13),
                            "VE"=dado.time(10,15))

      #e dal tipo di priorita:
      time.sche.sim.pr<-switch(priorita,
                               "1"=dado.time(1,12),
                               "2"=dado.time(2,21),
                               "3"=dado.time(8,27),
                               "4"=dado.time(10,21))
      
      data.new<-as.Date(data.new,"%d/%m/%Y") + time.sche.sim + time.sche.sim.pr
      
      #se sono in osp "2" aggiungo un'ulteriore latenza
      if(osp=="2")data.new<- data.new + dado(5)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))

      #EVENTO 4:  RT Start
      evento= "RT Start"
      
      #faccio variare l'inizio del trattamento in base al codice di priorita
      time.ct.start<-switch(priorita,
                            "1"=dado.time(2,11),
                            "2"=dado.time(1,13),
                            "3"=dado.time(8,15),
                            "4"=dado.time(11,32))
      
      data.new<-as.Date(data.new,"%d/%m/%Y") + time.ct.start
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      
      # EVENTO SOSPENSIONE: possibile che la terapia venga interrotta da un evento di sospensione!
      
      #durate la terapia posso avere delle sospensioni
      #suppongo che le sospensioni avvengano per:
      # - ragioni cliniche legate al tumore (1)
      # - ragioni cliniche non legate al tumore (2)
      # - malfunzionamento/manutenzione dei macchinari (3)
      
      #CASO 1: ragioni Manutenzione:
      p.random<-runif(1)
      if(p.random<p.sosp.RT.man){
        evento="Suspension"
        motivo<-"3"
        
        #calcolo il delta temporale tra start ed evento di sospensione
        time.start.sosp<-dado.time(3,7)
        
        #CALCOLO LA DURATA CHE AVRA LA SOSPENSIONE:
        #il tempo della sospensione dovuto alla manutenzione varia in base al tipo di macchinario e al tipo di ospedale,
        #l'ospedale 2 ha in generale tempi di manutenzione più lunghi
        time.sosp<-switch(linac,
                          "CY"={
                            if(osp=="1"){
                              time.sosp=dado.time(10,15)
                              # time.sosp=dado.time(4,7)
                            }else{
                              time.sosp= dado.time(5,8)
                            }
                          },
                          "TO3"= {
                            if(osp=="1"){
                              time.sosp=dado.time(11,15)
                              
                            }else{
                              time.sosp=dado.time(17,23)
                            }
                          },
                          "TY"={
                            if(osp=="1"){
                              time.sosp=dado.time(2,3)
                              
                            }else{
                              time.sosp=dado.time(5,7)
                            }
                          },
                          "VE"={
                            if(osp=="1"){
                              time.sosp=dado.time(20,25)
                              
                            }else{
                              time.sosp=dado.time(25,28)
                            }
                          })
        
        time.pr<-switch(priorita,"1"=dado(2),"2"=dado(2),
                        "3"=dado(4),"4"=dado(5))
        
        #DURATA DELLA SOSPENSIONE: componente del macchinario + componente priorità
        time.tot<-time.sosp+time.pr
        
        #data inizio sospensione : data di RT start + delta start-sosp
        data.new <- as.Date(data.new,"%d/%m/%Y") + time.start.sosp
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        sospeso <- TRUE
      }
      
      if(p.random<p.sosp.RT){
        
        evento="Suspension"
        motivo<-as.character(dado(2))
        
        #CALCOLO DELTA TEMPORALE:
        
        if(sospeso){
          #se ho già avuto sospensione di manutenzione il delta temporale tra sospensione1 e sospensione2 è data dalla durata
          #della sospensione 1 calcolata nell'if precedente
          time.delta<-time.tot
        }else{
          #se non ho avuto evento di sospensione prima calcolo la distanza temporale tra evento start ed evento sospensione clinica
          time.delta<-dado.time(3,7)
        }
        
        #calcolo la durata di questa sospensione
        time.sosp.cl<-dado.time(5,8)
        
        data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        sospeso.cl <- TRUE
      }
      
      #EVENTO 5:RT End
      evento<-"RT End"
      time.end<-switch(priorita,
                       "1"=dado.time(5,44),
                       "2"=dado.time(5,40),
                       "3"=dado.time(5,22),
                       "4"=dado.time(18,21))
      data.new<- as.Date(data.new,"%d/%m/%Y") + time.end
      
      if(sospeso){
        data.new<-as.Date(data.new,"%d/%m/%Y") + time.tot
      }
      
      if(sospeso.cl){
        data.new<-as.Date(data.new,"%d/%m/%Y") + time.sosp.cl
      }
      
      # data.new<-as.Date(data.new,"%d/%m/%Y") + time.end
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
    }
    
    colnames(matrice) <- c("ID","Event","Date","Priority","LINAC","Sex","Age","Hospital","Stop_Reason")
    
    if( giveBack == "dataLoader" ) {
      objDL <- dataLoader(verbose.mode = FALSE)
      objDL$load.data.frame(mydata = data.frame(matrice),IDName = "ID",EVENTName = "Event",dateColumnName = "Date",format.column.date = "%Y-%m-%d")
      matrice <- objDL
    }
    
    return(matrice);
  }
  
  cohort.RT3<-function(numOfPat = 100, starting.date = "01/01/2001", giveBack = "csv") {
    data.partenza <- starting.date
    
    matrice <- c()
    for ( id in seq(1,numOfPat) ) {
      sospeso<-FALSE
      sospeso.cl<-FALSE
      
      #attributi dell'evento
      priorita<-as.character(dado(4))
      lin.code<-as.character(dado(4))
      linac<-switch(lin.code,"1"="CY","2"="TO3","3"="TY","4"="VE")
      
      motivo<-NA
      
      sex<-dado(2)
      if(sex==2){
        sex<-"Female"
      }else{
        sex<-"Male"
      }
      
      age<-dado(80)
      
      while(age<=20){
        age<-dado(80)
      }
      
      osp<-as.character(dado(2))
      
      #probabilità base che il trattamento venga sospeso x clinici
      p.sosp.RT<-0.11
      
      #incremento della probabilità di sospensione x motivi clinici dipende dalla gravità del paziente
      p.sosp.RT.cl<-switch(priorita,"1"=0.46,"2"=0.02,"3"=0.01,"4"=0.1)
      
      p.sosp.RT<-p.sosp.RT+p.sosp.RT.cl
      
      #probabilità di sospensione legata alla manutenzione dei macchinari:
      p.sosp.RT.man<-0.46
      
      p.sosp.RT.mac<-switch(linac,"CY"=0.37,"TO3"= 0.05,"TY"=0.3,"VE"=0.02)

      p.sosp.RT.man<-p.sosp.RT.man+p.sosp.RT.mac
      
      #Suppongo che macchinari di ospedale 1 sono soliti andare in manutenzione molto più frequentemente di osp 2
      if(osp=="1"){
        p.sosp.RT.mac<-p.sosp.RT.mac*1.2
      }
      
      # EVENTO1: Prescrizione
      evento="Prescrizione"
      new.date<-as.Date(data.partenza,"%d/%m/%Y") + dado(5)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(new.date,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      
      
      #EVENTO2: Scheduling
      evento="Scheduling"
      
      #tempi di passaggio da Prescription a Scheduling sulla base del tipo di Priorità
      time.pr.sche<-switch(priorita,
                           "1"=dado.time(1,2),
                           "2"=dado.time(1,3),
                           "3"=dado.time(1,56),
                           "4"=dado.time(1,28))
      
      data.new<-as.Date(new.date,"%d/%m/%Y") + time.pr.sche
      
      #se sono in osp "2" aggiungo un'ulteriore latenza
      if(osp=="2") data.new<-data.new + dado(3)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y") ),priorita,linac,sex,age,osp,motivo))
      
      
      #EVENTO3: CT Simulation
      evento="CT Simulation"
      
      #tempi di attesa tra Scheduling e prescription dipendono dal macchinario usato:
      time.sche.sim<-switch(linac,
                            "CY"=dado.time(9,14),
                            "TO3"= dado.time(1,13),
                            "TY"=dado.time(1,13),
                            "VE"=dado.time(10,15))
      
      #e dal tipo di priorita:
      time.sche.sim.pr<-switch(priorita,
                               "1"=dado.time(1,12),
                               "2"=dado.time(2,21),
                               "3"=dado.time(8,27),
                               "4"=dado.time(10,21))
      
      data.new<-as.Date(data.new,"%d/%m/%Y") + time.sche.sim + time.sche.sim.pr
      
      #se sono in osp "2" aggiungo un'ulteriore latenza
      if(osp=="2")data.new<- data.new + dado(5)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      
      
      #EVENTO 4:  RT Start
      evento= "RT Start"
      
      #faccio variare l'inizio del trattamento in base al codice di priorita
      time.ct.start<-switch(priorita,
                            "1"=dado.time(2,11),
                            "2"=dado.time(1,13),
                            "3"=dado.time(8,15),
                            "4"=dado.time(11,32))
      
      #calcolo la durata del trattamento (tempo che dovrà passare da RT Start a Rt End): dipenderà dal tipo di classe priorità
      time.tratt<-switch(priorita,
                         "1"=dado.time(10,44),
                         "2"=dado.time(10,40),
                         "3"=dado.time(12,22),
                         "4"=dado.time(18,21))
      
      data.new<-as.Date(data.new,"%d/%m/%Y") + time.ct.start
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))

      # EVENTO SOSPENSIONE: possibile che la terapia venga interrotta da un evento di sospensione!
      
      #durate la terapia posso avere delle sospensioni
      #suppongo che le sospensioni avvengano per:
      # - ragioni cliniche legate al tumore (1)
      # - ragioni cliniche non legate al tumore (2)
      # - malfunzionamento/manutenzione dei macchinari (3)

      p.random<-runif(1)
      if(p.random<p.sosp.RT.man){
        evento="Suspension"
        motivo<-"3"
        
        #calcolo il delta temporale tra start ed evento di sospensione
        time.start.sosp<-dado.time(1,(time.tratt-2))
        
        #vado a sottrarre da time.tratt il tempo già passato da RT Start a Suspension
        time.tratt<-time.tratt-time.start.sosp
        
        #CALCOLO LA DURATA CHE AVRA LA SOSPENSIONE:
        #il tempo della sospensione dovuto alla manutenzione varia in base al tipo di macchinario e al tipo di ospedale,
        #l'ospedale 2 ha in generale tempi di manutenzione più lunghi
        time.sosp<-switch(linac,
                          "CY"={
                            if(osp=="1"){
                              time.sosp=dado.time(10,15)
                              # time.sosp=dado.time(4,7)
                            }else{
                              time.sosp= dado.time(5,8)
                            }
                          },
                          "TO3"= {
                            if(osp=="1"){
                              time.sosp=dado.time(11,15)
                              
                            }else{
                              time.sosp=dado.time(17,23)
                            }
                          },
                          "TY"={
                            if(osp=="1"){
                              time.sosp=dado.time(2,3)
                              
                            }else{
                              time.sosp=dado.time(5,7)
                            }
                          },
                          "VE"={
                            if(osp=="1"){
                              time.sosp=dado.time(20,25)
                              
                            }else{
                              time.sosp=dado.time(25,28)
                            }
                          })
        
        time.pr<-switch(priorita, "1"=dado(2),"2"=dado(2),
                        "3"=dado(4),"4"=dado(5))
        
        #DURATA DELLA SOSPENSIONE: componente del macchinario + componente priorità
        time.tot<-time.sosp+time.pr
        
        #data inizio sospensione : data di RT start + delta start-sosp
        data.new <- as.Date(data.new,"%d/%m/%Y") + time.start.sosp
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        sospeso <- TRUE
      }

      if(runif(1)<p.sosp.RT){
        evento="Clinical Suspension"
        motivo<-as.character(dado(2))
        
        #tempo tra Sosp cl e il suo evento precedente che può essere o RT Start o SUspension
        #NB il tempo tra sosp e il suo evento precedente può durare da 1 alla durata totale del trattamento -5
        if(sospeso){
          time.delta<-dado.time(1,(time.tratt))
        }else{
          time.delta<-dado.time(1,(time.tratt-2))
        }
        
        #sottraggo questo arco temporale al time.tratt
        time.tratt<-time.tratt-time.delta
        
        #calcolo la durata di questa sospensione
        time.sosp.cl<-dado.time(5,8)
        
        if(sospeso){
          #se ho già avuto sospensione di manutenzione il delta temporale tra sospensione1 e sospensione2 è data dalla durata
          #della sospensione 1 calcolata nell'if precedente
          time.delta<-time.delta+time.tot
          
          data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
          matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          #EVENTO 5:RT End
          evento<-"RT End"
          data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt + time.sosp.cl
          matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          
        }else{
          if(runif(1)<p.sosp.RT.man){
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            
            evento="Suspension"
            motivo<-"3"
            
            #calcolo il delta temporale tra start ed evento di sospensione
            time.start.sosp<-dado.time(1,(time.tratt-2))
            
            #vado a sottrarre da time.tratt il tempo già passato da RT Start a Suspension
            time.tratt<-time.tratt-time.start.sosp
            
            #CALCOLO LA DURATA CHE AVRA LA SOSPENSIONE:
            #il tempo della sospensione dovuto alla manutenzione varia in base al tipo di macchinario e al tipo di ospedale,
            #l'ospedale 2 ha in generale tempi di manutenzione più lunghi
            time.sosp<-switch(linac,
                              "CY"={
                                if(osp=="1"){
                                  time.sosp=dado.time(10,15)
                                  # time.sosp=dado.time(4,7)
                                }else{
                                  time.sosp= dado.time(5,8)
                                }
                              },
                              "TO3"= {
                                if(osp=="1"){
                                  time.sosp=dado.time(11,15)
                                  
                                }else{
                                  time.sosp=dado.time(17,23)
                                }
                              },
                              "TY"={
                                if(osp=="1"){
                                  time.sosp=dado.time(2,3)
                                  
                                }else{
                                  time.sosp=dado.time(5,7)
                                }
                              },
                              "VE"={
                                if(osp=="1"){
                                  time.sosp=dado.time(20,25)
                                  
                                }else{
                                  time.sosp=dado.time(25,28)
                                }
                              })
            
            time.pr<-switch(priorita,"1"=dado(2),"2"=dado(2),
                            "3"=dado(4),"4"=dado(5))
            
            #DURATA DELLA SOSPENSIONE: componente del macchinario + componente priorità
            time.tot<-time.sosp+time.pr
            
            #data inizio sospensione : data di RT start + delta start-sosp
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.start.sosp+time.sosp.cl
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            evento<-"RT End"
            data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt + time.tot
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          }else{
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            evento<-"RT End"
            data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt+time.sosp.cl
          }
        }
        sospeso.cl<-TRUE
      }

      if(!sospeso & !sospeso.cl){
        #EVENTO 5:RT End
        evento<-"RT End"
        data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      }
    }
    
    colnames(matrice) <- c("ID","Event","Date","Priority","LINAC","Sex","Age","Hospital","Stop_Reason")
    
    if( giveBack == "dataLoader" ) {
      objDL <- dataLoader(verbose.mode = FALSE)
      objDL$load.data.frame(mydata = data.frame(matrice),IDName = "ID",EVENTName = "Event",dateColumnName = "Date",format.column.date = "%Y-%m-%d")
      matrice <- objDL
    }
    return(matrice);
  }
  
  cohort.RT4<-function(numOfPat = 100, starting.date = "01/01/2001", giveBack = "csv") {
    data.partenza <- starting.date
    
    matrice <- c()
    for ( id in seq(1,numOfPat) ) {
      sospeso<-FALSE
      sospeso.cl<-FALSE
      is.CHT<-FALSE
      is.IO<-FALSE
      
      #attributi dell'evento
      priorita<-as.character(dado(4))
      lin.code<-as.character(dado(4))
      linac<-switch(lin.code,"1"="CY","2"="TO3","3"="TY","4"="VE")
      
      motivo<-NA
      
      sex<-dado(2)
      if(sex==2){
        sex<-"Female"
      }else{
        sex<-"Male"
      }
      
      age<-dado(80)
      
      while(age<=20){
        age<-dado(80)
      }
      
      osp<-as.character(dado(2))
      
      #probabilità base che il trattamento venga sospeso x clinici
      p.sosp.RT<-0.11
      
      #incremento della probabilità di sospensione x motivi clinici dipende dalla gravità del paziente
      p.sosp.RT.cl<-switch(priorita,"1"=0.46,"2"=0.02,"3"=0.01,"4"=0.1)
      
      p.sosp.RT<-p.sosp.RT+p.sosp.RT.cl
      
      #probabilità di sospensione legata alla manutenzione dei macchinari:
      p.sosp.RT.man<-0.46
      
      p.sosp.RT.mac<-switch(linac,"CY"=0.37,"TO3"= 0.05,"TY"=0.3,"VE"=0.02)
      
      p.sosp.RT.man<-p.sosp.RT.man+p.sosp.RT.mac
      
      #prob Chemio
      p.CHT<-0.1
      d.CHT<-switch(priorita,"1"=0,"2"=0.02,"3"=0.5,"4"=0.4)
      
      p.CHT<-p.CHT+d.CHT
      
      #prob IO
      pIO<-0.01

      #Suppongo che macchinari di ospedale 1 sono soliti andare in manutenzione molto più frequentemente di osp 2
      if(osp=="1"){
        p.sosp.RT.mac<-p.sosp.RT.mac*1.2
      }

      # EVENTO1: Prescrizione
      evento="Medical Visit"
      data.new<-as.Date(data.partenza,"%d/%m/%Y") + dado(5)
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))

      #EVENTO CHT:
      if(runif(1)<p.CHT){
        evento="Chemotherapy"
        time.visit.che<-dado.time(5,12)
        data.new<-as.Date(data.new,"%d/%m/%Y") + time.visit.che
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        is.CHT<-T
      }
      
      #EVENTO CHT:
      if(runif(1)<pIO & !is.CHT){
        evento="IO therapy"
        time.visit.che<-dado.time(5,10)
        data.new<-as.Date(data.new,"%d/%m/%Y") + time.visit.che
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        is.IO<-T
      }

      #EVENTO 4:  RT Start
      evento= "RT Start"
      
      #faccio variare l'inizio del trattamento in base al codice di priorita
      if(is.CHT){
        delta.timeRTStart<-dado.time(35,80)
      }else if(is.IO){
        delta.timeRTStart<-dado.time(80,100)
      }else{
        delta.timeRTStart<-0
      }
      
      time.ct.start<-switch(priorita,
                            "1"=dado.time(2,11),
                            "2"=dado.time(1,13),
                            "3"=dado.time(8,15),
                            "4"=dado.time(11,32))
      
      time.ct.start<-time.ct.start+delta.timeRTStart

      #calcolo la durata del trattamento (tempo che dovrà passare da RT Start a Rt End): dipenderà dal tipo di classe priorità
      time.tratt<-switch(priorita,
                         "1"=dado.time(10,44),
                         "2"=dado.time(10,40),
                         "3"=dado.time(12,22),
                         "4"=dado.time(18,21))
      
      data.new<-as.Date(data.new,"%d/%m/%Y") + time.ct.start
      matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))

      # EVENTO SOSPENSIONE: possibile che la terapia venga interrotta da un evento di sospensione!
      
      #durate la terapia posso avere delle sospensioni
      #suppongo che le sospensioni avvengano per:
      # - ragioni cliniche legate al tumore (1)
      # - ragioni cliniche non legate al tumore (2)
      # - malfunzionamento/manutenzione dei macchinari (3)

      p.random<-runif(1)
      if(p.random<p.sosp.RT.man){
        evento="Suspension"
        motivo<-"3"
        
        #calcolo il delta temporale tra start ed evento di sospensione
        time.start.sosp<-dado.time(1,(time.tratt-2))
        
        #vado a sottrarre da time.tratt il tempo già passato da RT Start a Suspension
        time.tratt<-time.tratt-time.start.sosp
        
        #CALCOLO LA DURATA CHE AVRA LA SOSPENSIONE:
        #il tempo della sospensione dovuto alla manutenzione varia in base al tipo di macchinario e al tipo di ospedale,
        #l'ospedale 2 ha in generale tempi di manutenzione più lunghi
        time.sosp<-switch(linac,
                          "CY"={
                            if(osp=="1"){
                              time.sosp=dado.time(10,15)
                              # time.sosp=dado.time(4,7)
                            }else{
                              time.sosp= dado.time(5,8)
                            }
                          },
                          "TO3"= {
                            if(osp=="1"){
                              time.sosp=dado.time(11,15)
                              
                            }else{
                              time.sosp=dado.time(17,23)
                            }
                          },
                          "TY"={
                            if(osp=="1"){
                              time.sosp=dado.time(2,3)
                              
                            }else{
                              time.sosp=dado.time(5,7)
                            }
                          },
                          "VE"={
                            if(osp=="1"){
                              time.sosp=dado.time(20,25)
                              
                            }else{
                              time.sosp=dado.time(25,28)
                            }
                          })
        
        time.pr<-switch(priorita,"1"=dado(2),"2"=dado(2),"3"=dado(4),"4"=dado(5))
        
        #DURATA DELLA SOSPENSIONE: componente del macchinario + componente priorità
        time.tot<-time.sosp+time.pr
        
        #data inizio sospensione : data di RT start + delta start-sosp
        data.new <- as.Date(data.new,"%d/%m/%Y") + time.start.sosp
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
        sospeso <- TRUE
      }

      if(runif(1)<p.sosp.RT){
        evento="Clinical Suspension"
        motivo<-as.character(dado(2))
        
        #tempo tra Sosp cl e il suo evento precedente che può essere o RT Start o SUspension
        #NB il tempo tra sosp e il suo evento precedente può durare da 1 alla durata totale del trattamento -5
        if(sospeso){
          time.delta<-dado.time(1,(time.tratt))
        }else{
          time.delta<-dado.time(1,(time.tratt-2))
        }
        
        #sottraggo questo arco temporale al time.tratt
        time.tratt<-time.tratt-time.delta
        
        #calcolo la durata di questa sospensione
        time.sosp.cl<-dado.time(5,8)
        
        if(sospeso){
          #se ho già avuto sospensione di manutenzione il delta temporale tra sospensione1 e sospensione2 è data dalla durata
          #della sospensione 1 calcolata nell'if precedente
          time.delta<-time.delta+time.tot
          
          data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
          matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          #EVENTO 5:RT End
          evento<-"RT End"
          data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt + time.sosp.cl
          matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          
        }else{
          if(runif(1)<p.sosp.RT.man){
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            
            evento="Suspension"
            motivo<-"3"
            
            sospeso<-T
            
            #calcolo il delta temporale tra start ed evento di sospensione
            time.start.sosp<-dado.time(1,(time.tratt-2))
            
            #vado a sottrarre da time.tratt il tempo già passato da RT Start a Suspension
            time.tratt<-time.tratt-time.start.sosp
            
            #CALCOLO LA DURATA CHE AVRA LA SOSPENSIONE:
            #il tempo della sospensione dovuto alla manutenzione varia in base al tipo di macchinario e al tipo di ospedale,
            #l'ospedale 2 ha in generale tempi di manutenzione più lunghi
            time.sosp<-switch(linac,
                              "CY"={
                                if(osp=="1"){
                                  time.sosp=dado.time(10,15)
                                  # time.sosp=dado.time(4,7)
                                }else{
                                  time.sosp= dado.time(5,8)
                                }
                              },
                              "TO3"= {
                                if(osp=="1"){
                                  time.sosp=dado.time(11,15)
                                  
                                }else{
                                  time.sosp=dado.time(17,23)
                                }
                              },
                              "TY"={
                                if(osp=="1"){
                                  time.sosp=dado.time(2,3)
                                  
                                }else{
                                  time.sosp=dado.time(5,7)
                                }
                              },
                              "VE"={
                                if(osp=="1"){
                                  time.sosp=dado.time(20,25)
                                  
                                }else{
                                  time.sosp=dado.time(25,28)
                                }
                              })
            
            time.pr<-switch(priorita,"1"=dado(2),"2"=dado(2),"3"=dado(4),"4"=dado(5))
            
            #DURATA DELLA SOSPENSIONE: componente del macchinario + componente priorità
            time.tot<-time.sosp+time.pr
            
            #data inizio sospensione : data di RT start + delta start-sosp
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.start.sosp+time.sosp.cl
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            evento<-"RT End"
            data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt + time.tot
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
          }else{
            data.new <- as.Date(data.new,"%d/%m/%Y") + time.delta
            matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
            evento<-"RT End"
            data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt+time.sosp.cl
          }
        }
        sospeso.cl<-TRUE
      }

      if(!sospeso & !sospeso.cl){
        #EVENTO 5:RT End
        evento<-"RT End"
        data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      }else if(sospeso & !sospeso.cl){
        #EVENTO 5:RT End
        evento<-"RT End"
        data.new<- as.Date(data.new,"%d/%m/%Y") + time.tratt+time.tot
        matrice <- rbind( matrice, c(id , evento , as.character(as.Date(data.new,"%d/%m/%Y")),priorita,linac,sex,age,osp,motivo))
      }
    }
    
    colnames(matrice) <- c("ID","Event","Date","Priority","LINAC","Sex","Age","Hospital","Stop_Reason")
    
    if( giveBack == "dataLoader" ) {
      objDL <- dataLoader(verbose.mode = FALSE)
      objDL$load.data.frame(mydata = data.frame(matrice),IDName = "ID",EVENTName = "Event",dateColumnName = "Date",format.column.date = "%Y-%m-%d")
      matrice <- objDL
    }
    
    return(matrice);
  }
  getABCSequence <- function( n.symbols = 5, arr.p.back = c() , n.crazy = 0, k.coeff = 2 ) {
    arr.simboli.possibili <- c("ABCDEFGHILMNOPQRSTUVZ")
    if( length(arr.p.back) == 0 ) {
      arr.p.back <- runif(n = n.symbols , 0 , abs(rnorm(n = 1,mean = 0,sd = 0.2))   )
    }
    if( n.crazy > 0 ) {
      quali <- sample( 1:length(arr.p.back), n.crazy)  
      arr.p.back[quali] <- arr.p.back[quali] * k.coeff
    }
    arr.symb <- unlist(str_split(substr(arr.simboli.possibili,1,n.symbols),""))
    
    arr.traccia <- arr.symb[1]; posizione <- 1;
    EndOfTrack <- FALSE
    while( EndOfTrack == FALSE ) {
      i <- which( arr.symb == arr.traccia[length(arr.traccia)]   )
      
      # jump back
      if( runif(1) < arr.p.back[i] ) {
        nuovaPosizione <- order(arr.p.back[1:posizione],decreasing = F)[1]
        arr.traccia <- c( arr.traccia , arr.traccia[ nuovaPosizione ]  )
        posizione <- nuovaPosizione
      } else { 
        # end of trace?
        if( arr.traccia[length(arr.traccia)] == arr.symb[length(arr.symb)]  ) {
          EndOfTrack <- TRUE  
        } else {
          # ok, move  to the next event
          nuovaPosizione <- posizione + 1
          arr.traccia <- c( arr.traccia , arr.symb[ (i + 1) ] )
          posizione <- nuovaPosizione
        }
      }
    }
    return( list("arr.traccia" = arr.traccia , 
                 "arr.symb" = arr.symb 
                 ) 
            )
  }
  
  #===========================================================
  # costructor
  # E' il costruttore della classe
  #===========================================================
  costructor<-function( ) {
  }
  #===========================================================
  costructor( );
  #===========================================================
  return( list(
    "cohort.RT"=cohort.RT,
    "cohort.RT2"=cohort.RT2,
    "cohort.RT3"=cohort.RT3,
    "cohort.RT4"=cohort.RT4,
    "getABCSequence"=getABCSequence
    )
  )
  
}