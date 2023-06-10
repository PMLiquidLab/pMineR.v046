#' A simple conformance checking class
#'
#' @description  A first module for making conformance checking
#' @import XML DiagrammeR survival 
#' @param verbose.mode boolean. If TRUE some messages will appear in console, during the computation; otherwise the computation will be silent.
#' @export
confCheck_easy<-function( verbose.mode = TRUE ) {
  WF.xml <- c()                 # The XML with the WF
  WF.xml.fileName <- c()        # Just the XML filename
  dataLog <- c()                # The data structure with the LOGs
  WF.struct <- list()           # the WF structure
  notebook <- list()            # internal notebook for events-log
  tmpAttr <- list()
  play.output.format.date <- ""
  list.computation.matrix<-list()    # lista che contiene i risultati di una computazione

  param.verbose <- c()
  param.use.global.comp.cache<-TRUE
  obj.LogHandler<-c()               # gestore dei messaggi
  global.lista.comandi.condition<-list()
  global.arr.comandi.rilevati<-c()
  global.comp.cache<-list()         # cache per la computazione
  global.personal.ID<-NA
  auto.detected.UM<-""              # Unita' di misura temporale rilevata

  #=================================================================================
  # clearAttributes
  # If we want to 'reset' the obj, this can clear all the attributes
  #=================================================================================
  clearAttributes<-function() {
    costructor();
  }
  #=================================================================================
  # loadWorkFlow
  # It allows to load an XML with a WF
  #=================================================================================
  loadWorkFlow<-function( WF.fileName=NA, WF.text=NA) {
    if(is.na(WF.fileName) & is.na(WF.text)) stop("\n\n\n ERRORE: o 'WF.fileName o 'WF.text' deve essere diverso da 'NA'")

    if(!is.na(WF.fileName)){
      WF.xml <<- xmlInternalTreeParse( WF.fileName )
      WF.xml.fileName <<- WF.fileName
      pre.parsing.confCheck_easy.file(fileName = WF.fileName)
    }
    if(!is.na(WF.text)){
      WF.xml <<- xmlInternalTreeParse(WF.text, asText=TRUE)
      WF.xml.fileName <<- ''
      pre.parsing.confCheck_easy.file(text = WF.text)
    }
    # dopo aver caricato l'XML negli attributi, costruisci la struttura in memoria
    build.Workflow.model()
  }
  #===========================================================
  # array.match
  # verifica se un array sia simile (o no) ad un array di array
  #===========================================================
  array.match<-function( arrIn=c(), lstIn=list() ) {
    for(i in seq(1,length(lstIn))) {
      if( all(arrIn == lstIn[[i]]) == TRUE ) return('TRUE');
    }
    return('FALSE');
  }
  #===========================================================
  # int
  # verifica se un array sia simile (o no) ad un array di array
  #===========================================================
  int<-function( stringa ) {
    suppressWarnings (i <- as.numeric(stringa))
    if(is.na((i))) i<-'';
    return(i);
  }
  #===========================================================
  # build.Workflow.model
  # it parses the XML to build in memory the WF model
  #===========================================================
  build.Workflow.model<-function() {
    lista.trigger<-list()
    lista.stati<-list()
    lista.link <- list();

    # carica la lista degli stati e dei triggers
    array.stati<-unlist(xpathApply(WF.xml,'//xml/workflow/node',xmlGetAttr, "name"))
    array.trigger<-unlist(xpathApply(WF.xml,'//xml/workflow/trigger',xmlGetAttr, "name"))
    array.link<-unlist(xpathApply(WF.xml,'//xml/workflow/link',xmlGetAttr, "name"))

    # Per ogni LINK carica gli attributi 
    if(length(array.link)>0) { 
      for(link.name in array.link) {
        lista.link[[link.name]] <- list()
        obj.link<- xpathApply(WF.xml,paste(c('//xml/workflow/link[@name="',link.name,'"]'),collapse = ""),xmlGetAttr,"obj")[[1]]
        complete.log<- xpathApply(WF.xml,paste(c('//xml/workflow/link[@name="',link.name,'"]'),collapse = ""),xmlGetAttr,"complete")[[1]]      
  
        if(length(obj.link)==0) obj.link <- "PWF"
        if(length(complete.log)==0) complete.log <- "TRUE"
  
        if(obj.link!="PWF") stop("\n Error: at the moment only PWF-objs can be linked, please specify 'PWF'")

        lista.link[[link.name]]$obj <- obj.link
        lista.link[[link.name]]$complete.log <- complete.log    
        lista.link[[link.name]]$arr.states<-c()
        lista.link[[link.name]]$arr.triggers<-c()
      }
    }
      
    # Per ogni stato carica gli attributi (ad es. 'plotIt')
    for(state.name in array.stati) {
      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      st.type<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"type")[[1]]
      st.label<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"label")[[1]]
      st.col<- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]'),collapse = ""),xmlGetAttr,"col")[[1]]

      # aggancia eventuali LINK
      tmp.lista.link <- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]/link'),collapse = ""))
      struttura.link <- list()
      if(length(tmp.lista.link)>0) { 

        arr.link.names <- unlist(xpathApply(tmp.lista.link[[1]],'//xml/workflow/node/link',xmlGetAttr,"name"))
        for(linkName in arr.link.names) {

          executeOn <- xpathApply(WF.xml,paste(c('//xml/workflow/node[@name="',state.name,'"]/link[@name="',linkName,'"]'),collapse = ""),xmlGetAttr,"executeOn")[[1]]
 
          if(length(executeOn)==0) executeOn <- "activations"
          executeOn <- str_trim(str_to_lower(executeOn))

          struttura.link[[linkName]]$name <- linkName
          struttura.link[[linkName]]$executeOn <- executeOn
          
          lista.link[[link.name]]$arr.states <- c(lista.link[[link.name]]$arr.states,state.name)
        }
      }
      
      # default value per il plotIt
      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "")

      # default value per il type
      if(length(st.type)==0) st.type="normal"
      else st.type = str_replace_all(string = st.type,pattern = "'",replacement = "")

      # default value per la label
      if(length(st.label)==0) st.label=state.name
      
      # defailt value per il colore
      if(length(st.col)==0) st.col=""

      # Carica quanto indicato nell'XML nella variabile che poi andra' copiata
      # negli attributi globali
      lista.stati[[ state.name ]]<-list()
      lista.stati[[ state.name ]][["plotIt"]]<-plotIt
      lista.stati[[ state.name ]][["type"]]<-st.type
      lista.stati[[ state.name ]][["label"]]<-st.label
      lista.stati[[ state.name ]][["col"]]<-st.col
      lista.stati[[ state.name ]][["link"]]<-struttura.link
    }

    # Per ogni trigger, carica la 'condition', i 'set' e gli 'unset'
    # (e domani, pure altro)
    for(trigger.name in array.trigger) {

      condizione.lst<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/condition'),collapse = ""),xmlValue)
      if(length(condizione.lst)>0) condizione<-condizione.lst[[1]]
      else condizione<-NA

      plotIt<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"plotIt")[[1]]
      arr.set<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/set'),collapse = ""),xmlValue)
      arr.unset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unset'),collapse = ""),xmlValue)
      arr.unsetAll<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/unsetAll'),collapse = ""),xmlValue)
      arr.hset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/hset'),collapse = ""),xmlValue)
      arr.hunset<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/hunset'),collapse = ""),xmlValue)
      pri<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"pri")[[1]]
      st.col<- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]'),collapse = ""),xmlGetAttr,"col")[[1]]
      
      tmp.lista.link <- xpathApply(WF.xml,paste(c('//xml/workflow/trigger[@name="',trigger.name,'"]/link'),collapse = ""))
      # aggancia eventuali LINK
      struttura.link <- list()
      
      if(length(tmp.lista.link)>0) { 
        # browser()  
        arr.link.names <- unlist(xpathApply(tmp.lista.link[[1]],'//xml/workflow/trigger/link',xmlGetAttr,"name"))
        for(linkName in arr.link.names) {
          struttura.link[[linkName]]$name <- linkName
          struttura.link[[linkName]]$executeOn <- "activations"
          
          lista.link[[link.name]]$arr.triggers <- c(lista.link[[link.name]]$arr.triggers,trigger.name)
        }
      }
      
      if(is.null(pri)) pri<-0;
      pri <- as.numeric(pri)

      if(length(plotIt)==0) plotIt=TRUE
      else plotIt = str_replace_all(string = plotIt,pattern = "'",replacement = "")

      if(length(arr.unsetAll)==0) arr.unsetAll <- FALSE
      else arr.unsetAll <- TRUE
      
      # defailt value per il colore
      if(length(st.col)==0) st.col=""      

      # Ora leggi tutte le condition di ogni trigger ed associa l'attributo "need.time.condition"
      # settato a TRUE o FALSE in funzione che tale trigger abbia un'analisi temporale nella condizione.
      # (Se no, verra' messa in cache, dato che dipende solo dallo stato degli stati)
      has.temporal.condition <- FALSE
      for( comandoToCheck in names(global.lista.comandi.condition) ) {
        matrice.match <- str_locate_all(string = condizione, pattern = comandoToCheck )[[1]]
        if( length(matrice.match) > 0  ) has.temporal.condition <- TRUE
      }

      # Carica quanto indicato nell'XML nella variabile che poi andra' copiata
      # negli attributi globali
      lista.trigger[[ trigger.name ]]<-list()
      lista.trigger[[ trigger.name ]][["condition"]]<-condizione
      lista.trigger[[ trigger.name ]][["set"]]<-arr.set
      lista.trigger[[ trigger.name ]][["hset"]]<-arr.hset
      lista.trigger[[ trigger.name ]][["hunset"]]<-arr.hunset
      lista.trigger[[ trigger.name ]][["pri"]]<-pri
      lista.trigger[[ trigger.name ]][["unset"]]<-arr.unset
      lista.trigger[[ trigger.name ]][["unsetAll"]]<-arr.unsetAll
      lista.trigger[[ trigger.name ]][["plotIt"]]<-plotIt
      lista.trigger[[ trigger.name ]][["st.col"]]<-st.col
      lista.trigger[[ trigger.name ]][["has.temporal.condition"]]<-has.temporal.condition
      lista.trigger[[ trigger.name ]][["link"]]<-struttura.link
    }

    # Costruisci la lista dei nodi 'END'
    arr.nodi.end<-c()
    for(nomeStato in names(lista.stati)) {
      if( lista.stati[[ nomeStato ]]$type == 'END') {
        arr.nodi.end<-c(arr.nodi.end,str_c("'",nomeStato,"'"))
      }
    }
    # popola l'attributo della classe
    if(length(WF.struct)==0) WF.struct[[ "info" ]]<<- list()
    if(is.null(WF.struct[[ "info" ]])) WF.struct[[ "info" ]]<<- list()
    
    WF.struct[[ "info" ]][[ "stati" ]] <<- lista.stati
    WF.struct[[ "info" ]][[ "trigger" ]] <<- lista.trigger
    WF.struct[[ "info" ]][[ "link" ]] <<- lista.link
    WF.struct[[ "info" ]][[ "arr.nodi.end" ]] <<- arr.nodi.end
  }
  #===========================================================
  # replay (ex playLoadedData)
  # esegue il conformanche checking con l'insieme dei LOG precedentemente caricati
  #===========================================================
  replay<-function( number.perc = 1 , event.interpretation = "soft", UM="", resolve.netwok = FALSE, debug = FALSE) {

    if( UM == "" ) UM <- auto.detected.UM
    # Chiama addNote, che via via popola una stringa
    # che alla fine conterra' l'intero XML
    ct<-1
    csv.date.format <- dataLog$csv.date.format

    # Ricrea la struttura di matrici che conterranno l'esito della computazione
    arr.nodi.end <- str_replace_all(string = WF.struct$info$arr.nodi.end, pattern  = "'",replacement = "")
    list.computation.matrix$trigger <<- matrix(0,ncol = length(names(WF.struct$info$trigger)),nrow = length(names(dataLog$pat.process)))
    list.computation.matrix$stati.transizione <<- matrix(0, ncol=length(names(WF.struct$info$stati)),nrow = length(names(dataLog$pat.process)))
    list.computation.matrix$stati.finali <<- matrix(0, ncol=length(names(WF.struct$info$stati)),nrow = length(names(dataLog$pat.process)))

    colnames(list.computation.matrix$stati.transizione)<<-names(WF.struct$info$stati)
    colnames(list.computation.matrix$trigger)<<-names(WF.struct$info$trigger)
    colnames(list.computation.matrix$stati.finali)<<-names(WF.struct$info$stati)
    rownames(list.computation.matrix$stati.transizione) <<- names(dataLog$pat.process)
    rownames(list.computation.matrix$trigger) <<- names(dataLog$pat.process)
    rownames(list.computation.matrix$stati.finali) <<- names(dataLog$pat.process)

    # clear the notebook
    notebook <<- list()
    # and begin to note!
    addNote(msg = "\n<xml>")
    # Per ogni paziente
    for( indice in names(dataLog$wordSequence.raw)) {

      if(dim(dataLog$pat.process[[ indice ]])[1]>0) {

        if(param.verbose == TRUE) cat("\n Doing:",indice)

        addNote(msg = str_c("\n\t<computation n='",ct,"' IDPaz='",indice,"'>"))
        if(param.verbose == TRUE) cat(str_c("\nBeginning Pat ",indice,"..."))

        res <- playSingleSequence( matriceSequenza = dataLog$pat.process[[ indice ]],
                                                      col.eventName = dataLog$csv.EVENTName,
                                                      col.dateName = dataLog$csv.dateColumnName ,
                                                      IDPaz = indice,
                                                      event.interpretation = event.interpretation,
                                                      date.format = dataLog$csv.date.format, UM = UM, 
                                                      store.computation.matrix= TRUE , debug = debug)

        if(param.verbose == TRUE) cat(str_c("\nPat ",indice," done;"))
        addNote(msg = "\n\t\t<atTheEnd>")

        ultimi.stati.computatzione <- unique(str_replace_all(string = res$st.ACTIVE,pattern = "'",replacement = ""))

        # Leva BEGIN dagli stati finali o mi si incastra il tutto
        ultimi.stati.computatzione <- ultimi.stati.computatzione[ which( !(ultimi.stati.computatzione=="BEGIN")) ]

        list.computation.matrix$stati.finali[ indice, ultimi.stati.computatzione ] <<- 1

        for(i in res$st.ACTIVE) addNote(msg = str_c("\n\t\t\t<finalState name=",i,"></finalState>"))
        for(i in res$last.fired.trigger) addNote(msg = str_c("\n\t\t\t<last.fired.trigger name='",i,"'></last.fired.trigger>"))
        addNote(msg = "\n\t\t</atTheEnd>")
        addNote(msg = "\n\t</computation>")
        ct <- ct + 1
        if(   (ct/length(dataLog$wordSequence.raw)) > number.perc  ) break;
      }
    }
    # Chiudi l'XML
    addNote(msg = "\n</xml>")
    
    # Azzera la strutture che conterra' il risultato della computazione
    WF.struct$linked.PWF.obj.computation.results<<-list()
    
    # Se e' stato scelto di explodere eventuali link
    if( resolve.netwok == TRUE ) {
      # Se ci sono link censiti
      if(length(WF.struct$info$link)>0) {
        # Per ogni Link dichiarato, cerca i nodi ed i trigger che possono averlo invocato
        for( nome.link in names(WF.struct$info$link)) {
          
          WF.struct$linked.PWF.obj.computation.results[[nome.link]]<<-list()          
          # Per ogni stato che lo riguarda...
          for( nome.stato in WF.struct$info$link$link_01$arr.states ) {
            # crea un oggetto confCheck_easy e carica lo PWF
            # (usando l'informazione sul suo path precedentemente caricata nella struttura')
            # browser()
            WF.struct$linked.PWF.obj.computation.results[[nome.link]][[nome.stato]]<<-confCheck_easy()
            fileName <- WF.struct$info$link[[nome.link]]$fileName
            WF.struct$linked.PWF.obj.computation.results[[nome.link]][[nome.stato]]$loadWorkFlow(WF.fileName = fileName)
            
            # FASI PRELIMINARI DI LANCIO DELL'ISTANZA.....
            struttura.risultati <- get.list.replay.result()
            # Estrai il dataset nelle due direzioni:
            # (a) per i soli pazienti in 'attivazione' o in 'fine'
            # (b) nel tempo (dall'inizio o dal momento dell'attivazione)
            da.eseguire.on <- WF.struct$info$stati[[nome.stato]]$link[[nome.link]]$executeOn
            tabella <- c()
            if(da.eseguire.on == "activations") tabella <- struttura.risultati$list.computation.matrix$stati.transizione
            if(da.eseguire.on == "terminations") tabella <- struttura.risultati$list.computation.matrix$stati.finali

            arr.pazienti.da.tenere <- rownames(tabella)[  which(tabella[ , which(colnames(tabella) == nome.stato) ] != 0) ]

            if(length(arr.pazienti.da.tenere) > 0) { 
              # Ricostruisci il CSV completo, includendo SOLO i pazienti da preservare
              # (avrei potuto usare il dataLoader::applyFilter(), ma sarebbe stato piu' lento!)
              CSV.completo <- do.call(rbind,  dataLog$pat.process[  names(dataLog$pat.process) %in% arr.pazienti.da.tenere ])
              
              # pulisci le colonne frutto delle elaborazioni precedenti
              # (al momento mi risulta solo la 'pMineR.deltaDate')
              CSV.completo <- CSV.completo[ !(colnames(CSV.completo) %in% c("pMineR.deltaDate")) ]
              # crea l'oggetto dataLoader all'uopo
              tmp.obj.L <- dataLoader()
              tmp.obj.L$load.data.frame(mydata = CSV.completo,IDName = dataLog$csv.IDName,EVENTName = dataLog$csv.EVENTName,dateColumnName = dataLog$csv.dateColumnName,convertUTF = FALSE,suppress.invalid.date = FALSE,format.column.date = "%d/%m/%Y %H:%M:%S"  )
              tmp.dati.da.far.frullare <- tmp.obj.L$getData()
              # Fai il load nell'oggetto confCheck_easy            
              WF.struct$linked.PWF.obj.computation.results[[nome.link]][[nome.stato]]$loadDataset(dataList = tmp.dati.da.far.frullare)
              # ed infine, fai il replay...
              WF.struct$linked.PWF.obj.computation.results[[nome.link]][[nome.stato]]$replay()
            }
            # Se, invece, non sono restati pazienti, casta il nodo a NA
            if(length(arr.pazienti.da.tenere) == 0) WF.struct$linked.PWF.obj.computation.results[[nome.link]][[nome.stato]]<-NA
          }
        }
      }
    }
  }
  #===========================================================
  # get.list.replay.result (ex getPlayedSequencesStat.00)
  # Ops! Non ricordo piu' nemmeno io cosa fa questa funzione.....
  #===========================================================
  get.list.replay.result<-function( linkName=NA, fromState=NA, csv.EventLog.inAddition = FALSE  ) {
    list.fired.trigger<-list()
    list.final.states<-list()
    termination.END.states<-list()
    arr.nodi.end<-c()
    csv.EventLog<-c()

    # prendo la lista dei nodi END
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
    arr.nodi.end <- str_replace_all(string = arr.nodi.end, pattern  = "'",replacement = "")

    doc <- xmlInternalTreeParse(file = get.XML.replay.result(),asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    array.fired.trigger<-c()
    final.states<-c()
    fired.trigger<-c()

    for( i in arr.Computazioni) {
      arr.step<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step'),collapse = ""),xmlGetAttr,"n"))

      array.fired.trigger<-c()
      final.states<-c()
      fired.trigger<-c()

      # Scorri tutti gli step di quella computazione
      for( s  in arr.step) {
        trg<-xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]'),collapse = ""),xmlGetAttr,"trg")[[1]]
        if(trg == "TRUE") {
          # Prendi i trigger attivati
          fired.trigger<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]/fired.trigger'),collapse = ""),xmlGetAttr,"name"))
          array.fired.trigger<-c( array.fired.trigger , fired.trigger )

          final.states<-unlist(xpathApply(doc,paste(c('//xml/computation[@n="',i,'"]/step[@n="',s,'"]/st.ACTIVE.POST'),collapse = ""),xmlGetAttr,"name"))
        }
      }
      list.fired.trigger[[i]] <-array.fired.trigger
      list.final.states[[i]] <- final.states

      if(sum(final.states %in% arr.nodi.end)>0) { termination.END.states[[i]] <- TRUE }
      else { termination.END.states[[i]] <- FALSE }
    }

    if( csv.EventLog.inAddition == TRUE ) {
      for( nomePaziente in names(list.computation.matrix$stati.timeline)) {
        csv.EventLog <- rbind(csv.EventLog,
                              cbind(rep(nomePaziente,nrow(list.computation.matrix$stati.timeline[[nomePaziente]])),list.computation.matrix$stati.timeline[[nomePaziente]] ))
      }
      colnames(csv.EventLog)<-c("idPatient","event","event.status","eventDateTime","deltaTimeFromBegin")
    }

    link.results <- WF.struct$linked.PWF.obj.computation.results
    
    tmp <- lapply( names(list.computation.matrix$stati.timeline), function(ID){
      if( class(list.computation.matrix$stati.timeline[[ID]]) == "matrix") {
          colnames(list.computation.matrix$stati.timeline[[ID]]) <<- c("node","node.status","eventDateTime","deltaTimeFromBegin")  
      }
    })
    
    return(list(
      "list.fired.trigger"=list.fired.trigger,
      "list.final.states"=list.final.states,
      "termination.END.states"=termination.END.states,
      "list.computation.matrix"=list.computation.matrix,
      "link.results"=link.results,
      "csv.EventLog"=csv.EventLog
    ))
  }

  #===========================================================
  # playSingleSequence
  # esegue il conformanche checking con una specifica sequenza
  # di LOG (di un paziente)
  #===========================================================
  playSingleSequence<-function( matriceSequenza , col.eventName, col.dateName, IDPaz,
                                event.interpretation="soft" , date.format="%d/%m/%Y %H:%M:%S", UM="days",
                                store.computation.matrix = FALSE,
                                debug = FALSE) {
    # Cerca lo stato che viene triggerato dal BEGIN
    st.LAST<-""
    st.DONE<-c("")
    st.ACTIVE<-c("'BEGIN'")
    st.ACTIVE.time<-c()
    last.fired.trigger<-c()
    ct <- 0; riga <- 0
    error<-""
    computation.result<-"normally terminated"
    history.hop<-list()
    sequenza <- as.array(matriceSequenza[ ,col.eventName ])
    stop.computation <- FALSE
    lista.stati.possibili <- names(WF.struct$info$stati)

    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]

    # popola l'array dei tempi di uptime degli stati (di attivazione)
    st.ACTIVE.time<-rep( 0 , length(lista.stati.possibili) )
    names(st.ACTIVE.time) <- lista.stati.possibili
    # e crea anche il cumulativo
    st.ACTIVE.time.cum <- st.ACTIVE.time

    # Analizza TUTTI gli eventi della sequenza
    for( indice.di.sequenza in seq(1,length(sequenza) )) {
      riga.completa.EventLog <- matriceSequenza[ indice.di.sequenza, ]

      # due variabili comode per dopo
      ev.NOW <- sequenza[indice.di.sequenza]
      indice.di.sequenza.ch <- as.character(indice.di.sequenza)
      history.hop[[indice.di.sequenza.ch]]<-list()
      fired.trigger.in.this.iteration <- FALSE

      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0
      
      # browser()
      # ORA scorri i giorni che passano fra l'evento precedente e quello in esame
      # Va fatto PRIMA di attivare il controllo sull'effetto dell'evento
      if( indice.di.sequenza > 1 & indice.di.sequenza < 1 ) {

        data.iniziale <- matriceSequenza[ ,col.dateName ][ indice.di.sequenza - 1 ]
        data.finale <- matriceSequenza[ ,col.dateName ][ indice.di.sequenza ]

        data.attuale <- data.iniziale
        fired.trigger.in.this.iteration <- TRUE

        # Cicla per tutti i giotni/ore/settimane dall'inizio alla fine
        # per vedere che non partano trigger legati a qualche DURATA
        while(  as.numeric(difftime(as.POSIXct(data.finale, format = date.format),
                                    as.POSIXct(data.attuale, format = date.format),units = 'mins')) > 0 ) {

          kkk.giorni <- format(as.POSIXct(data.attuale, format=date.format),format="%d")
          kkk.mesi <- format(as.POSIXct(data.attuale, format=date.format),format="%m")
          kkk.anni <- format(as.POSIXct(data.attuale, format=date.format),format="%Y")
          kkk.ore <- format(as.POSIXct(data.attuale, format=date.format),format="%H")
          kkk.minuti <- format(as.POSIXct(data.attuale, format=date.format),format="%M")
          kkk.secondi <- format(as.POSIXct(data.attuale, format=date.format),format="%S")
          
          seq(ISOdate(kkk.anni,kkk.mesi,kkk.giorni,kkk.ore,kkk.minuti,kkk.secondi), by = "month", length.out = 4)
          
          format(seq(ISOdate(2000,1,31,09,08,07), by = "week", length.out = 2),format="%Y-%m-%d %H:%M:%S")[2]

          if(UM=="mins")
            data.attuale <- format(seq(ISOdate(kkk.anni,kkk.mesi,kkk.giorni,kkk.ore,kkk.minuti,kkk.secondi), by = "min", length.out = 2),format=date.format)[2]
          if(UM=="days")
            data.attuale <- format(seq(ISOdate(kkk.anni,kkk.mesi,kkk.giorni,kkk.ore,kkk.minuti,kkk.secondi), by = "day", length.out = 2),format=date.format)[2]
          if(UM=="hours")
            data.attuale <- format(seq(ISOdate(kkk.anni,kkk.mesi,kkk.giorni,kkk.ore,kkk.minuti,kkk.secondi), by = "hour", length.out = 2),format=date.format)[2]
          if(UM=="weeks")
            data.attuale <- format(seq(ISOdate(kkk.anni,kkk.mesi,kkk.giorni,kkk.ore,kkk.minuti,kkk.secondi), by = "week", length.out = 2),format=date.format)[2]
          
          # if(UM=="mins")
          #   data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + min(1),format = date.format)
          # if(UM=="days")
          #   data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + days(1),format = date.format)
          # if(UM=="hours")
          #   data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + hours(1),format = date.format)
          # if(UM=="weeks")
          #   data.attuale <- format(as.POSIXct(data.attuale,format=date.format) + weeks(1),format = date.format)

            newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = '', st.DONE = st.DONE,
                                      st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                      st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                      EOF = FALSE , UM = UM, debug = debug,
                                      riga.completa.EventLog = c())
            # Se c'e' un errore, ferma tutto
            if(newHop$error==TRUE) {
              note.set.error(error = newHop$error)
              note.flush()
              return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW   ) );
            }
            # Se hai rilevato dei trigger attivi
            if(length(newHop$active.trigger)!=0) {
              ct <- ct + 1

              # gestisci il log
              newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
              note.setStep(number = ct)
              note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
              note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
              note.setEvent(eventType = '', eventDate = as.character(data.attuale) )
              note.flush()

              # Aggiorna le variabili
              st.ACTIVE <- newHop$st.ACTIVE
              last.fired.trigger<-newHop$active.trigger
              fired.trigger.in.this.iteration <- TRUE

              # Azzera il tempo di eventuali stati che sono stati resettati
              stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
              stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
              st.ACTIVE.time [ stati.da.resettare ] <- 0

            }
            # Aggiungi il delta data a quelli da aggiornare
            st.ACTIVE.time.cum [ stati.da.uppgradare ] <- st.ACTIVE.time.cum [ stati.da.uppgradare ] + 1
            st.ACTIVE.time [ stati.da.uppgradare ] <- st.ACTIVE.time [ stati.da.uppgradare ] + 1

            # Azzera il tempo di eventuali stati che sono stati resettati
            # si intende il "tempo" da cui sono ATTIVI (per valutare nel prossimo giro, se le durate superano
            # una certa threshold)
            stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
            stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
            st.ACTIVE.time [ stati.da.resettare ] <- 0
        }
      }
      # browser()
      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0

      if(param.verbose == TRUE) cat(str_c("\n\t processing:",ev.NOW))

      # costruisco un contatore della riga della tabella in analisi
      riga <- riga + 1
      data.ev.NOW <- matriceSequenza[ riga ,col.dateName ]
      ct <- ct + 1
      # gestisci il log
      newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
      note.setStep(number = ct)
      if("pMineR.internal.ID.Evt" %in% colnames(matriceSequenza))
      {
        note.setEvent(eventType = ev.NOW, eventDate =  data.ev.NOW , pMineR.internal.ID.Evt = matriceSequenza[riga,"pMineR.internal.ID.Evt"])
      }
      else  {
        note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW )
      }
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      # browser()
      
      if( debug == TRUE) cat("\n\t: ev.NOW=",ev.NOW)
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = ev.NOW, st.DONE = st.DONE,
                                st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                EOF = FALSE  , UM = UM, debug = debug,
                                riga.completa.EventLog = riga.completa.EventLog )
      # browser()
      history.hop[[indice.di.sequenza.ch]]$active.trigger<-newHop$active.trigger
      history.hop[[indice.di.sequenza.ch]]$ev.NOW<-ev.NOW
      history.hop[[indice.di.sequenza.ch]]$st.ACTIVE<-newHop$st.ACTIVE
      # Se c'e' un errore, ferma tutto
      if(newHop$error==TRUE) {
        note.set.error(error = newHop$error)
        note.flush()
        return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW   ) );
      }

      # Se hai rilevato dei trigger attivi
      if(length(newHop$active.trigger)!=0) {
        note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)

        st.ACTIVE <- newHop$st.ACTIVE

        last.fired.trigger<-newHop$active.trigger
        fired.trigger.in.this.iteration <- TRUE
      } else {
        # altrimenti segnala che NON ci sono trigger attivi
        note.set.fired.trigger(array.fired.trigger = '')
        # E i nuovi stati validi sono esattamente i vecchi
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = st.ACTIVE)
      }
      # Fai il flush
      note.flush()

      # Azzera il tempo di eventuali stati che sono stati resettati
      stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
      stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
      st.ACTIVE.time [ stati.da.resettare ] <- 0

      # Ora ripeti la ricerca dei trigger senza passare alcun evento, giusto per
      # vedere i trigger che si possono eventualmente attivare a seguito di
      # pregressi triggers.
      devo.restare.in.trigger.loop<-TRUE
      list.to.avoid.inifinte.loop <- list()
      index.inf.loop <- 1
      sono.in.un.loop.infinito <- FALSE
      # Continua a loopare fino a che e' vero che qualche trigger e' scattato
      while(  devo.restare.in.trigger.loop == TRUE & sum(arr.nodi.end %in% st.ACTIVE) == 0 ) {

        # Azzera il tempo di eventuali stati che sono stati resettati
        stati.da.uppgradare <- str_replace_all(st.ACTIVE [ !(st.ACTIVE %in% c("'BEGIN'","'END")) ],"'", "")
        stati.da.resettare <- lista.stati.possibili[ !(lista.stati.possibili %in% stati.da.uppgradare) ]
        st.ACTIVE.time [ stati.da.resettare ] <- 0

        # browser()
        if( debug == TRUE) cat("\n\t: ev.NOW=''")        
        # Verifica se c'e' un trigger
        newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = "", st.DONE = st.DONE,
                                  st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                  st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                  EOF = FALSE  , UM = UM, debug = debug,
                                  riga.completa.EventLog = c())
        # browser()
        # inzializza il log in caso di errore o in caso di trigger
        if(newHop$error==TRUE | length(newHop$active.trigger)!=0) {
          ct <- ct + 1
          newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
          note.setStep(number = ct)
          note.setEvent(eventType = '', eventDate = data.ev.NOW, pMineR.internal.ID.Evt = '')
          note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
        }
        # Se c'e' un errore, ferma tutto
        if(newHop$error==TRUE) {
          note.set.error(error = newHop$error)
          note.flush()
          return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger"=last.fired.trigger, "date" = data.ev.NOW  ) )
        }
        # Se hai rilevato qualche trigger attivo
        if(length(newHop$active.trigger)!=0)  {
          # verifica che la lista dei nodi attivi ed trigger non siano gia' avvenuto Se no, rischio un loop infinito
          # Se non e' null, significa che e' gia' scattato in passato
          if(!is.null(list.to.avoid.inifinte.loop[[ last.fired.trigger ]] )) {
              for(tmptmptmp in names(list.to.avoid.inifinte.loop[[ last.fired.trigger ]])) {
                # se la lunghezza e' uguale
                if(length(list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[tmptmptmp]]) == length(newHop$st.ACTIVE)) {
                  # e se sono TUTTI uguali
                  if(sum(newHop$st.ACTIVE %in% list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[tmptmptmp]]) == length(newHop$st.ACTIVE)) {
                    sono.in.un.loop.infinito <- TRUE
                  }
                }
              }
          }
          if( sono.in.un.loop.infinito == FALSE ) {
            note.setEvent(eventType = '', eventDate = data.ev.NOW, pMineR.internal.ID.Evt = '' )
            note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
            note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
            note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
            st.ACTIVE <- newHop$st.ACTIVE
            last.fired.trigger<-newHop$active.trigger
            # e fai il flush
            note.flush()

            # aggiorna la lista che contiene i trigger eseguiti e le condizioni si stato cosi' da poterla poi confrontare
            # in futuro. Se non lo facessi, rischiei un loop infinito
            if( is.null(list.to.avoid.inifinte.loop[[ last.fired.trigger ]] )) list.to.avoid.inifinte.loop[[ last.fired.trigger ]]<-list()
            list.to.avoid.inifinte.loop[[ last.fired.trigger ]][[as.character(length(list.to.avoid.inifinte.loop[[ last.fired.trigger ]])+1) ]] <-  st.ACTIVE
          }
          else devo.restare.in.trigger.loop<-FALSE
        }
        # altrimenti (se non ci sono stati trigger, vedi di uscire dal loop)
        else devo.restare.in.trigger.loop<-FALSE
      }
      # Se i vincoli di interpretazione degli event log sono "hard" allora non posso accettare
      # di passare ad un altro evento, se un evento non ha scatenato trigger!
      if(event.interpretation == "hard" & fired.trigger.in.this.iteration == FALSE)  {
        computation.result <- "event not predicted in hard checking"
        stop.computation <- TRUE
        break;
      }
      # Se ho beccato uno stato END, esci
      if(sum(arr.nodi.end %in% st.ACTIVE) != 0) {
        break;
      }
    }
# browser()
    if( debug == TRUE) cat("\n\t: ev.NOW=EOF")     
    # Se la computazione non e', per qualche motivo, interrotta
    if( stop.computation == FALSE  & sum(arr.nodi.end %in% st.ACTIVE) == 0) {
      # Now process the EOF !!
      ct <- ct + 1
      # gestisci il log
      newNote(store.computation.matrix = store.computation.matrix,idPatient = IDPaz);
      note.setStep(number = ct)
      note.setEvent(eventType = ev.NOW, eventDate = data.ev.NOW , pMineR.internal.ID.Evt = 'EOF')
      note.set.st.ACTIVE.PRE(array.st.ACTIVE.PRE = st.ACTIVE)
      # Cerca chi ha soddisfatto le precondizioni
      newHop <- attiva.trigger( st.LAST = st.LAST, ev.NOW = '', st.DONE = st.DONE,
                                st.ACTIVE = st.ACTIVE, st.ACTIVE.time = st.ACTIVE.time,
                                st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                EOF = TRUE, debug = debug,
                                riga.completa.EventLog = c())

      # Se c'e' un errore, ferma tutto
      if(newHop$error==TRUE) {
        note.set.error(error = newHop$error)
        note.flush()
        return( list( "st.ACTIVE"=st.ACTIVE,"error"=error,"last.fired.trigger" = last.fired.trigger , "date" = data.ev.NOW ) );
      }

      # Se hai rilevato dei trigger attivi
      if(length(newHop$active.trigger)!=0) {
        note.set.fired.trigger(array.fired.trigger = newHop$active.trigger)
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = newHop$st.ACTIVE)
        st.ACTIVE <- newHop$st.ACTIVE
        last.fired.trigger<-newHop$active.trigger
      } else {
        # altrimenti segnala che NON ci sono trigger attivi
        note.set.fired.trigger(array.fired.trigger = '')
        # E i nuovi stati validi sono esattamente i vecchi
        note.set.st.ACTIVE.POST(array.st.ACTIVE.POST = st.ACTIVE)
      }
      # Fai il flush
      note.flush()

      # Se ho uno stato di 'END' attivo, chiudi la computazione (fai giusto l'EOF)
      if( sum(arr.nodi.end %in% st.ACTIVE) != 0 ) {
        stop.computation <- TRUE
      }
    }

    # Ritorna
    return( list( "st.ACTIVE"=st.ACTIVE,
                  "error"=error,
                  "last.fired.trigger" = last.fired.trigger,
                  "date" = data.ev.NOW,
                  "history.hop" = history.hop,
                  "computation.result" = computation.result) );
  }
  #===========================================================
  # attiva.trigger
  # is the "core": it finds out the trigger that should be fired
  # INPUT:
  #   st.LAST - l'array degli stati attivati nel tempo (almeno una volta)
  #   ev.NOW  - l'evento letto ora.
  #   st.ACTIVE - l'array degli stati attivi prima di attivare l'evento attuale
  #   st.ACTIVE.time - il tempo di 'uptime' degli stati (prima di un 'unset', che rimette a zero)
  #   st.ACTIVE.time.cum - il tempo di 'uptime' degli stati (cumulativo)
  #   EOF - 'TRUE' indica che la computazione e' finita
  #===========================================================
  attiva.trigger<-function( st.LAST, ev.NOW, st.DONE, st.ACTIVE, st.ACTIVE.time, st.ACTIVE.time.cum, 
                            EOF , UM="days",riga.completa.EventLog = c(), debug = FALSE ) {
    # inizializza
    new.st.DONE<-st.DONE;
    new.st.LAST<-c()
    new.st.ACTIVE<-st.ACTIVE
    active.trigger<-c()
    global.array.to.set<-c(); global.array.to.unset<-c();
    errore <- FALSE;   errMsg<-"";
    tabella.set.unset <- c()
    # debug <- TRUE

    # Crea le stringhe per la cache  ------------------------------------------
    string.st.NOW <- str_trim(ev.NOW)
    EOF <- as.character(EOF)
    string.st.ACTIVE <- paste(  sort(unique(st.ACTIVE)) , collapse = '_')
    stringone.stato <- paste( c(string.st.NOW,EOF,string.st.ACTIVE), collapse = '_'  )
    # fine creazione stringhe per la cache ------------------------------------

    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct[[ "info" ]][[ "trigger" ]]) ) {
      
      if( debug == TRUE) cat("\n\t\t: trigger=",trigger.name)  
      # if( trigger.name == "Localization high?" & ev.NOW=="Site_localization" ) browser() 
      # Prendi la condizione
      precondizione <- WF.struct[["info"]][["trigger"]][[trigger.name]]$condition
      stringa.to.eval<-precondizione
      
      if(debug == TRUE) cat("\n\t\t: condition: ",stringa.to.eval)

      # Agisci solo nel caso in cui una CONDITION sia stata definita,
      # per quel trigger (alcuni trigger potrebbero NON avere una CONDITION)
      if(!is.na(precondizione)) {

        # Verifica che in cache non ci sia gia' il caso in questione!
        trigger.gia.calcolato <- FALSE
        if( param.use.global.comp.cache == TRUE & (trigger.name %in% names(global.comp.cache))  ) {
          if(stringone.stato %in% names(global.comp.cache[[trigger.name]])) {
            risultato <- global.comp.cache[[trigger.name]][[stringone.stato]][["fired"]]
            trigger.gia.calcolato <- TRUE
          }
        }

        if( trigger.gia.calcolato == FALSE ) {
          # Se sono qui significa che il tutto NON e' in cache
          # Inizia a costruire la stringa da parsare, rimpiazzando gli array
          rimpiazzo.ev.NOW<-paste( c("'",ev.NOW,"'") ,collapse='');
          rimpiazzo.st.ACTIVE<- paste(c("c(",paste(c(st.ACTIVE),collapse=","),")" ),collapse='')

          stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$ev.NOW\\$",replacement = rimpiazzo.ev.NOW)
          stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$st.ACTIVE\\$",replacement = rimpiazzo.st.ACTIVE)
          stringa.to.eval <- str_replace_all(string = stringa.to.eval,pattern = "\\$EOF\\$",replacement = str_c("'",EOF,"'") )

          stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " OR ",replacement = " | ")
          stringa.to.eval<- str_replace_all(string = stringa.to.eval,pattern = " AND ",replacement = " & ")

          # Fai il parse sugli attributi
          # browser();
          stringa.to.eval <- parse.for.attribute.conditions(
            stringa = stringa.to.eval,
            st.ACTIVE.time = st.ACTIVE.time,
            st.ACTIVE.time.cum = st.ACTIVE.time.cum,
            riga.completa.EventLog = riga.completa.EventLog)

          # Fai il parse sui DELTA T
          # -im
          # stringa.to.eval <- parse.for.temporal.conditions(
          #                           stringa = stringa.to.eval,
          #                           st.ACTIVE.time = st.ACTIVE.time,
          #                           st.ACTIVE.time.cum = st.ACTIVE.time.cum,
          #                           UM = UM)
          # -fm

          # Parsa la stringa
          if(stringa.to.eval=="") risultato <- TRUE
          else risultato <- eval(expr = parse(text = stringa.to.eval))
          
          if(debug == TRUE) cat("\n\t\t: condition: ",stringa.to.eval)
          if(debug == TRUE) cat("\n\t\t: risultato: ",risultato)          
          
        }

        # Se previsto, aggiorna la cache! ---------------------------------------------
        if( param.use.global.comp.cache == TRUE ) {
          # se non c'e' l'elemento di lista, crealo
          if( !(trigger.name %in% names(global.comp.cache))  ) {
            global.comp.cache[[trigger.name]] <-  list()
          }
          global.comp.cache[[trigger.name]][[stringone.stato]][["fired"]] <- risultato
        }
        # Fine aggiornamneto cache ----------------------------------------------------


        # Se la condizione e' soddisfatta, aggiorna le variabili
        if(is.na(risultato)) risultato <- FALSE
        if( risultato == TRUE ) {
          # prendi la priorita'
          pri <- WF.struct[["info"]][["trigger"]][[trigger.name]]$pri

          array.to.set<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$set))
          array.to.unset<-unlist((WF.struct[["info"]][["trigger"]][[trigger.name]]$unset))

          # Se devi fare un unsetAll, provvedi, prendendo per esplicito la lista degli stati di cui fare l'unset
          if(   WF.struct[["info"]][["trigger"]][[trigger.name]]$unsetAll == TRUE) {
            aaa <- names(WF.struct$info$stati)[ !(names(WF.struct$info$stati) %in% str_replace_all(string = array.to.set,pattern = "'",""))   ]
            array.to.unset <- unique(c(array.to.unset, aaa))
            for(i in seq(1,length(array.to.unset))) {
              array.to.unset[ i ] <- paste(c("'",array.to.unset[ i ],"'"),collapse = '' )
            }
          }

          # aggiungi le righe alla matrice che definisce le azioni
          for(i in seq(1,length(array.to.set))) {
            if(!is.null(array.to.set)) {
              tabella.set.unset <- rbind( tabella.set.unset, c( trigger.name,array.to.set[i],"set",pri  )   )
            }
          }
          for(i in seq(1,length(array.to.unset))) {
            if(!is.null(array.to.unset)) {
              tabella.set.unset <- rbind( tabella.set.unset, c( trigger.name,array.to.unset[i],"unset",pri  )   )
            }
          }
        }

      }
    }
    # if(debug == TRUE) {
    #   cat(tabella.set.unset)
    # }
# browser()
    # Se esista la tabella, verifica i conflitti di set/unset
    if(!is.null(tabella.set.unset)) {

      res <- check.conflitti.set.unset( tabella = tabella.set.unset )

      # Ora non dimenticare eventuali stati gia' presenti!
      new.st.ACTIVE <- c(res$to.set,st.ACTIVE)
      new.st.ACTIVE <- unique(new.st.ACTIVE[!(new.st.ACTIVE %in% res$to.unset)])
      active.trigger <- res$triggers.name
      new.st.LAST <- res$to.set
      new.st.DONE <- c(st.DONE,res$to.set)
      errMsg <- res$errorMsg
      errore <- res$errore
    }

    if(debug == TRUE) cat("\n--------------------------------------------")
    if(debug == TRUE) cat("\n\t\t: nuovi stati ACTIVE: ",new.st.ACTIVE)
    if(debug == TRUE) cat("\n\t\t: nuovi stati LAST: ",new.st.LAST)
    if(debug == TRUE) cat("\n\t\t: nuovi stati DONE: ",new.st.DONE) 
    if(debug == TRUE) cat("\n\t\t: trigger attivati: ",active.trigger) 
    if(debug == TRUE) cat("\n--------------------------------------------")
    
    # Ritorna i nuovi stati e la lista dei trigger attivati
    return(list(
      "st.ACTIVE"=new.st.ACTIVE,      # lista nuovi stati ACTIVE
      "st.LAST"=new.st.LAST,          # lista nuovi stati LAST
      "st.DONE"=new.st.DONE,          # lista nuovi stati DONE
      "active.trigger"=active.trigger,# lista dei trigger attivati per il LOG passato
      "error" = errore,
      "errorMsg" = errMsg
    ))
  }
  #===========================================================
  # check.conflitti.set.unset
  # verifica che nella tabella set/unset non ci siano conflitti
  #===========================================================
  check.conflitti.set.unset<- function( tabella ) {

    global.array.to.set <- c()
    global.array.to.unset <- c()

    # metti dei nomi decenti alle colonne
    colnames(tabella)<-c("trigger","stato","action","pri")
    # prendi la lista delle priorita' censite
    arr.priorita.censite <- unique(tabella[,"pri"])
    errMsg <- ""; errore<-FALSE;

    # Per ogni livello di priorita'
    for( i in sort(arr.priorita.censite)) {

      # Verifica che a questo livello di priorita' non ci siano contraddizioni
      local.array.to.set <- unique( tabella[which( tabella[,"action"]=="set" & tabella[,"pri"]==as.character(i)),"stato"]   )
      local.array.to.unset <- unique( tabella[which( tabella[,"action"]=="unset" & tabella[,"pri"]==as.character(i) ),"stato"]   )

      local.conflitto <- sum(local.array.to.set %in% local.array.to.unset)

      # se ci sono conflitti
      if(local.conflitto>0) {
        conflicting.Triggers <- tabella[which( tabella[,"pri"]==as.character(i) ),"trigger"]
        errMsg <- c("Set e unset conflicting. Check the following triggers: ",paste( conflicting.Triggers,collapse=','  ));
        print(tabella)
        # browser()
        errore = TRUE;
        obj.LogHandler$sendLog(msg = errMsg, type="NMI")
      }
      # calcola gli array set/uset da proporre ai livelli di priorita' piu' alti
      # Togli eventuali set/unset di livello inferiore
      global.array.to.set <- global.array.to.set[ !(global.array.to.set %in% local.array.to.unset)  ]
      global.array.to.unset <- global.array.to.unset[ !(global.array.to.unset %in% local.array.to.set)  ]
      # Accoda i nuovi contributi di set/unset
      global.array.to.set <- unique(c(global.array.to.set,local.array.to.set))
      global.array.to.unset <- unique(c(global.array.to.unset,local.array.to.unset))
    }
    triggers.name <- unique(tabella[,"trigger"])
    return(list(
      "to.set" = global.array.to.set,
      "to.unset" = global.array.to.unset,
      "triggers.name" = triggers.name,
      "errMsg" = errMsg,
      "errore" = errore
    ))
  }
  #===========================================================
  # parse.for.attribute.conditions
  # esegue il parse per eventuali vincoli sugli attributi
  #===========================================================
  parse.for.attribute.conditions<- function(stringa , st.ACTIVE.time , st.ACTIVE.time.cum, riga.completa.EventLog) {
    a <- 1

    stringaToMatch <- "\\$ev\\.NOW::attr\\(['\"][0-9a-zA-Z_ ,\\.\\?]+['\"]\\)"
    stringa.run <- stringa
    # cat("\n ==> ",stringa);
    # if( stringa =="'cT3 (MRF-) N0 M0 Rectal cancer' %in% c('cT3 (MRF-) N0 M0 Rectal cancer') & 'Site_localization'=='Site_localization' & $ev.NOW::attr('Site_localization')$=='high'") browser()
# browser()
    matrice.match <- str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]
    while( length(matrice.match) > 0  ) {
      # Lavora sempre e solo sulla prima riga
      # (tanto poi la matrice si riduce di dimensioni all'iterazione successiva)
      riga <- 1
      # scorri per tutte le occorrenze del match
      pos.par <- str_locate(string = stringaToMatch, pattern = "\\(")[1]-2
      attributeName <- str_sub(string = stringa.run,start = matrice.match[riga, "start"]+pos.par,end = matrice.match[riga, "end"]-2)
      
      if( is.factor(riga.completa.EventLog[,attributeName] ) ) {
        valore.da.sostituire <- levels(riga.completa.EventLog[,attributeName])[which(levels(riga.completa.EventLog[,attributeName])==riga.completa.EventLog[,attributeName]) ]
      }
      else  {
        valore.da.sostituire <- riga.completa.EventLog[,attributeName]  
      }

      stringa.pre <- str_sub(string = stringa.run, start = 1, end = matrice.match[riga, "start"]-1)
      stringa.post <- str_sub(string = stringa.run, start  = matrice.match[riga, "end"]+2,end = str_length(string = stringa.run))

      nuova.stringa <- paste(c(stringa.pre," '",valore.da.sostituire,"' ",stringa.post),collapse = '')
      stringa.run <- nuova.stringa

      matrice.match <- str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]
    }

    return(stringa.run)
  }
  #===========================================================
  # parse.for.temporal.conditions
  # esegue il parse per eventuali 'condition' con aspetti temporali. Li metto su una
  # funzione a parte per facilitare la visibilita' della funzione chiamante (ben
  # piu' importante)
  #===========================================================
  parse.for.temporal.conditions <- function(stringa , st.ACTIVE.time , st.ACTIVE.time.cum , UM = "days") {

    stringa.run <- stringa

    lista.comandi.condition <- global.lista.comandi.condition
    lista.comandi.condition <-  lista.comandi.condition[ global.arr.comandi.rilevati ]

    # Passa il contenuto di st.ACTIVE.time e st.ACTIVE.time.cum in minuti

    if( UM == "mins")  moltiplicatore <- 1
    if( UM == "hours") moltiplicatore <- 60
    if( UM == "days")  moltiplicatore <- 60 * 24
    if( UM == "weeks") moltiplicatore <- 60 * 24 * 7

    st.ACTIVE.time <- st.ACTIVE.time * moltiplicatore
    st.ACTIVE.time.cum <- st.ACTIVE.time.cum * moltiplicatore

    for( comandoToCheck in names(lista.comandi.condition) ) {

      # poni un default per l'esito
      esito = FALSE

      stringaToMatch <- lista.comandi.condition[[ comandoToCheck ]]
      pos.par <- str_locate(string = stringaToMatch, pattern = "\\(")[1]-1

      matrice.match <- str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]
      while( length(matrice.match) > 0  ) {
        riga <- 1

        # poni un default per l'esito
        esito = FALSE

        # estrai la quantita' fra le parentesi
        quantita <- str_sub(string = stringa.run,start = matrice.match[riga, "start"]+pos.par,end = matrice.match[riga, "end"]-1)
        quantita <- as.numeric(quantita)

        # estrai il nome dello stato
        nomeStato <- str_sub(string = stringa.run,end = matrice.match[riga, "start"]-1)
        subMatrix.nomeStato <- str_locate_all(string = nomeStato, pattern = "'" )[[1]]
        nomeStato <- str_sub(string = nomeStato,
                             start = subMatrix.nomeStato[ nrow(subMatrix.nomeStato)-1, "start"]+1,
                             end = subMatrix.nomeStato[ nrow(subMatrix.nomeStato), "start"]-1)
        # Ora fai due riflessioni...
        if(comandoToCheck == "afmtm") { esito = st.ACTIVE.time[ nomeStato ] > (quantita) }
        if(comandoToCheck == "afmetm") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita) }
        if(comandoToCheck == "afmth") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 ) }
        if(comandoToCheck == "afmeth") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 ) }
        if(comandoToCheck == "afmtd") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 * 24) }
        if(comandoToCheck == "afmetd") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 * 24) }
        if(comandoToCheck == "afmtw") { esito = st.ACTIVE.time[ nomeStato ] > (quantita * 60 * 24 * 7) }
        if(comandoToCheck == "afmetw") { esito = st.ACTIVE.time[ nomeStato ] >= (quantita * 60 * 24 * 7) }

        if(comandoToCheck == "afltm") { esito = st.ACTIVE.time[ nomeStato ] < (quantita) }
        if(comandoToCheck == "afletm") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita) }
        if(comandoToCheck == "aflth") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 ) }
        if(comandoToCheck == "afleth") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 ) }
        if(comandoToCheck == "afltd") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 * 24) }
        if(comandoToCheck == "afletd") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 * 24) }
        if(comandoToCheck == "afltw") { esito = st.ACTIVE.time[ nomeStato ] < (quantita * 60 * 24 * 7) }
        if(comandoToCheck == "afletw") { esito = st.ACTIVE.time[ nomeStato ] <= (quantita * 60 * 24 * 7) }

        stringa.run <- paste(c( str_sub(stringa.run,end = subMatrix.nomeStato[ nrow(subMatrix.nomeStato)-1, "start"]-1), "'",esito,"'",str_sub(stringa.run,start = matrice.match[riga, "end"] +1) ), collapse='')

        matrice.match <- str_locate_all(string = stringa.run, pattern = stringaToMatch )[[1]]
      }
    }

    stringa.new <- stringa.run


    return(stringa.new);
  }
  #===========================================================
  # get.XML.replay.result (ex getXML)
  # it returns the XML file
  #===========================================================
  get.XML.replay.result<-function(notebook.name='computationLog', writeToFile = FALSE, fileName="./output.xml"){

    # if the output is not on file
    if( writeToFile == FALSE )    return(notebook[[notebook.name]])

    # otherwise, write a file
    fileConn<-file(fileName)
    writeLines(notebook[[notebook.name]], fileConn)
    close(fileConn)
  }
  #===========================================================
  # getPatientLog
  # it returns the LOG of a single patient
  #===========================================================
  getPatientLog<-function( patientID ){
    return(dataLog$wordSequence.raw[[patientID]] )
  }
  #===========================================================
  # getPatientXML
  # it returns the XML of a single patient
  #===========================================================
  getPatientXML<-function( patientID ){
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    valore<- xpathApply(doc,paste(c('//xml/computation[@IDPaz="',patientID,'"]'),collapse = ""))[[1]]
    return(valore)
  }
  #===========================================================
  # plot (ex plotGraph)
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  #===========================================================
  plot<-function( giveBack.grVizScript = FALSE, plotIt = TRUE, old=T ) {
    arr.st.plotIt<-c("'BEGIN'");  arr.nodi.end<-c()
    arr.stati.raggiungibili<-c();
    arr.trigger.rappresentabili<-c();
    stringa.nodo.from<-c()
    stringa.nodo.to<-c()
    stringa.nodo.from.hidden <- c()
    stringa.nodo.to.hidden <- c()
    # Costruisci subito la lista dei nodi plottabili (cosi' non ci penso piu')
    # Faccio anche la lista dei nodi END
    for(nomeStato in names(WF.struct$info$stati)) {
      if( WF.struct$info$stati[[nomeStato]]$plotIt == TRUE) {
        arr.st.plotIt<-c(arr.st.plotIt,str_c("'",nomeStato,"'"))
      }
    }
    # prendo i nodi end
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
    
    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct$info$trigger) ) {
      # if( trigger.name == 'T13a') browser()
      
      # Se il trigger e' plottabile
      if(WF.struct$info$trigger[[trigger.name]]$plotIt == TRUE) {
        
        stringa.nodo.from<-str_c( stringa.nodo.from,"\n" )
        stringa.nodo.to<-str_c( stringa.nodo.to,"\n" )
        # Prendi i nodi 'unset' (from)
        arr.nodi.from<-unlist(WF.struct$info$trigger[[trigger.name]]$unset)
        # Prendi i nodi 'set' (to)
        arr.nodi.to<-unlist(WF.struct$info$trigger[[trigger.name]]$set)
        # Considera solo i nodi plottabili (from e to)
        arr.nodi.from <- arr.nodi.from [arr.nodi.from %in% arr.st.plotIt]
        arr.nodi.to <- arr.nodi.to [arr.nodi.to %in% arr.st.plotIt]
        arr.nodi.from.hidden <- unlist(WF.struct$info$trigger[[trigger.name]]$hunset)
        arr.nodi.to.hidden<-unlist(WF.struct$info$trigger[[trigger.name]]$hset)
        # if( trigger.name == "T13a") browser()
        if(length(arr.nodi.to)>0 | length(arr.nodi.to.hidden)>0 | length(arr.nodi.from.hidden)>0   ) {
          # Aggiorna l'array degli stati raggiungibili (in generale)
          # e l'array con i nomi dei trigger rappresentabili
          arr.stati.raggiungibili <- unique(c( arr.stati.raggiungibili, arr.nodi.to, arr.nodi.from ))
          arr.trigger.rappresentabili <- c( arr.trigger.rappresentabili, str_c("'",trigger.name,"'") )
          
          if(length(arr.nodi.to)>0) {
            # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
            for( st.nome in arr.nodi.from ) {
              stringa.nodo.from<-str_c( stringa.nodo.from," ",st.nome,"->'",trigger.name,"'" )
            }
            for( st.nome in arr.nodi.to ) {
              stringa.nodo.to<-str_c( stringa.nodo.to," ","'",trigger.name,"'->",st.nome )
            }
          }
          if(length(arr.nodi.to.hidden)>0) {
            # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
            for( st.nome in arr.nodi.to.hidden ) {
              stringa.nodo.to.hidden<-str_c( stringa.nodo.to.hidden," ","'",trigger.name,"'->",st.nome )
            }
          }
          if(length(arr.nodi.from.hidden)>0) {
            # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
            for( st.nome in arr.nodi.from.hidden ) {
              stringa.nodo.from.hidden<-str_c( stringa.nodo.from.hidden," ",st.nome,"->'",trigger.name,"'" )
            }
          }
        }
      }
    }

    # Distingui fra nodi end e nodi nnormali (questione di colore)
    arr.terminazioni.raggiungibili <- arr.nodi.end[arr.nodi.end %in% arr.stati.raggiungibili]
    arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    
    clean.arr.terminazioni.raggiungibili <- str_replace_all(string = arr.terminazioni.raggiungibili,pattern = "'","")
    clean.arr.stati.raggiungibili <- str_replace_all(string = arr.stati.raggiungibili,pattern = "'","")
    
    # Cambia i colori, se necessario
    nuova.stringa.nodi<-""
    for(i in clean.arr.terminazioni.raggiungibili) {
      colore <- str_trim(WF.struct$info$stati[[i]]$col)
      if(colore=="") colore <- "red"
      nuova.stringa.nodi <- str_c(nuova.stringa.nodi,"\n node [fillcolor = ",colore,"] '",i,"' ")
    }
    
    for(i in clean.arr.stati.raggiungibili) {
      if(i!="BEGIN"){ 
        colore <- str_trim(WF.struct$info$stati[[i]]$col)
        if(colore=="") colore <- "orange"
        if(str_sub(string = colore,start = 1,end = 1)=="#") colore<- str_c("'",colore,"'")
        nuova.stringa.nodi <- str_c(nuova.stringa.nodi,"\n node [fillcolor = ",colore,"] '",i,"' ")
      }
    }    
    # browser()
    a<-paste(c("digraph boxes_and_circles {

               # a 'graph' statement
               graph [overlap = true, fontsize = 10]
               
               # several 'node' statements
               node [shape = oval,
               fontname = Helvetica,
               style = filled]
               node [fillcolor = green]
               'BEGIN';
               ",nuova.stringa.nodi,"
               
               node [fillcolor = white, shape = box ]
               ",paste(arr.trigger.rappresentabili,collapse=" "),"
               
               edge [arrowsize = 1 ]
               # several edge
               ",stringa.nodo.from,"
               ",stringa.nodo.to,"

               edge [arrowsize = 1, style=dashed ]
               # several edge
               ",stringa.nodo.from.hidden,"
               ",stringa.nodo.to.hidden,"

  }"), collapse='')
    

    if( plotIt == TRUE ) grViz(a);
    if( giveBack.grVizScript == TRUE ) return(a)
  }
  #===========================================================
  # plotPatientEventTimeLine
  # plot the event timeline for a ginven patient
  #===========================================================
  plotPatientEventTimeLine<-function( patientID , cex.axis = 0.6, cex.text = 0.7) {

    st.POST<-list(); st.PRE<-list(); tr.fired<-list()
    txt.section<-"";

    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.step <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"evt"))
    arr.date <- unlist(xpathApply(doc,str_c('//xml/computation[@IDPaz="',patientID,'"]/step'),xmlGetAttr,"date"))

    matrice<-c()
    for( riga in seq(1,length(arr.step))) {
      matrice <- rbind(matrice, cbind(  arr.date[ riga ] , arr.step[ riga ]  ) )
    }

    plotTimeline(matrice, dataLog$csv.date.format, cex.axis = cex.axis, cex.text = cex.text)
  }

  #===========================================================
  # plotPatientReplayedTimeline (ex plotPatientComputedTimeline)
  # plot the computed timeline for a given patient
  #===========================================================
  plotPatientReplayedTimeline<-function( patientID,
                                             text.cex=.7, y.intra.gap = 40, x.offset = 100,
                                             thickness=5 ,
                                             bar.border = "Navy",bar.volume = "lightsteelblue1",
                                             text.date.cex =.6) {
    plotPatientReplayedTimelineFunction(list.computation.matrix = get.list.replay.result(), patientID = patientID)

  }
  #===========================================================
  # plot.replay.result  ( ex plotComputationResult)
  # plot the Graph
  # 'clear' is the graph as passed
  # 'computed' is the graph weighted by real computation flows
  # 'avoidFinalStates' is an array containing the final states
  #                to filter the computation
  # kindOfNumber : i numeri: 'relative' o 'absolute'
  #===========================================================
  plot.replay.result<-function( whatToCount='activations' ,     kindOfNumber='relative',
                                   avoidFinalStates=c(), avoidTransitionOnStates=c(), avoidToFireTrigger=c(),
                                whichPatientID=c("*"), plot.unfired.Triggers = FALSE,
                                giveBack.grVizScript = FALSE
                                ) {

    arr.st.plotIt<-c("'BEGIN'");  arr.nodi.end<-c()
    arr.stati.raggiungibili<-c();
    arr.trigger.rappresentabili<-c();
    stringa.nodo.from<-c()
    stringa.nodo.to<-c()
    stringa.nodo.from.hidden <- c()
    stringa.nodo.to.hidden <- c()
    howMany<-list()
# browser()
    debug <- TRUE

    matrice.nodi.from<-c();    matrice.nodi.to<-c()
    # Costruisci subito la lista dei nodi plottabili (cosi' non ci penso piu')
    # Faccio anche la lista dei nodi END
    for(nomeStato in names(WF.struct$info$stati)) {
      if( WF.struct$info$stati[[nomeStato]]$plotIt == TRUE) {
        arr.st.plotIt<-c(arr.st.plotIt,str_c("'",nomeStato,"'"))
      }
    }
    # prendo i nodi end
    arr.nodi.end <- WF.struct[[ "info" ]][[ "arr.nodi.end" ]]
    # Frulla per ogni possibile trigger, verificando se si puo' attivare
    for( trigger.name in names(WF.struct$info$trigger) ) {
      # Se il trigger e' plottabile
      if(WF.struct$info$trigger[[trigger.name]]$plotIt == TRUE) {

        stringa.nodo.from<-str_c( stringa.nodo.from,"\n" )
        stringa.nodo.to<-str_c( stringa.nodo.to,"\n" )
        # Prendi i nodi 'unset' (from)
        arr.nodi.from<-unlist(WF.struct$info$trigger[[trigger.name]]$unset)
        # Prendi i nodi 'set' (to)
        arr.nodi.to<-unlist(WF.struct$info$trigger[[trigger.name]]$set)
        # Considera solo i nodi plottabili (from e to)
        arr.nodi.from <- arr.nodi.from [arr.nodi.from %in% arr.st.plotIt]
        arr.nodi.to <- arr.nodi.to [arr.nodi.to %in% arr.st.plotIt]
        arr.nodi.from.hidden <- unlist(WF.struct$info$trigger[[trigger.name]]$hunset)
        arr.nodi.to.hidden<-unlist(WF.struct$info$trigger[[trigger.name]]$hset)        
        
        if(length(arr.nodi.to)>0 | length(arr.nodi.to.hidden)>0 | length(arr.nodi.from.hidden)>0   ) {
          # Aggiorna l'array degli stati raggiungibili (in generale)
          # e l'array con i nomi dei trigger rappresentabili
          arr.stati.raggiungibili <- unique(c( arr.stati.raggiungibili, arr.nodi.to, arr.nodi.from ))
          arr.trigger.rappresentabili <- c( arr.trigger.rappresentabili, str_c("'",trigger.name,"'") )

          # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
          if(length(arr.nodi.to)>0) {
            for( st.nome in arr.nodi.from ) {
              stringa.nodo.from<-str_c( stringa.nodo.from," ",st.nome,"->'",trigger.name,"'" )
              matrice.nodi.from <- rbind(matrice.nodi.from,c(st.nome,trigger.name))
            }
            for( st.nome in arr.nodi.to ) {
              stringa.nodo.to<-str_c( stringa.nodo.to," ","'",trigger.name,"'->",st.nome )
              matrice.nodi.to <- rbind(matrice.nodi.to,c(trigger.name,st.nome))
            }
          }
          if(length(arr.nodi.to.hidden)>0) {
            # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
            for( st.nome in arr.nodi.to.hidden ) {
              stringa.nodo.to.hidden<-str_c( stringa.nodo.to.hidden," ","'",trigger.name,"'->",st.nome )
            }
          }
          if(length(arr.nodi.from.hidden)>0) {
            # Costruisci le stringhe dei nomi degli archi (from e to) con in mezzo il trigger
            for( st.nome in arr.nodi.from.hidden ) {
              stringa.nodo.from.hidden<-str_c( stringa.nodo.from.hidden," ",st.nome,"->'",trigger.name,"'" )
            }
          }          
        }
      }
    }

    # Distingui fra nodi end e nodi normali (questione di colore)
    arr.terminazioni.raggiungibili <- arr.nodi.end[arr.nodi.end %in% arr.stati.raggiungibili]
    arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    # arr.stati.raggiungibili<- arr.stati.raggiungibili[!(arr.stati.raggiungibili %in% arr.nodi.end)]
    

    # -im 
    # Cambia i colori, se necessario
    
    clean.arr.terminazioni.raggiungibili <- str_replace_all(string = arr.terminazioni.raggiungibili,pattern = "'","")
    clean.arr.stati.raggiungibili <- str_replace_all(string = arr.stati.raggiungibili,pattern = "'","")
    
    nuova.stringa.nodi<-""
    for(i in clean.arr.terminazioni.raggiungibili) {
      colore <- str_trim(WF.struct$info$stati[[i]]$col)
      if(colore=="") colore <- "red"
      nuova.stringa.nodi <- str_c(nuova.stringa.nodi,"\n node [fillcolor = ",colore,"] '",i,"' ")
    }
    
    for(i in clean.arr.stati.raggiungibili) {
      if(i!="BEGIN"){ 
        colore <- str_trim(WF.struct$info$stati[[i]]$col)
        if(colore=="") colore <- "orange"
        if(str_sub(string = colore,start = 1,end = 1)=="#") colore<- str_c("'",colore,"'")
        nuova.stringa.nodi <- str_c(nuova.stringa.nodi,"\n node [fillcolor = ",colore,"] '",i,"' ")
      }
    } 
    # -fm
    
    # Ora sistema le froceries grafiche
    # PER I NODI
    stringa.stati<-"node [fillcolor = Orange]"
    stringa.stati.finali <- "node [fillcolor = Red]";
    if(debug == TRUE) bb = new.giveBackComputationCounts( whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID)
    # browser()
    for(nome.stato in arr.stati.raggiungibili)  {
      nome.stato.pulito <- str_replace_all(string = nome.stato,pattern = "'",replacement = "")
      if(debug == FALSE) aa = giveBackComputationCounts(nomeElemento = nome.stato.pulito, tipo='stato', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID)
      if(debug == TRUE) { aa<-list();     aa$howMany <- bb$states.howMany[nome.stato.pulito]; aa$totalNumber <- bb$totalNumber; aa$array.patID <- bb$array.patID; }
      if(debug == TRUE & nome.stato.pulito=="BEGIN") aa$howMany<-aa$totalNumber
      # browser()
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
      penwidth<- 1
      colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
      if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
      else numberToPrint<-str_c("# ",aa$howMany)
      # Se non e' uno stato finale

      if(!(nome.stato %in% arr.nodi.end)) {

        stringa.stati <- str_c(stringa.stati,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"',  pencolor='Gray",colore,"']")
      }
      else {
        stringa.stati.finali <- str_c(stringa.stati.finali,"\n\t ",nome.stato," [label = '",nome.stato.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"',  pencolor='Gray",colore,"']")
      }
    }
    # PER I TRIGGER
    lista.freq.trigger<-list()
    stringa.trigger<-"node [fillcolor = white, shape = box ]"
    for(nome.trigger in arr.trigger.rappresentabili)  {
      nome.trigger.pulito <- str_replace_all(string = nome.trigger,pattern = "'",replacement = "")
      if(debug == FALSE) aa = giveBackComputationCounts(nomeElemento = nome.trigger.pulito, tipo='trigger', whatToCount = whatToCount, avoidFinalStates = avoidFinalStates, avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger, whichPatientID = whichPatientID )
      if(debug == TRUE) { aa<-list();     aa$howMany <- bb$trigger.howMany[nome.trigger.pulito]; aa$totalNumber <- bb$totalNumber; aa$array.patID <- bb$array.patID; }
      howMany <- as.character((aa$howMany * 100 / aa$totalNumber))
      if( plot.unfired.Triggers == TRUE | (plot.unfired.Triggers==FALSE & howMany>0)) {
        lista.freq.trigger[[nome.trigger]]<-(aa$howMany  / aa$totalNumber)
        penwidth<- 1 + 5 * (aa$howMany  / aa$totalNumber)
        penwidth<- 1
        colore <- as.integer(100-(30+(aa$howMany  / aa$totalNumber)*70))
        if(kindOfNumber=='relative') numberToPrint<-str_c(round(as.numeric(howMany),2)," %")
        else numberToPrint<-str_c("# ",aa$howMany)
        stringa.trigger <- str_c(stringa.trigger,"\n\t ",nome.trigger," [label = '",nome.trigger.pulito,"\n",numberToPrint,"', penwidth='",penwidth,"', fontcolor='Gray",colore,"']")
      }
    }
    # STRINGA NODO FROM (ARCO)
    stringa.nodo.from<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.from))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.from[i,2],"'") ]]
      arrowsize<- .5 + 7 * val.perc
      colore <- as.integer(100-(30+val.perc*70))
      labelArco <- str_c(round(val.perc*100,2),"%")
      labelArco<-''
      nuovaRiga<-str_c("\n\t",matrice.nodi.from[i,1],"->'",matrice.nodi.from[i,2],"' [label = '",labelArco,"', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"'  ]")
      stringa.nodo.from<-c(stringa.nodo.from,nuovaRiga)
    }

    # STRINGA NODO TO (ARCO)
    stringa.nodo.to<-"\nedge [arrowsize = 1 ]"
    for(i in seq(1,nrow(matrice.nodi.to))) {
      val.perc<-lista.freq.trigger[[ str_c("'",matrice.nodi.to[i,1],"'") ]]
      arrowsize<- .5 + 7 * val.perc
      colore <- as.integer(100-(30+val.perc*70))
      labelArco <- str_c(round(val.perc*100,2),"%")
      labelArco<-''
      nuovaRiga<-str_c("\n\t'",matrice.nodi.to[i,1],"'->",matrice.nodi.to[i,2]," [label = '",labelArco,"', penwidth='",arrowsize,"', fontcolor='Gray",colore,"', pencolor='Gray",colore,"' ]")
      stringa.nodo.to<-c(stringa.nodo.to,nuovaRiga)
    }
    # browser()
    # nuova.stringa.nodi
    a<-paste(c("digraph boxes_and_circles {

               # a 'graph' statement
               graph [overlap = true, fontsize = 10]

               # several 'node' statements
               node [shape = oval,
               fontname = Helvetica,
               style = filled]
               node [fillcolor = green]
               'BEGIN';
               ",nuova.stringa.nodi,"

               ",stringa.stati,"
               ",stringa.trigger,"

               edge [arrowsize = 1 ]
               # several edge
               ",stringa.nodo.from,"
               ",stringa.nodo.to,"

               edge [arrowsize = 1, style=dashed ]
               # several edge
               ",stringa.nodo.from.hidden,"
               ",stringa.nodo.to.hidden,"

  }"), collapse='')
    b<-paste(c("digraph boxes_and_circles {

               # a 'graph' statement
               graph [overlap = true, fontsize = 10]

               # several 'node' statements
               node [shape = oval,
               fontname = Helvetica,
               style = filled]
               node [fillcolor = green]
               'BEGIN';
               ",stringa.stati.finali,"

               ",stringa.stati,"
               ",stringa.trigger,"

               edge [arrowsize = 1 ]
               # several edge
               ",stringa.nodo.from,"
               ",stringa.nodo.to,"

               edge [arrowsize = 1, style=dashed ]
               # several edge
               ",stringa.nodo.from.hidden,"
               ",stringa.nodo.to.hidden,"

  }"), collapse='')    
    if(giveBack.grVizScript == TRUE) return (a)
    grViz(a);
  }
  #===========================================================
  # giveBackComputationCounts
  # fa la conta di quanto quello 'stato' o 'trigger' e' stato
  # toccato nella computazione.
  # perOgni indica se la conta deve avvenire per ogni istanza
  # o per al massimo una per ogni paziente
  # INPUT
  # tipo = 'stato' o 'trigger' in funzione di cosa sia
  # whatToCount = 'activations'  se si vogliono contare le attivazioni,
  #               'terminations' se si vogliono contare quante volte vi e' terminato
  # avoidFinalStates = un array che contiene gli stati che non devono essere
  #                    finali per i pazienti da considerare (serve per fare un filtro)
  # avoidTransitionOnStates = un array che indica gli stati che non devono essre
  #                           transitati dai pazienti da considerare (e' un filtro)
  # avoidToFireTrigger = badali', indovina un po'?
  #===========================================================
  new.giveBackComputationCounts<-function( whatToCount, avoidFinalStates, avoidTransitionOnStates, avoidToFireTrigger , whichPatientID) {

    if( !(whatToCount %in% c("activations","terminations"))) stop("\n ERR: dfj9dfds0jf90d")

    # aa = giveBackComputationCounts(nomeElemento = nome.stato.pulito, tipo='stato',
    # whatToCount = whatToCount, avoidFinalStates = avoidFinalStates,
    # avoidTransitionOnStates = avoidTransitionOnStates, avoidToFireTrigger = avoidToFireTrigger,
    # whichPatientID = whichPatientID)

    # Prendi la matrice con i risultati della computazione
    matrice <- get.list.replay.result()
    m.stati.transizione <- matrice$list.computation.matrix$stati.transizione
    m.trigger <- matrice$list.computation.matrix$trigger
    m.stati.finali <- matrice$list.computation.matrix$stati.finali

    # Riduci la dimensione della matrice ai soli pazienti indicati
    if( !("*" %in% whichPatientID) & length(whichPatientID)>0 ){
      m.stati.transizione <- m.stati.transizione[ which(rownames(m.stati.transizione) %in% whichPatientID),  ]
      m.trigger <- m.trigger[ which(rownames(m.trigger) %in% whichPatientID),  ]
      m.stati.finali <- m.stati.finali[ which(rownames(m.stati.finali) %in% whichPatientID),  ]
    }

    arr.Pazienti.ulteriori.da.rimuovere <- c()

    # Riduci ulteriormente la dimensione della matrice nel caso si
    # debbano evitare delle attivazioni di trigger
    if(length(avoidToFireTrigger)>1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere,names(which(  !(rowSums(m.trigger[  , avoidToFireTrigger])>0)    )))
    }
    if(length(avoidToFireTrigger)==1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere,names(which(  !(m.trigger[  , avoidToFireTrigger]>0)   ))  )
    }

    # Riduci ulteriormente la dimensione della matrice nel caso si
    # debbano evitare delle attivazioni di stati
    if(length(avoidTransitionOnStates)>1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere, names(which(  !(rowSums(m.stati.transizione[  , avoidTransitionOnStates])>0)   )))
    }
    if(length(avoidTransitionOnStates)==1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere, names(which(  !(m.stati.transizione[  , avoidTransitionOnStates]>0)   )) )
    }

    # Riduci ulteriormente la dimensione della matrice nel caso si
    # debbano evitare dei pazienti che terminano in uno o piu' determinati stati
    if(length(avoidFinalStates)>1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere, names(which(  !(rowSums(m.stati.finali[  , avoidFinalStates])>0)  ))   )
    }
    if(length(avoidFinalStates)==1) {
      arr.Pazienti.ulteriori.da.rimuovere <- c(arr.Pazienti.ulteriori.da.rimuovere, names(which(   !(m.stati.finali[  , avoidFinalStates]>0)   ))   )
    }

    # Riduci la dimensione della matrice ai soli pazienti indicati
    if( length(arr.Pazienti.ulteriori.da.rimuovere) > 0 ){
      m.stati.transizione <- m.stati.transizione[ which(rownames(m.stati.transizione) %in% arr.Pazienti.ulteriori.da.rimuovere),  ]
      m.trigger <- m.trigger[ which(rownames(m.trigger) %in% arr.Pazienti.ulteriori.da.rimuovere),  ]
      m.stati.finali <- m.stati.finali[ which(rownames(m.stati.finali) %in% arr.Pazienti.ulteriori.da.rimuovere),  ]
    }
# browser()
    # Se e' stato chiesto uno stato
      if(whatToCount =="activations") {
        # Trasforma la matrice in soli uni (per non contare piu' volte uno stesso paziente)
        m.stati.transizione[  which(m.stati.transizione>0,arr.ind = T) ] <- 1
        
        if(is.matrix(m.stati.transizione) == TRUE) { somma <- colSums(m.stati.transizione) }
        else { somma <- m.stati.transizione }
        
        states.howMany <- somma
        array.patID<- rownames(m.stati.transizione)
      }
      if(whatToCount =="terminations") {
        # Trasforma la matrice in soli uni (per non contare piu' volte uno stesso paziente)
        m.stati.finali[  which(m.stati.finali>0,arr.ind = T) ] <- 1
        
        if( is.matrix(m.stati.finali) == TRUE ) { somma <- colSums(m.stati.finali) }
        else {  somma <- m.stati.finali }
        
        states.howMany <- somma
        array.patID<- rownames(m.stati.finali)
      }

      # Trasforma la matrice in soli uni (per non contare piu' volte uno stesso paziente)
      m.trigger[  which(m.trigger>0,arr.ind = T) ] <- 1
      if( is.matrix(m.trigger) == TRUE ) { somma <- colSums(m.trigger)  }
      else { somma <- m.trigger  }
      
      trigger.howMany <- somma
    return( list( "trigger.howMany"=trigger.howMany, "states.howMany"=states.howMany,  "totalNumber"=length(array.patID) ,"array.patID" = array.patID)  )
  }
  giveBackComputationCounts<-function( nomeElemento, tipo, whatToCount, avoidFinalStates, avoidTransitionOnStates, avoidToFireTrigger , whichPatientID) {
    # Carica l'XML
    doc <- xmlInternalTreeParse(file = notebook$computationLog,asText = TRUE)
    arr.Computazioni<- unlist(xpathApply(doc,'//xml/computation',xmlGetAttr,"n"))
    totalAmount <- 0
    totalNumber <- 0
    array.patID <- c()

    # Loopa per ogni computazione
    for(i in seq(1,length(arr.Computazioni))) {
      skipComputation <- FALSE

      # Differenzia il caso in cui si vogliano contare gli stati da quello in cui si vogliano
      # contare i trigger
      st.FINAL.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/finalState'),xmlGetAttr,"name"))
      st.TRANSITION.ON.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/st.ACTIVE.POST'),xmlGetAttr,"name"))
      tr.fired.arr <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/fired.trigger'),xmlGetAttr,"name"))
      PatientID <- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]'),xmlGetAttr,"IDPaz"))

      if( !("*" %in% whichPatientID) & !(PatientID %in% whichPatientID)){
        next;
      }

      if(tipo=="stato") {
        if(whatToCount=='activations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/st.ACTIVE.POST[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
        if(whatToCount=='terminations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/finalState[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      }
      if(tipo=="trigger") {
        if(whatToCount=='activations') howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/step/fired.trigger[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
        if(whatToCount=='terminations')  howMany<- unlist(xpathApply(doc,str_c('//xml/computation[@n="',i,'"]/atTheEnd/last.fired.trigger[@name="',nomeElemento,'"]'),xmlGetAttr,"name"))
      }

      # Verifica se il finale e' incluso in quelli da scartare: se si', skippa tutto il paziente
      # (la computazione corrente)
      if(sum(st.FINAL.arr %in% avoidFinalStates) >=1 |
         sum(st.TRANSITION.ON.arr %in% avoidTransitionOnStates) >=1 |
         sum(tr.fired.arr %in% avoidToFireTrigger) >=1 )  {
            skipComputation<-TRUE;
      }

      if(skipComputation == TRUE) next;

      # Se e' null, metti a zero
      if(length(howMany)==0) howMany<-0
      else howMany<-1;

      totalAmount <- totalAmount + howMany
      totalNumber <- totalNumber + 1
      array.patID <- c(array.patID,PatientID)
    }
    return( list( "howMany"=totalAmount,  "totalNumber"=totalNumber ,"array.patID" = array.patID)  )
  }
  #===========================================================
  # loadDataset
  #===========================================================
  loadDataset<-function( dataList ) {
    dataLog <<- dataList
  }
  #===========================================================
  # NOTEs
  # this is a BORING serie of functions used to build the XML
  # they are a lot, so I will not comment all of them.
  #===========================================================
  newNote<-function( number , store.computation.matrix = FALSE , idPatient = NA){
    tmpAttr<<-list()
    tmpAttr$stepNumber<<-''
    tmpAttr$boolean.fired.trigger<<-FALSE
    tmpAttr$event<<-""
    tmpAttr$event.date<<-""
    tmpAttr$store.computation.matrix <<- store.computation.matrix
    tmpAttr$idPatient <<- idPatient
  }
  note.setStep<-function( number ){
    tmpAttr$stepNumber <<- number
  }
  note.setEvent<-function( eventType , eventDate , pMineR.internal.ID.Evt=''){
    tmpAttr$event <<- eventType
    tmpAttr$event.date <<- eventDate
    tmpAttr$pMineR.internal.ID.Evt <<- pMineR.internal.ID.Evt
  }
  note.set.st.ACTIVE.PRE<-function( array.st.ACTIVE.PRE ){
    tmpAttr$st.ACTIVE.PRE <<- array.st.ACTIVE.PRE
  }
  note.set.st.ACTIVE.POST<-function( array.st.ACTIVE.POST ){
    tmpAttr$st.ACTIVE.POST <<- array.st.ACTIVE.POST
  }
  note.set.fired.trigger<-function( array.fired.trigger ){
    if(array.fired.trigger=="''" || array.fired.trigger=="") return()
    tmpAttr$boolean.fired.trigger <<- TRUE
    tmpAttr$fired.trigger <<- array.fired.trigger
  }
  note.set.error<-function( error ){
    tmpAttr$error <<- error
  }
  note.flush<-function( ){
    testo<-str_c("\n\t\t<step n='",tmpAttr$stepNumber,"' trg='",tmpAttr$boolean.fired.trigger,"' evt='",tmpAttr$event,"' date='",tmpAttr$event.date,"' pMineR.internal.ID.Evt='",tmpAttr$pMineR.internal.ID.Evt,"'>")
    if(tmpAttr$boolean.fired.trigger==TRUE)  {
      for(i in tmpAttr$st.ACTIVE.PRE) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.PRE name=",i,"></st.ACTIVE.PRE>")
      for(i in tmpAttr$fired.trigger) testo<-str_c(testo,"\n\t\t\t<fired.trigger name='",i,"'></fired.trigger>")
      for(i in tmpAttr$st.ACTIVE.POST) testo<-str_c(testo,"\n\t\t\t<st.ACTIVE.POST name=",i,"></st.ACTIVE.POST>")
    }
    testo<-str_c(testo,"\n\t\t</step>")
    addNote(msg = testo)

    # Vedi se devi aggiornare anche la lista delle matrici
    # con i log della computazione
    if( tmpAttr$store.computation.matrix == TRUE ) {
      tmp.stati.post <- str_replace_all(string = tmpAttr$st.ACTIVE.POST, pattern  = "'",replacement = "")
      tmp.stati.pre <- str_replace_all(string = tmpAttr$st.ACTIVE.PRE, pattern  = "'",replacement = "")
      tmp.stati.pre.da.abbassare <- tmp.stati.pre[ which( !(tmp.stati.pre %in% tmp.stati.post))   ]
      tmp.stati.post <- tmp.stati.post[!(tmp.stati.post %in% tmp.stati.pre)]
      list.computation.matrix$trigger[tmpAttr$idPatient, unique(tmpAttr$fired.trigger)]<<- list.computation.matrix$trigger[tmpAttr$idPatient,unique(tmpAttr$fired.trigger)] +1
      list.computation.matrix$stati.transizione[tmpAttr$idPatient, unique(tmp.stati.post)]<<- list.computation.matrix$stati.transizione[tmpAttr$idPatient,unique(tmp.stati.post)] +1

      if(length(list.computation.matrix$stati.timeline)==0) {
        list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]]<<-list()
        list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]]<<-c()
      }

      # Dichiara gli inizi di stato
      for(nome.stato in tmp.stati.post ) {
        # browser()
        if(tmpAttr$pMineR.internal.ID.Evt=="") tempo.dall.inizio = "NA"
        else tempo.dall.inizio <- dataLog$pat.process[[ tmpAttr$idPatient  ]][ which( dataLog$pat.process[[ tmpAttr$idPatient  ]][ ,"pMineR.internal.ID.Evt"]==tmpAttr$pMineR.internal.ID.Evt )  ,"pMineR.deltaDate"]

        if ( tmpAttr$pMineR.internal.ID.Evt =="EOF") tempo.dall.inizio <- max(dataLog$pat.process[[ tmpAttr$idPatient  ]][ , "pMineR.deltaDate"])
        valoreDifferenzaData.se.ignota<-as.numeric(difftime(as.POSIXct(tmpAttr$event.date, format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(dataLog$pat.process[[ tmpAttr$idPatient  ]][1, dataLog$csv.dateColumnName ], format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
        if(tempo.dall.inizio == "NA") tempo.dall.inizio <- valoreDifferenzaData.se.ignota

        if(length( list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] )==0) {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<-  c(nome.stato,"begin",tmpAttr$event.date, tempo.dall.inizio  )
        }
        else {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- rbind(
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]],
          c(nome.stato,"begin",tmpAttr$event.date, tempo.dall.inizio  ))
        }

      }
      # Dichiara la fine di stato
      for(nome.stato in tmp.stati.pre.da.abbassare  ) {
        # browser()
        if(tmpAttr$pMineR.internal.ID.Evt=="") tempo.dall.inizio = "NA"
        else tempo.dall.inizio <- dataLog$pat.process[[ tmpAttr$idPatient  ]][ which( dataLog$pat.process[[ tmpAttr$idPatient  ]][ ,"pMineR.internal.ID.Evt"]==tmpAttr$pMineR.internal.ID.Evt )  ,"pMineR.deltaDate"]

        if ( tmpAttr$pMineR.internal.ID.Evt =="EOF") tempo.dall.inizio <- max(dataLog$pat.process[[ tmpAttr$idPatient  ]][ , "pMineR.deltaDate"])
        valoreDifferenzaData.se.ignota<-as.numeric(difftime(as.POSIXct(tmpAttr$event.date, format = "%d/%m/%Y %H:%M:%S"),as.POSIXct(dataLog$pat.process[[ tmpAttr$idPatient  ]][1, dataLog$csv.dateColumnName ], format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))

        if(tempo.dall.inizio == "NA") tempo.dall.inizio <- valoreDifferenzaData.se.ignota
        
        if(length( list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] ) ==0) {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- c(nome.stato,"end",tmpAttr$event.date,tempo.dall.inizio)
        }
        else  {
          list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]] <<- rbind(
            list.computation.matrix$stati.timeline[[ tmpAttr$idPatient  ]],
            c(nome.stato,"end",tmpAttr$event.date,tempo.dall.inizio))
        }
      }
    }
  }
  addNote<-function( msg, level='1', notebook.name='computationLog' ) {
    # Crea la posizione, se ancora non c'e'
    if(length(notebook)==0) notebook[[notebook.name]]<<-c()
    else {  if( !(notebook.name %in% names(notebook))) notebook[[notebook.name]]<<-c()  }
    # Accoda la nota
    notebook[[notebook.name]]<<-str_c(notebook[[notebook.name]],msg)
    return;
  }
  #=================================================================================
  # play (ex play.easy)
  #   number.of.cases : numero di casi da generare
  #   min.num.of.valid.words : numero minimo di parole valide
  #   max.word.length : numero massimo di eventi per parola
  #=================================================================================
  play<-function(number.of.cases, min.num.of.valid.words=NA,
                      max.word.length=100, howToBuildBad="resample",
                      toReturn="csv", debug.mode = FALSE, output.format.date = "%d/%m/%Y %H:%M:%S",
                      typeOfRandomDataGenerator="dayAfterDay") {
    # Aggiorna il formato data dell'ultimp PLAY
    play.output.format.date <<- output.format.date

    if(is.na(min.num.of.valid.words)) min.num.of.valid.words = number.of.cases
    quante.da.sbagliare <- number.of.cases - min.num.of.valid.words

    # Se e' stato chiesto di generare anche delle sequenze NON VALIDE, genera delle
    # sequenze NON VALIDE (scusate il nome di sta fava 'play.easy.impreciso')
    if(min.num.of.valid.words>0) {
      a <- play.easy.impreciso(number.of.cases = min.num.of.valid.words,min.num.of.valid.words = min.num.of.valid.words,
                               max.word.length = max.word.length, howToBuildBad = howToBuildBad,
                               output.format.date = output.format.date , typeOfRandomDataGenerator=typeOfRandomDataGenerator)
    }
    if(quante.da.sbagliare>0) {
      totalizzati = 0
      while(totalizzati < quante.da.sbagliare) {
        b <- play.easy.impreciso(number.of.cases = 1,min.num.of.valid.words = 0,
                                 max.word.length = max.word.length, howToBuildBad = howToBuildBad,
                                 output.format.date = output.format.date, typeOfRandomDataGenerator=typeOfRandomDataGenerator)
        if(b$arr.matching.parola==FALSE) {
          if(min.num.of.valid.words==0 & totalizzati==0) {a <- b}
          else  { a <- join.giving.new.ID(a,b) }
          totalizzati <- totalizzati + 1
        }
      }
    }
    # Restituisci cio' che serve venga restituito
    # nel caso del CSV
    if(toReturn=="csv") {
      daRestituire <- a
    }
    # nel caso del dataLoader
    if(toReturn=="dataLoader") {
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader(verbose.mode = param.verbose)
      daRestituire$load.data.frame(mydata = a$valid.data.frame,
                                   IDName = "patID",EVENTName = "event",
                                   dateColumnName = "date" ,
                                   format.column.date = output.format.date)
    }
    if(toReturn!="csv" & toReturn!="dataLoader") stop("ERRORE: 'toReturn' non valorizzata correttamente")
    return(daRestituire)

  }
  join.giving.new.ID<-function( a , b ) {
    new.b <- as.character(max(as.numeric(names(a$lista.parole$list.nodes)))+1)
    attuale.b<- names(b$lista.parole$list.LOGs)

    a$lista.parole$list.LOGs[[new.b]] <- b$lista.parole$list.LOGs[[attuale.b]]
    a$lista.parole$list.nodes[[new.b]] <- b$lista.parole$list.nodes[[attuale.b]]
    a$arr.matching.parola <- c(a$arr.matching.parola,b$arr.matching.parola)
    b$valid.data.frame$patID <- rep(new.b,length(b$valid.data.frame$patID))
    a$valid.data.frame<-rbind(a$valid.data.frame,b$valid.data.frame)

    return(a)
  }
  play.easy.impreciso<-function(number.of.cases, min.num.of.valid.words=NA, max.word.length=100,
                                howToBuildBad="resample", output.format.date = "%d/%m/%Y %H:%M:%S",
                                date.format = "%d/%m/%Y %H:%M:%S", UM="days",
                                typeOfRandomDataGenerator="dayAfterDay") {
    obj.utils <- utils()
    if(is.na(min.num.of.valid.words)) min.num.of.valid.words = as.integer(number.of.cases/2)
    arr.matching.parola<-c()

    # Genera un buon numero di parole valide
    lista.res <- genera.parola.valida(number.of.cases = number.of.cases,
                                      max.word.length = max.word.length )

    # Ora prendine la meta' e fai uno shuffle
    quante.da.mescolare <- number.of.cases - min.num.of.valid.words
    aaa <- lista.res$list.LOGs
    if(quante.da.mescolare>0) {
      for(indice.parola in seq(1,quante.da.mescolare)) {
        if(param.verbose == TRUE)  cat("\n ",indice.parola)
          if(howToBuildBad=="resample") {
            lista.res$list.LOGs[[ indice.parola ]] <- sample(x = lista.res$list.LOGs[[ indice.parola ]] ,
                                                             size = length(lista.res$list.LOGs[[ indice.parola ]]))
          }
        if(howToBuildBad=="subtle") {

          if(length(lista.res$list.LOGs[[ indice.parola ]])>=5){
            dado <- as.integer(runif(n = 1,min = 2,max = length(lista.res$list.LOGs[[ indice.parola ]])-2))
            tmp.val<-lista.res$list.LOGs[[ indice.parola ]][ dado ]
            lista.res$list.LOGs[[ indice.parola ]][ dado ] <- lista.res$list.LOGs[[ indice.parola ]][ dado+1 ]
            lista.res$list.LOGs[[ indice.parola ]][ dado+1 ] <- tmp.val
          }
        }
      }
      # Ora devo controllare, di quelle che ho "shuffellato", quante sono ancora valide!
      for(indice.parola in seq(1,quante.da.mescolare)) {
        # Costruisci la matrice per consentire l'eseguibilita'

        marice.dati<-c()
        numeroGiorno<-1
        for(index.car in seq(1,length(lista.res$list.LOGs[[ indice.parola ]]))) {
          sing.car <- lista.res$list.LOGs[[ indice.parola ]][index.car]
          nuovaDatatmp <- as.Date("01/01/2000 12:00:00",format="%d/%m/%Y %H:%M:%S") + numeroGiorno
          nuovaDatatmp <- as.character(format( nuovaDatatmp, format = output.format.date ))
          nuovaRiga<-c(nuovaDatatmp,sing.car)

          numeroGiorno<-numeroGiorno+1

          marice.dati <- rbind(marice.dati,nuovaRiga)
        }
        colnames(marice.dati)<-c("data","evento")

        res <- playSingleSequence( matriceSequenza = marice.dati,
                                   col.eventName = "evento",
                                   col.dateName = "data" ,
                                   IDPaz = indice.parola,
                                   date.format = date.format, UM = UM )
        # scorri tutta la storia alla ricerca di qualche hop che non ha
        # scatenato un trigger. Se lo trovi, la parola e' sbagliata!
        parola.corretta <- TRUE
        for(indice.hops in names(res$history.hop)) {
          if(length(res$history.hop[[ indice.hops ]]$active.trigger)==0) parola.corretta<-FALSE
          # tuttavia se quanto analizzato ora e' relativo ad un nodo END, non proseguire oltre
          # (cio' che c'e' dopo, ipotizzo che non mi interessi)
          arr.nodi.attivati <- res$history.hop[[ indice.hops ]]$st.ACTIVE
          stop.search.END<-FALSE
          for( tmp.run in arr.nodi.attivati){
            tmp.run<-str_replace_all(string = tmp.run,pattern = "'",replacement = "")
            if(tmp.run!="BEGIN"){
              if(WF.struct$info$stati[[ tmp.run ]]$type=="END") stop.search.END<-TRUE
            }
          }
          if(stop.search.END==TRUE) break;
        }
        arr.matching.parola<-c(arr.matching.parola,parola.corretta)
      }
    }
    # dichiara certamente vere quelle iniziali, quelle non shuffellate
    arr.matching.parola<-c(arr.matching.parola,rep(TRUE,number.of.cases-quante.da.mescolare))
    valid.csv<-obj.utils$format.data.for.csv(listaProcessi = lista.res$list.LOGs,
                                             lista.validi = arr.matching.parola,
                                             typeOfRandomDataGenerator= typeOfRandomDataGenerator)
    valid.data.frame<-as.data.frame(valid.csv)

    # E mo' ritorna tutto il BOLO!
    return(list( "lista.parole"=lista.res,
                 "arr.matching.parola" = arr.matching.parola,
                 "valid.data.frame" = valid.data.frame
    ))
  }
  #=================================================================================
  # genera.parola.valida
  #=================================================================================
  genera.parola.valida<-function(number.of.cases, max.word.length=100, parola.valida = TRUE) {
    stringhe<- unlist(xpathApply(WF.xml,str_c('//xml/workflow/trigger/condition'),xmlValue  )  )
    arr.parole<-get.possible.words.in.WF.easy()
    # Inizializza gli array
    list.LOGs<-list()
    list.nodes<-list()

    # popola l'array dei tempi di uptime degli stati (di attivazione)
    lista.stati.possibili <- names(WF.struct$info$stati)
    st.ACTIVE.time<-rep( 0 , length(lista.stati.possibili) )
    names(st.ACTIVE.time) <- lista.stati.possibili
    # e crea anche il cumulativo
    st.ACTIVE.time.cum <- st.ACTIVE.time
    # browser()
    # Genera il numero desiderato di parole
    for( num.parola in seq(1,number.of.cases)) {
      # initialization
      st.LAST<-"";  st.DONE<-c(""); st.ACTIVE<-c("'BEGIN'")
      last.fired.trigger<-c();
      arr.low.level<-c()
      list.high.level<-list()
      terminate.run = FALSE
      # genera il numero di sequenze desiderate
      for( indice in seq(1, max.word.length)) {
        # costruisci l'array delle parole possibili, rimescolato
        # (per evitare starvation)
        arr.parole.sampled <- sample(x = arr.parole,size = length(arr.parole))

        # loopa su tutte la parole disponibili, cercando di uscirne
        # ( in realta' loop infiniti sono teoricamente possibili)
        trovato.qualcosa <- FALSE

        for( ev.NOW in arr.parole.sampled){
          newHop <- attiva.trigger( st.LAST = st.LAST,
                                    ev.NOW = ev.NOW,
                                    st.DONE = st.DONE,
                                    st.ACTIVE = st.ACTIVE,
                                    st.ACTIVE.time = st.ACTIVE.time,
                                    st.ACTIVE.time.cum = st.ACTIVE.time.cum,
                                    EOF = FALSE   )
          if( !is.null(newHop$active.trigger ) & newHop$error == FALSE) {
            trovato.qualcosa <- TRUE
            break;
          }
        }

        if(trovato.qualcosa==FALSE){
          # browser()
          stop("ERROR: non ci sono eventi che consentono di far evolvere la stringa")
        }

        # ora dovrei avere la nuova parola
        arr.low.level <- c(arr.low.level,ev.NOW)
        # cat("\n arr.low.level = ",arr.low.level)
        list.high.level[[ as.character(length(arr.low.level)) ]] <- newHop$st.ACTIVE
        st.ACTIVE <- newHop$st.ACTIVE
        st.LAST <- newHop$st.LAST
        st.DONE <- newHop$st.DONE
        # Se uno degli stati raggiunti e' di END, ferma la corsa
        # (la parola e' finita)
        for(nomeStato in st.ACTIVE) {
          nomeStato<-str_replace_all(string = nomeStato,pattern = "'",replacement = "")
          if(WF.struct$info$stati[[ nomeStato ]]$type=="END") terminate.run <- TRUE
        }

        if(length(arr.low.level)>max.word.length) terminate.run <- TRUE
        if(terminate.run==TRUE) break;
      }
      list.LOGs[[ as.character(num.parola) ]] <- arr.low.level
      list.nodes[[ as.character(num.parola) ]] <- list.high.level
    }

    # restituisci la stringa valida
    return(
      list("list.LOGs"   = list.LOGs,
           "list.nodes" = list.nodes)
    )
  }

  #=================================================================================
  # get.possible.words.in.WF.easy
  # Parsa l'XML dello PWF gia' in memoria per costruire un array dei possibili
  # $ev.NOW$ (gli eventi dell'event log).
  #=================================================================================
  get.possible.words.in.WF.easy<-function() {
    stringhe<- unlist(xpathApply(WF.xml,str_c('//xml/workflow/trigger/condition'),xmlValue  )  )
    arr.parole<-c()
    for(stringa in stringhe) {
      matched = str_locate_all(stringa,pattern = "(?<=ev.NOW.==').*?(?=')")
      # Se hai trovato almeno una occorrenza
      if(dim(matched[[1]])[1]>0){
        # allora estrai la sottostringa
        for(i in 1:dim(matched[[1]])[1]){
          tmp.1 <- str_sub(string = stringa,start = matched[[1]][i,1],end = matched[[1]][i,2])
          if(!is.na(tmp.1)) {
            # e popola l'array dei possibili eventi
            if( !(tmp.1 %in% arr.parole)) arr.parole<-c(arr.parole,tmp.1)
          }
        }
      }
    }
    return(arr.parole)
  }
  loadLinkedWorkFlow <- function( fileName, linkName, verbose.mode = TRUE)  {
    # popola l'attributo della classe
    if(length(WF.struct)==0) stop("\n ERRORE: prima bisogna caricare il main PWF")
    if(is.null(WF.struct[[ "info" ]])) stop("\nERRORE: prima bisogna caricare il main PWF")
    # Limitati a copiare il path: il caricamento avverra' successivamente per ogni
    # stato che lo invoca
    if(!file.exists(fileName)) stop("sembra che il file non esista.... prego, verifica")
    WF.struct$info$link[[linkName]]$fileName <<- fileName
    WF.struct$info$link[[linkName]]$verbose.mode <<- verbose.mode
  }

  stat.desc.grupedBarplot<-function( state1, state2, cov1, cov2, terminations = TRUE   ) {
    risultato <- get.list.replay.result()

    # Estrai la matrice in funzione del fatto che vogliamo vedere le terminazioni o le transizioni
    if(terminations == TRUE) { matrice <- ooo$list.computation.matrix$stati.finali }
    else { matrice <- ooo$list.computation.matrix$stati.transizione }

    # # Prendi gli ID dei pazienti per i due rispettivi stati (state1 e state2)
    # colnames(ooo$list.computation.matrix$stati.finali)

  }
  getInfo<-function(){
    states <-  names(WF.struct$info$stati)
    triggers <-  names(WF.struct$info$trigger)
    return(list(
      "states" = states,
      "triggers" = triggers
    ))
  }
  #=================================================================================
  # pre.parsing.confCheck_easy.file
  # Fai un pre-parsing per capire quale subset di espressioni regolari devi andare
  # ad indagare e .... con che UM temporale
  #=================================================================================
  pre.parsing.confCheck_easy.file<-function( fileName=NA , text=NA ) {

    # Leggi le righe dell'XML e vedi se trovi le occorrenze per l'analisi temporale: se si', cerca
    # di capire quali
    if(!is.na(fileName))
      oo <- readLines( fileName )
    if(!is.na(text))
      oo <- str_split(string = text,pattern = "\n")
    
    global.arr.comandi.rilevati <<- c()
    for(i in seq(1,length(oo))) {
      for( comandoToCheck in names(global.lista.comandi.condition) ) {
        matrice.match <- str_locate_all(string = oo[i], pattern = comandoToCheck )[[1]]
        if( length(matrice.match) > 0  ) global.arr.comandi.rilevati <<- c(global.arr.comandi.rilevati, comandoToCheck)
      }
    }

    # Cerca di intuire l'unita' di misura  da usare :)
    # partiamo dalle settimane
    suggested.UM <- 60 * 60 * 24 * 7
    if( "afmth" %in% global.arr.comandi.rilevati |
        "afmeth" %in% global.arr.comandi.rilevati |
        "aflth" %in% global.arr.comandi.rilevati |
        "afleth" %in% global.arr.comandi.rilevati
        ) suggested.UM <- min(suggested.UM,  60 )

    if( "afmtd" %in% global.arr.comandi.rilevati |
        "afmetd" %in% global.arr.comandi.rilevati |
        "afltd" %in% global.arr.comandi.rilevati |
        "afletd" %in% global.arr.comandi.rilevati
    ) suggested.UM <- min(suggested.UM, 60 * 24)

    if( "afmtw" %in% global.arr.comandi.rilevati |
        "afmetw" %in% global.arr.comandi.rilevati |
        "afltw" %in% global.arr.comandi.rilevati |
        "afletw" %in% global.arr.comandi.rilevati
    ) suggested.UM <- min(suggested.UM,  60 * 24 *7)

    if( "afmtm" %in% global.arr.comandi.rilevati |
        "afmetm" %in% global.arr.comandi.rilevati |
        "afltm" %in% global.arr.comandi.rilevati |
        "afletm" %in% global.arr.comandi.rilevati
    ) suggested.UM <- min(suggested.UM, 1)
    # browser()
    auto.detected.UM <<- "weeks"
    if(suggested.UM==1) auto.detected.UM <<- "mins"
    if(suggested.UM==60) auto.detected.UM <<- "hours"
    if(suggested.UM==60*24) auto.detected.UM <<- "days"
    if(suggested.UM==60*24 *7) auto.detected.UM <<- "weeks"

    global.arr.comandi.rilevati <<- unique(global.arr.comandi.rilevati)
  }
  set.param<-function(use.cache=NA){
    if(!is.na(use.cache)) param.use.global.comp.cache<<-use.cache
  }
  getClass<-function() {
    return(list(
      "class"="confCheck_easy",
      "obj.ID"=global.personal.ID
      ))
  }
  
  estrai.pazienti.da.percorso <- function( fromState, toState, passingThrough, passingNotThrough,
                                      stoppingAt, stoppingNotAt, withPatientID )  {

    # prendi i risultati dell'ultimo RUN
    res <- get.list.replay.result();
    # prendi gli ID
    mtr.thr <- res$list.computation.matrix$stati.transizione
    mtr.end <- res$list.computation.matrix$stati.finali
    PatID <- rownames(mtr.thr)
    
    # I pazienti da selezionare devono passare per il from e per il to
    # passingThrough <- c( fromState, toState,  passingThrough)
    
    # restringi PatID in caso di
    # passingThrough
    if( length(passingThrough) > 0 )  {
      for( stato in passingThrough ) {
        PatID <- intersect( PatID , rownames(mtr.thr[ which(mtr.thr[ , c(stato)]>0),  ]) )
      }
    }
    # passingNotThrough
    if( length(passingNotThrough) > 0 )  {
      for( stato in passingNotThrough ) {
        PatID <- intersect( PatID , rownames(mtr.thr[ which(mtr.thr[ , c(stato)]==0),  ])  )
      }    
    }
    # stoppingAt
    if( length(stoppingAt) > 0 )  {
      for( stato in stoppingAt ) {
        PatID <- intersect( PatID , rownames(mtr.end[ which(mtr.end[ , c(stato)]>0),  ])   )
      }
    }    
    # stoppingNotAt
    if( length(stoppingNotAt) > 0 )  {
      for( stato in stoppingNotAt ) {
        PatID <- intersect( PatID , rownames(mtr.end[ which(mtr.end[ , c(stato)]==0),  ])   )
      }
    }    
    # withPatientID
    if( length(withPatientID) > 0 ) {
      PatID <- withPatientID
    }

    return( list("PatID" = PatID ) )    
  }
  
  old.KaplanMeier <- function( fromState, toState, passingThrough=c(), passingNotThrough=c(), stoppingAt=c(), 
                               stoppingNotAt=c(), withPatientID=c()   )  { 

    # prendi i risultati dell'ultimo RUN
    res <- get.list.replay.result();
    
    PatID <- patientID
    
    # now, get the times
    tabellona <- c()
    for( ID in PatID ) {
      tmpHac <- res$list.computation.matrix$stati.timeline[[ID]]
      fromMin <- tmpHac[ which(tmpHac[,1]==fromState & tmpHac[,2]=="begin" )[1], 4 ]
      toMin <- tmpHac[ which(tmpHac[,1]==toState & tmpHac[,2]=="begin" )[1], 4 ]
      deltaMin <- as.numeric(toMin) - as.numeric(fromMin)
      tabellona <- rbind(tabellona,c(ID,deltaMin,1))
    }
    
    tabellona <- tabellona[  sort(as.numeric(tabellona[,2]),index.return = T)$ix, ]
    
    if(!is.matrix(tabellona)) return( list("table"=NA, "KM"=NA ) )
    
    colnames(tabellona) <- c("ID","time","outcome")
    
    aaa <- data.frame("ID"=tabellona[,1],"time"=as.numeric(tabellona[,2]),"outcome"=as.numeric(tabellona[,3]) )
    KM0 <- survfit(Surv(time, outcome)~1,   data=aaa)
    
    return( list("table"=aaa, "KM"=KM0, "ID"=tabellona[,1] ) )
    
  }
  Time.To.Flight <- function( fromState, toState, 
                              passingThrough=c(), passingNotThrough=c(), stoppingAt=c(), 
                              stoppingNotAt=c(), PDVAt=c(), withPatientID=c(), UM = "mins" ) {
    
    # prendi i risultati dell'ultimo RUN
    res <- get.list.replay.result();
    
    # estrai i pazienti che soddisfano i passaggi fra i nodi
    tmpPatID <- estrai.pazienti.da.percorso( fromState, toState, passingThrough, passingNotThrough,
                                             stoppingAt, stoppingNotAt, withPatientID )
    PatID <- tmpPatID$PatID
    
    # -im
    if (length(PatID)==0) {
      return( list( "TOF.table" = NA , "error" = -1 ))
    }
    # -fm
    
    # now, get the times
    tabellona <- c()
    # browser()
    ID.skippati <-c()
    for( ID in PatID ) {
      toProcess <- TRUE
      # -im (2)
      if( length(res$list.computation.matrix$stati.timeline[[ID]]) <= 4 )  {
        toProcess <- FALSE; ID.skippati <- c( ID.skippati , ID ) 
      }
      # -fm (2)
      # -im 
      # if(param.verbose == TRUE)  cat("\n ID=",ID)
      if( !(ID %in% names(res$list.computation.matrix$stati.timeline))) {
        toProcess <- FALSE; ID.skippati <- c( ID.skippati , ID ) 
      }
      
      if( toProcess == TRUE ) {
        tmpHac <- res$list.computation.matrix$stati.timeline[[ID]]
        fromMin <- tmpHac[ which(tmpHac[,1]==fromState & tmpHac[,2]=="begin" )[1], 4 ]
        toMin <- tmpHac[ which(tmpHac[,1]==toState & tmpHac[,2]=="begin" )[1], 4 ]
        PDVMin <- tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ] 
        # PDVMin_vect <- tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ] 
        # if (sum(is.na(PDVMin_vect))==length(PDVMin_vect)) {
        #   PDVMin <- NA
        # } else {
        #   PDVMin <- min(tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ], na.rm = T)
        # }
        # evento <- NA
        
        if (is.na(fromMin)) {
          evento <- -1
        } else { # !is.na(fromMin)
          if (is.na(toMin)) {
            if (is.na(PDVMin)) {
              evento <- -1
            } else {
              evento <- 0
            }
          } else { # !is.na(toMin)
            if (is.na(PDVMin)) {
              evento <- 1
            } else {
              if (PDVMin < toMin) {
                evento <- 0
              } else {
                evento <- 1
              }
            }
          }
        }
        
        if (evento == -1) deltaMin <- NA
        if (evento == 0) deltaMin <- as.numeric(PDVMin) - as.numeric(fromMin)
        if (evento == 1) deltaMin <- as.numeric(toMin) - as.numeric(fromMin)
        
        tabellona <- rbind(tabellona,c(ID,deltaMin,evento))
        
      }
      # tmpHac <- res$list.computation.matrix$stati.timeline[[ID]]
      # fromMin <- tmpHac[ which(tmpHac[,1]==fromState & tmpHac[,2]=="begin" )[1], 4 ]
      # toMin <- tmpHac[ which(tmpHac[,1]==toState & tmpHac[,2]=="begin" )[1], 4 ]
      # PDVMin <- tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ] 
      # # PDVMin_vect <- tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ] 
      # # if (sum(is.na(PDVMin_vect))==length(PDVMin_vect)) {
      # #   PDVMin <- NA
      # # } else {
      # #   PDVMin <- min(tmpHac[ which(tmpHac[,1] %in% PDVAt & tmpHac[,2]=="begin" )[1], 4 ], na.rm = T)
      # # }
      # # evento <- NA
      # 
      # if (is.na(fromMin)) {
      #   evento <- -1
      # } else { # !is.na(fromMin)
      #   if (is.na(toMin)) {
      #     if (is.na(PDVMin)) {
      #       evento <- -1
      #     } else {
      #       evento <- 0
      #     }
      #   } else { # !is.na(toMin)
      #     if (is.na(PDVMin)) {
      #       evento <- 1
      #     } else {
      #       if (PDVMin < toMin) {
      #         evento <- 0
      #       } else {
      #         evento <- 1
      #       }
      #     }
      #   }
      # }
      # 
      # if (evento == -1) deltaMin <- NA
      # if (evento == 0) deltaMin <- as.numeric(PDVMin) - as.numeric(fromMin)
      # if (evento == 1) deltaMin <- as.numeric(toMin) - as.numeric(fromMin)
      # 
      # tabellona <- rbind(tabellona,c(ID,deltaMin,evento))
      # tmpHac <- res$list.computation.matrix$stati.timeline[[ID]]
      # fromMin <- tmpHac[ which(tmpHac[,1]==fromState & tmpHac[,2]=="begin" )[1], 4 ]
      # toMin <- tmpHac[ which(tmpHac[,1]==toState & tmpHac[,2]=="begin" )[1], 4 ]
      # evento <- 1
      # if(is.na(toMin)) { 
      #   toMin <- max(as.numeric(res$list.computation.matrix$stati.timeline[[ID]][,4] ))
      #   evento <- 0
      # }
      # deltaMin <- as.numeric(toMin) - as.numeric(fromMin)
      # tabellona <- rbind(tabellona,c(ID,deltaMin,evento))
      # - fm
    }
    
    for( ID in ID.skippati ) { cat( "\n WARNING: the patient ",ID," has been skipped " )}
    
    # -im ET
    tabellona <- matrix(tabellona[which(tabellona[,3]!=-1),], ncol=3)
    tabellona <- matrix(tabellona[  sort(as.numeric(tabellona[,2]),index.return = T)$ix, ], ncol=3)
    # tabellona <- tabellona[  sort(as.numeric(tabellona[,2]),index.return = T)$ix, ]
    # if(!is.matrix(tabellona)) return( list("table"=NA, "KM"=NA ) )
    # -fm ET
    
    colnames(tabellona) <- c("ID","time","outcome")
    
    aaa <- data.frame("ID"=tabellona[,1],"time"=as.numeric(tabellona[,2]),"outcome"=as.numeric(tabellona[,3]) )
    
    if( UM == "mins")  moltiplicatore <- 1
    if( UM == "hours") moltiplicatore <- 60
    if( UM == "days")  moltiplicatore <- 60 * 24
    if( UM == "weeks") moltiplicatore <- 60 * 24 * 7
    aaa$time <- aaa$time / moltiplicatore
    
    return( list( "TOF.table" = aaa, "error" = 0 ) )    
  }  
    
  KaplanMeier <- function( fromState, toState, 
                           passingThrough=c(), passingNotThrough=c(), stoppingAt=c(), 
                           stoppingNotAt=c(), PDVAt=c(),  withPatientID=c(), UM = "mins" )  {
    
    TOF.list <- Time.To.Flight( fromState, toState, passingThrough, passingNotThrough, stoppingAt, 
             stoppingNotAt, PDVAt, withPatientID, UM = UM )
    # browser()
    if (TOF.list$error != 0) {
      return(list("table"=NA, "KM"=NA, "ID"=NA, "error"=TOF.list$error ))
    }
    
    aaa <- TOF.list$TOF.table
    # browser()
    
    KM0 <- survfit(Surv(time, outcome)~1,   data=aaa)
    
    # return( list("table"=aaa, "KM"=KM0, "ID"=tabellona[,1] ) )
    return( list("table"=aaa, "KM"=KM0, "ID"=aaa$ID, "error"=0 ) )
    
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
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function( verboseMode ) {
    WF.xml <<- c()
    WF.xml.fileName <<- c()
    dataLog <<- c()
    WF.struct <<- list()
    notebook <<- list()
    tmpAttr <<- list()
    param.verbose <<- verboseMode
    play.output.format.date <<- "%d/%m/%Y";
    obj.LogHandler <<- logHandler();
    list.computation.matrix <<-list();
    list.computation.matrix$stati.transizione <<- c()
    list.computation.matrix$trigger <<- c()
    list.computation.matrix$stati.finali <<- c()
    list.computation.matrix$stati.timeline <<- list()
    # array e lista dei comandi da parsare
    global.lista.comandi.condition <<- list(
      "afmth" = ".afmth\\([0-9]+\\)",
      "afmeth" = ".afmeth\\([0-9]+\\)",
      "afmtd" = ".afmtd\\([0-9]+\\)",
      "afmetd" = ".afmetd\\([0-9]+\\)",
      "afmtw" = ".afmtw\\([0-9]+\\)",
      "afmetw" = ".afmetw\\([0-9]+\\)",
      "afmtm" = ".afmtm\\([0-9]+\\)",
      "afmetm" = ".afmetm\\([0-9]+\\)",
      "aflth" = ".aflth\\([0-9]+\\)",
      "afleth" = ".afleth\\([0-9]+\\)",
      "afltd" = ".afltd\\([0-9]+\\)",
      "afletd" = ".afletd\\([0-9]+\\)",
      "afltw" = ".afltw\\([0-9]+\\)",
      "afletw" = ".afletw\\([0-9]+\\)",
      "afltm" = ".afltm\\([0-9]+\\)",
      "afletm" = ".afletm\\([0-9]+\\)"
    )
    global.arr.comandi.rilevati<<-names(global.lista.comandi.condition)
    # Unita' di misura temporale minima rilevata automaticamente
    auto.detected.UM<<-"mins"
    # Cache di calcolo
    param.use.global.comp.cache<<-FALSE
    global.comp.cache<<-list()
    global.personal.ID<<-paste( c(as.character(runif(1,1,100000)),as.character(runif(1,1,100000)),as.character(runif(1,1,100000))), collapse = '' )
  }
  costructor( verboseMode = verbose.mode);
  #=================================================================================
  return(list(
    "loadWorkFlow"=loadWorkFlow,
    "loadDataset"=loadDataset,
    "loadLinkedWorkFlow"=loadLinkedWorkFlow,
    "play"=play,  # rimpiazza la play.easy
    "replay"=replay, # rimpiazza la playLoadedData
    "plot"=plot, # rimpiazza la plotGraph
    "plot.replay.result"=plot.replay.result, # rimpiazza la plotComputationResult
    # "query.Patient"=query.Patient,
    "get.list.replay.result"=get.list.replay.result, # rimpiazza la getPlayedSequencesStat.00
    "get.XML.replay.result"=get.XML.replay.result, # rimpiazza la getXML
    "plotPatientReplayedTimeline" = plotPatientReplayedTimeline, # rimpiazza la plotPatientComputedTimeline
    "getPatientLog"=getPatientLog,
    "getPatientXML"=getPatientXML,
    "giveBackComputationCounts"=giveBackComputationCounts,
    "new.giveBackComputationCounts"=new.giveBackComputationCounts,
    "KaplanMeier"=KaplanMeier,
    "Time.To.Flight"=Time.To.Flight,
    "logRankTest"=LogRankTest,
    "set.param"=set.param,
    "getInfo"=getInfo,
    "getClass"=getClass
  ))
}
