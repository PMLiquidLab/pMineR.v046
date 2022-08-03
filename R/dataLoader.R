#' Load the event-logs
#' 
#' @description  A loader for csv based log files. It also calculates the footprint table, transition matrix probabilities, and presents data in different shapes. The public methods are:
#'              \itemize{
#'              \item \code{dataLoader() } the costructor
#'              \item \code{load.csv( ... ) } loads the csv file into the \code{dataLoader} object
#'              \item \code{load.data.frame() } loads a data.frame into the \code{dataLoader} object
#'              \item \code{getData() } return the processed, previously-loaded, data
#'              \item \code{addDictionary() } add a dictionary in order, afterward, to translate or group some event name
#'              } 
#'              You can use the cited methods directly on the object, (they are made available as element of the object list) or can be invocked using the 'wrapping function', with the prefix 'DL.'
#'              
#'              The consturctor admit the following parameters:
#' verbose.mode are some notification wished, during the computation? The defaul value is \code{true}
#' @param verbose.mode boolean. If TRUE some messages will appear in console, during the computation; otherwise the computation will be silent.
#' @param save.memory boolean. If TRUE, dataLoader() avoid to keep in memory the entire original csv.
#' @param max.char.length.label numeric. It defines the max length of the event name strings
#' @import progress
#' @importFrom data.table data.table 
#' @export
dataLoader<-function( verbose.mode = TRUE, max.char.length.label = 50, save.memory = FALSE ) {
  arrayAssociativo<-''
  footPrint<-''
  MMatrix<-''
  pat.process<-''   
  wordSequence.raw<-''
  MM.mean.time<-''
  MM.mean.outflow.time<-''
  MM.density.list<-''
  MM.den.list.high.det<-''
  list.dictionary<-''
  list.dict.column.event.name<-''
  input.format.date<-''
  max.pMineR.internal.ID.Evt <-''
  original.CSV <- ''
  param.IDName<-''
  param.EVENTName<-''
  param.dateColumnName<-''  
  param.verbose<-''
  param.max.char.length.label<-'';
  param.column.names<-''
  param.save.memory<-'';
  obj.LH<-''
  global.personal.ID<-NA
  #=================================================================================
  # clearAttributes
  # this method clear all the attributes in order to make the object re-useable
  # for other issues ( dirty variables could have dramatic effetcs! )
  #=================================================================================    
  clearAttributes<-function() {
    costructor( verboseMode = param.verbose , max.char.length.label = param.max.char.length.label,saveMemory = param.save.memory )
  }
  #=================================================================================
  # addDictionary
  #=================================================================================    
  addDictionary<-function( fileName, sep =',', dict.name='main' , column.event.name) {
    list.dictionary[[ dict.name ]] <<- read.csv(fileName,header = T,sep = sep)
    list.dict.column.event.name[[ dict.name ]] <<- column.event.name
  }    
  #=================================================================================
  # getTranslation
  #=================================================================================   
  getTranslation<-function(  column.name , dict.name = 'main', toReturn="csv") {
    # Se era stato indicato un dizionario (e la relativa colonna) caricalo
    # e popola una colonna aggiuntiva
    new.myData<-c()
    jacopoIsGreat <- "true"
    jacopoHello <- "TRUE"
    
    if(param.verbose == TRUE) obj.LH$sendLog(" 1) Converting the Events for all the patients :\n")
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(names(pat.process)), style = 3)
    pb.ct <- 0
    
    for(idPaz in names(pat.process)) {
      
      pb.ct <- pb.ct + 1; 
      if(param.verbose == TRUE) setTxtProgressBar(pb, pb.ct)
      
      matrice<-pat.process[[idPaz]]
      names(matrice)<-names(pat.process[[idPaz]])
      
      aaa<-as.character(pat.process[[idPaz]][[param.EVENTName]])

      bbb<-unlist(lapply(aaa, function(x) { 
        # prendi la voce corrispondente al nome dell'evento
        column.event.name<-list.dict.column.event.name[[ dict.name ]] 
        arrPosizioniTMP<-which(list.dictionary[[ dict.name ]][[ column.event.name ]]==x )
        if(length(arrPosizioniTMP)>1) stop("Error! an Event is associated to more possible new Event names!")
        # e sostituisci

        if(length(arrPosizioniTMP)==0) return( "" )
        else return(as.character( list.dictionary[[ dict.name ]][[ column.name ]][arrPosizioniTMP])  )
      }  ))   

      matrice[[param.EVENTName]] <- bbb
      matrice <- matrice[  which(matrice[[param.EVENTName]]!="") ,   ]
        
      new.myData <- rbind(new.myData,matrice)
    }
    
    if(param.verbose == TRUE) close(pb)
    
    if(toReturn=="csv") { daRestituire <- new.myData  }
    if(toReturn=="dataLoader"){
      if(param.verbose == TRUE) obj.LH$sendLog(" 2) Create a new dataLoader object  (this splits in many steps) :\n")
      # Istanzia un oggetto dataLoader che eridita il parametro "verbose"
      daRestituire<-dataLoader()
      daRestituire$load.data.frame(mydata = new.myData,
                                   IDName = param.IDName,EVENTName = param.EVENTName,
                                   dateColumnName = param.dateColumnName,format.column.date = "%d/%m/%Y %H:%M:%S")      
    }    
    return(daRestituire)
  }
  #=================================================================================
  # filterPatProcess
  #=================================================================================   
  filterPatProcess <- function( CSV.completo, array.events.to.remove, array.events.to.keep, posizione.colonna.evento) {
    if( length(array.events.to.remove) > 0 ){
      CSV.completo <- CSV.completo[!(CSV.completo[,param.EVENTName] %in% array.events.to.remove),]
    }
    if( length(array.events.to.keep) > 0 ){
      CSV.completo <- CSV.completo[ CSV.completo[,param.EVENTName] %in% array.events.to.keep ,]
    }
    return( CSV.completo )
  }
  
  #=================================================================================
  # ricalcolaCSV
  # Ricalcola il CSV togliendo pazienti e/o eventi a piacere
  #=================================================================================   
  ricalcolaCSV<-function( 
                      array.events.to.remove=c(), 
                      array.events.to.keep=c(), 
                      array.pazienti.to.remove=c(),
                      array.pazienti.to.keep=c(),
                      
                      remove.patients.by.attribute.name = NA,
                      remove.events.by.attribute.name = NA,
                      keep.events.by.attribute.name = NA,
                      keep.patients.by.attribute.name = NA,
                      
                      by.arr.attribute.value= c()  ,
                      is.debug = FALSE
                    ) {
    matriciona <- c()
    
    # Costruisci il super CSV
    if(param.save.memory == FALSE) CSV.completo <- original.CSV
    else CSV.completo <- do.call(rbind,  pat.process)
    
    # array.pazienti.to.keep
    if(length(array.pazienti.to.keep)>0) {
      CSV.completo <- CSV.completo[ CSV.completo[ ,param.IDName] %in% array.pazienti.to.keep, ]
    }
    # array.pazienti.to.remove
    if(length(array.pazienti.to.remove)>0) {
      CSV.completo <- CSV.completo[ !(CSV.completo[ ,param.IDName] %in% array.pazienti.to.remove), ]
    }
    
    # array.events.to.remove e array.events.to.keep
    posizione.colonna.evento <- which(colnames(pat.process[[1]]) == param.EVENTName) -1
    if(length(array.events.to.remove)>0 | length(array.events.to.keep)>0) {
      # -im
      CSV.completo <- filterPatProcess( CSV.completo, array.events.to.remove, array.events.to.keep , posizione.colonna.evento  ) 
      # res <- filterPatProcess( CSV.completo, array.events.to.remove, array.events.to.keep , posizione.colonna.evento  ) 
      # CSV.completo <- CSV.completo[res$rigaDaTenere,]
      # -fm
    }

    # 'remove.events.by.attribute.name' e 'keep.events.by.attribute.name'
    # Rimuovi i record in cui una colonna specifica ha il valore indicato. 
    # Nome della colonna e valori sono passati in due array dalle posizioni corrispondenti
    if( !is.na(remove.events.by.attribute.name) ) {
      for( iii in seq(1,length(remove.events.by.attribute.name))) {
        CSV.completo <- CSV.completo[ which(CSV.completo[, remove.events.by.attribute.name[iii] ] == by.arr.attribute.value[iii]), ]
      }
    }
    if( !is.na(keep.events.by.attribute.name) ) {
      for( iii in seq(1,length(remove.events.by.attribute.name))) {
        CSV.completo <- CSV.completo[ which(CSV.completo[, remove.events.by.attribute.name[iii] ] != by.arr.attribute.value[iii]), ]
      }
    }  
    
    # 'remove.patients.by.attribute.name' e 'keep.patients.by.attribute.name'
    # Rimuovi i record in cui una colonna specifica ha il valore indicato. 
    # Nome della colonna e valori sono passati in due array dalle posizioni corrispondenti
    if(!is.na(remove.patients.by.attribute.name)) {
      for( iii in seq(1,length(remove.patients.by.attribute.name))) {
        lista.pazienti <- unique(CSV.completo[ which(CSV.completo[, remove.patients.by.attribute.name[iii] ] == by.arr.attribute.value[iii]), param.IDName])
        CSV.completo <- CSV.completo[ which( !(CSV.completo[, param.IDName ] %in% lista.pazienti)), ]
      }
    }
    if(!is.na(keep.patients.by.attribute.name)) {
      for( iii in seq(1,length(keep.patients.by.attribute.name))) {
        lista.pazienti <- unique(CSV.completo[ which(CSV.completo[, keep.patients.by.attribute.name[iii] ] == by.arr.attribute.value[iii]), param.IDName])
        CSV.completo <- CSV.completo[ which(CSV.completo[, param.IDName ] %in% lista.pazienti), ]
      }
    }    
    
    return(CSV.completo)

  }  
  #=================================================================================
  # applyFilter
  #================================================================================= 
  applyFilter<-function(
                                   array.events.to.keep=c(), 
                                   array.events.to.remove=c(),
                                   array.pazienti.to.keep=c(),
                                   array.pazienti.to.remove=c(),
                                   remove.events.by.attribute.name = NA,
                                   remove.patients.by.attribute.name = NA,
                                   keep.events.by.attribute.name = NA,
                                   keep.patients.by.attribute.name = NA,
                                   by.arr.attribute.value = c(),
                                   whatToReturn="itself",
                                   is.debug=FALSE) {
    
    if(!(whatToReturn %in% c( "itself" , "csv" ,"dataLoader" ) ) ) {
      obj.LH$sendLog( c(" 'whatToReturn can only be 'itself', 'csv' or 'dataLoader'! ")  ,"ERR"); return()
    }
    
    matriciona <- as.data.frame(ricalcolaCSV( array.events.to.remove = array.events.to.remove,
                                              array.events.to.keep = array.events.to.keep,
                                              array.pazienti.to.remove = array.pazienti.to.remove,
                                              array.pazienti.to.keep = array.pazienti.to.keep,
                                              remove.patients.by.attribute.name = remove.patients.by.attribute.name,
                                              remove.events.by.attribute.name = remove.events.by.attribute.name,
                                              keep.events.by.attribute.name = keep.events.by.attribute.name,
                                              keep.patients.by.attribute.name = keep.patients.by.attribute.name,
                                              by.arr.attribute.value = by.arr.attribute.value,
                                              is.debug = is.debug
                                              )) 
    IDName <- param.IDName
    EVENTName <- param.EVENTName
    dateColumnName <- param.dateColumnName
    if( whatToReturn == "itself"){
      load.data.frame( mydata = matriciona, IDName = IDName, EVENTName = EVENTName, 
                       dateColumnName = dateColumnName , format.column.date = "%d/%m/%Y %H:%M:%S", 
                       convertUTF = FALSE, suppress.invalid.date = FALSE)  
    }
    if( whatToReturn == "csv"){
      return(matriciona);  
    }    
    if( whatToReturn == "dataLoader"){
      newObj <- dataLoader(verbose.mode = param.verbose);
      newObj$load.data.frame( mydata = matriciona, IDName = IDName, EVENTName = EVENTName, 
                       dateColumnName = dateColumnName , format.column.date = "%d/%m/%Y %H:%M:%S", 
                       convertUTF = FALSE, suppress.invalid.date = FALSE)        
      return(newObj);  
    }      
    
  }
  #=================================================================================
  # getAttribute
  #=================================================================================  
  getAttribute<-function( attributeName ) {
    if(attributeName=="pat.process") return( pat.process )
    if(attributeName=="MMatrix.perc") {
      MM<-MMatrix;
      for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
      return(MM);
    } 
    if(attributeName=="MMatrix") return( MMatrix )
    if(attributeName=="footPrint") return( footPrint )
    if(attributeName=="MMatrix.perc.noLoop") {
      MM<-MMatrix;
      diag(MM)<-0;
      for( i in seq( 1 , nrow(MM)) ) {if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);} } 
      return(MM);     
    }
    return();
  }  
  #=================================================================================
  # groupPatientLogActivity
  # raggruppa i dati, come sono da CSV in una maniera piu' consona ad essere analizzati
  #=================================================================================   
  groupPatientLogActivity<-function(mydata, ID.list.names) {
    
    # prendi la lista di pazienti e
    # per ogni paziente costruisci i gruppi 
    ID.list<-unique(mydata[[ID.list.names]])
    ID.act.group<-list();
    paziente.da.tenere<-c()

    dimensioni.tabelle <- table(mydata[,ID.list.names])
    
    # Fai lo split del data.frame in una lista di data.frame, rispetto al campo dell' ID
    ID.act.group = split(mydata, list(mydata[[ID.list.names]]))
    # prendi i pazienti da tenere (con almeno due eventi)
    paziente.da.tenere <- names(dimensioni.tabelle)[which(dimensioni.tabelle>=2)]

    return(
      list(
        "ID.act.group" = ID.act.group,
        "paziente.da.tenere" = paziente.da.tenere
      )
    )    
  }  

  setData<-function(   dataToSet  ) {
    # set the desired attribute (the ones passed as !is.na() )
    nomiAttributi<-names(dataToSet)
    
    if( "arrayAssociativo" %in%  nomiAttributi  ) arrayAssociativo<<-dataToSet$arrayAssociativo
    if( "footPrint" %in%  nomiAttributi  ) footPrint<<-dataToSet$footPrint
    if( "MMatrix" %in%  nomiAttributi  ) MMatrix<<-dataToSet$MMatrix
    if( "pat.process" %in%  nomiAttributi  ) pat.process<<-dataToSet$pat.process
    if( "wordSequence.raw" %in%  nomiAttributi  ) wordSequence.raw<<-dataToSet$wordSequence.raw

  }
  order.list.by.date<-function(   listToBeOrdered, dateColumnName, deltaDate.column.name='pMineR.deltaDate', 
                                  format.column.date = "%d/%m/%Y %H:%M:%S" ) {

    # browser()
    if(param.verbose == TRUE) pb <- txtProgressBar(min = 0, max = length(listToBeOrdered), style = 3)
    # Cicla per ogni paziente
    for( paziente in seq(1,length(listToBeOrdered)) ) {
      if( param.verbose == TRUE ) setTxtProgressBar(pb, paziente)
      # Estrai la matrice
      # cat("\n PAZIENTE:",paziente)
      matrice.date<-listToBeOrdered[[paziente]]
      # Leggi la colonna data secondo la formattazione indicata in ingresso e riscrivila nel formato %d/%m/%Y (lo stesso viene fatto in plot.Timeline)
      newdate <- strptime(as.character(matrice.date[,dateColumnName]), format.column.date)
      matrice.date[,dateColumnName] <- format(newdate, "%d/%m/%Y %H:%M:%S")
      # Calcola la colonna delle differenze di date rispetto ad una data di riferimento ed azzera rispetto al minore
      # colonna.delta.date.TMPh898h98h9<-as.numeric(difftime(as.POSIXct(matrice.date[, dateColumnName], format = "%d/%m/%Y"),as.POSIXct("01/01/2001", format = "%d/%m/%Y"),units = 'days'))
      colonna.delta.date.TMPh898h98h9<-as.numeric(difftime(as.POSIXct(matrice.date[, dateColumnName], format = "%d/%m/%Y %H:%M:%S"),as.POSIXct("01/01/2001 00:00:00", format = "%d/%m/%Y %H:%M:%S"),units = 'mins'))
      colonna.delta.date.TMPh898h98h9<-colonna.delta.date.TMPh898h98h9-min(colonna.delta.date.TMPh898h98h9)
      # Aggiungi la colonna dei delta data
      listToBeOrdered[[paziente]]<-cbind(listToBeOrdered[[paziente]],colonna.delta.date.TMPh898h98h9)
      colnames(listToBeOrdered[[paziente]])<-c(colnames(listToBeOrdered[[paziente]])[1:length(colnames(listToBeOrdered[[paziente]]))-1],deltaDate.column.name)
      # Ordina il data.frame di ogni paziente per la colonna DeltaT
      listToBeOrdered[[paziente]]<-listToBeOrdered[[paziente]][order(listToBeOrdered[[paziente]][[deltaDate.column.name]]),]
    }
    if(param.verbose == TRUE) close(pb)
    return(listToBeOrdered);
  } 
  load.data.frame<-function( mydata, IDName, EVENTName, dateColumnName=NA, 
                             format.column.date = "%d/%m/%Y %H:%M:%S", 
                             convertUTF = TRUE, suppress.invalid.date = TRUE , guessDataFormat = FALSE) {
    # clear all the attributes
    obj.Utils <- utils()
    clearAttributes( );
    param.column.names<<-colnames(mydata)
    # browser()
    stocazzo <- mydata
    if( guessDataFormat == TRUE ) {
      ooo <- dateTimeWizard()
      format.column.date <- ooo$guess_datetime_format(arr.string = mydata[[dateColumnName]])
    }    

    if(length(mydata[[dateColumnName]]) == 0) { obj.LH$sendLog( c("dateColumnName '",dateColumnName,"' not present! ")  ,"ERR"); return() }
    if(length(mydata[[EVENTName]]) == 0) { obj.LH$sendLog( c("EVENTName '",EVENTName,"' not present! ")  ,"ERR"); return() }
    if(length(mydata[[IDName]]) == 0) { obj.LH$sendLog( c("IDName '",IDName,"' not present! ")  ,"ERR"); return() }    
    
    obj.dataProcessor <- dataProcessor()
    
    # Add an internal ID attribute to myData (to uniquely identify Logs)
    if(!("pMineR.internal.ID.Evt" %in% colnames(mydata) ))
      { mydata <- cbind("pMineR.internal.ID.Evt"=seq(1,nrow(mydata)),mydata ) }

    max.pMineR.internal.ID.Evt <<- max(mydata$pMineR.internal.ID.Evt)

    # Change the DATA FORMAT!
    mydata[[dateColumnName]] <- as.character(mydata[[dateColumnName]] )
    mydata[[dateColumnName]] <- strptime(as.character(mydata[[dateColumnName]]), format.column.date)
    mydata[[dateColumnName]] <- format(mydata[[dateColumnName]],"%d/%m/%Y %H:%M:%S")
    format.column.date <- "%d/%m/%Y %H:%M:%S"
    
    if(suppress.invalid.date==TRUE) {
      mydata <- mydata[ which(mydata[[dateColumnName]]!="" ),]
    }
    
    # Just to have then an idea of the passed parameters...
    param.IDName<<-IDName
    param.EVENTName<<-EVENTName
    param.dateColumnName<<-dateColumnName
    input.format.date<<- format.column.date
    
    # ok, let's begin!
    ID.list.names<-IDName
    EVENT.list.names<-EVENTName    

    mydata[[EVENT.list.names]]<-as.character(mydata[[EVENT.list.names]])
    
    if(convertUTF == TRUE) {
      mydata <- obj.Utils$cleanUTF(mydata,EVENT.list.names)
      mydata[[EVENT.list.names]] <- gsub("\"", "", mydata[[EVENT.list.names]])
      mydata[[EVENT.list.names]] <- gsub("$", "", mydata[[EVENT.list.names]])
      mydata[[EVENT.list.names]] <- gsub("'", "", mydata[[EVENT.list.names]])
    }
    
    mydata[[ID.list.names]]<-as.character(mydata[[ID.list.names]])
    if(!is.na(dateColumnName)) {
      mydata[[dateColumnName]]<-as.character(mydata[[dateColumnName]])
    }
    if(verbose.mode == TRUE) obj.LH$sendLog("\n 1) internal Grouping (1/3):\n Please wait ....... ")
    # group the log of the patient in a structure easier to handle
    ooo <- groupPatientLogActivity(mydata, ID.list.names) 
    if(verbose.mode == TRUE) obj.LH$sendLog("\n Done! \n ")
    ID.act.group<-ooo$ID.act.group
    paziente.da.tenere<-ooo$paziente.da.tenere
    # Se non ci sono almeno due eventi per ogni paziente, togli il paziente dalla lista
    # (e dal data frame originale)
    # Se non ci sono almeno due eventi per il paziente, toglilo dalla lista
    ID.act.group <- ID.act.group[  paziente.da.tenere  ]
    mydata <- mydata[ ( mydata[[IDName]] %in% paziente.da.tenere  ), ]
    
    if(verbose.mode == TRUE) obj.LH$sendLog(" 2) Ordering date (2/3):\n")
    # Order the list by the interested date (if exists)
    if(!is.na(dateColumnName)) {
      if(length(ID.act.group)==0) browser()
      ID.act.group<-order.list.by.date(listToBeOrdered = ID.act.group, dateColumnName = dateColumnName, format.column.date = format.column.date)
    }

    if(verbose.mode == TRUE) obj.LH$sendLog(" 3) Building MMatrices and other stuff (3/3):\n")
    
    # build the MM matrix and other stuff...
    res <- obj.dataProcessor$buildMMMatrices.and.other.structures(mydata = mydata, 
                                                                  EVENT.list.names = EVENT.list.names, 
                                                                  EVENTName = EVENTName,
                                                                  EVENTDateColumnName = param.dateColumnName,
                                                                  ID.act.group = ID.act.group,
                                                                  max.char.length.label = param.max.char.length.label,
                                                                  verbose.mode = param.verbose 
                                                                  )

    if(res$error == TRUE) { 
      if(res$errCode == 1) {obj.LH$sendLog( "event '' (BLANK) detected, please check the file\n"  ,"ERR"); return()}
      if(res$errCode == 2) {obj.LH$sendLog( c("an event has a label with a length greter than ",param.max.char.length.label," chars...\n")  ,"ERR"); return()}
      if(res$errCode == 3) {obj.LH$sendLog( "at least an event has an invalid char in the label (',$,\")\n"  ,"ERR"); return()}      
    }
    if(  sum( is.na(mydata[[dateColumnName]]) ) > 0  ) {  obj.LH$sendLog( c("at least one date is set to NA, please check loaded data and data format! (patients: ",paste(    mydata[which(is.na(mydata[[dateColumnName]])),IDName]  ,collapse = ','),") \n")  ,"ERR"); return()}      

    arrayAssociativo<<-res$arrayAssociativo
    footPrint<<-res$footPrint
    MMatrix<<-res$MMatrix
    pat.process<<-res$pat.process
    wordSequence.raw<<-res$wordSequence.raw    
    MM.mean.time<<-res$MM.mean.time  
    MM.mean.outflow.time<<-res$MM.mean.outflow.time
    MM.density.list<<-res$MM.density.list   
    MM.den.list.high.det <<- res$MM.den.list.high.det
    if(save.memory == FALSE) original.CSV <<- mydata
  }
  #=================================================================================
  # load.csv
  #=================================================================================  
  load.csv<-function( nomeFile, IDName, EVENTName,  quote="\"",sep = ",", dateColumnName=NA, 
                      format.column.date="%d/%m/%Y %H:%M:%S", 
                      convertUTF = TRUE, suppress.invalid.date = TRUE) {
    
    
    # load the file
    if(!file.exists(nomeFile)) { obj.LH$sendLog(c( "'",nomeFile,"' does not exist!\n" ),"ERR"); return() }
    mydata = read.table(file=nomeFile,sep = sep,header = T,quote=quote)
    
    if(length(mydata)==0) { obj.LH$sendLog(c( "'",nomeFile,"' seems to be empty....\n" ),"ERR"); return() }
    if(dim(mydata)[2]==1) { obj.LH$sendLog(c( "'",nomeFile,"' seems to have only one column... check the separator!\n" ),"ERR"); return() }
    
    # Now "load" the data.frame
    load.data.frame( mydata = mydata, IDName = IDName, EVENTName = EVENTName, 
                     dateColumnName = dateColumnName , format.column.date = format.column.date, 
                     convertUTF = convertUTF, suppress.invalid.date = suppress.invalid.date)
  }
  #=================================================================================
  # loader
  #=================================================================================  
  getData<-function( ) {
    
    MM<-MMatrix;
    for( i in seq( 1 , nrow(MM)) ) {  if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);}  } 
    MMatrix.perc<-MM
    
    MM<-MMatrix;
    diag(MM)<-0;
    for( i in seq( 1 , nrow(MM)) ) {  if(sum(MM[i,])>0)  {MM[i,]<-MM[i,]/sum(MM[i,]);}  } 
    MMatrix.perc.noLoop<-MM     

    return(list(
      "arrayAssociativo"=arrayAssociativo,
      # "footPrint"=footPrint,
      "MMatrix"=MMatrix,
      "MMatrix.perc"=MMatrix.perc,
      "MMatrix.perc.noLoop"=MMatrix.perc.noLoop,
      "pat.process"=pat.process,
      "wordSequence.raw"=wordSequence.raw,
      "MM.mean.time"=MM.mean.time,
      "MM.density.list"=MM.density.list,
      "MM.den.list.high.det"=MM.den.list.high.det,
      "MM.mean.outflow.time"=MM.mean.outflow.time,
      "original.CSV"=original.CSV,
      "csv.column.names" = param.column.names,
      "csv.IDName"=param.IDName,
      "csv.EVENTName"=param.EVENTName,
      "csv.dateColumnName"=param.dateColumnName,
      "csv.date.format"=input.format.date,
      "csv.max.pMineR.internal.ID.Evt"=max.pMineR.internal.ID.Evt
    ))
  }
  getClass<-function(){
    return(list(
      "class"="dataLoader",
      "obj.ID"=global.personal.ID,
      "version"="0.41"
    ))
  }  
  plotPatientTimeline<-function( PatID , table.format.date="%d/%m/%Y %H:%M:%S", output.format.date = "%d/%m/%Y" ,cex.axis = 0.6, cex.text = 0.7) {
    eventTable <- cbind(
      pat.process[[as.character(PatID)]][,param.dateColumnName],
      pat.process[[as.character(PatID)]][,param.EVENTName]
    )
    
    colnames(eventTable)<-c("DATA","DES");
    df<-as.data.frame(eventTable)
    df$DATA<-as.character.factor(df$DATA)
    df$DES<-as.character.factor(df$DES)
    df$YM <- as.Date(df$DATA, format=table.format.date)
    min.data <- df$DATA[1]
    max.data <- df$DATA[length(df$DATA)]
    
    delta.date <- as.numeric(difftime(as.POSIXct(max.data, format = table.format.date),as.POSIXct(min.data, format = table.format.date),units = 'mins'))
    arr.delta.date <- as.numeric(difftime(as.POSIXct(df$DATA, format = table.format.date),as.POSIXct(rep(min.data,length(df$DATA)), format = table.format.date),units = 'mins'))
    
    color.bar = "#5B7FA3";
    col.vert.ar <- "#5B7FA3"
    col.stanga <- "gray80"
    plot(NA,ylim=c(-1,1),xlim=c(0,delta.date),ann=FALSE,axes=FALSE)
    abline(h=0,lwd=2,col=color.bar)
    
    segments(arr.delta.date,rep(-.05,length(arr.delta.date)),arr.delta.date,rep(.05,length(arr.delta.date))  ,pch='8',col=col.vert.ar )
    
    ypts <- rep_len(c(-1,-0.7,-0.3,0.3,0.7,1), length.out=nrow(df))
    txtpts <- rep_len(c(1,3), length.out=nrow(df))
    
    for( indice in seq(1,length(df$DATA))) {
      label.data <- as.character(format(as.POSIXct(df$DATA[indice], format = table.format.date),format=output.format.date))
      label.evt <- df$DES[indice]
      label <- paste( c( label.evt,"\n",label.data), collapse = ''  )
      text(x = arr.delta.date[indice], y = ypts[indice], labels = label  , cex = cex.text )
      segments( arr.delta.date[indice], ypts[indice], arr.delta.date[indice],0,col=col.stanga)
    }
  }   
  #=================================================================================
  # costructor
  #=================================================================================  
  costructor<-function( verboseMode , max.char.length.label, saveMemory  ) {
    arrayAssociativo<<-''
    footPrint<<-''
    MMatrix<<-''
    pat.process<<-'' 
    wordSequence.raw<<-''
    MM.mean.time<<-''  
    MM.mean.outflow.time<<-''
    MM.density.list<<-''    
    MM.den.list.high.det<<-''
    list.dictionary<<-list()
    list.dict.column.event.name<<-list()
    input.format.date<<-''
    max.pMineR.internal.ID.Evt<<-0
    # Not true data, but useful anyway
    param.IDName<<-''
    param.EVENTName<<-''
    param.dateColumnName<<-''
    param.verbose<<-verbose.mode
    param.column.names<<-''
    param.max.char.length.label<<-max.char.length.label
    param.save.memory<<- saveMemory
    original.CSV <<- ''
    
    obj.LH<<-logHandler()
    global.personal.ID<<-paste( c(as.character(runif(1,1,100000)),as.character(runif(1,1,100000)),as.character(runif(1,1,100000))), collapse = '' )
  }
  costructor( verboseMode = verbose.mode, max.char.length.label = max.char.length.label, saveMemory = save.memory )
  #================================================================================= 
  return(list(
    "load.csv"=load.csv,
    "load.data.frame"=load.data.frame,
    "getData"=getData,
    "applyFilter"=applyFilter,
    "addDictionary"=addDictionary,
    "getTranslation"=getTranslation,
    "plotPatientTimeline"=plotPatientTimeline,
    "getClass"=getClass
  ))
}