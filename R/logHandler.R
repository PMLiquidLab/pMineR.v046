#' error handler class
#' 
#' @description  A class to deal with errors
logHandler<-function() {
  behaviourTable<-c()
  #=================================================================================
  # sendLog
  # Send a Message Log according to the object policies for the type 
  # indicated in 'type'
  #=================================================================================
  sendLog<-function( msg , type="MSG" ) {
    if(length(msg)>1) msg<-paste(msg,collapse='')
    else msg<-msg 
    
    printPrefix <- behaviourTable[which(behaviourTable[,"msg"]==type),"printPrefix"]
    if(printPrefix=="T") msg <- paste( c("\n",type,": ",msg),collapse = '')
    else msg <- msg
    
    what2Do<-behaviourTable[which(behaviourTable[,"msg"]==type),"behaviour"]
    
    if(what2Do == "display")  {
      cat(msg)
    }
    if(what2Do == "stop")  {
      cat(msg)
      stop();
    }    
  }
  #=================================================================================
  # setBehaviour
  # change a single line in the behaviour table
  #=================================================================================
  setBehaviour<-function( msg , behaviour) {
    behaviourTable[which(behaviourTable[,"msg"]==msg),"behaviour"] <<- behaviour
  }
  #=================================================================================
  # costructor
  #=================================================================================
  costructor<-function() {
    bht<-c()
    bht<-rbind(bht,c("WRN","display","T"))
    bht<-rbind(bht,c("MSG","display","F"))
    bht<-rbind(bht,c("ERR","display","T"))
    bht<-rbind(bht,c("NMI","stop","T"))
    colnames(bht)<-c("msg","behaviour","printPrefix")
    behaviourTable<<-bht
  }
  #=================================================================================
  costructor();
  #=================================================================================
  return(
    list(
      "sendLog"=sendLog,
      "setBehaviour"=setBehaviour
    )
  )
}
