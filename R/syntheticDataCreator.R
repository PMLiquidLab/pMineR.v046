#' A classto build Synthetic Data
#'
#' @description  A class with many methods for synthetic data generation
#' @export
syntheticDataCreator<-function() {
  
  dado <- function( facce ) {
    return(as.integer(runif(1)*facce+1))
  }
  
  cohort.RT<-function(numOfPat = 100, starting.date = "01/01/2001" ) {
    data.partenza <- starting.date

    matrice <- c()
    for ( id in seq(1,numOfPat) ) {
      vecchia.data <- data.partenza
      morto <- FALSE
      resezioneCompleta <- FALSE
      CHT <- FALSE
      RT <- FALSE
      
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
        }
      }
      
      # FOLLOWUP
      for( ct in seq(1,dado(8))) {
        
        vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") +  (180 - dado(120))  
        matrice <- rbind( matrice, c(id , "MedicalVisit" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))      
        
        pMorte <- pMorte + 0.1
        
        if(runif(1)<pMorte) {
          vecchia.data <- as.Date(vecchia.data,"%d/%m/%Y") + dado(1)
          matrice <- rbind( matrice, c(id , "death" , as.character(as.Date(vecchia.data,"%d/%m%/Y") )))
          morto <- TRUE
        }
        if(morto == TRUE) break
        if( resezioneCompleta == TRUE ) pMorte <- pMorte - 0.03
        if( CHT == TRUE ) pMorte <- pMorte - 0.02
        if( RT == TRUE ) pMorte <- pMorte - 0.02
        
      }
    }
    colnames(matrice) <- c("ID","Event","Date")
    return(matrice);
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
    "cohort.RT"=cohort.RT)
  )
  
}