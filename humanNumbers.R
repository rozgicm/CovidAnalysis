# Time to separate some functions
humanNumbers = function(x = NULL, smbl ="", signif = 1){
  humanity = function(y){
    
    if (!is.na(y)){
      tn = round(abs(y) / 1e12, signif) #Bio
      b = round(abs(y) / 1e9, signif) #Mrd
      m = round(abs(y) / 1e6, signif) #Mio
      k = round(abs(y) / 1e3, signif)
      
      if ( y >= 0 ){
        y_is_positive = ""
      } else {
        y_is_positive = "-"
      }
      
      if ( k < 1 ) {
        paste0( y_is_positive, smbl, round(abs(y), signif ))
      } else if ( m < 1){
        paste0 (y_is_positive, smbl,  k , "K")
      } else if (b < 1){
        paste0 (y_is_positive, smbl, m ,"Mio")
      }else if(tn < 1){
        paste0 (y_is_positive, smbl, b ,"Mrd")
      } else {
        paste0 (y_is_positive, smbl,  comma(tn), "Bio")
      }
    } else if (is.na(y) | is.null(y)){
      "-"
    }
  }
  
  sapply(x,humanity)
}