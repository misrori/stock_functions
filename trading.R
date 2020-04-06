
get_one_ticker  <- function(my_ticker, start_date = "1900-01-01", end_date = Sys.Date(),  mas=c(50, 100, 200), emas=c(3, 5, 10, 20, 30), saveit=F, keepma=T, keepema=T, addmacd=T, addrsi=T, remove_nas = T, calc_diff = T, add_boilinger= T, add_macd_cross=T, folder_to_save= '/home/mihaly/R_codes/ema_strat/hist_data/', check_if_exist=F) {
  if (check_if_exist) {
      if (file.exists(paste0(folder_to_save, my_ticker, '.rds'))) {
        t <- readRDS(paste0(folder_to_save, my_ticker, '.rds'))
        if (  (Sys.Date()-7) < max(t$date)     ) {
          #print('I have it')
          return(t)
        }
      }
  }
  
  tryCatch({
    adatom <- data.frame(ds.getSymbol.yahoo(my_ticker, from = start_date, to =end_date ))
    names(adatom) <- tolower(sapply(strsplit(names(adatom), '.', fixed = T), '[[', 2))
    adatom$date <- as.Date(row.names(adatom)) 
    row.names(adatom) <- 1:nrow(adatom)
    adatom <- data.table(adatom)
    
    if( !identical(names(adatom) , c("open","high","low", "close","volume",  "adjusted","date"))) {
      text<- paste0('Error: ', my_ticker, ' # problem: names of dataframe is bad ', ' time: ', Sys.time())
      stop(text)
    }
    
    if ( nrow(adatom[complete.cases(adatom)==F,])> 0)  {
      adatom <- adatom[complete.cases(adatom),]
      if(nrow(adatom)==0){
        text<- paste0('Error: ', my_ticker, ' # problem: empty dataframe ', ' time: ', Sys.time())
        stop(text)
      }
    }
    
  }, error=function(x) {
    print(x)
    stop('No ticker')
  })
  
  
  for (simple_mas in mas) {
    adatom[[paste0('ma_', simple_mas, '_value')]] <- movavg(adatom[['close']], simple_mas, type="s")
    if (calc_diff==T) {
      
      adatom[[paste0('diff_',simple_mas,'_ma_value')]] <-  (( adatom[["close"]]  /adatom[[paste0('ma_', simple_mas, '_value')]] )-1)*100
      if (keepma==F) {
        adatom[[paste0('ma_', simple_mas, '_value')]] <- NULL
      }
      
    }
  }
  
  for (exp_mas in emas) {
    adatom[[paste0('ma_', exp_mas, '_exp_value')]] <- movavg(adatom[['close']], exp_mas, type="e")
    if (calc_diff==T) {
      adatom[[paste0('diff_',exp_mas,'_exp_ma_value')]] <-  (( adatom[["close"]]  / adatom[[paste0('ma_', exp_mas, '_exp_value')]] )-1)*100
      if (keepema==F) {
        adatom[[paste0('ma_', exp_mas, '_exp_value')]]<- NULL
      }
    }
  }
  
  if (addmacd==T) {
    adatom <- cbind(adatom, data.table(MACD(adatom[['close']], nFast=12, nSlow=26,nSig=9,maType=EMA, percent = FALSE)))
  }  
  
  if (add_boilinger) {
    boil <- data.table(BBands( adatom[,c("high","low","close")] ))
    adatom$boil_dow_diff <- (((adatom$close /boil$dn)-1)*100)
    adatom$boil_up_diff <- (((adatom$close /boil$up)-1)*100)
    adatom$boil_mid_diff <- (((adatom$close /boil$mavg)-1)*100)
  }

  
  if (addrsi==T) {
    adatom$rsi <- RSI(adatom[['close']])
  }
  if (add_macd_cross==T) {
    adatom$macd_cross <- F
    for (i in 2:nrow(adatom)) {
      adatom$macd_cross[i] <- (adatom$macd[(i-1)]< adatom$signal[(i-1)]) & (adatom$macd[i]> adatom$signal[i])
      
    }
    
    adatom$macd_cross <- ifelse(adatom$macd_cross, 1, 0)
  }
  
  
  adatom <- adatom[complete.cases(adatom)]
  if (saveit) {
    saveRDS(adatom, paste0(folder_to_save, my_ticker, '.rds'))
  }
  return(adatom)  
  
} # end of fg

