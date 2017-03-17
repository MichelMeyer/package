
if( ! "xts" %in% installed.packages()) install.packages("xts", dependencies = T)
library(xts)

# Internal functions:

get.files <- function(link) {
  # This is a function for internal uses!
  
  if( ! dir.exists(tempdir())) # Yes, some times occurs erros because of the inexistence of this directory
    dir.create(tempdir(), showWarnings = F)
  if( ! dir.exists(paste0(tempdir(),"\\omni")))
    dir.create(paste0(tempdir(),"\\omni"))
  
  if(file.exists(paste0(tempdir(), "\\omni\\", link, ".txt"))) {
    file <- read.table(paste0(tempdir(), "\\omni\\", link, ".txt"),
                       header = T, stringsAsFactors = F)
    
  } else {
    links <- data.frame(
      dicionariocompleto = "https://dl.dropboxusercontent.com/s/sqqhn6d7g5vnhxf/dicionariocompleto.txt?dl=0",
      dicionariobruto = "https://dl.dropboxusercontent.com/s/yntx5i6xi66ld66/dicionariobruto.txt?dl=0",
      linkseventos = "https://dl.dropboxusercontent.com/s/ecafaixl0lw0292/linkseventos.txt?dl=0",
      linkscotacoes = "https://dl.dropboxusercontent.com/s/x4uzbx1wahh6xy3/linkscotacoes.txt?dl=0",
      linksbalancos = "https://dl.dropboxusercontent.com/s/iaabhpybci6jy4x/linksbalancos.txt?dl=0",
      codbdi = "https://dl.dropboxusercontent.com/s/b1la2bhs1nddd2x/codbdi.txt?dl=0",
      stringsAsFactors = F
    )
    
    file <- read.table(url(links[link][[1]]), header = T, stringsAsFactors = F)
    write.table(file, paste0(tempdir(), "\\omni\\", link, ".txt"), row.names = F)
  }
  
  
  closeAllConnections()
  return(file)
}
get.shares <- function(Shares, envir = NULL) {
  # This is a function used in other functions.
  
  if( ! is.null(envir)) {
    prog.bar <- get("prog.bar", envir = envir)
    progresso <- get("progresso", envir = envir)
    shares <- get("shares", envir = envir)
  }
  if( ! exists("sharelinks"))
    sharelinks <- get.files("linkscotacoes")
  teste <- lapply(Shares, function(share) {
    if(exists("prog.bar")) {
      if(share == shares[[1]]) cat("Progress of download: \n")
      x <- c(which(shares == share) - (1 : 0)) / length(shares)
      cat(paste(progresso[prog.bar >= x[1] & prog.bar < x[2]], collapse = ""))
      if(share == tail(shares, 1)) cat("100%\n")
    }
    x <- sharelinks[sharelinks$PAPEL == share, "LINK"]
    if(length(x) == 0) {
      return("This share is misspecified or does not exist.")
    } else {
      
      t <- NULL
      while( ! is.data.frame(t)) {
        dest <- paste0(tempdir(), "\\omni\\", share)
        if(file.exists(dest)) {
          tfile <- file.info(dest)$mtime
          tupdate <- strsplit(as.character(Sys.time()), " ")[[1]][1]
          tupdate <- as.POSIXct(paste(tupdate, "22:00:00"))
          if(tfile <= tupdate & Sys.time() >= tupdate) {
            x <- paste0("https://dl.dropboxusercontent.com/s/",
                        x, "/", share, ".csv?dl=0")
            download.file(x, destfile = dest, mode = "wb", quiet = T) 
          }
          rm(tfile, tupdate)
        } else {  
          x <- paste0("https://dl.dropboxusercontent.com/s/", x, "/", share, ".csv?dl=0")
          download.file(x, destfile = dest, mode = "wb", quiet = T)
        }
        t <- try(x <- readRDS(dest), silent = T)
        if(class(t) == "try-error") unlink(dest)
      }
      rm(t)
      
      names(x)[8 : 17] <- c("Open", "High", "Low", "Mean", "Close",
                            "Bid", "Ask", "TotNeg", "QuaTot", "Volume")
      
      x$DATAPREG <- as.Date(as.character(x$DATAPREG), "%Y%m%d")
      x$DATVEN <- as.Date(as.character(x$DATVEN), "%Y%m%d")
      x$QuaTot <- as.numeric(x$QuaTot)
      
      if(length(unique(x[, "CODBDI"])) > 1)
        x <- x[x[, "CODBDI"] %in%
                 names(sort(table(x[, "CODBDI"]), decreasing = T))[[1]], ]
      
      return(x)
    }
  })
  closeAllConnections()
  
  return(teste)
}
get.fin.stat <- function(firms, quarter = NULL) {
  
  if( ! is.null(quarter) & length(firms) > 1) {
    quarter <<- quarter <- NULL
    warning("Argument quarter is not applied when there is more than one code.")
  }
  
  dicionario <- get.files("dicionariobruto")
  sharelinks <- get.files("linkscotacoes")
  dic2 <- get.files("linkseventos")
  balancos <- get.files("linksbalancos")
  
  if("all" %in% firms) {
    firms <- sort(c(firms[firms != "all"], unique(dicionario[, 1])))
  }
  
  if(all(firms %in% 3)) {
    warning("The CVM code 3 is a example, This firm actually does not exists.")
    return(NULL)
  } else {
    firms <- firms[firms != 3]
  }
  
  envir <- NULL
  if(length(firms) > 20) {
    
    cat("It can take a while.\n")
    
    progresso <- 0 : 10
    div <- 6
    envir <- environment()
    
    progresso <- paste(paste0("", progresso * (100 / max(progresso)), "% "),
                       collapse = paste0(rep("| ", div), collapse = ""))
    progresso <- strsplit(progresso, " ")[[1]]
    prog.bar <- (1 : length(progresso)) / length(progresso)
    prog.bar <- prog.bar - min(prog.bar)
    prog.bar <- prog.bar / max(prog.bar)
  }
  
  fin.stat <- lapply(firms, function(firm) {
    if( ! is.null(envir)) {
      prog.bar <- get("prog.bar", envir = envir)
      progresso <- get("progresso", envir = envir)
    }
    if(exists("prog.bar")) {
      x <- c(which(firms == firm) - (1 : 0)) / length(firms)
      cat(paste(progresso[prog.bar >= x[1] & prog.bar < x[2]], collapse = ""))
      if(firm == tail(firms, 1)) cat("100%\n")
    }
    if ( ! exists("especificacao")) {
      if (nchar(firm) <= 6) {
        if ( ! any ( ! (strsplit(as.character(firm), "")[[1]] %in% 0:9))) {
          especificacao <- "CodigoCvm"
        }
      }
    }
    if ( ! exists("especificacao")) {
      x <- which(strsplit(firm, "")[[1]] %in% c(".", "/", "-"))
      if (length(x) > 0){
        if ( ! any ( ! diff(x)  == c(4, 4, 5))) {
          if(any( ! strsplit(firm, "")[[1]][setdiff(1 : nchar(firm), x)] %in% c(0:9))) {
            stop("There are some invalid characters in the input.")
          } else {
            especificacao <- "NumeroCnpjCompanhiaAberta"
          }
        }
      }
      rm(x)
    }
    if ( ! exists("especificacao")) {
      if (firm %in% dicionario[,"NomeRazaoSocial"])
        especificacao <- "NomeRazaoSocial"
    }
    if ( ! exists("especificacao")) {
      if(all(strsplit(substr(firm, 1, 4), "")[[1]] %in% LETTERS))
        if (substr(firm, 1, 4) %in% dicionario[, "codNeg"]){
          firm <- substr(firm, 1, 4)
          especificacao <- "codNeg"
        }
    }
    if ( ! exists("especificacao")) {
      return("We do not found the balance sheets of the selected firm.")
    } else {
      if (especificacao == "CodigoCvm") {
        x <- which(dicionario[, especificacao] == as.numeric(firm))
        x <- max(x)
      } else {
        x <- which(dicionario[, especificacao] == firm)
        x <- max(x)
        firm <- dicionario[x, "CodigoCvm"]  
      }
      nome <- paste(dicionario[x, "codNeg"], sep=".")
      if (nome == "NA" | is.na(nome))
        nome <- paste("CVM", dicionario[x, "CodigoCvm"], sep=".")
      dest <- paste0(tempdir(), "\\omni\\", firm)
      x <- NULL
      if(file.exists(dest)) {
        tfile <- file.info(dest)$mtime
        tupdate <- strsplit(as.character(Sys.time()), " ")[[1]][1]
        tupdate <- as.POSIXct(paste(tupdate, "22:00:00"))
        if(tfile <= tupdate & Sys.time() >= tupdate) {
          link <- paste0("https://dl.dropboxusercontent.com/s/",
                         balancos[balancos$EMPRESA == firm, ]$LINK, "/", firm, ".csv?dl=0")
          download.file(link, destfile = dest, mode = "wb", quiet = T)
        }
        rm(tfile, tupdate)
      } else {  
        link <- paste0("https://dl.dropboxusercontent.com/s/",
                       balancos[balancos$EMPRESA == firm, ]$LINK, "/", firm, ".csv?dl=0")
        download.file(link, destfile = dest, mode = "wb", quiet = T)
      }
      try(x <- readRDS(dest), silent = T)
      if(is.null(x))
        if( ! system(paste("ping", "www.dropbox.com"), show.output.on.console = F)) {  
          link <- paste0("https://dl.dropboxusercontent.com/s/",
                         balancos[balancos$EMPRESA == firm, ]$LINK, "/", firm, ".csv?dl=0")
          download.file(link, destfile = dest, mode = "wb", quiet = T)
          try(x <- readRDS(dest), silent = T)
        } else {
          warning("Problems in the connection!")
          return(NULL)
        }
      if( ! is.null(quarter)) {
        
        x <- x[, which(colnames(x) == "V1") : ncol(x)]
        
        if(any(substr(x[, "V6"], 1, 7) == quarter)) {
          
          x <- x[c(1 : 3, which(substr(x[, "V6"], 1, 7) == quarter)), ]
          x <- x[, x[4, ] != ""]
          x <- t(x)
          
          rownames(x) <- NULL
          colnames(x) <- c("kind of information", "id of account", "name of account", "Value")
          x[x[, 1] == "-", 1] <- "firm's basic information"
          x[x[, 1] == "1", 1] <- "Individual Result"
          x[x[, 1] == "2", 1] <- "Consolidated Result"
          
        } else {
          
          x <- substr(x[ - c(1 : 3), "V6"], 1, 7)
          print("This specified quarter isn't in our data")
          print("Currently the quarters available are the periods below:")
          print(paste(paste(head(x, 4), collapse = ", "), "...",
                      paste(tail(x, 4), collapse = ", "), sep = ", "))
          
          rm(x)
          return(invisible(NULL))
          break
        }
        
      } else {
        x <- x[, 1 : (which(colnames(x) == "V1") - 1)]
        x <- x[1 : (nrow(x) - 3), ]
      }
      return(x)
    }
  })
  closeAllConnections()
  return(fin.stat)
}

# Package principal functions:

getPrices <- function(shares,
                      info = "simplified", value = "Close", fill = NA) {
  # Returns the diary infomation about the prices and
  # the transactioned volume of the share.
  # Arguments:
  #   shares: the name of the shares and/or a combination of the options below:
  #           shares, rights and receipts, real estate funds, private bonus,
  #           insolvensy (shares of the firms that in some moment between 1998 and now were in insolvency state),
  #           debentures (also in this class there are investment certificates and public debt titles),
  #           forward market, options, futures and fractionary.
  #     Eg.: getPrices(shares = c("VALE3", "options", "futures"))
  #   info:
  #     full: returns a data.frame with the bovespa's quotation data.
  #     simplified: returns a data.frame with the open, close, min, max, mean, bid and ask prices,
  #            transactioned volume of the day and other informations that changed between the specified shares.
  #     single: returns a xts object with a single column specified in paramenter value.
  #   value: only used if info = "single".
  #          The setted argument is the "Close" column, but it can be changed at will.
  #   fill: Only used in the same case that the value is used.
  #         Some of the shares aren't negotiated every period, so this parameter set how must be filled
  #         the missing values. The options are below:
  #     last: uses the last negotiated day value to fill the gaps.
  #     NA: fills with NAs.
  #     drop: Drop the day out of the time serie.
  
  if (missingArg(shares)) {
    warning ("You have to specify at list a share.")
  } else {
    
    sharelinks <- get.files("linkscotacoes")
    quiet = F
    
    if("all" %in% shares)
      shares <- c(shares[shares != "all"], sharelinks[, 1])
    if(any( ! shares %in% sharelinks[, 1]))
      codbdi = get.files("codbdi")
    
    if(any(shares %in% "shares"))
      shares <- c(shares[ ! shares %in% "shares"],
                  strsplit(codbdi[codbdi[, 1] ==  2, 3], " ")[[1]])
    
    if(any(shares %in% "insolvency"))
      shares <- c(shares[ ! shares %in% "insolvency"],
                  strsplit(codbdi[codbdi[, 1] ==  6, 3], " ")[[1]])
    
    if(any(shares %in% "rights and receipts"))
      shares <- c(shares[ ! shares %in% "rights and receipts"],
                  strsplit(codbdi[codbdi[, 1] == 10, 3], " ")[[1]])
    
    if(any(shares %in% "real estate funds"))
      shares <- c(shares[ ! shares %in% "real estate funds"],
                  strsplit(codbdi[codbdi[, 1] == 12, 3], " ")[[1]])
    
    if(any(shares %in% "debentures"))
      shares <- c(shares[ ! shares %in% "debentures"],
                  strsplit(codbdi[codbdi[, 1] %in% c(14, 66, 68, 83), 3], " ")[[1]])
    
    if(any(shares %in% "private bonus"))
      shares <- c(shares[ ! shares %in% "private bonus"],
                  strsplit(codbdi[codbdi[, 1] == 22, 3], " ")[[1]])
    
    if(any(shares %in% "options"))
      shares <- c(shares[ ! shares %in% "options"],
                  strsplit(codbdi[codbdi[, 1] %in% c(32, 33, 38, 42, 74, 75, 78,
                                                     82), 3], " ")[[1]])
    if(any(shares %in% "auctions"))
      shares <- c(shares[ ! shares %in% "auctions"],
                  strsplit(codbdi[codbdi[, 1] %in% c(46, 48, 50, 51, 52, 53, 54,
                                                     56), 3], " ")[[1]])
    if(any(shares %in% "forward market"))
      shares <- c(shares[ ! shares %in% "forward market"],
                  strsplit(codbdi[codbdi[, 1] == 62, 3], " ")[[1]])
    
    if(any(shares %in% "futures"))
      shares <- c(shares[ ! shares %in% "futures"],
                  strsplit(codbdi[codbdi[, 1] %in% c(70, 71), 3], " ")[[1]])
    
    if(any(shares %in% "fractionary"))
      shares <- c(shares[ ! shares %in% "fractionary"],
                  strsplit(codbdi[codbdi[, 1] == 96, 3], " ")[[1]])
    
    shares <- sort(unique(shares))
    
    envir <- NULL
    if(quiet == F & length(shares) > 20) {
      
      cat("It can take a while.\n")
      envir <- environment()
      progresso <- 0 : 10
      div <- 6
      
      progresso <- paste(paste0("", progresso * (100 / max(progresso)), "% "),
                         collapse = paste0(rep("| ", div), collapse = ""))
      progresso <- strsplit(progresso, " ")[[1]]
      prog.bar <- (1 : length(progresso)) / length(progresso)
      prog.bar <- prog.bar - min(prog.bar)
      prog.bar <- prog.bar / max(prog.bar)
    }
    teste <- get.shares(shares, envir)
    
    x = misspecified = NULL
    if(info == "single")
      for(i in seq_along(teste))
        if(any(duplicated(teste[[i]][, "DATAPREG"]))) {
          warning("Some of the assets belongs to the forward market or they have a duplicated date.
                  Due this the output is goind to be a data.frame.")
          info <- "simplified"
          break
        }
    if(info == "single") {
      for(i in seq_along(teste)) {    
        if(exists("prog.bar")) {
          if(names(teste)[1] == shares[[1]]) cat("Progress of aggregation: \n")
          P <- c(which(shares == names(teste)[1]) - (1 : 0)) / length(shares)
          cat(paste(progresso[prog.bar >= P[1] & prog.bar < P[2]], collapse = ""))
          if(names(teste)[1] == tail(shares, 1)) cat("100%\n")
        }
        if(is.null(dim(teste[[1]]))) {
          misspecified <- c(misspecified, names(teste)[1])
        } else {
          share <- teste[[1]]
          if(value %in% colnames(share)) {
            share <- xts(share[, value], order.by = as.Date(share$DATAPREG))
          } else {
            warning(paste("The parameter value was misspecified. One of these must be selected:",
                          paste(colnames(share)[ - 2], collapse = ", ") ))
            return()
          }
          
          if(is.na(fill) | fill == "last") {
            x <- cbind(x, share)
          } else {
            if(is.null(x)) {
              x <- share
            } else {
              t <- as.Date(intersect(index(x), index(share)))
              x <- cbind(x[t, ], share[t, ])
              rm(t)
            }
          }
          
          colnames(x)[ncol(x)] <- shares[[i]]
          
          rm(share)
        }
        
        teste <- teste[ - 1]
      }
      
      if( ! is.na(fill))
        if(fill == "last") {
          for(i in colnames(x)) {
            t <- which(is.na(x[, i]))
            t.1 <- c(which((t[-1] - t[ - length(t)] > 1)), length(t)) + 1
            
            for(j in seq_along(t.1))
              if(t[max(t.1[j - 1], 1)] - 1 != 0)
                x[t[max(t.1[j - 1], 1)] : t[t.1[j] - 1], i] <- x[t[max(t.1[j - 1], 1)] - 1, i]
          }
        }
      
      
    }
    if(info %in% c("full", "simplified")) {
      for(i in seq_along(teste)) {
        if(exists("prog.bar")) {
          if(names(teste)[1] == shares[[1]]) cat("Progress of aggregation: \n")
          P <- c(which(shares == names(teste)[1]) - (1 : 0)) / length(shares)
          cat(paste(progresso[prog.bar >= P[1] & prog.bar < P[2]], collapse = ""))
          if(names(teste)[1] == tail(shares, 1)) cat("100%\n")
        }
        if(is.null(dim(teste[[1]]))) { 
          misspecified <- c(misspecified, shares[[i]])
        } else {
          share <- teste[[1]]
          share <- cbind(NegCode = share[, "CODNEG"], Date = share[, "DATAPREG"],
                         DeadLine = share[, "PRAZOT"], ExpirDate = share[, "DATVEN"],
                         share[, - which(colnames(share) %in%
                                           c("CODNEG", "DATAPREG", "PRAZOT", "DATVEN"))])
          rownames(share) <- NULL
          x <- rbind(x, share)
          rm(share)
        }
        teste <- teste[ - 1]
      }  
      if(info == "simplified") {
        complete <- 
          which(colnames(x) %in% c("NegCode", "Date", "NOMRES", "ESPECI",
                                   "Open", "Close", "High", "Low", "Mean", "Bid", "Ask",
                                   "Return", "QuaTot"))
        
        complete <- sort(unique(c(complete, which(unlist(lapply(names(x), function(i)
          length(unique(x[, i])) != 1))))))
        
        x <- x[, complete]
      }
    }
    
    rm(teste)
    
    if( ! quiet) {
      misspecified <- unique(misspecified)
      
      if( ! is.null(misspecified)) {
        if(length(misspecified) > 1) {
          miss <- paste(misspecified[ - length(misspecified)], collapse = ", ")
          miss <- paste(miss, misspecified[length(misspecified)], sep = " and ")
        } else {
          miss <- misspecified
        }
        
        
        i = sum(1, length(misspecified) > 1)
        miss <- paste(miss, c("is", "are")[i], "misspecified or",
                      c("this share does", "these shares do")[i], "not exist.")
        warning(miss)
        rm(i, miss)
      }
    }
    rm(misspecified)
    
    return(x)
  }
  
}
getAdjPrices <- function(shares,
                         by = "all", subscription = "rational",
                         info = "simplified", value = "Return", fill = NA) {
  # This function has as output the adjusted share prices (Volume is not changed)
  # Arguments:
  #   shares: A vector with the names of the shares or
  #           "all" to download shares with adjusted prices.
  #   by: A vector with the corporate events must be used in the adjust.
  #       Slipts and alterations in the factor of cotation are always adjusted.
  #     all = It uses all the events.
  #     Separately it's possible to specify in a vector a combination of these:
  #     Dividend, Interest, Bonus shares, Subscription Right, Spinoff, Return of Capital.
  #   subscription: There are three alternatives of specification for this argument:
  #     rational = buy the share when the price of subscriptied share is cheaper than
  #                the price of the share in the market
  #     neverbuy = self explanatory
  #     alwaysbuy = self explanatory
  #   info:
  #     full: returns a data.frame with the bovespa's quotation data.
  #     simplified: returns a data.frame with the open, close, min, max, mean, bid and ask prices,
  #            transactioned volume of the day and other informations that changed between the specified shares.
  #     single: returns a xts object with a single column specified in paramenter value.
  #   value: only used if info = "single".
  #          The setted argument is "Return", but it can be changed at will.
  #   fill: Only used in the same case that the value is used.
  #         Some of the shares aren't negotiated every period, so this parameter set how must be filled
  #         the missing values. The options are below:
  #     last: uses the last negotiated day value to fill the gaps.
  #     NA: fills with NAs.
  #     drop: Drop the days out of the time serie. Be carefull, the intersection of the time
  #           series could be empty.
  
  dicionario <- get.files("dicionariobruto")
  sharelinks <- get.files("linkscotacoes")
  dic2 <- get.files("linkseventos")
  
  if("all" %in% shares) {
    shares <- c(shares[shares != "all"], sharelinks[, 1])
    shares <- shares[substr(shares, 5, 999) %in% 3 : 8]
    shares <- 
      shares[substr(shares, 1, 4) %in%
               unique(unlist(apply(as.matrix(dicionario[dicionario[, 1] %in%
                                                 dic2[, 1], 5 : 8]), 2, unique)))]
    shares <- sort(unique(shares))
  }
  
  quiet <- F
  
  if( ! value %in%
      c("Date", "NegCode", "CODBDI", "NegCode",
        "TPMERC", "NOMRES", "ESPECI", "PRAZOT", 
        "Open", "High", "Low", "Mean", "Close",
        "Bid", "Ask", "Return", "TotNeg", "QuaTot", "Volume",
        "PREEXE", "INDOPC", "DATVEN", "PTOEXE", "CODISI")) {
    warning(paste("The parameter value was misspecified. One of these must be selected:",
                  paste(colnames(share)[ - 2], collapse = ", ") ))
    return()
  }
  
  envir <- NULL
  if(quiet == F & length(shares) > 10) {
    
    cat("It can take a while.\n")
    envir <- environment()
    progresso <- 0 : 10
    div <- 6
    
    progresso <- paste(paste0("", progresso * (100 / max(progresso)), "% "),
                       collapse = paste0(rep("| ", div), collapse = ""))
    progresso <- strsplit(progresso, " ")[[1]]
    prog.bar <- (1 : length(progresso)) / length(progresso)
    prog.bar <- prog.bar - min(prog.bar)
    prog.bar <- prog.bar / max(prog.bar)
  }
  
  teste <- lapply(shares, function(share) {
    
    matriz.ajuste <- which(dicionario[, c("codNeg", "LIVRE", "LIVRE.1", "LIVRE.2")] ==
                             substr(share, 1, 4), arr.ind = T)[, 1]
    matriz.ajuste <- unique(dicionario[matriz.ajuste, "CodigoCvm"])
    
    if(length(matriz.ajuste) == 0) {
      return(NULL)
      break
    }
    if( ! as.numeric(substr(share, 5, 5)) %in% 3 : 8) {
      return(NULL)
      break
    }
    
    if(length(matriz.ajuste) > 1)
      stop("VERIFICAR FUNCAO DE CORRECAO DOS RETORNOS")
    
    if(length(dic2[dic2$EMPRESA == matriz.ajuste, "LINK"]) == 0) {
      return(NULL)
      break
    }
    
    dist <- paste0(tempdir(), "\\omni\\m", matriz.ajuste)
    if( ! file.exists(dist)) {
      link <- paste0("https://dl.dropboxusercontent.com/s/", 
                     dic2[dic2$EMPRESA == matriz.ajuste, "LINK"], "/",
                     matriz.ajuste, ".csv?dl=0")
      download.file(link, dist,
                    mode = "wb", quiet = T)
      rm(link)
    }
    matriz.ajuste <- read.csv2(dist, stringsAsFactors = F, fileEncoding = "ISO8859-1")
    rm(dist)
    tipo <- c("fator.ON",  "fator.PN",  "fator.PNA",
              "fator.PNB", "fator.PNC", "fator.PND")
    if( length(intersect(tipo[as.numeric(substr(share, 5, 5)) - 2], colnames(matriz.ajuste))) == 0 ) {
      return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
      break
    }
    
    matriz.ajuste <- matriz.ajuste[, c("data", "evento",
                                       tipo[as.numeric(substr(share, 5, 5)) - 2])]
    
    matriz.ajuste[, "data"] <- as.Date(matriz.ajuste[, "data"])
    colnames(matriz.ajuste)[3] <- "fator"
    
    if(any(by %in% "all")) {
      by <- c("Dividend", "Interest", "Bonus shares", "Subscription Right",
               "Spinoff", "Return of Capital")
    }
    if(any(by %in% "Dividend")) {
      by <- c(by, "Dividendo", "Dividendo mensal", "DIVIDENDO")
    }
    if(any(by %in% "Interest")) {
      by <- c(by, "Juros", "Juros mensal", "JRS CAP PROPRIO")
    }
    if(any(by %in% "Bonus shares")) {
      by <- c(by, "Bonificacao")
    }
    if(any(by %in% "Subscription Right")) {
      by <- c(by, "Subscricao")
    }
    if(any(by %in% "Spinoff")) {
      by <- c(by, "Cisao")
    }
    if(any(by %in% "Return of Capital")) {
      by <- c(by, "Restituicao de capital", "REST CAP DIN")
    }
    
    if("Subscription Right" %in% by) {
      x <- which(matriz.ajuste[, "evento"] == "Subscricao")
      if(subscription == "rational") {
        x <- x[matriz.ajuste[x, "fator"] < 1]
        if(length(x) > 0)
          matriz.ajuste <- matriz.ajuste[ - x, ]
      }
      if(subscription == "neverbuy") {
        matriz.ajuste <- matriz.ajuste[ - x, ]
      }
      rm(x)
    }
    
    share <- get.shares(share, envir = envir)[[1]]
    
    if( ! is.null(share) & ! is.character(share)) {
      colnames(share)[c(1, 3)] <- c("Date", "NegCode")
      base <- as.matrix(share[, c("Date", "NegCode")])
      share <- as.xts(share[, - 1], order.by = as.Date(share[, "Date"]))
      
      if(nrow(matriz.ajuste) > 0)
        matriz.ajuste[which(regexec("Cisao", matriz.ajuste[, "evento"]) != "-1"), "evento"] <- "Cisao"
      x <- substr(matriz.ajuste[, "evento"], 1, 22) %in% by
      if(any(x)) {
        x <- matriz.ajuste[x, ]
        x <- t(data.frame(lapply(unique(x[, "data"]), function(z) {
          cisao <- ((1 / (1 - sum(1 - (1 / (x[x[, "data"] %in% z &
                                                x[, "evento"] == "Cisao", "fator"]))))) - 1)
          comum <- (as.numeric(x[x[, "data"] %in% z & x[, "evento"] != "Cisao", "fator"]) - 1)
          z <- t(data.frame(data = z, fator = sum(cisao, comum) + 1, stringsAsFactors = F))
          return(z)
        }), stringsAsFactors = F))
        rownames(x) <- NULL
        x <- as.data.frame(x, stringsAsFactors = F)
        x[, "data"] <- as.Date(x[, "data"])
        x[, "fator"] <- as.numeric(x[, "fator"])
      } else {
        x <- NULL
      }
      
      matriz.ajuste <- rbind(x, matriz.ajuste[matriz.ajuste[, "evento"] %in%
                                                c("alt.fator.cot", "Grupamento", "Desdobramento",
                                                  "bonificacao ou desdobramento"), c("data", "fator")])
      rm(x)
      
      matriz.ajuste <- 
        matriz.ajuste[matriz.ajuste[, "data"] <= max(index(share)) &
                        matriz.ajuste[, "data"] >= min(index(share)), ]
      
      x <- c("Open", "Close", "High", "Low", "Mean", "Bid", "Ask", "Volume")
      
      for(i in 1 : nrow(matriz.ajuste)) {
        p <- index(share) <= matriz.ajuste[i, "data"]    
        share[p, x] <- as.numeric(share[p, x]) / as.numeric(matriz.ajuste[i, "fator"])
      }
      share[, "Volume"] <- as.numeric(share[, "Volume"]) * as.numeric(share[, "FATCOT"])
      share <- share[, - which(colnames(share) == "FATCOT")]
      
      
      
      
      share <- cbind(data.frame(share[, 1 : 13], stringsAsFactors = F), 
                     Return = as.numeric(c(NA, as.numeric(share[ - 1, "Close"]) /
                                               as.numeric(share[ - nrow(share), "Close"]))),
                     data.frame(share[, 14 : ncol(share)], stringsAsFactors = F))
      share <- cbind(base, share)
      for(i in c(x, "CODBDI", "TPMERC", "Return", "TotNeg", "QuaTot", "PREEXE")) {
        share[, i] <- as.numeric(share[, i])
      }
      
      return(share)
      
    } else {
      
      return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
      
    }
    
  })
  
  if(info == "single")
    for(i in seq_along(teste))
      if(is.data.frame(teste[[i]]))
        if(any(duplicated(teste[[i]][, "Date"]))) {
          warning("Some of the assets belongs to the forward market or they have a duplicated date.
                  Due this the output is goind to be a data.frame.")
          info <- "simplified"
          break
        }
  
  names(teste) <- shares
  x = misspecified = NULL
  
  if( ! (is.null(unlist(teste)))) {
    if(info == "single") {
      for(i in seq_along(teste)) {
        if(exists("prog.bar")) {
          if(names(teste)[1] == shares[[1]]) cat("Progress of aggregation: \n")
          P <- c(which(shares == names(teste)[1]) - (1 : 0)) / length(shares)
          cat(paste(progresso[prog.bar >= P[1] & prog.bar < P[2]], collapse = ""))
          if(names(teste)[1] == tail(shares, 1)) cat("100%\n")
        }
        if(is.null(dim(teste[[1]]))) {
          misspecified <- c(misspecified, names(teste)[1])
        } else {
          share <- teste[[1]]
          
          if(value %in% colnames(share)) {
            share <- xts(share[, value], order.by = as.Date(share$Date))
          } else {
            warning(paste("The parameter value was misspecified. One of these must be selected:",
                          paste(colnames(share)[ - 2], collapse = ", ") ))
            return()
          }
          
          x <- cbind(x, share)
          colnames(x)[ncol(x)] <- shares[[i]]
          rm(share)
        }
        
        teste <- teste[ - 1]
      }
      
      if( ! is.na(fill)) {
        if(fill == "last") {
          for(i in colnames(x)) {
            if(value == "Return") {
              t <- is.na(x[, i])
              t.1 <- which( ! t)
              if(length(t.1) == 0) next
              t.1 <- c(head(t.1, 1), tail(t.1, 1))
              t <- which(t)
              t <- t[t > t.1[1] & t < t.1[2]]
              x[t, i] <- 1
            } else {
              t <- which(is.na(x[, i]))
              if(length(t) != 0) {
                t.1 <- c(which((t[-1] - t[ - length(t)] > 1)), length(t)) + 1
                
                for(j in seq_along(t.1))
                  if(t[max(t.1[j - 1], 1)] - 1 != 0)
                    x[t[max(t.1[j - 1], 1)] : t[t.1[j] - 1], i] <- x[t[max(t.1[j - 1], 1)] - 1, i]
              }
            }
          }
        }
        if(fill == "drop") {
          if(value == "Return") {
            na <- apply(x, 1, function(i) any(is.na(i)))
            if( ! all(na))
              x <- x[min(which( ! na)) : max(which( ! na)), ]
            na <- apply(x, 1, function(i) any(is.na(i)))
            if(nrow(x[ ! na, ]) > 0) {
              na <- sort(which(na), decreasing = T)
              for(i in na) {
                x[i + 1, ] <- apply(x[i : (i + 1)], 2, function(j) prod(as.numeric(j), na.rm = T))
                x <- x[ - i, ]
              }
              rm(i)
            } else {
              x <- x[ ! na, ]
            }
            
          } else {
            na <- apply(x, 1, function(i) any(is.na(i)))
            x <- x[ ! na, ]
          }
          rm(na)
        }
      }
    }
    if(info %in% c("full", "simplified")) {
      for(i in seq_along(teste)) {
        
        if(exists("prog.bar")) {
          if(names(teste)[1] == shares[[1]]) cat("Progress of aggregation: \n")
          P <- c(which(shares == names(teste)[1]) - (1 : 0)) / length(shares)
          cat(paste(progresso[prog.bar >= P[1] & prog.bar < P[2]], collapse = ""))
          if(names(teste)[1] == tail(shares, 1)) cat("100%\n")
        }
        if(is.null(dim(teste[[1]]))) { 
          misspecified <- c(misspecified, shares[[i]])
        } else {
          share <- teste[[1]]
          share <- cbind(NegCode = share[, "NegCode"], Date = share[, "Date"],
                         DeadLine = share[, "PRAZOT"], ExpirDate = share[, "DATVEN"],
                         share[, - which(colnames(share) %in%
                                           c("NegCode", "Date", "PRAZOT", "DATVEN"))])
          rownames(share) <- NULL
          x <- rbind(x, share)
          rm(share)
        }
        teste <- teste[ - 1]
      }  
      if(info == "simplified") {
        complete <- 
          which(colnames(x) %in% c("NegCode", "Date", "NOMRES", "ESPECI",
                                   "Open", "Close", "High", "Low", "Mean", "Bid", "Ask",
                                   "Return", "QuaTot"))
        
        complete <- sort(unique(c(complete, which(unlist(lapply(names(x), function(i)
          length(unique(x[, i])) != 1))))))
        
        x <- x[, complete]
      }
    }
  }
  
  rm(teste)
  misspecified <- unique(misspecified)
  
  if( ! quiet) {
    
    if( ! is.null(misspecified)) {
      if(length(misspecified) > 1) {
        miss <- paste(misspecified[ - length(misspecified)], collapse = ", ")
        miss <- paste(miss, misspecified[length(misspecified)], sep = " and ")
      } else {
        miss <- misspecified
      }
      
      i = sum(1, length(misspecified) > 1)
      miss <- paste(miss, c("is", "are")[i], "misspecified or we don't have the adjusted price or even",
                    c("this share does", "these shares do")[i], "not exist.")
      warning(miss)
      rm(i, miss)
    }
  }
  
  rm(misspecified)
  
  return(x)
}
getFinancialStatements <- function(firms, quarter = NULL) {
  # This functions load to R a table with the value of the most important accounts of the specified firm.
  # Arguments:
  #   firms: value that must have one of the values below:
  #     CVM code: uses the CVM code to search for the firm.
  #       Format: 123456.
  #       Examples (Itau Unibanco): getBalancesheet(firm = 19348) or getBalancesheet(firm = 019348)
  #     CNPJ: Search for the firm using their CNPJ (Cadastro Nacional de Pessoa Juridica - National Registry of Legal Person).
  #       Format: '12.345.678/9012-34'
  #       Example (Itau Unibanco): getBalancesheet(firm = '60.872.504/0001-23')
  #     Negotiation code: Uses the Negotiation Code to find the firm's balance sheet.
  #       Format: 'ABCD1'
  #       Example (Itau Unibanco): getBalancesheet(firm = 'ITUB3')
  #       P.S.: Inside the function, only the first four characters are used.
  #     Obs.: You can use firms = "all" too.
  #   quarter: If setted as NULL the function return a table with the principal accounts of
  #            all balances, some of this accound were calculed to make easier the use of the data
  #            The format to this parameter is: "yyyy-mm". Example,: "2016-12".
  
  
  if (missingArg(firms)) {
    warning ("You need to chose at list a firm.")
  } else {
    
    teste <- get.fin.stat(firms, quarter)
    
    x = misspecified = NULL
    for(i in seq_along(teste)) {
      if(is.null(dim(teste[[i]]))){
        misspecified <- c(misspecified, firms[[i]])
      } else {
        x <- rbind(x, teste[[i]])
      }
    }
    
    if(is.null(quarter))
      x <- x[, c(3, 6, 1 : 2, 4 : 5, 7 : ncol(x))]
    
    misspecified <- unique(misspecified)
    
    if( ! is.null(misspecified)) {
      if(length(misspecified) > 1) {
        miss <- paste(misspecified[ - length(misspecified)], collapse = ", ")
        miss <- paste(miss, misspecified[length(misspecified)], sep = " and ")
      } else {
        miss <- misspecified
      }
      
      
      i = sum(1, length(misspecified) > 1)
      miss <- paste(miss, c("is", "are")[i], "misspecified or",
                    c("this firm does", "these firms do")[i], "not exist.")
      warning(miss)
      rm(i, miss)
    }
    rm(misspecified)
    
    return(x)
  }
}

getBalanceSheet <- function(firms, quarter = NULL) {
  if (missingArg(firms)) {
    warning ("You need to chose at list a firm.")
  } else {
    FIRMS <- get.fin.stat(firms, quarter)
    
    x = misspecified = NULL
    for(i in seq_along(FIRMS)) {
      if(is.null(dim(FIRMS[[i]]))) {
        misspecified <- c(misspecified, firms[[i]])
      } else {
        FIRM <- FIRMS[[i]]
        if(is.null(quarter)) {
          col <- gsub(".individual|.consolidado", "", names(FIRM))
          p1 <- which(col == "ativo.total")
          p2 <- which(col == "resultado")
          x <- rbind(x, FIRM[, c("CodigoCvm", "DataReferenciaDocumento",
                                 names(FIRM)[c(p1[[1]] : (p2[[1]] - 1), p1[[2]] : (p2[[2]] - 1))])])
          rm(p1, p2, FIRM, col)
        } else {
          x <- FIRM[c(which(FIRM[, "name of account"] %in% c("CodigoCvm", "DataReferenciaDocumento")),
                      which(substr(FIRM[, "id of account"], 1, 1) %in% c(1, 2))), ]
        }
      }
    }
    
    if(is.null(quarter)) {
      x[, "CodigoCvm"] <- as.numeric(x[, "CodigoCvm"])
      x[, "DataReferenciaDocumento"] <- as.Date(x[, "DataReferenciaDocumento"])
      x[, 3 : ncol(x)] <- as.numeric(as.matrix(x[, 3 : ncol(x)]))
      x <- x[, unlist(apply(x, 2, function(i) ! all(is.na(i))))]
    }
    
    misspecified <- unique(misspecified)
    
    if( ! is.null(misspecified)) {
      if(length(misspecified) > 1) {
        miss <- paste(misspecified[ - length(misspecified)], collapse = ", ")
        miss <- paste(miss, misspecified[length(misspecified)], sep = " and ")
      } else {
        miss <- misspecified
      }
      
      
      i = sum(1, length(misspecified) > 1)
      miss <- paste(miss, c("is", "are")[i], "misspecified or",
                    c("this firm does", "these firms do")[i], "not exist.")
      warning(miss)
      rm(i, miss)
    }
    rm(misspecified)
    
    return(x)
  }
}

getIncomeStatements <- function(firms, quarter = NULL, adjust = F) {
  if (missingArg(firms)) {
    warning ("You need to chose at list a firm.")
  } else {
    FIRMS <- get.fin.stat(firms, quarter)
    
    x = misspecified = NULL
    for(i in seq_along(FIRMS)) {
      if(is.null(dim(FIRMS[[i]]))) {
        misspecified <- c(misspecified, firms[[i]])
      } else {
        FIRM <- FIRMS[[i]]
        if(is.null(quarter)) {
          col <- gsub(".individual|.consolidado", "", names(FIRM))
          p1 <- which(col == "resultado")
          p2 <- which(col == "resultado.abrangente")
          x <- rbind(x, FIRM[, c("CodigoCvm", "DataReferenciaDocumento", "TipoDemonstracaoFinanceira",
                                 names(FIRM)[c(p1[[1]] : (p2[[1]] - 1), p1[[2]] : (p2[[2]] - 1))])])
          rm(p1, p2, FIRM, col)
        } else {
          x <- FIRM[c(which(FIRM[, "name of account"] %in% c("CodigoCvm", "DataReferenciaDocumento", "TipoDemonstracaoFinanceira")),
                      which(substr(FIRM[, "id of account"], 1, 1) %in% 3)), ]
        }
      }
    }
    rm(FIRMS)
    
    if(is.null(quarter)) {
      x[, "CodigoCvm"] <- as.numeric(x[, "CodigoCvm"])
      x[, "DataReferenciaDocumento"] <- as.Date(x[, "DataReferenciaDocumento"])
      x[, 4 : ncol(x)] <- as.numeric(as.matrix(x[, 4 : ncol(x)]))
      x <- x[, unlist(apply(x, 2, function(i) ! all(is.na(i))))]
      
      if(adjust) {
        quarters <- paste(sort(rep(1997 : as.numeric(substr(Sys.time(), 1, 4)), 4)),
                          c("03", "06", "09", "12"), sep = "-")
        x <- lapply(unique(x[, "CodigoCvm"]), function(i) return(x[x[, "CodigoCvm"] == i, ]))
        x <- lapply(x, function(i) {
            for(l in 1 : nrow(i)) {
              if(i[l, "TipoDemonstracaoFinanceira"] == "ITR") {
                i[l, "TipoDemonstracaoFinanceira"] <- T
                next
              }
              q <- quarters %in% substr(i[l, "DataReferenciaDocumento"], 1, 7)
              f <- unlist(lapply(quarters[which(q) - c(3 : 1)], function(p) {
                f <- which(substr(i[, "DataReferenciaDocumento"], 1, 7) == p)
                if(length(f) == 0) {
                  return(NULL)
                } else {
                  return(max(f))
                }
              }))
              if(length(f) < 3) {
                i[l, "TipoDemonstracaoFinanceira"] <- F
                next
              } else {
                i[l, 4 : ncol(i)] <- i[l, 4 : ncol(i)] - colSums(i[f, 4 : ncol(i)])
                i[l, "TipoDemonstracaoFinanceira"] <- T
              }
            }
            rm(q, l, f)
            return(i)
          })
        FIRMS <- x
        x <- NULL
        for(i in seq_along(FIRMS))
          x <- rbind(x, FIRMS[[i]])
        x <- x[(x[, "TipoDemonstracaoFinanceira"]) == TRUE, c(1 : 2, 4 : ncol(x))]
        rm(quarters, FIRMS)
      }
      
    }
    
    misspecified <- unique(misspecified)
    
    if( ! is.null(misspecified)) {
      if(length(misspecified) > 1) {
        miss <- paste(misspecified[ - length(misspecified)], collapse = ", ")
        miss <- paste(miss, misspecified[length(misspecified)], sep = " and ")
      } else {
        miss <- misspecified
      }
      
      
      i = sum(1, length(misspecified) > 1)
      miss <- paste(miss, c("is", "are")[i], "misspecified or",
                    c("this firm does", "these firms do")[i], "not exist.")
      warning(miss)
      rm(i, miss)
    }
    rm(misspecified)
    
    return(x)
  }
}

info.search <- function(info = "", ...) {
  # This function searchs about the firms and their shares and derivatives
  # Arguments:
  #   info: any information about the firm or asset.
  
  info <- toupper(info)
  
  dicionario <- get.files("dicionariocompleto")
  
  if(all(strsplit(as.character(info), "")[[1]] %in% as.character(0:9))) {
    x <- grep(info, dicionario$CodigoCvm)
  } else {
    x <- c(unlist(lapply( 2 : 3, function(j) grep(info, dicionario[, j]))),
           grep(substr(info, 1, 4), dicionario[, 4]))
  }
  
  if(length(x) == 0) {
    return(NULL)
    break
  }
  
  x <- sort(table(x), decreasing = T)
  dicionario <- dicionario[names(x), ]
  
  x <- apply(dicionario, 1, function(dic) {
    
    x <- strsplit(strsplit(dic["INFORMACOES"], ";")[[1]], " = ")
    
    parte1 <- cbind(CVMCode = dic["CodigoCvm"],
                    CNPJ = paste0(strsplit(dic["CNPJ"], ";")[[1]], collapse = ""),
                    SocialName = last(strsplit(dic["RazaoSocial"], "; ")[[1]]),
                    NegotiationCode = paste(unlist(lapply(1 : length(x), function(j) x[[j]][1])),
                                            collapse = " "))
    
    parte2 <- cbind("Used Social Names" = strsplit(dic["RazaoSocial"], "; ")[[1]])
    
    
    parte <- lapply(x, function(j) {
      j.1 <- matrix(strsplit(j[2], " ")[[1]], ncol = 3, byrow = T)
      j.1[, 1] <- paste0(j[1], j.1[, 1])
      j.1 <- data.frame(j.1, stringsAsFactors = F)
      names(j.1) <- c("share-derivative", "FirstTimeNegociated", "LastTimeNegociated")
      return(j.1)
    })
    
    parte3 <- NULL
    for(j in seq_along(parte)) {
      parte3 <- rbind(parte3, parte[[j]])
    }
    
    parte <- list(parte1, parte2, parte3)
    
    names(parte) <- c("Information", "Names", "Share")
    
    return(parte)
    
  })
  
  names(x) <- unlist(lapply(seq_along(x), function(i) strsplit(x[[i]][[1]][4], " ")[[1]][[1]]))
  
  painel <- NULL
  for(i in seq_along(x))
    painel <- rbind(painel, x[[i]][[1]])
  
  rownames(painel) <- NULL
  painel <- data.frame(painel, stringsAsFactors = F)
  
  painel$CVMCode <- as.numeric(painel$CVMCode)
  
  x <- (c(list(painel), x))
  
  names(x)[1] <- "summary"
  
  print(x$summary)
  
  return(invisible(x))
  
}

