if( ! "xts" %in% installed.packages()) install.packages("xts", dependencies = T)
library(xts)

get.files <- function(link) {
  # This is a function for internal uses!
  
  if( ! dir.exists(tempdir())) # Yes, some times occurs erros because of the inexistence of this directory
    dir.create(tempdir(), showWarnings = F)
  
  if(file.exists(paste0(tempdir(), "\\", link, ".txt"))) {
    file <- read.table(paste0(tempdir(), "\\", link, ".txt"),
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
    write.table(file, paste0(tempdir(), "\\", link, ".txt"), row.names = F)
  }
  return(file)
}

get.shares <- function(shares) {
  # This is a function used in other functions.
  
  if( ! exists("sharelinks"))
    sharelinks <- get.files("linkscotacoes")
  
  
  if(length(shares) > 20) {
    
    cat("It can take a while.\n")
    
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
    
    if(length(shares) > 20) {
      x <- c(which(shares == share) - (1 : 0)) / length(shares)
      cat(paste(progresso[(x[1] == 0 | prog.bar > x[1]) & prog.bar <= x[2]], collapse = ""))
    }
    
    x <- sharelinks[sharelinks$PAPEL == share, "LINK"]
    if(length(x) == 0) {
      return("This share is misspecified or does not exist.")
    } else {
      
      x <- paste0("https://dl.dropboxusercontent.com/s/",
                  x, "/", share, ".csv?dl=0")
      
      download.file(x, destfile = paste0(tempdir(), "\\", share), mode = "wb")
      x <- readRDS(paste0(tempdir(), "\\", share))
      unlink(paste0(tempdir(), "\\", share))
      
      names(x)[8:17] <- c("Open", "High", "Low", "Mean", "Close",
                          "Bid", "Ask", "TotNeg", "QuaTot", "Volume")
      
      x$DATAPREG <- as.Date(as.character(x$DATAPREG), "%Y%m%d")
      
      return(x)
    }
  })
  
  return(teste)
}

getPrices <- function(shares,
                      as = "panel", complete = F, value = "Close", fill = NA) {
  # Returns the diary infomation about the prices and
  # the transactioned volume of the share.
  # Arguments:
  #   shares: the name of the shares and/or a combination of the options below:
  #           shares, rights and receipts, real estate funds, private bonus,
  #           insolvensy (shares of the firms that in some moment between 1998 and now were in insolvency state),
  #           debentures (also in this class there are investment certificates and public debt titles),
  #           forward market, options, futures and fractionary.
  #     Eg.: getPrices(shares = c("VALE3", "options", "futures"))
  #   as:
  #     panel: returns a data.frame with the asset name in the first column and the date in the second.
  #     xts: if only one share was chosen, returns all the prices in the columns.
  #          else returns a xts with, if not specified differently,
  #          the close price of the shares in the columns.
  #   complete:
  #     TRUE: Returns all information we have about the chosen shares.
  #     FALSE: Returns the open, close, min, max, mean, bid and ask prices,
  #            transactioned volume of the day and other informations that changed between the shares.
  #   value: only used if as = "xts" and there are two or more shares selected.
  #          The setted input is the "Close" price, but it can be changed at will.
  #   fill: Only used in the same case that the value is used.
  #         Some of the shares aren't negotiated every period, so this parameter set how must be filled
  #         the missing values. The options are below:
  #     last: uses the last negotiated day value to fill the gaps.
  #     NA: fills with NAs.
  #     drop: Drop the day out of the time serie.
  
  if (missingArg(shares)) {
    warning ("You have to specify at list a share.")
  } else {
    
    if( ! complete %in% c(T, F)) {
      warning("The argument complete must be TRUE or FALSE")
      return()
    }
    
    sharelinks <- get.files("linkscotacoes")
    
    if("all" %in% shares) {
      quiet = T
      shares <- c(shares[shares != "all"], sharelinks[, 1])
    } else {
      quiet = F
    }
    if(any( ! shares %in% sharelinks[, 1]))
      codbdi = get.files("codbdi")
    
    if(any(shares %in% "shares"))
      shares <- c(shares[ ! shares %in% "all.shares"],
                  strsplit(codbdi[codbdi[, 1] ==  2, 3], ", ")[[1]])
    
    if(any(shares %in% "insolvency"))
      shares <- c(shares[ ! shares %in% "insolvency"],
                  strsplit(codbdi[codbdi[, 1] ==  6, 3], ", ")[[1]])
    
    if(any(shares %in% "rights and receipts"))
      shares <- c(shares[ ! shares %in% "rights and receipts"],
                  strsplit(codbdi[codbdi[, 1] == 10, 3], ", ")[[1]])
    
    if(any(shares %in% "real estate funds"))
      shares <- c(shares[ ! shares %in% "real estate funds"],
                  strsplit(codbdi[codbdi[, 1] == 12, 3], ", ")[[1]])
    
    if(any(shares %in% "debentures"))
      shares <- c(shares[ ! shares %in% "debentures"],
                  strsplit(codbdi[codbdi[, 1] %in% c(14, 66, 68, 83), 3], ", ")[[1]])
    
    if(any(shares %in% "private bonus"))
      shares <- c(shares[ ! shares %in% "private bonus"],
                  strsplit(codbdi[codbdi[, 1] == 22, 3], ", ")[[1]])
    
    if(any(shares %in% "options"))
      shares <- c(shares[ ! shares %in% "options"],
                  strsplit(codbdi[codbdi[, 1] %in% c(32, 33, 38, 42, 74, 75, 78,
                                                     82), 3], ", ")[[1]])
    if(any(shares %in% "auctions"))
      shares <- c(shares[ ! shares %in% "auctions"],
                  strsplit(codbdi[codbdi[, 1] %in% c(46, 48, 50, 51, 52, 53, 54,
                                                     56), 3], ", ")[[1]])
    if(any(shares %in% "forward market"))
      shares <- c(shares[ ! shares %in% "forward market"],
                  strsplit(codbdi[codbdi[, 1] == 62, 3], ", ")[[1]])
    
    if(any(shares %in% "futures"))
      shares <- c(shares[ ! shares %in% "futures"],
                  strsplit(codbdi[codbdi[, 1] %in% c(70, 71), 3], ", ")[[1]])
    
    if(any(shares %in% "fractionary"))
      shares <- c(shares[ ! shares %in% "fractionary"],
                  strsplit(codbdi[codbdi[, 1] == 96, 3], ", ")[[1]])
    
    
    shares <- sort(unique(shares))
    
    if(length(shares) > 1 & as == "xts") {
      value <- value[[1]]
      fill  <- fill [[1]]
      
      if( ! fill %in% c("last", NA, "drop")) {
        warning("fill must be setted as last, NA or drop.")
        return()
      }
    }
    
    teste <- get.shares(shares)
    
    x = misspecified = NULL
    
    if(as == "xts")
      for(i in seq_along(teste))
        if(any(duplicated(teste[[i]][, "DATAPREG"]))) {
          warning("Some of the assets belongs to the forward market or it has a duplicated date.
                  Due this the output is goind to be a panel.")
          as <- "panel"
          break
        }
    
    if(as == "xts") {
      
      for(i in seq_along(teste)) {
        if(is.null(dim(teste[[i]]))) {
          misspecified <- c(misspecified, shares[[i]])
        } else {
          share <- teste[[i]]
          
          if(length(shares) > 1) {
            
            if(value %in% colnames(share)){
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
            
          } else {
            
            x <- share
            if( ! complete) {
              x <- x[, unlist(lapply(names(x), function(i) length(unique(x[, i])) != 1))]
              j <- NULL
              for(i in colnames(x)) {
                if(class(x[, i]) != "character")
                  j <- c(j, i)
              }
              x <- x[, j]
              rm(j)
            }
            x <- xts(x, order.by = as.Date(rownames(x)))
          }
          rm(share)
        }
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
    
    if(as == "panel") {
      
      for(i in seq_along(teste)) {
        if(is.null(dim(teste[[i]]))) {
          misspecified <- c(misspecified, shares[[i]])
        } else {
          share <- teste[[i]]
          share <- cbind(NegCode = share[, 3], Date = share[, 1], DeadLine = share[, 7],
                         ExpirDate = share[, 20], share[, c(2, 4 : 6, 8 : 19, 21 : ncol(share))])
          rownames(share) <- NULL
          x <- rbind(x, share)
          rm(share)
        }
      }
      
      if( ! complete) {
        complete <- 
          which(colnames(x) %in% c("NegCode", "Date", "Open", "Close", "High",
                                   "Low", "Mean", "Bid", "Ask"))
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
                         as = "panel", value = "Close", fill = NA) {
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
  #   as:
  #     panel: returns a data.frame with the asset name in the first column and the date in the second.
  #     xts: if only one share was chosen, returns all the prices in the columns.
  #          else returns a xts with, if not specified differently,
  #          the close price of the shares in the columns.
  #            transactioned volume of the day and other informations that changed between the shares.
  #   value: only used if as = "xts" and there are two or more shares selected.
  #          The setted input is the "Close" price, but it can be changed at will.
  #   fill: Only used in the same case that the value is used.
  #         Some of the shares aren't negotiated every period, so this parameter set how must be filled
  #         the missing values. The options are below:
  #     last: uses the last negotiated day value to fill the gaps.
  #     NA: fills with NAs.
  #     drop: Drop the day out of the time serie.
  
  
  por <- by
  subscricao <- subscription
  complete <- FALSE
  dicionario <- get.files("dicionariobruto")
  
  sharelinks <- get.files("linkscotacoes")
  
  if("all" %in% shares) {
    quiet = T
    shares <- c(shares[shares != "all"], sharelinks[, 1])
    shares <- shares[substr(shares, 5, 999) %in% 3 : 8]
  } else {
    quiet = F
  }
  
  if(length(shares) > 20) {
    
    cat("It can take a while.\n")
    
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
    
    if(exists("prog.bar")) {
      x <- c(which(shares == share) - (1 : 0)) / length(shares)
      cat(paste(progresso[prog.bar >= x[1] & prog.bar < x[2]], collapse = ""))
    }
    
    matriz.ajuste <- which(dicionario[, c("codNeg", "LIVRE", "LIVRE.1", "LIVRE.2")] ==
                             substr(share, 1, 4), arr.ind = T)[, 1]
    matriz.ajuste <- unique(dicionario[matriz.ajuste, "CodigoCvm"])
    
    if(length(matriz.ajuste) == 0) {
      return(NULL)
      break
    }
    
    if(length(matriz.ajuste) > 1)
      stop("VERIFICAR FUNCAO DE CORRECAO DOS RETORNOS")
    
    dic2 <- get.files("linkseventos")
    
    if(length(dic2[dic2$EMPRESA == matriz.ajuste, "LINK"]) == 0) {
      return(NULL)
      break
    }
    
    matriz.ajuste <- paste0("https://dl.dropboxusercontent.com/s/", 
                            dic2[dic2$EMPRESA == matriz.ajuste, "LINK"], "/",
                            matriz.ajuste, ".csv?dl=0")
    
    matriz.ajuste <- read.csv2(url(matriz.ajuste), stringsAsFactors = F, fileEncoding = "ISO8859-1")
    
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
    
    if(any(por %in% "all")) {
      por <- c("Dividend", "Interest", "Bonus shares", "Subscription Right",
               "Spinoff", "Return of Capital")
    }
    if(any(por %in% "Dividend")) {
      por <- c(por, "Dividendo", "Dividendo mensal", "DIVIDENDO")
    }
    if(any(por %in% "Interest")) {
      por <- c(por, "Juros", "Juros mensal", "JRS CAP PROPRIO")
    }
    if(any(por %in% "Bonus shares")) {
      por <- c(por, "Bonificacao")
    }
    if(any(por %in% "Subscription Right")) {
      por <- c(por, "Subscricao")
    }
    if(any(por %in% "Spinoff")) {
      por <- c(por, "Cisao")
    }
    if(any(por %in% "Return of Capital")) {
      por <- c(por, "Restituicao de capital", "REST CAP DIN")
    }
    
    if("Subscription Right" %in% por) {
      x <- which(matriz.ajuste[, "evento"] == "Subscricao")
      if(subscricao == "rational") {
        x <- x[matriz.ajuste[x, "fator"] < 1]
        if(length(x) > 0)
          matriz.ajuste <- matriz.ajuste[ - x, ]
      }
      if(subscricao == "neverbuy") {
        matriz.ajuste <- matriz.ajuste[ - x, ]
      }
      rm(x)
    }
    
    share <- get.shares(share)[[1]]
    colnames(share)
    
    if( ! is.null(share)) {
      colnames(share)[c(1, 3)] <- c("Date", "NegCode")
      base <- as.matrix(share[, c("Date", "NegCode")])
      share <- as.xts(share[, c("Open", "Close", "High", "Low", "Mean", "Bid", "Ask",
                                "Volume")], order.by = as.Date(share[, "Date"]))
      
      if(nrow(matriz.ajuste) > 0) {
        
        matriz.ajuste[which(regexec("Cisao", matriz.ajuste[, "evento"]) != "-1"), "evento"] <- "Cisao"
        x <- substr(matriz.ajuste[, "evento"], 1, 22) %in% por
        if(any(x)) {
          x <- matriz.ajuste[x, ]
          x <- t(data.frame(lapply(unique(x[, "data"]), function(z) {
            z <- t(data.frame(data = z, fator = sum(as.numeric(x[x[, "data"] %in% z, "fator"]) - 1) + 1,
                              stringsAsFactors = F))
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
        
        x <- c("Open", "Close", "High", "Low", "Mean", "Bid", "Ask")
        
        for(i in 1 : nrow(matriz.ajuste)) {
          p <- index(share) <= matriz.ajuste[i, "data"]    
          share[p, x] <- as.numeric(share[p, x]) / as.numeric(matriz.ajuste[i, "fator"])
        }
        
      }
      
      share <- cbind(base, as.data.frame(share))
      
      return(share)
      
    } else {
      
      return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
      
    }
    
  })
  
  x = misspecified = NULL
  
  if(as == "panel") {
    for(i in seq_along(teste)) {
      if(is.null(dim(teste[[i]]))) {
        misspecified <- c(misspecified, shares[[i]])
      } else {
        share <- teste[[i]]
        rownames(share) <- NULL
        x <- rbind(x, share)
      }
    }
    
    if( ! complete) {
      complete <- 
        which(colnames(x) %in% c("NegCode", "Date", "Open", "Close", "High",
                                 "Low", "Mean", "Bid", "Ask"))
      complete <- sort(unique(c(complete, which(unlist(lapply(names(x), function(i)
        length(unique(x[, i])) != 1))))))
      
      x <- x[, complete]
    }
  }
  
  if(as == "xts") {
    
    for(i in seq_along(teste)) {
      if(is.null(dim(teste[[i]]))) {
        misspecified <- c(misspecified, shares[[i]])
      } else {
        share <- teste[[i]]
        
        if(length(shares) > 1) {
          
          if(value %in% colnames(share)){
            share <- xts(share[, value], order.by = as.Date(rownames(share)))
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
          
        } else {
          x <- as.xts(share[, - c(1 : 2)], order.by = as.Date(share[, "Date"]))
        }
        rm(share)
      }
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
    
    if( ! complete) {
      complete <- 
        which(colnames(x) %in% c("NegCode", "Open", "Close", "High",
                                 "Low", "Mean", "Bid", "Ask"))
      complete <- sort(unique(c(complete, which(unlist(lapply(names(x), function(i)
        length(unique(x[, i])) != 1))))))
      
      x <- x[, complete]
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

getBalanceSheet <- function(firms, quarter = NULL) {
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
    
    if( ! is.null(quarter) & length(firms) > 1) {
      quarter <- NULL
      warning("Argument quarter is not applied when there is more than one code.")
    }
    
    
    dicionario <- get.files("dicionariobruto")
    
    if("all" %in% firms) {
      firms <- sort(c(firms[firms != "all"], unique(dicionario[, 1])))
    }
    
    if(all(firms %in% 3)) {
      warning("The CVM code 3 is a example, This firm actually does not exists.")
      return(NULL)
    } else {
      firms <- firms[firms != 3]
    }
    
    if(length(firms) > 20) {
      
      cat("It can take a while.\n")
      
      progresso <- 0 : 10
      div <- 6
      
      progresso <- paste(paste0("", progresso * (100 / max(progresso)), "% "),
                         collapse = paste0(rep("| ", div), collapse = ""))
      progresso <- strsplit(progresso, " ")[[1]]
      prog.bar <- (1 : length(progresso)) / length(progresso)
      prog.bar <- prog.bar - min(prog.bar)
      prog.bar <- prog.bar / max(prog.bar)
      
    }
    
    teste <- lapply(firms, function(firm) {
      
      
      if(length(firms) > 20) {
        x <- c(which(firms == firm) - (1 : 0)) / length(firms)
        cat(paste(progresso[(x[1] == 0 | prog.bar > x[1]) & prog.bar <= x[2]], collapse = ""))
        if(x[[2]] == 1) cat("\n")
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
        
        # print(dicionario[x, c("CodigoCvm", "NomeRazaoSocial", "NumeroCnpjCompanhiaAberta", "codNeg")])
        
        nome <- paste(dicionario[x, "codNeg"], sep=".")
        if (nome == "NA" | is.na(nome))
          nome <- paste("CVM", dicionario[x, "CodigoCvm"], sep=".")
        
        x <- get.files("linksbalancos")
        x <- paste0("https://dl.dropboxusercontent.com/s/",
                    x[x$EMPRESA == firm, ]$LINK, "/", firm, ".csv?dl=0")
        
        download.file(x, destfile = paste0(tempdir(), "\\", firm), mode = "wb")
        x <- readRDS(paste0(tempdir(), "\\", firm))
        unlink(paste0(tempdir(), "\\", firm))
        
        
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
