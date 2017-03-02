# Test some of the package's functions

source("package.r")

if( ! system(paste("ping", "www.dropbox.com"), show.output.on.console = F)) {
  
  y <- system.time({
    # The goal here is to test the outputs
    tests <- data.frame(single.NA = F, single.drop = F, single.last = F, simplified = F, full = F,
                        balance.quarter.right = F, balance.quarter.wrong = F, balance.NULL = F,
                        all.adj.downloaded = F, all.balances.downloaded = F)
    
    # Single info
    x <- getAdjPrices(c("PETR3", "WEGE3"), info = "single", fill = NA)
    x <- round(x[c("2006-06-16", "2006-06-19"), ], 6) == rbind(cbind(1.060838, 1.00125), cbind(0.975841, NA))
    if(is.na(x[2, 2]) + sum(x, na.rm = T) == 4) tests$single.NA = T
    
    x <- getAdjPrices(c("PETR3", "WEGE3"), info = "single", fill = "drop")
    x <- round(x[c("2006-06-16", "2006-06-19"), ], 6)
    if(nrow(x) == 1) if(sum(x == cbind(0.866732, 1.00125)) == 2) tests$single.drop = T
    
    x <- getAdjPrices(c("PETR3", "WEGE3"), info = "single", fill = "last")
    x <- round(x[c("2006-06-16", "2006-06-19"), ], 6) == rbind(cbind(1.060838, 1.00125), cbind(0.975841, 1))
    if(sum(x, na.rm = T) == 4) tests$single.last = T
    
    # Simplified info
    x <- getAdjPrices(c("PETR3", "WEGE3"), info = "simplified")
    x <- x[x[, "Date"] %in% c("2006-06-16", "2006-06-19"), ]
    if(all(dim(x) == c(3, 16)))
      if(all(round(x[, "Return"], 6) == c(1.060838, 0.975841, 1.001250))) tests$simplified = T
    
    x <- getAdjPrices(c("PETR3", "WEGE3"), info = "full")
    x <- x[x[, "Date"] %in% c("2006-06-16", "2006-06-19"), ]
    if(all(dim(x) == c(3, 23)))
      if(all(round(x[, "Return"], 6) == c(1.060838, 0.975841, 1.001250))) tests$full = T
    
    x <- getBalanceSheet("PETR", quarter = "2016-06")
    if(all(dim(x) == c(692, 4)) & x[x[, "name of account"] == "patrimonio liquido", "Value"] == "271395000000")
      tests$balance.quarter.right <- T
    
    x <- getBalanceSheet(c("PETR", "WEGE"), quarter = "2016-06")
    if(all(unique(x[, "CodigoCvm"]) == c("9512", "5410")) &
       all(x[x$DataReferenciaDocumento == "2016-09-30",
             "patrimonio.liquido.consolidado"] == c("262016000000", "5963354000")))
      tests$balance.quarter.wrong <- T
    
    x <- getBalanceSheet("PETR", quarter = NULL)
    if(all(unique(x[, "CodigoCvm"]) == c("9512")) &
       x[x$DataReferenciaDocumento == "2016-09-30",
         "patrimonio.liquido.consolidado"] == "262016000000") tests$balance.NULL <- T
    rm(x)
    
    x1 <- NULL
    try(x1 <- getAdjPrices("all", info = "single", fill = "last"))
    if( ! is.null(x1))
      tests$all.adj.downloaded = T
    
    x2 <- NULL
    try(x2 <- getBalanceSheet("all"))
    if( ! is.null(x2))
      tests$all.balances.downloaded = T  
  })
  
  Sys.sleep(5)
  gc()
  results <-
    c(processor = Sys.getenv("PROCESSOR_IDENTIFIER"),
      Sys.info()[1 : 2],
      date = as.character(Sys.time()),
      memory.size = memory.size(),
      memory.limit = memory.limit(),
      system.time = y,
      tests,
      dim.adj.shares = paste(dim(x1), collapse = " "),
      dim.balance.sheets = paste(dim(x2), collapse = " ")
    )
  
  if(file.exists("results.rds")) results <- rbind(readRDS("results.rds"), results)
  saveRDS(results, file = "results.rds")
  closeAllConnections()
  
  View(results)
  
} else {
  warning("Verify your connection!")
}
  

  


