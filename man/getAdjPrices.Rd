\name{getAdjPrices}
\alias{getAdjPrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getAdjPrices(shares, by = "all", subscription = "rational", info = "simplified", value = "Return", fill = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shares}{
%%     ~~Describe \code{shares} here~~
}
  \item{by}{
%%     ~~Describe \code{by} here~~
}
  \item{subscription}{
%%     ~~Describe \code{subscription} here~~
}
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{value}{
%%     ~~Describe \code{value} here~~
}
  \item{fill}{
%%     ~~Describe \code{fill} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (shares, by = "all", subscription = "rational", info = "simplified", 
    value = "Return", fill = NA) 
{
    dicionario <- get.files("dicionariobruto")
    sharelinks <- get.files("linkscotacoes")
    dic2 <- get.files("linkseventos")
    if (info \%in\% c("full", "simplified", "single")) {
        print("verify if you wrote correctly the paramenter 'info'")
        return(NULL)
    }
    if ("all" \%in\% shares) {
        shares <- c(shares[shares != "all"], sharelinks[, 1])
        shares <- shares[substr(shares, 5, 999) \%in\% 3:8]
        shares <- shares[substr(shares, 1, 4) \%in\% unique(unlist(apply(as.matrix(dicionario[dicionario[, 
            1] \%in\% dic2[, 1], 5:8]), 2, unique)))]
        shares <- sort(unique(shares))
    }
    quiet <- F
    if (!value \%in\% c("Date", "NegCode", "CODBDI", "NegCode", 
        "TPMERC", "NOMRES", "ESPECI", "PRAZOT", "Open", "High", 
        "Low", "Mean", "Close", "Bid", "Ask", "Return", "TotNeg", 
        "QuaTot", "Volume", "PREEXE", "INDOPC", "DATVEN", "PTOEXE", 
        "CODISI")) {
        warning(paste("The parameter value was misspecified. One of these must be selected:", 
            paste(colnames(share)[-2], collapse = ", ")))
        return(NULL)
    }
    envir <- NULL
    if (quiet == F & length(shares) > 10) {
        cat("It can take a while.\n")
        envir <- environment()
        progresso <- 0:10
        div <- 6
        progresso <- paste(paste0("", progresso * (100/max(progresso)), 
            "\% "), collapse = paste0(rep("| ", div), collapse = ""))
        progresso <- strsplit(progresso, " ")[[1]]
        prog.bar <- (1:length(progresso))/length(progresso)
        prog.bar <- prog.bar - min(prog.bar)
        prog.bar <- prog.bar/max(prog.bar)
    }
    teste <- lapply(shares, function(share) {
        matriz.ajuste <- which(dicionario[, c("codNeg", "LIVRE", 
            "LIVRE.1", "LIVRE.2")] == substr(share, 1, 4), arr.ind = T)[, 
            1]
        matriz.ajuste <- unique(dicionario[matriz.ajuste, "CodigoCvm"])
        if (length(matriz.ajuste) == 0) {
            return(NULL)
            break
        }
        if (!as.numeric(substr(share, 5, 5)) \%in\% 3:8) {
            return(NULL)
            break
        }
        if (length(matriz.ajuste) > 1) 
            stop("VERIFICAR FUNCAO DE CORRECAO DOS RETORNOS")
        if (length(dic2[dic2$EMPRESA == matriz.ajuste, "LINK"]) == 
            0) {
            return(NULL)
            break
        }
        dist <- paste0(tempdir(), "\\omni\\m", matriz.ajuste)
        if (!file.exists(dist)) {
            link <- paste0("https://dl.dropboxusercontent.com/s/", 
                dic2[dic2$EMPRESA == matriz.ajuste, "LINK"], 
                "/", matriz.ajuste, ".csv?dl=0")
            download.file(link, dist, mode = "wb", quiet = T)
            rm(link)
        }
        matriz.ajuste <- read.csv2(dist, stringsAsFactors = F, 
            fileEncoding = "ISO8859-1")
        rm(dist)
        tipo <- c("fator.ON", "fator.PN", "fator.PNA", "fator.PNB", 
            "fator.PNC", "fator.PND")
        if (length(intersect(tipo[as.numeric(substr(share, 5, 
            5)) - 2], colnames(matriz.ajuste))) == 0) {
            return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
            break
        }
        matriz.ajuste <- matriz.ajuste[, c("data", "evento", 
            tipo[as.numeric(substr(share, 5, 5)) - 2])]
        matriz.ajuste[, "data"] <- as.Date(matriz.ajuste[, "data"])
        colnames(matriz.ajuste)[3] <- "fator"
        if (any(by \%in\% "all")) {
            by <- c("Dividend", "Interest", "Bonus shares", "Subscription Right", 
                "Spinoff", "Return of Capital")
        }
        if (any(by \%in\% "Dividend")) {
            by <- c(by, "Dividendo", "Dividendo mensal", "DIVIDENDO")
        }
        if (any(by \%in\% "Interest")) {
            by <- c(by, "Juros", "Juros mensal", "JRS CAP PROPRIO")
        }
        if (any(by \%in\% "Bonus shares")) {
            by <- c(by, "Bonificacao")
        }
        if (any(by \%in\% "Subscription Right")) {
            by <- c(by, "Subscricao")
        }
        if (any(by \%in\% "Spinoff")) {
            by <- c(by, "Cisao")
        }
        if (any(by \%in\% "Return of Capital")) {
            by <- c(by, "Restituicao de capital", "REST CAP DIN")
        }
        if ("Subscription Right" \%in\% by) {
            x <- which(matriz.ajuste[, "evento"] == "Subscricao")
            if (subscription == "rational") {
                x <- x[matriz.ajuste[x, "fator"] < 1]
                if (length(x) > 0) 
                  matriz.ajuste <- matriz.ajuste[-x, ]
            }
            if (subscription == "neverbuy") {
                matriz.ajuste <- matriz.ajuste[-x, ]
            }
            rm(x)
        }
        share <- get.shares(share, envir = envir)[[1]]
        if (!is.null(share) & !is.character(share)) {
            colnames(share)[c(1, 3)] <- c("Date", "NegCode")
            base <- as.matrix(share[, c("Date", "NegCode")])
            share <- as.xts(share[, -1], order.by = as.Date(share[, 
                "Date"]))
            if (nrow(matriz.ajuste) > 0) 
                matriz.ajuste[which(regexec("Cisao", matriz.ajuste[, 
                  "evento"]) != "-1"), "evento"] <- "Cisao"
            x <- substr(matriz.ajuste[, "evento"], 1, 22) \%in\% 
                by
            if (any(x)) {
                x <- matriz.ajuste[x, ]
                x <- t(data.frame(lapply(unique(x[, "data"]), 
                  function(z) {
                    cisao <- ((1/(1 - sum(1 - (1/(x[x[, "data"] \%in\% 
                      z & x[, "evento"] == "Cisao", "fator"]))))) - 
                      1)
                    comum <- (as.numeric(x[x[, "data"] \%in\% z & 
                      x[, "evento"] != "Cisao", "fator"]) - 1)
                    z <- t(data.frame(data = z, fator = sum(cisao, 
                      comum) + 1, stringsAsFactors = F))
                    return(z)
                  }), stringsAsFactors = F))
                rownames(x) <- NULL
                x <- as.data.frame(x, stringsAsFactors = F)
                x[, "data"] <- as.Date(x[, "data"])
                x[, "fator"] <- as.numeric(x[, "fator"])
            }
            else {
                x <- NULL
            }
            matriz.ajuste <- rbind(x, matriz.ajuste[matriz.ajuste[, 
                "evento"] \%in\% c("alt.fator.cot", "Grupamento", 
                "Desdobramento", "bonificacao ou desdobramento"), 
                c("data", "fator")])
            rm(x)
            matriz.ajuste <- matriz.ajuste[matriz.ajuste[, "data"] <= 
                max(index(share)) & matriz.ajuste[, "data"] >= 
                min(index(share)), ]
            x <- c("Open", "Close", "High", "Low", "Mean", "Bid", 
                "Ask", "Volume")
            for (i in 1:nrow(matriz.ajuste)) {
                p <- index(share) <= matriz.ajuste[i, "data"]
                share[p, x] <- as.numeric(share[p, x])/as.numeric(matriz.ajuste[i, 
                  "fator"])
            }
            share[, "Volume"] <- as.numeric(share[, "Volume"]) * 
                as.numeric(share[, "FATCOT"])
            share <- share[, -which(colnames(share) == "FATCOT")]
            share <- cbind(data.frame(share[, 1:13], stringsAsFactors = F), 
                Return = as.numeric(c(NA, as.numeric(share[-1, 
                  "Close"])/as.numeric(share[-nrow(share), "Close"]))), 
                data.frame(share[, 14:ncol(share)], stringsAsFactors = F))
            share <- cbind(base, share)
            for (i in c(x, "CODBDI", "TPMERC", "Return", "TotNeg", 
                "QuaTot", "PREEXE")) {
                share[, i] <- as.numeric(share[, i])
            }
            return(share)
        }
        else {
            return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
        }
    })
    if (info == "single") 
        for (i in seq_along(teste)) if (is.data.frame(teste[[i]])) 
            if (any(duplicated(teste[[i]][, "Date"]))) {
                warning("Some of the assets belongs to the forward market or they have a duplicated date.\n                  Due this the output is goind to be a data.frame.")
                info <- "simplified"
                break
            }
    names(teste) <- shares
    x = misspecified = NULL
    if (!(is.null(unlist(teste)))) {
        if (info == "single") {
            for (i in seq_along(teste)) {
                if (exists("prog.bar")) {
                  if (names(teste)[1] == shares[[1]]) 
                    cat("Progress of aggregation: \n")
                  P <- c(which(shares == names(teste)[1]) - (1:0))/length(shares)
                  cat(paste(progresso[prog.bar >= P[1] & prog.bar < 
                    P[2]], collapse = ""))
                  if (names(teste)[1] == tail(shares, 1)) 
                    cat("100\%\n")
                }
                if (is.null(dim(teste[[1]]))) {
                  misspecified <- c(misspecified, names(teste)[1])
                }
                else {
                  share <- teste[[1]]
                  if (value \%in\% colnames(share)) {
                    share <- xts(share[, value], order.by = as.Date(share$Date))
                  }
                  else {
                    warning(paste("The parameter value was misspecified. One of these must be selected:", 
                      paste(colnames(share)[-2], collapse = ", ")))
                    return()
                  }
                  x <- cbind(x, share)
                  colnames(x)[ncol(x)] <- shares[[i]]
                  rm(share)
                }
                teste <- teste[-1]
            }
            if (!is.na(fill)) {
                if (fill == "last") {
                  for (i in colnames(x)) {
                    if (value == "Return") {
                      t <- is.na(x[, i])
                      t.1 <- which(!t)
                      if (length(t.1) == 0) 
                        next
                      t.1 <- c(head(t.1, 1), tail(t.1, 1))
                      t <- which(t)
                      t <- t[t > t.1[1] & t < t.1[2]]
                      x[t, i] <- 1
                    }
                    else {
                      t <- which(is.na(x[, i]))
                      if (length(t) != 0) {
                        t.1 <- c(which((t[-1] - t[-length(t)] > 
                          1)), length(t)) + 1
                        for (j in seq_along(t.1)) if (t[max(t.1[j - 
                          1], 1)] - 1 != 0) 
                          x[t[max(t.1[j - 1], 1)]:t[t.1[j] - 
                            1], i] <- x[t[max(t.1[j - 1], 1)] - 
                            1, i]
                      }
                    }
                  }
                }
                if (fill == "drop") {
                  if (value == "Return") {
                    na <- apply(x, 1, function(i) any(is.na(i)))
                    if (!all(na)) 
                      x <- x[min(which(!na)):max(which(!na)), 
                        ]
                    na <- apply(x, 1, function(i) any(is.na(i)))
                    if (nrow(x[!na, ]) > 0) {
                      na <- sort(which(na), decreasing = T)
                      for (i in na) {
                        x[i + 1, ] <- apply(x[i:(i + 1)], 2, 
                          function(j) prod(as.numeric(j), na.rm = T))
                        x <- x[-i, ]
                      }
                      rm(i)
                    }
                    else {
                      x <- x[!na, ]
                    }
                  }
                  else {
                    na <- apply(x, 1, function(i) any(is.na(i)))
                    x <- x[!na, ]
                  }
                  rm(na)
                }
            }
        }
        if (info \%in\% c("full", "simplified")) {
            for (i in seq_along(teste)) {
                if (exists("prog.bar")) {
                  if (names(teste)[1] == shares[[1]]) 
                    cat("Progress of aggregation: \n")
                  P <- c(which(shares == names(teste)[1]) - (1:0))/length(shares)
                  cat(paste(progresso[prog.bar >= P[1] & prog.bar < 
                    P[2]], collapse = ""))
                  if (names(teste)[1] == tail(shares, 1)) 
                    cat("100\%\n")
                }
                if (is.null(dim(teste[[1]]))) {
                  misspecified <- c(misspecified, shares[[i]])
                }
                else {
                  share <- teste[[1]]
                  share <- cbind(NegCode = share[, "NegCode"], 
                    Date = share[, "Date"], DeadLine = share[, 
                      "PRAZOT"], ExpirDate = share[, "DATVEN"], 
                    share[, -which(colnames(share) \%in\% c("NegCode", 
                      "Date", "PRAZOT", "DATVEN"))])
                  rownames(share) <- NULL
                  x <- rbind(x, share)
                  rm(share)
                }
                teste <- teste[-1]
            }
            if (info == "simplified") {
                complete <- which(colnames(x) \%in\% c("NegCode", 
                  "Date", "NOMRES", "ESPECI", "Open", "Close", 
                  "High", "Low", "Mean", "Bid", "Ask", "Return", 
                  "QuaTot"))
                complete <- sort(unique(c(complete, which(unlist(lapply(names(x), 
                  function(i) length(unique(x[, i])) != 1))))))
                x <- x[, complete]
            }
        }
    }
    rm(teste)
    misspecified <- unique(misspecified)
    if (!quiet) {
        if (!is.null(misspecified)) {
            if (length(misspecified) > 1) {
                miss <- paste(misspecified[-length(misspecified)], 
                  collapse = ", ")
                miss <- paste(miss, misspecified[length(misspecified)], 
                  sep = " and ")
            }
            else {
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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
