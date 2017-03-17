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
getAdjPrices(shares, by = "all", subscription = "rational", as = "panel", value = "Close", fill = NA)
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
  \item{as}{
%%     ~~Describe \code{as} here~~
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
function (shares, by = "all", subscription = "rational", as = "panel", 
    value = "Close", fill = NA) 
{
    por <- by
    subscricao <- subscription
    complete <- FALSE
    dicionario <- get.files("dicionariobruto")
    sharelinks <- get.files("linkscotacoes")
    if ("all" \%in\% shares) {
        quiet = T
        shares <- c(shares[shares != "all"], sharelinks[, 1])
        shares <- shares[substr(shares, 5, 999) \%in\% 3:8]
    }
    else {
        quiet = F
    }
    if (length(shares) > 20) {
        cat("It can take a while.\n")
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
        if (exists("prog.bar")) {
            x <- c(which(shares == share) - (1:0))/length(shares)
            cat(paste(progresso[prog.bar >= x[1] & prog.bar < 
                x[2]], collapse = ""))
        }
        matriz.ajuste <- which(dicionario[, c("codNeg", "LIVRE", 
            "LIVRE.1", "LIVRE.2")] == substr(share, 1, 4), arr.ind = T)[, 
            1]
        matriz.ajuste <- unique(dicionario[matriz.ajuste, "CodigoCvm"])
        if (length(matriz.ajuste) == 0) {
            return(NULL)
            break
        }
        if (length(matriz.ajuste) > 1) 
            stop("VERIFICAR FUNCAO DE CORRECAO DOS RETORNOS")
        dic2 <- get.files("linkseventos")
        if (length(dic2[dic2$EMPRESA == matriz.ajuste, "LINK"]) == 
            0) {
            return(NULL)
            break
        }
        matriz.ajuste <- paste0("https://dl.dropboxusercontent.com/s/", 
            dic2[dic2$EMPRESA == matriz.ajuste, "LINK"], "/", 
            matriz.ajuste, ".csv?dl=0")
        matriz.ajuste <- read.csv2(url(matriz.ajuste), stringsAsFactors = F, 
            fileEncoding = "ISO8859-1")
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
        if (any(por \%in\% "all")) {
            por <- c("Dividend", "Interest", "Bonus shares", 
                "Subscription Right", "Spinoff", "Return of Capital")
        }
        if (any(por \%in\% "Dividend")) {
            por <- c(por, "Dividendo", "Dividendo mensal", "DIVIDENDO")
        }
        if (any(por \%in\% "Interest")) {
            por <- c(por, "Juros", "Juros mensal", "JRS CAP PROPRIO")
        }
        if (any(por \%in\% "Bonus shares")) {
            por <- c(por, "Bonificacao")
        }
        if (any(por \%in\% "Subscription Right")) {
            por <- c(por, "Subscricao")
        }
        if (any(por \%in\% "Spinoff")) {
            por <- c(por, "Cisao")
        }
        if (any(por \%in\% "Return of Capital")) {
            por <- c(por, "Restituicao de capital", "REST CAP DIN")
        }
        if ("Subscription Right" \%in\% por) {
            x <- which(matriz.ajuste[, "evento"] == "Subscricao")
            if (subscricao == "rational") {
                x <- x[matriz.ajuste[x, "fator"] < 1]
                if (length(x) > 0) 
                  matriz.ajuste <- matriz.ajuste[-x, ]
            }
            if (subscricao == "neverbuy") {
                matriz.ajuste <- matriz.ajuste[-x, ]
            }
            rm(x)
        }
        share <- get.shares(share)[[1]]
        colnames(share)
        if (!is.null(share)) {
            colnames(share)[c(1, 3)] <- c("Date", "NegCode")
            base <- as.matrix(share[, c("Date", "NegCode")])
            share <- as.xts(share[, c("Open", "Close", "High", 
                "Low", "Mean", "Bid", "Ask", "Volume")], order.by = as.Date(share[, 
                "Date"]))
            if (nrow(matriz.ajuste) > 0) {
                matriz.ajuste[which(regexec("Cisao", matriz.ajuste[, 
                  "evento"]) != "-1"), "evento"] <- "Cisao"
                x <- substr(matriz.ajuste[, "evento"], 1, 22) \%in\% 
                  por
                if (any(x)) {
                  x <- matriz.ajuste[x, ]
                  x <- t(data.frame(lapply(unique(x[, "data"]), 
                    function(z) {
                      z <- t(data.frame(data = z, fator = sum(as.numeric(x[x[, 
                        "data"] \%in\% z, "fator"]) - 1) + 1, stringsAsFactors = F))
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
                matriz.ajuste <- matriz.ajuste[matriz.ajuste[, 
                  "data"] <= max(index(share)) & matriz.ajuste[, 
                  "data"] >= min(index(share)), ]
                x <- c("Open", "Close", "High", "Low", "Mean", 
                  "Bid", "Ask")
                for (i in 1:nrow(matriz.ajuste)) {
                  p <- index(share) <= matriz.ajuste[i, "data"]
                  share[p, x] <- as.numeric(share[p, x])/as.numeric(matriz.ajuste[i, 
                    "fator"])
                }
            }
            share <- cbind(base, as.data.frame(share))
            return(share)
        }
        else {
            return("We don't have the adjusted values for the chosen share or the chosen share does not exist")
        }
    })
    x = misspecified = NULL
    if (as == "panel") {
        for (i in seq_along(teste)) {
            if (is.null(dim(teste[[i]]))) {
                misspecified <- c(misspecified, shares[[i]])
            }
            else {
                share <- teste[[i]]
                rownames(share) <- NULL
                x <- rbind(x, share)
            }
        }
        if (!complete) {
            complete <- which(colnames(x) \%in\% c("NegCode", "Date", 
                "Open", "Close", "High", "Low", "Mean", "Bid", 
                "Ask"))
            complete <- sort(unique(c(complete, which(unlist(lapply(names(x), 
                function(i) length(unique(x[, i])) != 1))))))
            x <- x[, complete]
        }
    }
    if (as == "xts") {
        for (i in seq_along(teste)) {
            if (is.null(dim(teste[[i]]))) {
                misspecified <- c(misspecified, shares[[i]])
            }
            else {
                share <- teste[[i]]
                if (length(shares) > 1) {
                  if (value \%in\% colnames(share)) {
                    share <- xts(share[, value], order.by = as.Date(rownames(share)))
                  }
                  else {
                    warning(paste("The parameter value was misspecified. One of these must be selected:", 
                      paste(colnames(share)[-2], collapse = ", ")))
                    return()
                  }
                  if (is.na(fill) | fill == "last") {
                    x <- cbind(x, share)
                  }
                  else {
                    if (is.null(x)) {
                      x <- share
                    }
                    else {
                      t <- as.Date(intersect(index(x), index(share)))
                      x <- cbind(x[t, ], share[t, ])
                      rm(t)
                    }
                  }
                  colnames(x)[ncol(x)] <- shares[[i]]
                }
                else {
                  x <- as.xts(share[, -c(1:2)], order.by = as.Date(share[, 
                    "Date"]))
                }
                rm(share)
            }
        }
        if (!is.na(fill)) 
            if (fill == "last") {
                for (i in colnames(x)) {
                  t <- which(is.na(x[, i]))
                  t.1 <- c(which((t[-1] - t[-length(t)] > 1)), 
                    length(t)) + 1
                  for (j in seq_along(t.1)) if (t[max(t.1[j - 
                    1], 1)] - 1 != 0) 
                    x[t[max(t.1[j - 1], 1)]:t[t.1[j] - 1], i] <- x[t[max(t.1[j - 
                      1], 1)] - 1, i]
                }
            }
        if (!complete) {
            complete <- which(colnames(x) \%in\% c("NegCode", "Open", 
                "Close", "High", "Low", "Mean", "Bid", "Ask"))
            complete <- sort(unique(c(complete, which(unlist(lapply(names(x), 
                function(i) length(unique(x[, i])) != 1))))))
            x <- x[, complete]
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
