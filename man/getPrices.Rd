\name{getPrices}
\alias{getPrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Loads the prices data.
}
\description{
Returns the diary infomation about the prices and the transactioned volume of the shares.
}
\usage{
getPrices(shares, info = "full")
getPrices(shares, info = "simplified")
getPrices(shares, info = "single", value = "Close", fill = NA)
}
\arguments{
  \item{shares}{
The name of the shares and/or a combination of the options below:

shares, rights and receipts, real estate funds, private bonus,
insolvensy (shares of the firms that in some moment between 1998 and now were in insolvency state), debentures (also in this class there are investment certificates and public debt titles), forward market, options, futures and fractionary.

Eg.: getPrices(shares = c("VALE3", "options", "futures"))
}
  \item{info}{
\strong{full} - returns a data.frame with the bovespa's quotation data.

\strong{simplified} - returns a data.frame with the open, close, min, max, mean, bid and ask prices, transactioned volume of the day and other informations that changed between the specified shares.

\strong{single} - returns a xts object with a single column specified in paramenter value.
}
  \item{value}{
only used if info = "single".

The setted argument is the "Close" column, but it can be changed at will.
}
  \item{fill}{
Some of the shares aren't negotiated every period, so this parameter set how must be treated the missing days.
Notice that this paramenter will only used only if info = "single".
The options are below:

\strong{last} - uses the last negotiated day value to fill the gaps.

\strong{NA} - fills with NAs.

\strong{drop} - Drop the day out of the time serie.
}
}
\value{
The function has two different outputs.

A data.frame with the share in the first collum and date as second for info setted as "full" or "simplified"

A xts with a collum (the one from value parameter) taken from the original data.frame for each share.
}
\references{
To the original information see:

\href{http://www.bmfbovespa.com.br/en_us/services/market-data/historical-data/equities/historical-data-1/}{Bovespa's Historical Data}

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





## The function is currently defined as
function (shares, info = "simplified", value = "Close", fill = NA) 
{
    if (missingArg(shares)) {
        warning("You have to specify at list a share.")
    }
    else {
        sharelinks <- get.files("linkscotacoes")
        quiet = F
        if ("all" \%in\% shares) 
            shares <- c(shares[shares != "all"], sharelinks[, 
                1])
        if (any(!shares \%in\% sharelinks[, 1])) 
            codbdi = get.files("codbdi")
        if (any(shares \%in\% "shares")) 
            shares <- c(shares[!shares \%in\% "shares"], strsplit(codbdi[codbdi[, 
                1] == 2, 3], " ")[[1]])
        if (any(shares \%in\% "insolvency")) 
            shares <- c(shares[!shares \%in\% "insolvency"], strsplit(codbdi[codbdi[, 
                1] == 6, 3], " ")[[1]])
        if (any(shares \%in\% "rights and receipts")) 
            shares <- c(shares[!shares \%in\% "rights and receipts"], 
                strsplit(codbdi[codbdi[, 1] == 10, 3], " ")[[1]])
        if (any(shares \%in\% "real estate funds")) 
            shares <- c(shares[!shares \%in\% "real estate funds"], 
                strsplit(codbdi[codbdi[, 1] == 12, 3], " ")[[1]])
        if (any(shares \%in\% "debentures")) 
            shares <- c(shares[!shares \%in\% "debentures"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(14, 66, 68, 83), 3], " ")[[1]])
        if (any(shares \%in\% "private bonus")) 
            shares <- c(shares[!shares \%in\% "private bonus"], 
                strsplit(codbdi[codbdi[, 1] == 22, 3], " ")[[1]])
        if (any(shares \%in\% "options")) 
            shares <- c(shares[!shares \%in\% "options"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(32, 33, 38, 42, 74, 75, 78, 82), 3], 
                " ")[[1]])
        if (any(shares \%in\% "auctions")) 
            shares <- c(shares[!shares \%in\% "auctions"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(46, 48, 50, 51, 52, 53, 54, 56), 3], 
                " ")[[1]])
        if (any(shares \%in\% "forward market")) 
            shares <- c(shares[!shares \%in\% "forward market"], 
                strsplit(codbdi[codbdi[, 1] == 62, 3], " ")[[1]])
        if (any(shares \%in\% "futures")) 
            shares <- c(shares[!shares \%in\% "futures"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(70, 71), 3], " ")[[1]])
        if (any(shares \%in\% "fractionary")) 
            shares <- c(shares[!shares \%in\% "fractionary"], strsplit(codbdi[codbdi[, 
                1] == 96, 3], " ")[[1]])
        shares <- sort(unique(shares))
        envir <- NULL
        if (quiet == F & length(shares) > 20) {
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
        teste <- get.shares(shares, envir)
        if (info == "single") 
            for (i in seq_along(teste)) if (is.data.frame(teste[[i]])) 
                if (any(duplicated(teste[[i]][, "DATAPREG"]))) {
                  warning("Some of the assets belongs to the forward market or they have a duplicated date.\n                    Due this the output is goind to be a data.frame.")
                  info <- "simplified"
                  break
                }
        names(teste) <- shares
        x = misspecified = NULL
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
                    share <- xts(share[, value], order.by = as.Date(share$DATAPREG))
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
                  rm(share)
                }
                teste <- teste[-1]
            }
            if (!is.na(fill)) 
                if (fill == "last") {
                  for (i in colnames(x)) {
                    t <- which(is.na(x[, i]))
                    t.1 <- c(which((t[-1] - t[-length(t)] > 1)), 
                      length(t)) + 1
                    for (j in seq_along(t.1)) if (t[max(t.1[j - 
                      1], 1)] - 1 != 0) 
                      x[t[max(t.1[j - 1], 1)]:t[t.1[j] - 1], 
                        i] <- x[t[max(t.1[j - 1], 1)] - 1, i]
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
                  share <- cbind(NegCode = share[, "CODNEG"], 
                    Date = share[, "DATAPREG"], DeadLine = share[, 
                      "PRAZOT"], ExpirDate = share[, "DATVEN"], 
                    share[, -which(colnames(share) \%in\% c("CODNEG", 
                      "DATAPREG", "PRAZOT", "DATVEN"))])
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
        rm(teste)
        if (!quiet) {
            misspecified <- unique(misspecified)
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
                miss <- paste(miss, c("is", "are")[i], "misspecified or", 
                  c("this share does", "these shares do")[i], 
                  "not exist.")
                warning(miss)
                rm(i, miss)
            }
        }
        rm(misspecified)
        return(x)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
