\name{getPrices}
\alias{getPrices}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getPrices(shares, as = "panel", complete = F, value = "Close", fill = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shares}{
%%     ~~Describe \code{shares} here~~
}
  \item{as}{
%%     ~~Describe \code{as} here~~
}
  \item{complete}{
%%     ~~Describe \code{complete} here~~
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
function (shares, as = "panel", complete = F, value = "Close", 
    fill = NA) 
{
    if (missingArg(shares)) {
        warning("You have to specify at list a share.")
    }
    else {
        if (!complete \%in\% c(T, F)) {
            warning("The argument complete must be TRUE or FALSE")
            return()
        }
        sharelinks <- get.files("linkscotacoes")
        if ("all" \%in\% shares) {
            quiet = T
            shares <- c(shares[shares != "all"], sharelinks[, 
                1])
        }
        else {
            quiet = F
        }
        if (any(!shares \%in\% sharelinks[, 1])) 
            codbdi = get.files("codbdi")
        if (any(shares \%in\% "shares")) 
            shares <- c(shares[!shares \%in\% "all.shares"], strsplit(codbdi[codbdi[, 
                1] == 2, 3], ", ")[[1]])
        if (any(shares \%in\% "insolvency")) 
            shares <- c(shares[!shares \%in\% "insolvency"], strsplit(codbdi[codbdi[, 
                1] == 6, 3], ", ")[[1]])
        if (any(shares \%in\% "rights and receipts")) 
            shares <- c(shares[!shares \%in\% "rights and receipts"], 
                strsplit(codbdi[codbdi[, 1] == 10, 3], ", ")[[1]])
        if (any(shares \%in\% "real estate funds")) 
            shares <- c(shares[!shares \%in\% "real estate funds"], 
                strsplit(codbdi[codbdi[, 1] == 12, 3], ", ")[[1]])
        if (any(shares \%in\% "debentures")) 
            shares <- c(shares[!shares \%in\% "debentures"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(14, 66, 68, 83), 3], ", ")[[1]])
        if (any(shares \%in\% "private bonus")) 
            shares <- c(shares[!shares \%in\% "private bonus"], 
                strsplit(codbdi[codbdi[, 1] == 22, 3], ", ")[[1]])
        if (any(shares \%in\% "options")) 
            shares <- c(shares[!shares \%in\% "options"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(32, 33, 38, 42, 74, 75, 78, 82), 3], 
                ", ")[[1]])
        if (any(shares \%in\% "auctions")) 
            shares <- c(shares[!shares \%in\% "auctions"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(46, 48, 50, 51, 52, 53, 54, 56), 3], 
                ", ")[[1]])
        if (any(shares \%in\% "forward market")) 
            shares <- c(shares[!shares \%in\% "forward market"], 
                strsplit(codbdi[codbdi[, 1] == 62, 3], ", ")[[1]])
        if (any(shares \%in\% "futures")) 
            shares <- c(shares[!shares \%in\% "futures"], strsplit(codbdi[codbdi[, 
                1] \%in\% c(70, 71), 3], ", ")[[1]])
        if (any(shares \%in\% "fractionary")) 
            shares <- c(shares[!shares \%in\% "fractionary"], strsplit(codbdi[codbdi[, 
                1] == 96, 3], ", ")[[1]])
        shares <- sort(unique(shares))
        if (length(shares) > 1 & as == "xts") {
            value <- value[[1]]
            fill <- fill[[1]]
            if (!fill \%in\% c("last", NA, "drop")) {
                warning("fill must be setted as last, NA or drop.")
                return()
            }
        }
        teste <- get.shares(shares)
        x = misspecified = NULL
        if (as == "xts") 
            for (i in seq_along(teste)) if (any(duplicated(teste[[i]][, 
                "DATAPREG"]))) {
                warning("Some of the assets belongs to the forward market or it has a duplicated date.\n                  Due this the output is goind to be a panel.")
                as <- "panel"
                break
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
                  }
                  else {
                    x <- share
                    if (!complete) {
                      x <- x[, unlist(lapply(names(x), function(i) length(unique(x[, 
                        i])) != 1))]
                      j <- NULL
                      for (i in colnames(x)) {
                        if (class(x[, i]) != "character") 
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
        if (as == "panel") {
            for (i in seq_along(teste)) {
                if (is.null(dim(teste[[i]]))) {
                  misspecified <- c(misspecified, shares[[i]])
                }
                else {
                  share <- teste[[i]]
                  share <- cbind(NegCode = share[, 3], Date = share[, 
                    1], DeadLine = share[, 7], ExpirDate = share[, 
                    20], share[, c(2, 4:6, 8:19, 21:ncol(share))])
                  rownames(share) <- NULL
                  x <- rbind(x, share)
                  rm(share)
                }
            }
            if (!complete) {
                complete <- which(colnames(x) \%in\% c("NegCode", 
                  "Date", "Open", "Close", "High", "Low", "Mean", 
                  "Bid", "Ask"))
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
