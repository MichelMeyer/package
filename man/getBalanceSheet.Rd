\name{getBalanceSheet}
\alias{getBalanceSheet}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getBalanceSheet(firms, quarter = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{firms}{
%%     ~~Describe \code{firms} here~~
}
  \item{quarter}{
%%     ~~Describe \code{quarter} here~~
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
function (firms, quarter = NULL) 
{
    if (missingArg(firms)) {
        warning("You need to chose at list a firm.")
    }
    else {
        if (!is.null(quarter) & length(firms) > 1) {
            quarter <- NULL
            warning("Argument quarter is not applied when there is more than one code.")
        }
        dicionario <- get.files("dicionariobruto")
        if ("all" \%in\% firms) {
            firms <- sort(c(firms[firms != "all"], unique(dicionario[, 
                1])))
        }
        if (all(firms \%in\% 3)) {
            warning("The CVM code 3 is a example, This firm actually does not exists.")
            return(NULL)
        }
        else {
            firms <- firms[firms != 3]
        }
        if (length(firms) > 20) {
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
        teste <- lapply(firms, function(firm) {
            if (length(firms) > 20) {
                x <- c(which(firms == firm) - (1:0))/length(firms)
                cat(paste(progresso[(x[1] == 0 | prog.bar > x[1]) & 
                  prog.bar <= x[2]], collapse = ""))
                if (x[[2]] == 1) 
                  cat("\n")
            }
            if (!exists("especificacao")) {
                if (nchar(firm) <= 6) {
                  if (!any(!(strsplit(as.character(firm), "")[[1]] \%in\% 
                    0:9))) {
                    especificacao <- "CodigoCvm"
                  }
                }
            }
            if (!exists("especificacao")) {
                x <- which(strsplit(firm, "")[[1]] \%in\% c(".", 
                  "/", "-"))
                if (length(x) > 0) {
                  if (!any(!diff(x) == c(4, 4, 5))) {
                    if (any(!strsplit(firm, "")[[1]][setdiff(1:nchar(firm), 
                      x)] \%in\% c(0:9))) {
                      stop("There are some invalid characters in the input.")
                    }
                    else {
                      especificacao <- "NumeroCnpjCompanhiaAberta"
                    }
                  }
                }
                rm(x)
            }
            if (!exists("especificacao")) {
                if (firm \%in\% dicionario[, "NomeRazaoSocial"]) 
                  especificacao <- "NomeRazaoSocial"
            }
            if (!exists("especificacao")) {
                if (all(strsplit(substr(firm, 1, 4), "")[[1]] \%in\% 
                  LETTERS)) 
                  if (substr(firm, 1, 4) \%in\% dicionario[, "codNeg"]) {
                    firm <- substr(firm, 1, 4)
                    especificacao <- "codNeg"
                  }
            }
            if (!exists("especificacao")) {
                return("We do not found the balance sheets of the selected firm.")
            }
            else {
                if (especificacao == "CodigoCvm") {
                  x <- which(dicionario[, especificacao] == as.numeric(firm))
                  x <- max(x)
                }
                else {
                  x <- which(dicionario[, especificacao] == firm)
                  x <- max(x)
                  firm <- dicionario[x, "CodigoCvm"]
                }
                nome <- paste(dicionario[x, "codNeg"], sep = ".")
                if (nome == "NA" | is.na(nome)) 
                  nome <- paste("CVM", dicionario[x, "CodigoCvm"], 
                    sep = ".")
                x <- get.files("linksbalancos")
                x <- paste0("https://dl.dropboxusercontent.com/s/", 
                  x[x$EMPRESA == firm, ]$LINK, "/", firm, ".csv?dl=0")
                download.file(x, destfile = paste0(tempdir(), 
                  "\\", firm), mode = "wb")
                x <- readRDS(paste0(tempdir(), "\\", firm))
                unlink(paste0(tempdir(), "\\", firm))
                if (!is.null(quarter)) {
                  x <- x[, which(colnames(x) == "V1"):ncol(x)]
                  if (any(substr(x[, "V6"], 1, 7) == quarter)) {
                    x <- x[c(1:3, which(substr(x[, "V6"], 1, 
                      7) == quarter)), ]
                    x <- x[, x[4, ] != ""]
                    x <- t(x)
                    rownames(x) <- NULL
                    colnames(x) <- c("kind of information", "id of account", 
                      "name of account", "Value")
                    x[x[, 1] == "-", 1] <- "firm's basic information"
                    x[x[, 1] == "1", 1] <- "Individual Result"
                    x[x[, 1] == "2", 1] <- "Consolidated Result"
                  }
                  else {
                    x <- substr(x[-c(1:3), "V6"], 1, 7)
                    print("This specified quarter isn't in our data")
                    print("Currently the quarters available are the periods below:")
                    print(paste(paste(head(x, 4), collapse = ", "), 
                      "...", paste(tail(x, 4), collapse = ", "), 
                      sep = ", "))
                    rm(x)
                    return(invisible(NULL))
                    break
                  }
                }
                else {
                  x <- x[, 1:(which(colnames(x) == "V1") - 1)]
                  x <- x[1:(nrow(x) - 3), ]
                }
                return(x)
            }
        })
        x = misspecified = NULL
        for (i in seq_along(teste)) {
            if (is.null(dim(teste[[i]]))) {
                misspecified <- c(misspecified, firms[[i]])
            }
            else {
                x <- rbind(x, teste[[i]])
            }
        }
        if (is.null(quarter)) 
            x <- x[, c(3, 6, 1:2, 4:5, 7:ncol(x))]
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
                c("this firm does", "these firms do")[i], "not exist.")
            warning(miss)
            rm(i, miss)
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
