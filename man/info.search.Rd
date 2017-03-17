\name{info.search}
\alias{info.search}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
info.search(info = "", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{info}{
%%     ~~Describe \code{info} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (info = "", ...) 
{
    info <- toupper(info)
    dicionario <- get.files("dicionariocompleto")
    if (all(strsplit(as.character(info), "")[[1]] \%in\% as.character(0:9))) {
        x <- grep(info, dicionario$CodigoCvm)
    }
    else {
        x <- c(unlist(lapply(2:3, function(j) grep(info, dicionario[, 
            j]))), grep(substr(info, 1, 4), dicionario[, 4]))
    }
    if (length(x) == 0) {
        return(NULL)
        break
    }
    x <- sort(table(x), decreasing = T)
    dicionario <- dicionario[names(x), ]
    x <- apply(dicionario, 1, function(dic) {
        x <- strsplit(strsplit(dic["INFORMACOES"], ";")[[1]], 
            " = ")
        parte1 <- cbind(CVMCode = dic["CodigoCvm"], CNPJ = paste0(strsplit(dic["CNPJ"], 
            ";")[[1]], collapse = ""), SocialName = last(strsplit(dic["RazaoSocial"], 
            "; ")[[1]]), NegotiationCode = paste(unlist(lapply(1:length(x), 
            function(j) x[[j]][1])), collapse = " "))
        parte2 <- cbind(`Used Social Names` = strsplit(dic["RazaoSocial"], 
            "; ")[[1]])
        parte <- lapply(x, function(j) {
            j.1 <- matrix(strsplit(j[2], " ")[[1]], ncol = 3, 
                byrow = T)
            j.1[, 1] <- paste0(j[1], j.1[, 1])
            j.1 <- data.frame(j.1, stringsAsFactors = F)
            names(j.1) <- c("share-derivative", "FirstTimeNegociated", 
                "LastTimeNegociated")
            return(j.1)
        })
        parte3 <- NULL
        for (j in seq_along(parte)) {
            parte3 <- rbind(parte3, parte[[j]])
        }
        parte <- list(parte1, parte2, parte3)
        names(parte) <- c("Information", "Names", "Share")
        return(parte)
    })
    names(x) <- unlist(lapply(seq_along(x), function(i) strsplit(x[[i]][[1]][4], 
        " ")[[1]][[1]]))
    painel <- NULL
    for (i in seq_along(x)) painel <- rbind(painel, x[[i]][[1]])
    rownames(painel) <- NULL
    painel <- data.frame(painel, stringsAsFactors = F)
    painel$CVMCode <- as.numeric(painel$CVMCode)
    x <- (c(list(painel), x))
    names(x)[1] <- "summary"
    print(x$summary)
    return(invisible(x))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
