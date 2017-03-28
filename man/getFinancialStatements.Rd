\name{getFinancialStatements}
\alias{getFinancialStatements}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Loads the firm's Financial Statements.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFinancialStatements(firms, quarter = NULL)
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
        teste <- get.fin.stat(firms, quarter)
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
