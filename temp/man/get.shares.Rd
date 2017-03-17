\name{get.shares}
\alias{get.shares}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.shares(shares)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{shares}{
%%     ~~Describe \code{shares} here~~
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
function (shares) 
{
    if (!exists("sharelinks")) 
        sharelinks <- get.files("linkscotacoes")
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
        if (length(shares) > 20) {
            x <- c(which(shares == share) - (1:0))/length(shares)
            cat(paste(progresso[(x[1] == 0 | prog.bar > x[1]) & 
                prog.bar <= x[2]], collapse = ""))
        }
        x <- sharelinks[sharelinks$PAPEL == share, "LINK"]
        if (length(x) == 0) {
            return("This share is misspecified or does not exist.")
        }
        else {
            x <- paste0("https://dl.dropboxusercontent.com/s/", 
                x, "/", share, ".csv?dl=0")
            download.file(x, destfile = paste0(tempdir(), "\\", 
                share), mode = "wb")
            x <- readRDS(paste0(tempdir(), "\\", share))
            unlink(paste0(tempdir(), "\\", share))
            names(x)[8:17] <- c("Open", "High", "Low", "Mean", 
                "Close", "Bid", "Ask", "TotNeg", "QuaTot", "Volume")
            x$DATAPREG <- as.Date(as.character(x$DATAPREG), "\%Y\%m\%d")
            return(x)
        }
    })
    return(teste)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
