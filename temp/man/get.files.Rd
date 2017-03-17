\name{get.files}
\alias{get.files}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
get.files(link)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{link}{
%%     ~~Describe \code{link} here~~
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
function (link) 
{
    if (!dir.exists(tempdir())) 
        dir.create(tempdir(), showWarnings = F)
    if (file.exists(paste0(tempdir(), "\\", link, ".txt"))) {
        file <- read.table(paste0(tempdir(), "\\", link, ".txt"), 
            header = T, stringsAsFactors = F)
    }
    else {
        links <- data.frame(dicionariocompleto = "https://dl.dropboxusercontent.com/s/sqqhn6d7g5vnhxf/dicionariocompleto.txt?dl=0", 
            dicionariobruto = "https://dl.dropboxusercontent.com/s/yntx5i6xi66ld66/dicionariobruto.txt?dl=0", 
            linkseventos = "https://dl.dropboxusercontent.com/s/ecafaixl0lw0292/linkseventos.txt?dl=0", 
            linkscotacoes = "https://dl.dropboxusercontent.com/s/x4uzbx1wahh6xy3/linkscotacoes.txt?dl=0", 
            linksbalancos = "https://dl.dropboxusercontent.com/s/iaabhpybci6jy4x/linksbalancos.txt?dl=0", 
            codbdi = "https://dl.dropboxusercontent.com/s/b1la2bhs1nddd2x/codbdi.txt?dl=0", 
            stringsAsFactors = F)
        file <- read.table(url(links[link][[1]]), header = T, 
            stringsAsFactors = F)
        write.table(file, paste0(tempdir(), "\\", link, ".txt"), 
            row.names = F)
    }
    return(file)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
