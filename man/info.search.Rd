\name{info.search}
\alias{info.search}
\title{
 This function searchs about the firms and their shares and derivatives
}
\description{
Like in title, this function searchs about the firms and their shares and derivatives, giving a list with the results.
}
\usage{
info.search(info = "")
}
\arguments{
  \item{info}{
CNPJ number, CVM or negotiation code, company name, or any other piece of information about the company.
}
}
\value{
Firstly it will show you a table with the principal results.

If you create a objet with the function, the object will be a list. The first element of the list is the table shown before (here called summary).

The other elements of this object will be lists too. These lists have the detailment of the search.

First element (Information) of these lists is the seen line in summary

The second (Names) is a vector with some of the used social names (after 1998).

The third (Share) is a data.frame with the first time negociated, after the change in the way the negotiation code was written, and the last time negociated for each share or derivative.

}
\author{
Michel Ricardo Meyer \email{michel.ricardo.meyer@gmail.com}
}
\examples{
# Summary will be automatically printed in the screen.
test <- info.search("PETRO")

# See the basic information of the firm with:
test$PETR$Information

# All names used in published financial statements:
test$PETR$Names

# Negotiation code, first and last time negotiated
# (only six lines because it's a long data.frame to be printed)
head(test$PETR$Share, 6)
}