\name{get.fin.stat}
\alias{get.fin.stat}
\title{
Internal Function!
}
\description{
This is a function used in other functions and must not work outside it's enviroments.
}
\usage{
get.fin.stat(firms, quarter = NULL)
}
\arguments{
  \item{firms}{
a vector with some information of each firm (CNPJ, name, negotiation code, CVM code). For further informations see getFinancialStatements.
}
  \item{quarter}{
Quarter date. Ex.: 2006-06
}
}
\author{
Michel Ricardo Meyer \email{michel.ricardo.meyer@gmail.com}
}
\seealso{
\code{\link{getFinancialStatements}}
\code{\link{getBalanceSheet}}
\code{\link{getIncomeStatements}}
}