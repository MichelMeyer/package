\name{getIncomeStatements}
\alias{getIncomeStatements}
\title{
Loads the firm's Income Statements.
}
\description{
This functions load to R a data.frame with all income statements for the specified firms.

It also could do some adjustments in data to all data be comparable. It is needed because there are more than one kind of financial statements. Here we show two of them one with annual scale (DFP) and another with quarter scale (ITR).
}
\usage{
getIncomeStatements(firms, quarter = NULL, adjust = F)
getIncomeStatements(firms, quarter = NULL, adjust = T)
getIncomeStatements(firms, quarter = "yyyy-mm")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
For \strong{firms} and \strong{quarter}, same examples and possibilities as in \code{\link{getFinancialStatements}} and for

\strong{adjust} there are two possibilities:

FALSE to do not adjust to each kind of financial demonstration and
TRUE to do the adjustment.
}
\author{
Michel Ricardo Meyer \email{michel.ricardo.meyer@gmail.com}
}
\seealso{
\code{\link{getFinancialStatements}}
\code{\link{getBalanceSheet}}
}
\examples{
# Here we have a panel with the third collum showing the kind of demonstration.
View(getIncomeStatements("VALE", adjust = F))

# Here we have a panel too, but without the column evidencing the type of
# demonstration (ITR or DFP).
# Some rows are removed (not enough data to make the adjustment)
# in order to avoid getting the package's user to error.
View(getIncomeStatements("VALE", adjust = T))

# Here the adjust argument is ignored, setting a quarter differently of NULL
# will force the function to output the original from CVM's system for that quarter.
View(getIncomeStatements("VALE", quarter = "2015-12", adjust = F))
}
