\name{getFinancialStatements}
\alias{getFinancialStatements}
\title{
Loads the firm's Financial Statements.
}
\description{
This functions load to R a data.frame with all financial statements for the specified firms.
}
\usage{
getFinancialStatements(firms, quarter = NULL)
getFinancialStatements(firms, quarter = "yyyy-mm")
}
\arguments{
  \item{firms}{
Value that must have one of the values below:

\strong{CVM code}

Uses the CVM code to search for the firm.

Format: 123456

Examples (Itau Unibanco): getFinancialStatements(firms = 19348) or getFinancialStatements(firms = 019348)

\strong{CNPJ}

Search for the firm using their CNPJ (Cadastro Nacional de Pessoa Juridica - National Registry of Legal Person).
  
Format: '12.345.678/9012-34'
  
Example (Itau Unibanco): getFinancialStatements(firms = '60.872.504/0001-23')

\strong{Negotiation code}

Uses the Negotiation Code to find the firm's balance sheet. Inside the function, only the first four characters are used.
Format: 'ABCD1'

Example (Itau Unibanco): getFinancialStatements(firms = 'ITUB3')

As the parameter suggests, it is also possible to use a vector with several companies (firms = c("VALE", "PETRO", "WEGE")) or \strong{you can use firms = "all".}
}
  \item{quarter}{
  If setted as NULL the function return a table with the principal accounts of all financial statements.
  
  The format to this parameter is: "yyyy-mm". Example.: "2016-12".
}
}
\details{
In Brazil, some operations of a financial nature are calculated within the operating result, with many fundamentalist indexes.

In the future we intend to create functions that return the financial statements to international accounting standards.
}
\value{
Described in the examples section!
}
\references{
The documents with financial statements are available \href{http://cvmweb.cvm.gov.br/SWB/Sistemas/SCW/CPublica/CiaAb/FormBuscaCiaAb.aspx?TipoConsult=c}{here} in the CVM system. More information could also be found in the \href{cvm.gov.br}{CVM's site}
}
\author{
Michel Ricardo Meyer \email{michel.ricardo.meyer@gmail.com}
}
\note{
}
\seealso{
\code{\link{getBalanceSheet}}
\code{\link{getIncomeStatements}}
}
\examples{
# There are two ultimate examples, The first is:
View(getFinancialStatements(c(4170, "CIA VALE DO RIO DOCE", "33.592.510/0001-54", "VALE")))
# That downloads the vale financial statements four times,
# One time for each kind of company information given.
# Notice that the function also accepts the name of the firm, unlike said in arguments section.

# The second example is:
View(getFinancialStatements("VALE", "2015-12"))
# Here there are a difference in the output, the information looks much more like
# the original format that it's available in CVM's system. it's intentional,
# this is the original infomation. Use it if you need to.
}