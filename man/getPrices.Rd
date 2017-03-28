\name{getPrices}
\alias{getPrices}
\title{
Loads the prices data.
}
\description{
Returns the diary infomation about the prices and/or the transactioned volume of the shares.
}
\usage{
getPrices(shares, info = "full")
getPrices(shares, info = "simplified")
getPrices(shares, info = "single", value = "Close", fill = NA)
}
\arguments{
  \item{shares}{
The name of the shares and/or a combination of the options below:

shares, rights and receipts, real estate funds, private bonus,
insolvensy (shares of the firms that in some moment between 1998 and now were in insolvency state), debentures (also in this class there are investment certificates and public debt titles), forward market, options, futures and fractionary.

Eg.: getPrices(shares = c("VALE3", "options", "futures"))

\strong{Be very careful}, depending on the chosen values it could take a very long period.
}
\item{info}{
\strong{full} - returns a data.frame with the bovespa's quotation data.

\strong{simplified} - returns a data.frame with the open, close, min, max, mean, bid and ask prices, transactioned volume of the day and other informations that changed between the specified shares.

\strong{single} - returns a xts object with a single column specified in paramenter value.
}
  \item{value}{
only used if info = "single".

The setted argument is the "Close" column, but it can be changed at will.
}
  \item{fill}{
Some of the shares aren't negotiated every period, so this parameter set how must be treated the missing days.
Notice that this paramenter will only used only if info = "single".
The options are below:

\strong{last} - uses the last negotiated day value to fill the gaps.

\strong{NA} - fills with NAs.

\strong{drop} - Drop the day out of the time serie.
}
}
\value{
The function has two different outputs.

A data.frame with the share in the first collum and date as second for info setted as "full" or "simplified"

A xts with a collum (the one from value parameter) taken from the original data.frame for each share.
}
\references{
To the original information see:

\href{http://www.bmfbovespa.com.br/en_us/services/market-data/historical-data/equities/historical-data-1/}{Bovespa's Historical Data}
}
\author{
Michel Ricardo Meyer /email{michel.ricardo.meyer@gmail.com}
}

\seealso{
\code{\link{getAdjPrices}}
}
\examples{

tests <- getPrices(c("PETR3", "VALE3", "WEGE3"))

# Notice that the output is a panel
head(tests)

# Lets try with info = "single"
tests <- getPrices(c("PETR3", "VALE3", "WEGE3"), info = "single")
# Now notice that the object tests is a xts and the missing values are shown as NA
head(tests)

# One more time, now with value = "Volume" and fill = "drop"
tests <- getPrices(c("PETR3", "VALE3", "WEGE3"), info = "single", value = "Volume", fill = "last")
head(tests)
}
