\name{getAdjPrices}
\alias{getAdjPrices}
\title{
Loads the prices data and do the adjusts in returns which you specify!
}
\description{
Returns the diary infomation about the prices and/or the transactioned volume of the shares adjusted by the factors that you choose.
}
\usage{
getPrices(shares, info = "full", subscription = "rational")
getPrices(shares, info = "simplified", subscription = "rational")
getPrices(shares, info = "single", subscription = "rational", value = "Return")
}
\arguments{
  \item{shares}{
A vector with the names of the shares or
  #           "all" to download shares with adjusted prices.
}
  \item{by}{
A vector with the corporate events must be used in the adjust. Slipts and alterations in the factor of quotation are always adjusted:

\strong{all} - It uses all the events;

Separately it's possible to specify in a vector a combination of these:

\strong{Dividend, Interest, Bonus shares, Subscription Right, Spinoff, Return of Capital}.
}
  \item{subscription}{
There are three alternatives of specification for this argument:

\strong{rational} - buy the share when the price of subscriptied share is cheaper than the price of the share in the market;

\strong{neverbuy} - self explanatory;

\strong{alwaysbuy} - self explanatory
}
\item{info}{
\strong{full} - returns a data.frame with the bovespa's quotation data;

\strong{simplified} - returns a data.frame with the open, close, min, max, mean, bid and ask prices, transactioned volume of the day and other informations that changed between the specified shares;

\strong{single} - returns a xts object with a single column specified in paramenter value.
}
  \item{value}{
only used if info = "single".

The setted argument is the "Return" column, but it can be changed at will.
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
\details{

An explanation will be given as to how stock returns are adjusted


}
\value{
The function has two different outputs.

A data.frame with the share in the first collum and date as second for info setted as "full" or "simplified"

A xts with a collum (the one from value parameter) taken from the original data.frame for each share.
}
\references{
To the original information see:

\href{http://www.bmfbovespa.com.br/en_us/services/market-data/historical-data/equities/historical-data-1/}{Bovespa's Historical Data}

The BM&F Bovespa's site also have the firm's corporative events. Not everyone, unfortunately.
}
\author{
Michel Ricardo Meyer /email{michel.ricardo.meyer@gmail.com}
}

\seealso{
\code{\link{getPrices}}
}
\examples{

# Same as in \code{\link{getPrices}} but now we have one more parameter.
tests <- getAdjPrices(c("PETR3", "VALE3", "WEGE3"), by = "", info = "single", value = "Close")

# Here we have the PETR3 quotation adjusted only by the splits and alteration of quotation factor.
plot(tests[, "PETR3"])

tests <- getAdjPrices(c("PETR3", "VALE3", "WEGE3"), by = "all", info = "single", value = "Close")
# Now adjusted by all corporative events.
plot(tests[, "PETR3"])
}
