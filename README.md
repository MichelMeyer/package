# package

###########################################################################################################
This project was discontinued due the actual option of download the cvm data in table format.
To download the Financial Statements of the firms negotiated in bovespa, please use the package GetCVMData:
https://github.com/msperlin/GetCVMData
and to download the ohlc data, one can use the package quantmod.

Thank you.
###########################################################################################################


The base of the package to get the data of cvm and bovespa about the shares negociated in the Stock Exchange of Brasil.

To a easy install do:


if( ! "devtools" %in% installed.packages()) install.packages("devtools")
library(devtools)
install_github("MichelMeyer/package")
library(bovdata)
