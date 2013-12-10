# Working with financial data - Stocks and financial performance
library(quantmod)
library(zoo)
library(TTR)

# 1.- Get financial symbols (sources: yahoo,google,MySQL,FRED,csv,RData,oanda)
getSymbols("AAPL")
getSymbols("AAPL",src="yahoo")

# 2.- Get stock price historical data
hist <- getYahooData("AAPL",20130101,20131203)
str(hist)
head(hist)
plot(hist$Close)
plot(hist$Low/hist$High)

# Calculus
# % of change (delta)
open <- hist$Open
close <- hist$Close
delta <- Delt(open,close)
plot(delta)

# 3.- Corporate financial information
apple <- getFin("AAPL")
AAPL.f
# Balance sheet
viewFin(AAPL.f,type="BS",period="A")
# Income statement
viewFin(AAPL.f,type="IS",period="A")
# Cash Flow
viewFin(AAPL.f,type="CF",period="A")

# 4.- Forex
fx <- getFX("EUR/GBP",from="2013-01-01",to="2013-12-04")
head(fx)
plot(EURGBP)

# 5.- Charting
getSymbols("AAPL")
# Standard charting
chartSeries(AAPL)
# Candlesticks
chartSeries(AAPL,type="candlesticks")
candleChart(AAPL,multi.col=TRUE,theme='white')
# Barcharts
barChart(AAPL,theme='white.mono',bar.type='hlc')
# Linecharts
lineChart(AAPL,line.type='h',TA=NULL)
# Charting subsets
candleChart(AAPL,subset='2007-12::2008')
candleChart(AAPL,theme='white', type='candles')
reChart(major.ticks='months',subset='first 16 weeks')



