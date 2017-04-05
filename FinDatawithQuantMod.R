#In this analysis, we will be using the quantmod library
library(quantmod)

#Here we will input recent stock data
#We will find correlations between these stocks and output these correlations to a CSV file
#We can also view all of the relevant data for each stock in R, opening up a range of data analysis possibilities

#List of stock symbols in the universe we will be exploring
Nasdaq100_Symbols <- c("AAPL", "ADBE", "ADI", "ADP", "ADSK", "AKAM", "ALTR", "ALXN",
                       "AMAT", "AMGN", "AMZN", "ATVI", "AVGO", "BBBY", "BIDU", "BIIB",
                       "BRCM", "CA", "CELG", "CERN", "CHKP", "CHRW", "CHTR", "CMCSA",
                       "COST", "CSCO", "CTRX", "CTSH", "CTXS", "DISCA", "DISCK", "DISH",
                       "DLTR", "DTV", "EBAY", "EQIX", "ESRX", "EXPD", "EXPE", "FAST",
                       "FB", "FFIV", "FISV", "FOXA", "GILD", "GMCR", "GOOG", "GOOGL",
                       "GRMN", "HSIC", "ILMN", "INTC", "INTU", "ISRG", "KLAC", "KRFT",
                       "LBTYA", "LLTC", "LMCA", "LMCK", "LVNTA", "MAR", "MAT", "MDLZ",
                       "MNST", "MSFT", "MU", "MXIM", "MYL", "NFLX", "NTAP", "NVDA",
                       "NXPI", "ORLY", "PAYX", "PCAR", "PCLN", "QCOM", "QVCA", "REGN",
                       "ROST", "SBAC", "SBUX", "SIAL", "SIRI", "SNDK", "SPLS", "SRCL",
                       "STX", "SYMC", "TRIP", "TSCO", "TSLA", "TXN", "VIAB", "VIP",
                       "VOD", "VRSK", "VRTX", "WDC", "WFM", "WYNN", "XLNX", "YHOO")

#Input all of the data since Jan 1, 2012
getSymbols(Nasdaq100_Symbols, from = "2012-01-01") 

dfClosing <- NULL #initialize a blank dataframe of prices
#put closing prices into a dataframe
for(stock in Nasdaq100_Symbols){
    str_name = paste(stock, ".Close", sep="")
    stck = eval(parse(text=stock))
    Close <- stck[, str_name]
    dfClosing <- cbind(dfClosing, Close)
}

#Here we eliminate columns where the value of all rows is NA
dfClosing <- dfClosing[,colSums(is.na(dfClosing))<nrow(dfClosing)]

#Determine the number of rows and columns
nrow(dfClosing) #2572
ncol(dfClosing) #104

print(head(dfClosing)) #Head of new dataframe of closing prices

corr = cor(dfClosing)
corr <- corr[,colSums(is.na(corr))<50]
#write.csv2(corr, "corMat_r_3.csv") #Saved on computer as: StockCorrelations_R


#Find out the number of stocks with negative correlations
negativeCorrelations = apply(corr, 2, function(x) length(x[x<0]))
negativeCorrelations #Here we can see how many negative correlations each stock has 
negativeCorrelationsVec = as.vector(negativeCorrelations)

#Here we see the maximum number of negative correlations -- 103!
#The stock with 89 negative correlations is CERN (Cerner Corporation).
#CERN is a health information tech company with a market cap ~3.5B
max(negativeCorrelationsVec)

#Thus, the stock above with numerous negative correlations may be a good target for us,
#as this stock is relatively uncorrelated with the majority of the market. This could decrease
#the risk of our portfolio.

#We will now use the getFinancials() to design a stock filter
#We of course want to purchase stocks of companies with strong financial 
#As an example, let's start by looking at Apple's financials
AAPL <- getFin('AAPL')
applFin = viewFin(AAPL.f, "IS", "A") #IS = Income Statement, A = Annual
applFin <- applFin[, 1] #Income statement data of Apple for the most recent year
applFin #Let's see this data

#initialize blank dataframes to hold the financials for each company (income statement, cash flow report, balance sheet)
dfIncomeStatements <- NULL 
dfCashFlow <- NULL
dfBalanceSheet <- NULL

#Put financials for each company into these three dataframes
#Let's start with the Income Statements
for(stock in Nasdaq100_Symbols){
  tryCatch({
    stck = eval(parse(text=stock))
    stck <- getFin(stock)
    str_name = paste(stock, ".f", sep="")
    find = eval(parse(text=str_name))
    cur_fin = viewFin(find, "IS", "A") 
    cur_fin <- cur_fin[, 1] #Only want data for the most recent period
    dfIncomeStatements <- cbind(dfIncomeStatements, cur_fin) 
    colnames(dfIncomeStatements)[ncol(dfIncomeStatements)] <- stock
    stop("")}, error=function(e){})}

#Balance Sheet
for(stock in Nasdaq100_Symbols){
  tryCatch({
    stck = eval(parse(text=stock))
    stck <- getFin(stock)
    str_name = paste(stock, ".f", sep="")
    find = eval(parse(text=str_name))
    cur_fin = viewFin(find, "BS", "A") 
    cur_fin <- cur_fin[, 1]
    dfBalanceSheet <- cbind(dfBalanceSheet, cur_fin) 
    colnames(dfBalanceSheet)[ncol(dfBalanceSheet)] <- stock
    stop("")}, error=function(e){})}


#Cash Flow Statement
stock_list <- vector()
for(stock in Nasdaq100_Symbols){
  tryCatch({
    stck = eval(parse(text=stock))
    stck <- getFin(stock)
    str_name = paste(stock, ".f", sep="")
    find = eval(parse(text=str_name))
    cur_fin = viewFin(find, "CF", "A") 
    cur_fin <- cur_fin[, 1] 
    dfCashFlow <- cbind(dfCashFlow, cur_fin) 
    colnames(dfCashFlow)[ncol(dfCashFlow)] <- stock
    stop("")}, error=function(e){})}

#Take a quick look at what our data looks like -- can view all the information
View(dfIncomeStatements)
View(dfBalanceSheet)
View(dfCashFlow)

head(dfIncomeStatements)
head(dfBalanceSheet)
head(dfCashFlow)

dfIncomeStatements[["Diluted Normalized EPS"]]



#Next steps...

#Possible financials to look at: p/e, eps, ROA [net income/total assets], ROE [net income/shareholder equity]
#current ratio [current assets/curr liabilities], EV/EBITDA, asset turnover [net revenues/avg total assets]
#debt/assets [total liabilities/total assets], debt/equity [total debt/total shareholder equity], gross profit margin [gross income/net revenue]

#balanceshseet
"Total Inventory"
"Total Current Assets"
"Total Assets"
"Total Debt"
"Total Liabilities"
"Total Equity"

#cash flow
"Capital Expenditures"
"Changes in Working Capita"

#income statement
"Total Revenue"
"Operating Income"
"Gross Profit"
"Net Income"
"Diluted Normalized EPS"