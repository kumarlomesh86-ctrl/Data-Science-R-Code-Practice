
for(t in tickers){
  a=get(noquote(t))[,1] 
  print(c(t,length(a)))
  }

df = list ()
j = 0
for (t in tickers) {
  j = j + 1
  a = noquote(t)
  b = data.frame(get(a)[ ,3])
  b$dt = row.names(b)
  df[[ j ]] = b
}
stock_table = df[[1]]
for ( j in 2:length(df)) {
  stock_table =merge(stock_table ,df[[j]],by="dt")
}
dim(stock_table)


# Arrange plotting area into 3 rows and 2 columns, reduce margins
par(mfrow = c(1, 1), mar = c(2, 2, 2, 1))

# Loop through tickers and create plots
for (j in 1:length(tickers)) {
  plot(as.Date(stock_table[, 1]), stock_table[, j + 1],
       type = "l",
       ylab = tickers[j],
       xlab = "date")
}

# Reset plotting layout back to default
par(mfrow = c(1, 1))

n=length(stock_table[,1])

for(j in 1:length(tickers)){
  rets[2:n,j]=diff(log(rets[,1]))
}
rets$dt=stock_table$dt
rets=rets[2:n,] #lose the first rowwhen converting to returns
head(rets)

library(corrgram)
corrgram(rets[ ,1:length(tickers)] , order=TRUE, lower.panel=panel.ellipse ,
         upper.panel=panel.pts, text.panel=panel.txt)

library(quantmod)
nasdaq_names=stockSymbols(exchange="NASDAQ")
nyse_names=stockSymbols(exchange="NYSE")
amex_names=stockSymbols(exchange="AMEX")

co_names=rbind(nyse_names,nasdaq_names,amex_names)
dim(co_names)

install.packages("stringr")

library(stringr)
#READ IN THE LIST OF TICKERS
tickers=read.table("tickers.csv",header=FALSE,sep=":")
n=dim(tickers)[1]
names(tickers)=c("Exchange","Symbol")
tickers$ceo=NA
#PULL CEO NAMES FROM GOOGLE FINANCE
for ( j in 1:n) {
  url = paste("https://www.google.com/finance?q=",tickers[j,2],sep="")
  text=readLines(url)
  idx=grep("Chief Executive",text)
  if (length(idx)>0) {
    tickers[j,3]=str_split(text[idx2],">")[[1]][2]
  }
  else {
    tickers[j,3]=NA
  }
  print(tickers[j,])
}
#WRITECEO_NAMESTOCSV
write.table(tickers, file="ceo_names.csv",
             row.names=FALSE,sep=",")

head(tickers)

apply(rets[ ,1:(length(tickers)1)],2,mean),
colMeans(rets[,1:(length(tickers)-1)]),


inR.
#FUNCTIONTOREADINCSVFILESFROMFRED
#Enter SeriesID as a text string
readFRED=function(SeriesID){
  url = paste("https: / /research.stlouisfed.org/fred2/series/" ,SeriesID,
              "/downloaddata/" ,SeriesID,".csv" ,sep="")
  data = readLines(url)
  n = length(data)
  data = data[2:n]
  n = length(data)
  df =matrix(0,n,2) #top line is header
  for ( j in 1:n) {
    tmp= strsplit(data[ j ] ," ,")
    df[j ,1] = tmp[[1]][1]
    df[j ,2] = tmp[[1]][2]
  }
  rate = as.numeric(df[ ,2])
  idx =which(rate>0)
  idx = setdiff(seq(1,n) ,idx)
  rate[idx] = 99
  date = df[ ,1]
  df = data.frame(date,rate)
  names(df)[2] = SeriesID
  result = df
  }
  
#CONVERTALLDATESTONUMERICANDSORTBYDATE
dates = rates[,1]
library(stringr)
dates=as.numeric(str_replace_all(dates," " ,""))
res=sort(dates,index.return=TRUE)
for ( j in 1:dim(rates)[2]) {
  rates[,j] = rates[res$ix,j]
}
head(rates)

data=readLines("https://www.fdic.gov/bank/individual/failed/banklist.csv")
head(data)

data=read.csv("QS.csv",header=TRUE)
print(col(data))


print(head(data))
data$count = 1
print(head(data))

any(is.na(data))

res = sort(as.matrix(data$ST),index.return=TRUE)
print(data[res$ix,])
print(sort(unique(data$ST)))



data=read.csv("CA_Crimes_data_2004_2013.csv", header=TRUE)



library(data.table)
D_T=as.data.table(data)

setkey(D_T,Year)
crime=6
res=D_T[,sum(Reported),by=Year]
print(res)
class(res)

plot(res$Year,res$V1,type ="b",lwd=3,col="blue",xlab = "Year", ylab = "forced_Rape")

setkey(D_T, County)

res <- D_T[, .(Rape = sum(Rape)), by = County]   # better naming directly
setkey(res, Rape)

print(res)


par(las=2) #makes label horizontal
#par(mar=c(3,4,2,1)) #increase y axis margins
barplot(County_Rapes$Rapes,names.arg=County_Rapes$County,
        horiz=TRUE, cex.names=0.4, col=8)
