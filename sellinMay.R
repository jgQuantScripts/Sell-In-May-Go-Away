require("quantmod");require("PerformanceAnalytics");require("ggplot2")

# indices to test
idx <- c("^GSPC","^DJI","^IXIC","^RUT")

# get data
e <- new.env()
getSymbols(idx,env = e, from="1970-01-01")

# merge Close, Low, & High Prices
idxClose <- do.call(merge,eapply(e,Cl))
idxLow   <- do.call(merge,eapply(e,Lo))
idxHigh  <- do.call(merge,eapply(e,Hi))

# extract May only
idxClose <- idxClose[.indexmon(idxClose)==4]
idxLow   <- idxLow[.indexmon(idxLow)==4]
idxHigh  <- idxHigh[.indexmon(idxHigh)==4]

# adjust column names
colnames(idxClose) <- gsub(".Close","",names(idxClose))
colnames(idxLow) <- gsub(".Low","",names(idxLow))
colnames(idxHigh) <- gsub(".High","",names(idxHigh))

# get latest quote if not in dataset
if(Sys.Date() != last(index(idxClose)))
{
  qte <- getQuote(idx)
  qte$Symbol <- gsub("\\^","",rownames(qte))
  CLOSE <- cbind(qte$Last,qte$Symbol)
  noms <- CLOSE[,2]
  CLOSE <- xts(t(as.numeric(CLOSE[,1])), order.by = Sys.Date())
  colnames(CLOSE) <- t(noms)
  idxClose <- rbind(idxClose,CLOSE[,names(idxClose)])
  
  HIGH <- cbind(qte$High,qte$Symbol)
  noms <- HIGH[,2]
  HIGH <- xts(t(as.numeric(HIGH[,1])), order.by = Sys.Date())
  colnames(HIGH) <- t(noms)
  idxHigh <- rbind(idxHigh,HIGH[,names(idxHigh)])
  
  LOW <- cbind(qte$Low,qte$Symbol)
  noms <- LOW[,2]
  LOW <- xts(t(as.numeric(LOW[,1])), order.by = Sys.Date())
  colnames(LOW) <- t(noms)
  idxLow <- rbind(idxLow,LOW[,names(idxLow)])
  
}

# tickers to apply function
tickers <- names(idxClose)
# get May - returns
df <- lapply(as.list(tickers), function(tic){
  indx <- na.omit(idxClose[,paste(tic)])
  lows <- na.omit(idxLow[,paste(tic)])
  highs<- na.omit(idxHigh[,paste(tic)])
  
  # extract unique years to subset data
  yrs <- unique(.indexyear(indx))
  nYr <- unique(format(index(na.omit(indx)), format="%Y"))
    
  data <- lapply(as.list(1:length(yrs)), function(i){
    tmpCl <- indx[.indexyear(indx)==yrs[i]]
    tmpLo <- lows[.indexyear(lows)==yrs[i]]
    tmpHi <- highs[.indexyear(highs)==yrs[i]]
    cl2cl_return <- round(as.numeric(last(tmpCl))/as.numeric(first(tmpCl))-1,4)
    cl2worst     <- round(min(tmpLo)/as.numeric(first(tmpCl))-1,4)
    cl2best      <-  round(max(tmpHi)/as.numeric(first(tmpCl))-1,4)
    dta <- as.data.frame(cbind(tic,as.numeric(nYr[i]),cl2cl_return,cl2best,cl2worst))
    colnames(dta) <- c("Symbol","Year","moReturn","toHigh","toLow")
    dta
  })
  # rbind
  data<- do.call(rbind,data)
  # change column classes to numeric
  data$Year <- as.numeric(data$Year)
  data$moReturn <- as.numeric(data$moReturn)
  data$toHigh <- as.numeric(data$toHigh)
  data$toLow <- as.numeric(data$toLow)
  # return data
  data
})

# rbind
df <- do.call(rbind,df)

# histogram of Index Returns by Year
df %>% 
  ggplot(aes(x=moReturn)) +
  geom_histogram(alpha=0.8, binwidth = 0.02) +
  facet_wrap(~Symbol)

# is it any different for the year following election years
# new Presidents may invoke new policies...
postElectYrs <- seq(1973,2021,4)

electYrs <- lapply(as.list(postElectYrs), function(ii){
  # subset each yr following an election year
  subset(df, df$Year == ii)
})
electYrs<- do.call(rbind, electYrs)

# detailed summary
dtlStat <- cbind(table.Stats(R=electYrs$moReturn),table.Stats(R=df$moReturn)) 
colnames(dtlStat) <- c("postElection","All")
View(dtlStat)

# ************************************************************************************
#                                   ticker/Index specific
# ************************************************************************************
tic = "GSPC"
eYR <- subset(electYrs,electYrs$Symbol == tic)
ALL <- subset(df, df$Symbol == tic)

# detailed summary
dtlStat <- cbind(table.Stats(R=eYR$moReturn),table.Stats(R=ALL$moReturn)) 
colnames(dtlStat) <- c("postElection","All")
View(dtlStat)
# ****************************************************
# plot cumulative return per year
plotReturns = function(ticker)
{
  # subset data
  dt = na.omit(idxClose[,ticker])
 
  # extract unique years to subset data
  yrs <- unique(.indexyear(dt))
  nYr <- unique(format(index(na.omit(dt)), format="%Y"))
  
  # first available year
  tmpCl <- dt[.indexyear(dt)==yrs[1]] 
  tmpCl <- cumsum(coredata(na.omit(ROC(tmpCl, type="discrete"))))
  plot(tmpCl, type="l", ylim=c(-0.2,0.20), 
       main=paste0("May Cumulative Return by Year: ",ticker),
       xlab="Days", ylab="Return", col='grey')
  
  # plot all other years 
  for(ii in 2:length(yrs))
  {
    tmpCl <- dt[.indexyear(dt)==yrs[ii]] 
    tmpCl <- cumsum(coredata(na.omit(ROC(tmpCl, type="discrete"))))
    lines(tmpCl,col="grey")
  }
  # plot current year
  tmpCl <- dt[.indexyear(dt)==yrs[length(yrs)]] 
  lines(cumsum(coredata(na.omit(ROC(tmpCl, type="discrete")))), col="black")
}
# plot index/ticker of choice
plotReturns("DJI")
# ****************************************************
# plot post election cumulative return per year
plotPostElectionReturns = function(ticker)
{
  # subset data
  dt = na.omit(idxClose[,ticker])
  
  # years following a US election
  # postElectYrs <- unique(.indexyear(dt))
  #loc = which(unique(.indexyear(dt)) == 73)
  #postElectYrs <- postElectYrs[seq(loc,length(postElectYrs),4)]
  
  firstYr <- unique(.indexyear(dt))[1]
  postElectYrs <- seq(73,121,4)
  postElectYrs <- postElectYrs[postElectYrs >= firstYr]
  
  electYrs <- lapply(as.list(postElectYrs), function(ii){
    # subset each yr following an election year
    subset(dt, .indexyear(dt) == ii)
  })
  electYrs<- do.call(rbind, electYrs)
  
  # first available year
  tmpCl <- electYrs[.indexyear(electYrs)==postElectYrs[1]] 
  tmpCl <- cumsum(coredata(na.omit(ROC(tmpCl, type="discrete"))))
  plot(tmpCl, type="l", ylim=c(-0.1,0.10), 
       main=paste0("May Cumulative Return Post Election Year: ",ticker),
       xlab="Days", ylab="Return", col='grey')
  
  # plot all other years 
  for(ii in 2:length(postElectYrs))
  {
    tmpCl <- electYrs[.indexyear(electYrs)==postElectYrs[ii]] 
    tmpCl <- cumsum(coredata(na.omit(ROC(tmpCl, type="discrete"))))
    lines(tmpCl,col="grey")
  }
  # plot current year
  tmpCl <- dt[.indexyear(dt)==postElectYrs[length(postElectYrs)]] 
  lines(cumsum(coredata(na.omit(ROC(tmpCl, type="discrete")))), col="black")
}
# plot index/ticker of choice
plotPostElectionReturns("IXIC")


