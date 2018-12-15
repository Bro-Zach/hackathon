library(RODBC)
library(ggplot2)
library(ggvis)
library(dplyr)
library(plotly)
py <- plotly()

getDataFrame <- function(df, startDate, endDate, tIDColName = "CUST_ID", tDateColName = "TRX_DATE", tAmountColName = "AMOUNT") {
  # sort data frame by date descendingly
  df <- df[order(df[ , tDateColName], decreasing = TRUE), ]
  
  # remove records outside date range [startDate, endDate]
  # == dplyr POTENTIALS ==
  df <- df[df[ , tDateColName] >= startDate, ]
  df <- df[df[ , tDateColName] <= endDate, ]
  
  # remove rows with duplicate Customer ID
  # == dplyr POTENTIALS ==
  newdf <- df[!duplicated(df[ , tIDColName]), ]
  
  # calc. the Recency [days] to the endDate; smaller the Recency == more recent
  # == dplyr POTENTIALS ==
  Recency <- as.numeric(difftime(endDate, newdf[ , tDateColName], units = "days"))
  # combine Days column to the newdf data frame
  newdf <- cbind(newdf, Recency)
  
  # sort data frame by ID to fit the return order of table() and tapply()
  newdf <- newdf[order(newdf[ , tDateColName]), ]
  
  # calc. the Frequency
  tmp <- as.data.frame(table(df[ , tIDColName]))
  Frequency <- tmp[ , 2]
  # combine Frequency to the newdf data frame
  newdf <- cbind(newdf, Frequency)
  
  # calc. the Monetary
  tmp <- as.data.frame(tapply(df[ , tAmountColName], df[ , tIDColName], sum))
  Monetary <- tmp[ , 1] / Frequency
  # combine Monetary to the newdf data frame
  newdf <- cbind(newdf, Monetary)
  
  return(newdf)
}

getIndepScore <- function(df, r = 5, f = 5, m = 5) {
  if (r <= 0 || f <= 0 || m <= 0) return
  
  # sort data set for Recency with Recency (ascending) - Frequency (descending) - Monetary (descending)
  df <- df[order(df$Recency, -df$Frequency, -df$Monetary), ]
  R_score <- scoring(df, "Recency", r)
  # combine R_score to the df data frame
  df <- cbind(df, R_score)
  
  # sort data set for Frequency with Recency (descending) - Frequency (ascending) - Monetary (descending)
  df <- df[order(-df$Recency, df$Frequency, -df$Monetary), ]
  F_score <- scoring(df, "Frequency", f)
  # combine R_score to the df data frame
  df <- cbind(df, F_score)
  
  # sort data set for Monetary with Recency (descending) - Frequency (descending) - Monetary (ascending)
  df <- df[order(-df$Recency, -df$Frequency, df$Monetary), ]
  M_score <- scoring(df, "Monetary", f)
  # combine R_score to the df data frame
  df <- cbind(df, M_score)
  
  # sort data frame by R_score, F_score, and M_score (all in descending order)
  df <- df[order(-df$R_score, -df$F_score, -df$M_score), ]
  
  # calc. the Total Score
  Total_score <- c((100 * df$R_score) + (10 * df$F_score) + df$M_score)
  
  # combine Total_score to the df data frame
  df <- cbind(df, Total_score)
  
  return(df)
}

scoring <- function(df, column, r = 5) {
  # Determined number of measures of data frame
  len <- dim(df)[1]
  # create the number of rows found in passed-in data frame with "0" as value
  score <- rep(0, times = len)
  
  # Determine the quantity of rows per bin (default: 5 bins)
  nr <- round(len / r)
  if (nr > 0) {
    # seperate the rows by r aliquots
    rStart <- 0
    rEnd <- 0
    # interate through each bin (5)
    for (i in 1:r) {
      # initialize start row number and end row number
      rStart = rEnd + 1
      
      # skip one "i" if the rStart is already in the i+1 or i+2 or ... scope.
      if (rStart > i * nr) next
      
      # i is at last bin
      if (i == r) {
        if (rStart <= len) rEnd <- len else next
      } else {
        rEnd <- i * nr
      }
      
      # set Recency score | since data set is sorted (ascending), 1st-bin has scores=5, 2nd-bin has score=4, ..., 5th-bin has score=1
      score[rStart : rEnd] <- r - i + 1
      
      # ensure customer with same Recency have the same score
      s <- rEnd + 1
      if ((i < r) & (s <= len)) {
        for (u in s:len) {
          if (df[rEnd, column] == df[u, column]) {
            score[u] <- r - i + 1
            rEnd <- u
          } else {
            break;
          }
        }
      }
    }
  }
  
  return(score)
}

getScoreWithBreaks <- function(df, r, f, m) {
  
  # scoring the Recency
  len = length(r)
  R_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,R_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=r[i-1]
    }
    p2=r[i]
    
    if(dim(df[p1<df$Recency & df$Recency<=p2,])[1]>0) df[p1<df$Recency & df$Recency<=p2,]$R_Score = len - i+ 2
  }
  
  # scoring the Frequency	
  len = length(f)
  F_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,F_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=f[i-1]
    }
    p2=f[i]
    
    if(dim(df[p1<df$Frequency & df$Frequency<=p2,])[1]>0) df[p1<df$Frequency & df$Frequency<=p2,]$F_Score = i
  }
  if(dim(df[f[len]<df$Frequency,])[1]>0) df[f[len]<df$Frequency,]$F_Score = len+1
  
  # scoring the Monetary	
  len = length(m)
  M_Score <- c(rep(1,length(df[,1])))
  df <- cbind(df,M_Score)
  for(i in 1:len){
    if(i == 1){
      p1=0
    }else{
      p1=m[i-1]
    }
    p2=m[i]
    
    if(dim(df[p1<df$Monetary & df$Monetary<=p2,])[1]>0) df[p1<df$Monetary & df$Monetary<=p2,]$M_Score = i
  }
  if(dim(df[m[len]<df$Monetary,])[1]>0) df[m[len]<df$Monetary,]$M_Score = len+1
  
  #order the dataframe by R_Score, F_Score, and M_Score desc
  df <- df[order(-df$R_Score,-df$F_Score,-df$M_Score),]
  
  # calc. the total score
  Total_Score <- c(100*df$R_Score + 10*df$F_Score+df$M_Score)
  
  df <- cbind(df,Total_Score)
  
  return(df)
}


require(plyr)
getPercentages <- function(df, colNames){
  
  Var <- c(colNames, "Transact")
  
  df <- df[ ,names(df) %in% Var, drop = F]
  
  a <- ddply(df, Var, summarize, Number = length(Transact))
  b <- ddply(
    a, 
    .(), 
    .fun = function(x) {
      transform(x, Percentage=with(x,round(ave(Number,a[,names(a) %in% Var,drop=F],FUN=sum)/ave(Number,a[,names(a) %in% colNames,drop=F],FUN=sum),2)))
    }
  )
  
  b <- b[b$Transact == 1, -1]
  
  return(b)
}

getCLV <- function(r, f, rev, cost, n, periods, dr, pModel) {
  df <- data.frame(period = c(0), r = c(r), f = c(f), n = c(n), value = c(0))
  
  for (i in 1:periods) {
    backstep <- df[df$period == i-1, ]
    nrow <- nrow(backstep)
    
    for (j in 1:nrow) {
      r <- backstep[j, ]$r
      f <- backstep[j, ]$f
      n <- backstep[j, ]$n
      p <- predict(pModel, data.frame(Recency = r, Frequency = f), type = "response")[1]
      buyers <- n * p
      
      # Predict odds of a "Buy" for this period
      df <- rbind( df, c(i, 0, f+1, buyers, buyers*(rev-cost) / (1+dr)^i ))
      
      # Predict odds of a "No-Buy" for this period
      df <- rbind( df, c(i, r+1, f, n-buyers, (n-buyers)*(0-cost) / (1+dr)^i ))
    }
  }
  
  return(sum(df$value))
}

channel <- odbcConnect("demo_data", uid="hack_team", pwd="hack")
#transact_df <- sqlQuery(channel, "SELECT b.ra_cust_id, TO_CHAR(RA_TRX_DATE, 'YYYYMMDD') as TRANSACT_DATE,case when RA_DEBIT_CREDIT = 'D' then -1*sum(ra_txn_amount) else sum(ra_txn_amount) end amount FROM B_transact a, B_ACCOUNT b where a.ra_act_no=b.ra_act_no group by ra_cust_id, RA_DEBIT_CREDIT, TO_CHAR(RA_TRX_DATE, 'YYYYMMDD') order by ra_cust_id")
#cust_transact <- sqlQuery(channel, "SELECT b.ra_cust_id, TO_CHAR(RA_TRX_DATE, 'YYYYMMDD') as TRANSACT_DATE,case when RA_DEBIT_CREDIT = 'D' then -1*sum(ra_txn_amount) else sum(ra_txn_amount) end amount FROM B_transact a, B_ACCOUNT b where a.ra_act_no=b.ra_act_no group by ra_cust_id, RA_DEBIT_CREDIT, TO_CHAR(RA_TRX_DATE, 'YYYYMMDD') order by ra_cust_id")

summary(CDNow_df)

CDNow_df <- sqlQuery(channel, "SELECT CUST_ID, TO_CHAR(TRX_DATE, 'YYYYMMDD') AS TRX_DATE ,AMOUNT FROM TRX_DETAILS")
# transform Date column from text to date format
CDNow_df[,2] <- as.Date( as.character(CDNow_df[,2]), "%Y%m%d")

startDate_Hist <- as.Date("19980731", "%Y%m%d")
endDate_Hist <- as.Date("19981031", "%Y%m%d")

startDate_Forecast <- as.Date("19981130", "%Y%m%d")
endDate_Forecast <- as.Date("19981231", "%Y%m%d")

Hist_df <- getDataFrame(CDNow_df, startDate_Hist, endDate_Hist)

Forecast_df <- getDataFrame(CDNow_df, startDate_Forecast, endDate_Forecast)	

Forecast_Period <- as.numeric(difftime(endDate_Forecast, startDate_Forecast))
Hist_df$Recency <- Hist_df$Recency %/% Forecast_Period

breaks <- seq(0, round(max(Hist_df$Monetary) + 9), by = 10)	
Hist_df$Monetary <- as.numeric( cut(Hist_df$Monetary, breaks, labels = FALSE) )

Transact <- rep(0, nrow(Hist_df))
Hist_df <- cbind(Hist_df, Transact)

head(pRecency)


Hist_df[Hist_df$CUST_ID %in% Forecast_df$CUST_ID, ]$Transact <- 1

Training_df <- Hist_df

pRecency <- getPercentages(Training_df, "Recency")
pFreq <- getPercentages(Training_df, "Frequency")
pMonetary <- getPercentages(Training_df, "Monetary")

# plot and draw fit curves of Percentage ~ r,f,m
par( mfrow = c(1, 3), oma = c(0,0,2,0) )  									# set canvas for 1 row - 3 columns, with specified outer margin area
plot(pRecency$Recency, pRecency$Percentage * 100, xlab = "Recency", ylab = "Prob of Transaction (%)")
lines(lowess(pRecency$Recency, pRecency$Percentage * 100), col="blue", lty = 2)
plot(pFreq$Frequency, pFreq$Percentage * 100, xlab = "Frequency", ylab = "Prob of Transaction (%)")
lines(lowess(pFreq$Frequency, pFreq$Percentage * 100), col="blue", lty = 2)
plot(pMonetary$Monetary, pMonetary$Percentage * 100, xlab = "Monetary", ylab = "Prob of Transaction (%)")
lines(lowess(pMonetary$Monetary, pMonetary$Percentage * 100), col="blue", lty = 2)
title("Percentages ~ (Recency, Frequency, Monetary)", y=10, outer=TRUE)



# logistics regression on Purchase Pctg ~ Recency
r.glm = glm(Percentage~Recency, family = quasibinomial(link = "logit"), data = pRecency)
# logistics regression on Purchase Pctg ~ Frequency
f.glm = glm(Percentage~Frequency, family = quasibinomial(link = "logit"), data = pFreq)
# logistics regression on Purchase Pctg ~ Monetary
m.glm = glm(Percentage~Monetary, family = quasibinomial(link = "logit"), data = pMonetary)

par( mfrow = c(1, 1) )
# REF: Interpreting Logistics Regression: http://www.ats.ucla.edu/stat/r/dae/logit.htm
model <- glm(Transact ~ Recency + Frequency, data = Training_df, family = quasibinomial(link = "logit"))
pred_01 <- predict(model, data.frame(Recency = c(0), Frequency = c(1)), type = "response")
pred_02 <- predict(model, data.frame(Recency = c(0), Frequency = c(2)), type = "response")
pred_03 <- predict(model, data.frame(Recency = c(0), Frequency = c(3)), type = "response")

LogReg_Results.Transact_RF <- glm(Transact ~ Recency + Frequency, data = Training_df, family = quasibinomial(link = "logit"))

r = 0 			# inti. Recency state (e.g., 0)
f = 1 			# init. Frequency state (e.g., 1)
Rev = 100 		# Anticipated/Expected revenue from customer.
Cost = 0 		# AssoOkay suspicious of the road tociated cost for each potential customer (Buy or No-Buy) per period.
n = 1 			# Num. of customers with the same Recency and Frequency value.
periods = 3 	# Num. of period(s) customer will stay before churning.  Note, periods was specified as time range for Forecast data set above.
dr = 0.02 		# Discount rate
model = LogReg_Results.Transact_RF

Cust_Value <- getCLV(r, f, Rev, Cost, n, periods, dr, model)



