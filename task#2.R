install.packages("vcd") # install package for mosaicplots
instll.packages("gmodels") # install package for crosstabs
library(vcd) #load package for mosaicplots
library(gmodels) # load package for crosstabs

# Read data in ram
data <- read.delim("C:/Task#2UTF8.txt", encoding  = "UTF-8")
# Check data
summary(data)

# Check for duplicates in clients requests
agg_data<-aggregate(X.U.FEFF.REQUEST_ID~CLIENT_ID, data=data, FUN=length)
agg_data<-head(agg_data[order(agg_data$X.U.FEFF.REQUEST_ID, decreasing=TRUE),]) 
agg_data # check TOP6 clients by request count
 
# Remove duplicated records (clients with multiple request leaving with their latest request status)
data<-data[order(data$X.U.FEFF.REQUEST_ID, decreasing=TRUE),]
data<-data[!duplicated(data$CLIENT_ID),]

# Remove unnecessary columns
data$X.U.FEFF.REQUEST_ID<-NULL
data$REQUEST_ID<-NULL
data$CLIENT_ID<-NULL
data$WORK_STATUS<-NULL
data$MOBILE_PHONE_OPERATOR<-NULL
data$REGION<-NULL
data$EMAIL_DOMAIN<-NULL
data$REQUESTED_AMOUNT<-NULL
data$REQUESTED_TERM<-NULL


## data Preprocessing  & cleaning:
# check all variables
summary(data)
str(data)
table(is.na(data))


# There are outliers in age variable (min-131) assuming, that loans are issued only for persons, 
# who are at least 18y.o, all requests from  clients, who are younger than 18 are removed from data set
data<-subset(data, CLIENT_AGE>=18)

# remove blank values in GENDER variable & relevel
data<-data[!(is.na(data$GENDER) | data$GENDER==""), ]
data$GENDER<-factor(data$GENDER)

# check for blanks in all data set
table(is.na(data))
summary(data)


## exploatoary data analysis
# gender
plot(REQ_STATUS~GENDER, data=data, main="Request status by Age", ylab="Request status", xlab="Gender") # generating plot
CrossTable(data$GENDER, data$REQ_STATUS, chisq=FALSE, prop.chisq = FALSE, prop.t=FALSE, format="SPSS") # generating crosstable

## create histogram & boxplot
par(mfrow=c(1,2))
hist(data$CLIENT_AGE, main="Age histogram", col="Grey", xlab="Age") 
abline(v=mean(data$CLIENT_AGE), col="Red")
boxplot(data$CLIENT_AGE, main="Age boxplot", col="Grey" )
par(mfrow=c(1,1))
summary(data$CLIENT_AGE)
# histogram with ages is right skewed (more young clients)


## Create bins with ages.  Ages are divided by 5 years step (except 18-24 and 60+)
data$CLIENT_AGE_BINS<-cut(data$CLIENT_AGE, breaks=c(18,24,29,34,39,44,49,54,59,69), 
                          labels=c("18-24","25-29","30-34","35-39","40-44","45-49","50-54","55-59","60+"))
## Create plot & crosstable 
plot(REQ_STATUS~CLIENT_AGE_BINS, data=data, main="Request status by Age", ylab="Request status", xlab="Age")
CrossTable(data$CLIENT_AGE_BINS, data$REQ_STATUS, chisq=FALSE, prop.chisq = FALSE, prop.t=FALSE, format="SPSS")

# Create histogram & boxplot chart with Income variable.
par(mfrow=c(1,2))
  hist(data$INCOME/1000, main="Income (000) histogram", col="Grey", xlab="Income (000)")
  abline(v=mean(data$INCOME), col="Red")
  boxplot(data$INCOME/1000, main="Income (000) boxplot", col="Grey" )
par(mfrow=c(1,1))
summary(data$INCOME)
# Histogram with incomes is extremely right skewed with lot of outliers 


# Create Income bins using percentile
quantile(data$INCOME, probs=seq(0,1,0.20), na.rm=TRUE)
data$INCOME_BIN<-cut(data$INCOME, breaks=c(4000, 12499, 14999, 17999, 22499, 600000 ), 
                          labels=c("<12500", "12500-14999", "15000-17999", "18000-22499", "22500+"))
plot(REQ_STATUS~INCOME_BIN, data=data, main="Request status by Income", ylab="Request status", xlab="Income")
CrossTable(data$INCOME_BIN, data$REQ_STATUS, chisq=FALSE, prop.chisq = FALSE, prop.t=FALSE, format="SPSS")

# Create separate plots for Gender, Age & Income by REQ_STATUS
par(mfrow=c(1,3))
plot(REQ_STATUS~GENDER, data=data, main="Request status by Age", ylab="Request status", xlab="Gender")
plot(REQ_STATUS~CLIENT_AGE_BINS, data=data, main="Request status by Age", ylab="Request status", xlab="Age")
plot(REQ_STATUS~INCOME_BIN, data=data, main="Request status by Income", ylab="Request status", xlab="Income")
par(mfrow=c(1,1))

# Check interaction of all 3 variables - GENDERxCLIENT_AGE_BINSxINCOME_BIN
data$REQ_STATUS<-relevel(data$REQ_STATUS, "REJECTED")
mosaic(REQ_STATUS~GENDER+CLIENT_AGE_BINS+INCOME_BIN, data=data,
       varnames=FALSE,
       labeling_args=list(rot_labels=c(0,0,90,90),
                          gp_labels=(gpar(fontsize=8)),
                          pos_labels = "center",
                          pos_varnames="center",
                          just_labels = c("center", "left", "right", "center"),
                          offset_varnames=1))
data$REQ_STATUS<-relevel(data$REQ_STATUS, "CONFIRMED")

# create contingency table of all 3 variables interaction
round(prop.table(ftable(REQ_STATUS~GENDER+CLIENT_AGE_BINS+INCOME_BIN, data=data),1),2)

##### other variables
# create plots for other variables - "PAYOUTWAY", "MARKETING_ACCEPTED" & "WORK_STATUS_EN"
par(mfrow=c(1,3))
plot(REQ_STATUS~MARKETING_ACCEPTED, data=data, main="Request status by Marketing", ylab="Request status", xlab="Marketing Accepted")
plot(REQ_STATUS~PAYOUT_WAY, data=data, main="Request status by PayoutWay", ylab="Request status", xlab="PayoutWay")
data$WORK_STATUS_EN<-factor(data$WORK_STATUS_EN)
plot(REQ_STATUS~WORK_STATUS_EN, data=data, main="Request status by Work Status", ylab="Request status", xlab="Work Status")
par(mfrow=c(1,1))


# LD_INDICATOR analysis
##---------------------------------------------------------------------------------
# check data
summary(data$LD_INDICATOR)
CrossTable(data$LD_INDICATOR)
# A huge part (~91%) of all clients are classified as  "Indetermined" which doesn't give useful information about a client (in this case). 
# In further analysis, this factor level is removed from data set.

# remove level "Indetermined" from variable LD_INDICATOR
data_ld<-subset(data, LD_INDICATOR=="GOOD"|LD_INDICATOR=="BAD")
data_ld$LD_INDICATOR<-factor(data_ld$LD_INDICATOR)
data_ld$LD_INDICATOR<-relevel(data_ld$LD_INDICATOR, "GOOD")

# create CrossTabs for Gender, Age & Income
CrossTable(data_ld$GENDER, data_ld$LD_INDICATOR, chisq=FALSE, prop.chisq = FALSE, prop.t=TRUE, format="SPSS")
CrossTable(data_ld$CLIENT_AGE_BINS, data_ld$LD_INDICATOR, chisq=FALSE, prop.chisq = FALSE, prop.t=TRUE, format="SPSS")
CrossTable(data_ld$INCOME_BIN, data_ld$LD_INDICATOR, chisq=FALSE, prop.chisq = FALSE, prop.t=TRUE, format="SPSS")

# create separate plots for Gender, Age & Income by LD_INDICATOR
par(mfrow=c(1,3))
  plot(LD_INDICATOR~GENDER, data=data_ld, main="LD_INDICATOR by GENDER") # Females has beter BAD/GOOD LD_INDICATOR ratio
  plot(LD_INDICATOR~CLIENT_AGE_BINS, data=data_ld, main="LD_INDICATOR by AGE", ylab="LD indicator", xlab="Age") # the most of all BAD indicator is for youngsters. 
  plot(LD_INDICATOR~INCOME_BIN, data=data_ld, main="LD_INDICATOR by INCOME", ylab="LD indicator", xlab="Income") # 
par(mfrow=c(1,1))

# create mosaic plot for GENDERxAGExINCOME interaction effect
data_ld$LD_INDICATOR<-relevel(data_ld$LD_INDICATOR, "BAD")
mosaic(LD_INDICATOR~GENDER+CLIENT_AGE_BINS+INCOME_BIN, data=data_ld,
       varnames=FALSE,
       labeling_args=list(rot_labels=c(0,0,90,90),
                          gp_labels=(gpar(fontsize=8)),
                          pos_labels = "center",
                          pos_varnames="center",
                          just_labels = c("center", "left", "right", "center"),
                          offset_varnames=1))

# create contingency table of all 3 variables interaction
round(prop.table(ftable(REQ_STATUS~GENDER+CLIENT_AGE_BINS+INCOME_BIN, data=ld_data),1),2)


