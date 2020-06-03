##############################
#Time Series of Corona Tweets#
##############################

# set wd to script-filepath
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Get Tweets via API (you need a Twitter-Account)
library(rtweet)
TeamKenFM_Tweets <- get_timeline(user= "TeamKenFM", n=3200)

# Save Tweets as CSV
save_as_csv(TeamKenFM_Tweets, "TeamKenFM_Tweets_03-06-2020.csv")

# Create new dataframe with cols created_at=Date and text=Text 
TeamKenFM_Tweets2 <- data.frame("Date"= TeamKenFM_Tweets$created_at, "Text"= TeamKenFM_Tweets$text)

# Shorten dates and coverse to date
TeamKenFM_Tweets2$Date <- substr(TeamKenFM_Tweets2$Date,start=1, stop=10)
TeamKenFM_Tweets2$Date <- as.Date(TeamKenFM_Tweets2$Date, '%Y-%m-%d')

# Set start and end date
TeamKenFM_Tweets2 <- TeamKenFM_Tweets2[TeamKenFM_Tweets2[["Date"]] >= "2020-04-15", ]
TeamKenFM_Tweets2 <- TeamKenFM_Tweets2[TeamKenFM_Tweets2[["Date"]] <= "2020-06-02", ]

# Create frequency table with absolute tweet-freq/date
df1 <- as.data.frame(table(TeamKenFM_Tweets2$Date))
colnames(df1)[2] <- "Freq_Total"

# Keep only Tweets with Corona-Keywords
corona_words <- c('corona',  'impf', 'grundgesetz', 'demo', 'protest', 'pandemie', 'epidemie', 'covid', 'RKI', 'virus', 'infiziert', 'gates', 'maske', 'lockdown', 'kurve', 'lockerung', 'WHO', 'tests', 'Hopkins', 'Hygiene')
TeamKenFM_Tweets3<- TeamKenFM_Tweets2[grepl(paste(corona_words, collapse="|"),TeamKenFM_Tweets2$Text, ignore.case=TRUE),]

# Create frequency table with corona tweet-freq/date
df2 <- as.data.frame(table(TeamKenFM_Tweets3$Date))

# Merge df1, df2 by Var1
df3 <- merge(df1,df2,by="Var1",all=T)

# Calculate share of corona-tweets/date
df3$Freq_rel <- df3$Freq/df3$Freq_Total

# Replace NA with 0
df3[is.na(df3)] <- 0

# Converse to date-format
df3$Var1 <- as.Date(df3$Var1, '%Y-%m-%d')

# Plot timeseries 
library(ggplot2)

# Relative
ggplot( data = df3, aes( Var1, Freq_rel )) + geom_line() + geom_smooth(method = "lm")

# Absolute
ggplot( data = df3, aes( Var1, Freq )) + geom_line() + geom_smooth(method = "lm")
