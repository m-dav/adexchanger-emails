## clear variables
rm(list = ls())

## libraries
library(tm)
library(wordcloud) 
library(sqldf)
library(ggplot2)
library(dplyr)
library(topicmodels)
library (devtools)
library(qdapRegex)


## read in all emails
str <- '/Users/markdavenport/Documents/adexchanger/data/'
filelist <- list.files(str)
output <- data.frame()

## create start and stop flags
dts <- seq(as.Date('2011-11-01'),as.Date('2015-12-31'),"day")
startStopMatrix <- data.frame(dts)
startStopMatrix[,'start'] <- 'Today From AdExchanger.com...'
startStopMatrix[,'end'] <- 'But Wait. There\'s More!'

ix <- startStopMatrix[,'dts']==as.Date('2011-12-02') | 
  startStopMatrix[,'dts']==as.Date('2011-12-12')
startStopMatrix[ix,'end'] <- 'Training & Webinars  '

ix <- startStopMatrix[,'dts']>as.Date('2012-07-06')
startStopMatrix[ix,'start'] <- 'Today From AdExchanger...'

ix <- startStopMatrix[,'dts']>=as.Date('2012-09-16')
startStopMatrix[ix,'start'] <- 'To:\tMark Davenport'

ix <- startStopMatrix[,'dts']==as.Date('2013-07-09') 
startStopMatrix[ix,'end'] <- ' But Wait, There\'s More!'

ix <- startStopMatrix[,'dts']>=as.Date('2014-05-01') 
startStopMatrix[ix,'end'] <- 'But Wait. There\x92s More!'  

for (i in 1:length(filelist)){
  
  ## open set of monthly emails
  fileName <- paste(str,filelist[i],sep="") 
  print(fileName)
  con=file(fileName,open="r")
  emails=readLines(con)
  close(con)  
  
  ## process
  emails <- emails[2:length(emails)]
  emails <- sub("<", "", emails)
  emails <- sub(">", "", emails)
  
  ## grab dates
  ix_dates <- grep("Sent:",emails)
  dates <- gsub(',','', emails[ix_dates])
  dates <- strsplit(dates, split=' ', fixed=TRUE)
  dates <- t(as.data.frame.list(dates))
  
  month <- dates[,2]
  day <- as.numeric(dates[,3])
  year <- as.numeric(dates[,4])
  dates <- as.Date(paste(month, ' ',day,', ',year,sep=""), "%B %d, %Y")
  
  ## grab content    
  ix_dates <- c(ix_dates,length(emails)+2)
  rowsToKeep <- data.frame()
  for (j in 1:length(dates)){
    email_to_scan <- emails[seq(ix_dates[j],ix_dates[j+1]-2,1)]
    ix <- startStopMatrix[,'dts']==dates[j]
    startWord <- unique(startStopMatrix[ix,'start'])
    endWord <- unique(startStopMatrix[ix,'end'])
    
    if (sum(email_to_scan==startWord)>0){
      ix_start <- which(email_to_scan==startWord)          
    } else {
      print(paste(dates[j],'no start word'))
      break
    }
    
    if (sum(email_to_scan==endWord)>0){
      ix_end <- which(email_to_scan==endWord) # earliest reference              
    } else if (endWord=='But Wait. There\'s More!'){
      endWord <- 'But Wait. There\'s More! '
      if (sum(email_to_scan==endWord)>0){
        ix_end <- which(email_to_scan==endWord) # earliest reference              
      } else {
        endWord <- 'But Wait, There\'s More!'
        if (sum(email_to_scan==endWord)>0){
          ix_end <- which(email_to_scan==endWord) # earliest reference  
        } else {
          print(paste(dates[j],'no end word'))
          break
        }
      } 
    }
    
    
    
    if (ix_end<=ix_start){
      print('error: end word is found before start word') 
      break
    } else if (length(ix_start)>1){
      print('error: start word found multiple times')
      break
    } else {
      rowsToKeep <- rbind(rowsToKeep,c(ix_start+ix_dates[j],ix_end+ix_dates[j]-2))
    }
  }
  
  
  ## figure out which rows to keep
  stringToEvaluate <- paste('list(',paste(paste(rowsToKeep[,1],rowsToKeep[,2],sep=":"),collapse=","),')') 
  rowsToKeep <- eval(parse(text=stringToEvaluate))
  
  ## join the corresponding dates to the rows we're keeping
  datesForOutput <- data.frame()
  for (j in 1:length(rowsToKeep)){
    x <- as.matrix(as.character(rep(dates[j],length(rowsToKeep[[j]]))))
    datesForOutput <- rbind(datesForOutput,x)
  }
  
  emailTextForOutput <- emails[unlist(rowsToKeep)]
  output <- rbind(output,cbind(datesForOutput,emailTextForOutput))
  output <- output[output[,2]!="",]
  
}

colnames(output) <- c('dt','text')
#write.csv(output,'/Users/markdavenport/Documents/adexchanger/raw_output.csv')

#### process data
dts <- unique(format(as.Date(output$dt), "%Y%m"))
df <- matrix(0,ncol=2,nrow=length(dts))
df <- data.frame(df)
colnames(df) <- c('date','text')
for (i in 1:length(dts)){
  ix <- format(as.Date(output[,'dt']),"%Y%m")==dts[i]
  df[i,'date'] <- dts[i]
  df[i,'text'] <- paste(output[ix,'text'], collapse=' ')
}
#write.csv(df,'/Users/markdavenport/Documents/adexchanger/processed_output.csv')

# build a corpus, and specify the source to be character vectors
myCorpus <- Corpus(VectorSource(df$text))
myCorpus <- tm_map(myCorpus,
                   content_transformer(function(x) iconv(x, to='UTF-8-MAC', sub='byte')),
                   mc.cores=1)

### remove domains
removeDomain <- function(x) rm_url(x, pattern=pastex("@rm_url"))
myCorpus <- tm_map(myCorpus, content_transformer(removeDomain))  #??

#removeDomain <- function(x) gsub(" ?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", x) # remove http://
#myCorpus <- tm_map(myCorpus, content_transformer(removeDomain))  #??
#removeDomain <- function(x) gsub(" ?(www)(.*)[.|/](.*)", "", x) # remove www.
#myCorpus <- tm_map(myCorpus, content_transformer(removeDomain))  #??
#removeDomain <- function(x) gsub(" (.*)[.com][|/](.*)", "", x) # remove .coms/something
#myCorpus <- tm_map(myCorpus, content_transformer(removeDomain))  #??
#removeDomain <- function(x) gsub(" (.*)[.com]", "", x) # remove .coms
#myCorpus <- tm_map(myCorpus, content_transformer(removeDomain))  #??

# convert to lower case # myCorpus <- tm_map(myCorpus, tolower)
# tm v0.6
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation) 
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)


## join TTD
convertTTD <- function(x) gsub("the trade desk","thetradedesk",x)
myCorpus <- tm_map(myCorpus, content_transformer(convertTTD))

# add two extra stop words: 'available' and 'via'
myStopwords <- c(stopwords("english"),
                 'can',
                 'sponsor',
                 'new',
                 'will',
                 'said',
                 'available', 
                 'via',
                 'read',
                 'adexchangercom',
                 'newsletter',
                 'targetinfo',
                 'adexchanger',
                 'roundup',
                 'also')
# remove stopwords from corpus
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#￼# keep a copy of corpus to use later as a dictionary for stem
# completion
myCorpusCopy <- myCorpus
# stem words
myCorpus <- tm_map(myCorpus, stemDocument, language="english")

myCorpus <- tm_map(myCorpus, PlainTextDocument)

dtm <-  DocumentTermMatrix(myCorpus)
dtmNonStemmed <-  DocumentTermMatrix(myCorpusCopy)
#dtmTFIDF <- DocumentTermMatrix(myCorpus,
#                               control = list(weighting = function(x) weightTfIdf(x, normalize = FALSE)))
rownames(dtm) <- df$date
rownames(dtmNonStemmed) <- df$date
#rownames(dtmTFIDF) <- df$date

terms <- findFreqTerms(dtm,1000)
terms

terms <- findFreqTerms(dtmNonStemmed,1000)
terms

#termsTFIDF <- findFreqTerms(dtmTFIDF,100,1000)
#termsTFIDF


save(dtm, file = "/Users/markdavenport/Documents/adexchanger/dtm.RData")
save(dtmNonStemmed, file = "/Users/markdavenport/Documents/adexchanger/dtmNonStemmed.RData")
