######### Capstone Project ############

## Dataset: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip

# temp <- tempfile()
# download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",temp)
# data <- read.table(unz(temp, "Coursera-SwiftKey.dat"))
# unlink(temp)

library(tm)
data <- Corpus(readLines("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"))


docsblg <- readLines( "final/en_US/en_US.blogs.txt")
docsnws <- readLines( "final/en_US/en_US.news.txt")
docstwt <- readLines( "final/en_US/en_US.twitter.txt")

docsblg <- as.data.frame(docsblg)
docsblg$ccnt <- nchar(as.character(docsblg$docsblg))

docsnws <- as.data.frame(docsnws)
docsnws$ccnt <- nchar(as.character(docsnws$docsnws))

docstwt <- as.data.frame(docstwt)
docstwt$ccnt <- nchar(as.character(docstwt$docstwt))

docsblgC <- Corpus(VectorSource(docsblg$docsblg))

docsnwsC <- Corpus(VectorSource(docsnws$docsnws))

docstwtC <- Corpus(VectorSource(docstwt$docstwt))

dbc <- docsblgC
dbc <- tm_map(dbc, removePunctuation)
dbc <- tm_map(dbc, removeNumbers)
dbc <- tm_map(dbc, tolower)
dbc <- tm_map(dbc, removeWords, stopwords("english"))
dbc <- tm_map(dbc, stemDocument)
dbc <- tm_map(dbc, stripWhitespace)
dbc <- tm_map(dbc, PlainTextDocument)
#dbc <- as.data.frame(dbc)

#write(dbc, "dbc.txt")

tdmb <- TermDocumentMatrix(dbc)
ttb <- findFreqTerms(tdmb, lowfreq = 2000)
ttbdt <- as.data.frame(ttb)
rsb <- rowSums(as.matrix(tdmb[ttb,]))


dtc <- docstwtC
dtc <- tm_map(dtc, removePunctuation)
dtc <- tm_map(dtc, removeNumbers)
dtc <- tm_map(dtc, tolower)
dtc <- tm_map(dtc, removeWords, stopwords("english"))
dtc <- tm_map(dtc, stemDocument)
dtc <- tm_map(dtc, stripWhitespace)
dtc <- tm_map(dtc, PlainTextDocument)

tdmt <- TermDocumentMatrix(dtc)
ttt <- findFreqTerms(tdmt, lowfreq = 2000)
tttdt <- as.data.frame(ttt)
rst <- rowSums(as.matrix(tdmt[ttt,]))



dnc <- docsnwsC
dnc <- tm_map(dnc, removePunctuation)
dnc <- tm_map(dnc, removeNumbers)
dnc <- tm_map(dnc, tolower)
dnc <- tm_map(dnc, removeWords, stopwords("english"))
dnc <- tm_map(dnc, stemDocument)
dnc <- tm_map(dnc, stripWhitespace)
dnc <- tm_map(dnc, PlainTextDocument)

tdmn <- TermDocumentMatrix(dnc)
ttn <- findFreqTerms(tdmn, lowfreq = 2000)
ttndt <- as.data.frame(ttn)
rsn <- rowSums(as.matrix(tdmt[ttn,]))
