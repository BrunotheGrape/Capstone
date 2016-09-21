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
dtcc <- dtc
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



Stg <- grep("hate", docstwt, value = TRUE)
stg <- as.data.frame(Stg)



library("quanteda"); library(data.table); library(ggplot2)
library(dplyr)
qdtwt <- textfile("final/en_US/en_US.twitter.txt")
qdtwtC <- corpus(qdtwt)
summary(qdtwtC)
kwic(qdtwtC, "economic", valuetype = "regex")

DfmTwt <- dfm(qdtwtC)
summary(DfmTwt)
StemDfmTwt <- dfm(qdtwtC, ignoredFeatures = stopwords("english"), stem = TRUE)
tfTwt <- topfeatures(StemDfmTwt, 100)
tfTwt <- setDT(tfTwt, keep.rownames = TRUE)
colnames(tfTwt) <- c("word", "cnt")
ptft <- ggplot(tfTwt, aes(word, cnt)) + geom_point()
ptft


tknTwt <- tokenize(qdtwtC, verbose = TRUE)
summary(tknTwt)

Twt2g <- features(dfm(qdtwtC, ngrams = 2, ignoredFeatures = stopwords("english")))
Twt3g <- features(dfm(qdtwtC, ngrams = 3, ignoredFeatures = stopwords("english")))


qdBlg <- textfile("final/en_US/en_US.blogs.txt")
qdBlgC <- corpus(qdBlg)
summary(qdBlgC)
kwic(qdBlgC, "economic", valuetype = "regex")

DfmBlg <- dfm(qdBlgC)
summary(DfmBlg)
StemDfmBlg <- dfm(qdBlgC, ignoredFeatures = stopwords("english"), stem = TRUE)
tfBlg <- as.data.frame(topfeatures(StemDfmBlg, 100))
tfBlg <- setDT(tfBlg, keep.rownames = TRUE)
colnames(tfBlg) <- c("word", "cnt")
ptfb <- ggplot(tfblg, aes(word, cnt)) + geom_point()
ptfb

tknBlg <- tokenize(qdBlgC, verbose = TRUE)


blg2g <- features(dfm(qdBlgC, ngrams = 2, ignoredFeatures = stopwords("english")))
blg3g <- features(dfm(qdBlgC, ngrams = 3, ignoredFeatures = stopwords("english")))

set.seed(1234)
docsblg <- readLines( "final/en_US/en_US.blogs.txt")
docsblg <- as.data.frame(docsblg)
dfblg <- docsblg[sample(nrow(docsblg), 50),]
dfblg <- as.character(dfblg)
qdBlgC <- corpus(dfblg)
summary(qdBlgC)
kwic(qdBlgC, "economic", valuetype = "regex")

DfmBlg <- dfm(qdBlgC)
summary(DfmBlg)
StemDfmBlg <- dfm(qdBlgC, stem = TRUE)
tfBlg <- as.data.frame(topfeatures(StemDfmBlg, 100))
tfBlg <- setDT(tfBlg, keep.rownames = TRUE)
colnames(tfBlg) <- c("word", "cnt")
ptfb <- ggplot(tfBlg, aes(word, cnt)) + geom_point()
ptfb

tknBlg <- tokenize(qdBlgC, verbose = TRUE)

blg2g <- features(dfm(qdBlgC, ngrams = 2, ignoredFeatures = stopwords("english")))
blg3g <- features(dfm(qdBlgC, ngrams = 3, ignoredFeatures = stopwords("english")))

blg2gC <- corpus(blg2g)
summary(blg2g)

dfBlg2g <- dfm(blg2gC)
tfblg2g <- as.data.frame(topfeatures(dfBlg2g, 5000))
tfblg2g <- setDT(tfblg2g, keep.rownames = TRUE)
colnames(tfblg2g) <- c("2gram", "cnt")

blg3gC <- corpus(blg3g)
summary(blg3g)

dfBlg3g <- dfm(blg3gC)
tfblg3g <- as.data.frame(topfeatures(dfBlg3g, 5000))
tfblg3g <- setDT(tfblg3g, keep.rownames = TRUE)
colnames(tfblg3g) <- c("3gram", "cnt")


library("quanteda"); library(data.table); library(ggplot2)
library(dplyr); library(qdap); library(RColorBrewer)

set.seed(1234)
docsblg <- readLines( "final/en_US/en_US.blogs.txt")
docsnws <- readLines( "final/en_US/en_US.news.txt")
docstwt <- readLines( "final/en_US/en_US.twitter.txt")

dfblg <- as.data.frame(docsblg)
dfblg <- dfblg[sample(nrow(dfblg), 5000),]
dfblg <-gsub("[^[:alpha:][:space:]']", " ", dfblg)
dfnws <- as.data.frame(docsnws)
dfnws <- dfnws[sample(nrow(dfnws), 5000),]
dfnws <-gsub("[^[:alpha:][:space:]']", " ", dfnws)
dftwt <- as.data.frame(docstwt)
dftwt <- dftwt[sample(nrow(dftwt), 5000),]
dftwt <-gsub("[^[:alpha:][:space:]']", " ", dftwt)

dfb <- as.data.frame(dfblg)
dfn <- as.data.frame(dfnws)
dft <- as.data.frame(dftwt)

colnames(dfb) <- "txt"; colnames(dfn) <- "txt"; colnames(dft) <- "txt"

dfz <- rbind(dfb, dfn)
dfz <- rbind(dfz,dft)

dfz <- as.character(dfz$txt)
dfzC <- corpus(dfz)
DfN2 <- dfm(dfz, ngrams = 2, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN2 <- as.data.frame(as.matrix(docfreq(DfN2)))
#MatN2 <- sort(rowSums(DfN2.mat), decreasing=TRUE)


FtN2 <- setDT(MatN2, keep.rownames = TRUE)
colnames(FtN2) <- c("Bigram", "Frequency")
FtN2 <- arrange(FtN2, desc(Frequency))
Pn2 <- ggplot(FtN2[1:15, ], aes(Bigram, Frequency))
Pn2 <- Pn2 + geom_bar(stat="identity", fill="cyan") + ggtitle("15 Most Common Bigrams")
Pn2 <- Pn2 + theme(axis.text.x=element_text(angle=45, hjust=1))
Pn2

DfN3 <- dfm(dfz, ngrams = 3, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN3 <- as.data.frame(as.matrix(docfreq(DfN3)))

FtN3 <- setDT(MatN3, keep.rownames = TRUE)
colnames(FtN3) <- c("Trigram", "Frequency")
FtN3 <- arrange(FtN3, desc(Frequency))
Pn3 <- ggplot(FtN3[1:15, ], aes(Trigram, Frequency))
Pn3 <- Pn3 + geom_bar(stat="identity", fill="violetred") + ggtitle("15 Most Common Trigrams")
Pn3 <- Pn3 + theme(axis.text.x=element_text(angle=45, hjust=1))
Pn3

DfN1 <- dfm(dfz, ngrams = 1, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN1 <- as.data.frame(as.matrix(docfreq(DfN1)))

FtN1 <- setDT(MatN1, keep.rownames = TRUE)
colnames(FtN1) <- c("Word", "Frequency")
FtN1 <- arrange(FtN1, desc(Frequency))
Pn1 <- ggplot(FtN1[1:15, ], aes(Word, Frequency))
Pn1 <- Pn1 + geom_bar(stat="identity", fill="bisque") + ggtitle("15 Most Common Words")
Pn1 <- Pn1 + theme(axis.text.x=element_text(angle=45, hjust=1))
Pn1
