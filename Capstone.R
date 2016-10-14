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


library(quanteda); library(data.table); library(ggplot2)
library(dplyr); library(stringr); library(RColorBrewer)
library(tm); library(stringi)

set.seed(1234)
docsblg <- readLines( "final/en_US/en_US.blogs.txt")
docsnws <- readLines( "final/en_US/en_US.news.txt")
docstwt <- readLines( "final/en_US/en_US.twitter.txt")

BWL <- as.data.frame(read.csv("ProfanityList.csv", header = FALSE))
BWL <- as.character(BWL$V1)
BWL <- as.vector(BWL)

pattern <- paste0("\\b(?:", paste(BWL, collapse = "|"), ")\\b ?")

dfblg <- as.data.frame(docsblg)
dfblg <- dfblg[sample(nrow(dfblg), 5000),]
dfblg <-gsub("[^[:alpha:][:space:]']", " ", dfblg)
dfblg <- gsub("'", "", dfblg)
dfblg <- gsub(x = dfblg, pattern = paste(BWL, collapse = "|"), replacement = "")
dfnws <- as.data.frame(docsnws)
dfnws <- dfnws[sample(nrow(dfnws), 5000),]
dfnws <-gsub("[^[:alpha:][:space:]']", " ", dfnws)
dfnws <- gsub("'", "", dfnws)
dfnws <- gsub(x = dfnws, pattern = paste(BWL, collapse = "|"), replacement = "")
dftwt <- as.data.frame(docstwt)
dftwt <- dftwt[sample(nrow(dftwt), 5000),]
dftwt <-gsub("[^[:alpha:][:space:]']", " ", dftwt)
dftwt <- gsub("'", "", dftwt)
dftwt <- gsub(x = dftwt, pattern = paste(BWL, collapse = "|"), replacement = "")


dfb <- as.data.frame(dfblg)
dfn <- as.data.frame(dfnws)
dft <- as.data.frame(dftwt)


colnames(dfb) <- "txt"; colnames(dfn) <- "txt"; colnames(dft) <- "txt"

dfz <- rbind(dfb, dfn)
dfz <- rbind(dfz,dft)
dfz <- as.character(dfz$txt)

DfN6 <- dfm(dfz, ngrams = 6, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN6 <- as.data.frame(as.matrix(docfreq(DfN6)))

FtN6 <- setDT(MatN6, keep.rownames = TRUE)
colnames(FtN6) <- c("Hexegram", "Frequency")
FtN6 <- arrange(FtN6, desc(Frequency))
Split6 <- t(as.data.frame(strsplit(FtN6$Hexegram, " ", fixed = TRUE)))
colnames(Split6) <- c("txt1", "txt2", "txt3", "txt4", "txt5", "txt6")
rownames(Split6) <- 1:nrow(Split6)
FtN6 <- cbind(FtN6, Split6)
FtN6$PRED <- paste(FtN6$txt1, FtN6$txt2, FtN6$txt3, FtN6$txt4, FtN6$txt5, sep = " ")

DfN5 <- dfm(dfz, ngrams = 5, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN5 <- as.data.frame(as.matrix(docfreq(DfN5)))

FtN5 <- setDT(MatN5, keep.rownames = TRUE)
colnames(FtN5) <- c("Pentegram", "Frequency")
FtN5 <- arrange(FtN5, desc(Frequency))
Split5 <- t(as.data.frame(strsplit(FtN5$Pentegram, " ", fixed = TRUE)))
colnames(Split5) <- c("txt1", "txt2", "txt3", "txt4", "txt5")
rownames(Split5) <- 1:nrow(Split5)
FtN5 <- cbind(FtN5, Split5)
FtN5$PRED <- paste(FtN5$txt1, FtN5$txt2, FtN5$txt3, FtN5$txt4, sep = " ")

DfN4 <- dfm(dfz, ngrams = 4, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN4 <- as.data.frame(as.matrix(docfreq(DfN4)))

FtN4 <- setDT(MatN4, keep.rownames = TRUE)
colnames(FtN4) <- c("Tetragram", "Frequency")
FtN4 <- arrange(FtN4, desc(Frequency))
Split4 <- t(as.data.frame(strsplit(FtN4$Tetragram, " ", fixed = TRUE)))
colnames(Split4) <- c("txt1", "txt2", "txt3", "txt4")
rownames(Split4) <- 1:nrow(Split4)
FtN4 <- cbind(FtN4, Split4)
FtN4$PRED <- paste(FtN4$txt1, FtN4$txt2, FtN4$txt3, sep = " ")

DfN3 <- dfm(dfz, ngrams = 3, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN3 <- as.data.frame(as.matrix(docfreq(DfN3)))

FtN3 <- setDT(MatN3, keep.rownames = TRUE)
colnames(FtN3) <- c("Trigram", "Frequency")
FtN3 <- arrange(FtN3, desc(Frequency))
Split3 <- t(as.data.frame(strsplit(FtN3$Trigram, " ", fixed = TRUE)))
colnames(Split3) <- c("txt1", "txt2", "txt3")
rownames(Split3) <- 1:nrow(Split3)
FtN3 <- cbind(FtN3, Split3)
FtN3$PRED <- paste(FtN3$txt1, FtN3$txt2, sep = " ")



DfN2 <- dfm(dfz, ngrams = 2, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN2 <- as.data.frame(as.matrix(docfreq(DfN2)))
#MatN2 <- sort(rowSums(DfN2.mat), decreasing=TRUE)

FtN2 <- setDT(MatN2, keep.rownames = TRUE)
colnames(FtN2) <- c("Bigram", "Frequency")
FtN2 <- arrange(FtN2, desc(Frequency))
Split2 <- t(as.data.frame(strsplit(FtN2$Bigram, " ", fixed = TRUE)))
colnames(Split2) <- c("txt1", "txt2")
rownames(Split2) <- 1:nrow(Split2)
FtN2 <- cbind(FtN2, Split2)
FtN2$PRED <- FtN2$txt1

Pn2 <- ggplot(FtN2[1:15, ], aes(Bigram, Frequency))
Pn2 <- Pn2 + geom_bar(stat="identity", fill="cyan") + ggtitle("15 Most Common Bigrams")
Pn2 <- Pn2 + theme(axis.text.x=element_text(angle=45, hjust=1))
Pn2

DfN3 <- dfm(dfz, ngrams = 3, verbose = TRUE, concatenator = " ", stopwords=TRUE)
MatN3 <- as.data.frame(as.matrix(docfreq(DfN3)))

FtN3 <- setDT(MatN3, keep.rownames = TRUE)
colnames(FtN3) <- c("Trigram", "Frequency")
FtN3 <- arrange(FtN3, desc(Frequency))
Split3 <- t(as.data.frame(strsplit(FtN3$Trigram, " ", fixed = TRUE)))
colnames(Split3) <- c("txt1", "txt2", "txt3")
rownames(Split3) <- 1:nrow(Split3)
FtN3 <- cbind(FtN3, Split3)
FtN3$PRED <- paste(FtN3$txt1, FtN3$txt2, sep = " ")

#FtN3 <- setDT(MatN3, keep.rownames = TRUE)
#colnames(FtN3) <- c("Trigram", "Frequency")
#FtN3 <- arrange(FtN3, desc(Frequency))
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


p <- c("quite some")
PredN3 <- filter(FtN3, PRED == p)
PredN3 <- mutate(PredN3, WtFreq = Frequency/sum(PredN3$Frequency))

Ps <- t(as.data.frame(strsplit(p, " ", fixed = TRUE)))

Psl <- as.list(Ps)

PredN2 <- filter(FtN2, txt1 == Ps[2])
PredN2 <- mutate(PredN2, WtFreq = Frequency/sum(PredN2$Frequency))

PredDf <- select(PredN3, WtFreq, txt3)
PredDf <- rename(PredDf, PredWrd = txt3)

PredN2 <- select(PredN2, WtFreq, txt2)
PredN2 <- rename(PredN2, PredWrd = txt2)

PredDf <- rbind(PredDf, PredN2)
PredDf <- arrange(PredDf, desc(WtFreq))
PredWrd <- as.data.frame(PredDf$PredWrd)
PredWrd1 <- as.data.frame(FtN1$Word)
colnames(PredWrd1) <- c("PredWrd")
colnames(PredWrd) <- c("PredWrd")
#PredWrd <- rbind(PredWrd, PredWrd1)
PredWrd <- distinct(PredWrd)


library("textcat"); library(dplyr); library(scales)

Lang <- as.data.frame(textcat(dfnws, p = ECIMCI_profiles))
colnames(Lang) <- c("language")
lancnt <- count(Lang, language)
lancnt <- arrange(lancnt, desc(n))

wrdFreq <- setDT(FtN1, keep.rownames = TRUE)
wrdFreq <- mutate(wrdFreq, cum = cumsum(Frequency))
wrdFreq$cum <- as.numeric(wrdFreq$cum)
wrdFreq$rn <- as.numeric(wrdFreq$rn)
wrdFreqN <- wrdFreq[seq(1, NROW(wrdFreq), by = 100), ]

pwf <- ggplot(wrdFreqN, aes(rn, cum)) + geom_point() + scale_y_continuous(labels = comma)
pwf <- pwf + theme(axis.text.x = element_text(angle=45))
pwf

Mx <- max(wrdFreq$cum)
Mx5 <- Mx * .5
Med <- filter(wrdFreq, cum < Mx5)
Mx9 <- Mx * .9
Nty <- filter(wrdFreq, cum < Mx9)





