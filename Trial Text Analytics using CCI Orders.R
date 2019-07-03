##Text Analytics using R
install.packages("SnowballC") # only need to do once
library(SnowballC)
install.packages("pdftools")
library(pdftools)
getwd()
setwd("C:/Users/bidis/Dropbox/Coursewra/Text analytics")
filing2018 <- list.files(pattern = "pdf$")
filing2018
Rpdf <- readPDF(control = list(text = "-layout"))
docs <- Corpus(URISource(filing2018), 
                   readerControl = list(reader = Rpdf))
# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))
# Remove numbers
# Remove numbers
docs <- tm_map(docs, removeNumbers)
# Remove english common stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("commission", "combination","competition","india","etc","act"))
# Remove punctuations
docs <- tm_map(docs, removePunctuation)
# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)
dtm <- TermDocumentMatrix(docs)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

# Generate the WordCloud
library("wordcloud")
library("RColorBrewer")
par(bg="grey30")
png(file="WordCloud.png",width=1300,height=800, bg="gray30")
wordcloud(d$word, d$freq, col=terrain.colors(length(d$word), alpha=0.9), random.order=FALSE, rot.per=0.3 )
title(main = "CCI's Most Used Used words in the 2018-Comibation Orders", font.main = 1, col.main = "seashell", cex.main = 2)
dev.off()

inspect(s272018.tdm[1:10,])
findFreqTerms(opinions.tdm, lowfreq = 20, highfreq = Inf)