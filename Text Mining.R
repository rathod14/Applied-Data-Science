# IST687 – HW 11  - Wielding the tm package and counting words
# Rahul Rathod 
# Due date- 11/28/2018
# Submitted Date-11/28/2018


install.packages("jsonlite")
library(jsonlite)

install.packages("RCurl")
library(RCurl)

#Part A: Load and condition the text file that contains the speech  
#1.	The data is available on blackboard, as a JSON file (see HW8 if you need a reminder on the dataset or how to load the dataset).

hotelData<-fromJSON("hotelSurveyBarriot.json")
hotelSurvey<-as.data.frame(hotelData)
str(hotelSurvey)

#2.	The key column to focus on is the ???freeText??? column.

hotelSurvey$freeText
View(hotelSurvey)

#Part B: Create a list of word counts from the speech
#3.	Starting with the code at the bottom of page 180 in the text book, use a similar approach to transform the free text into a term document matrix, and then determine positive and negative word matches.

install.packages("tm")
library(tm)

words.vec <- VectorSource(hotelSurvey$freeText)
words.corpus <- Corpus(words.vec)
words.corpus


words.corpus <- tm_map(words.corpus, content_transformer(tolower)) 
words.corpus <- tm_map(words.corpus,removePunctuation) 
words.corpus <- tm_map(words.corpus, removeNumbers) 
words.corpus <- tm_map(words.corpus, removeWords,stopwords("english"))


tdm <- TermDocumentMatrix(words.corpus)
tdm

m <- as.matrix(tdm)
wordCounts <- rowSums(m)
wordCounts
wordCounts <- sort(wordCounts, decreasing=TRUE) 
str(wordCounts)
head(wordCounts,6)


install.packages("wordcloud")
library(wordcloud)

pos <- "positive-words.txt"
neg <- "negative-words.txt"

p <- scan(pos, character(0),sep = "\n")
n <- scan(neg, character(0),sep = "\n")


p <- p[-1:-34] 
n <- n[-1:-34] 
head(p)


words <- names(wordCounts)
#Matching words with positive word list 
pmatched <- match(words, p, nomatch = 0)
head(pmatched)
words[6]
p[772]


#Selecting words which match with positive word list
mCounts <- wordCounts[which(pmatched != 0)] 
mCounts_df<-as.data.frame(mCounts)
names(mCounts_df)<-c("occurance")
mWords <- names(mCounts)
nPos<-sum(mCounts)


#Similar approach is followed for matching negative words
nmatched <- match(words, n, nomatch = 0)



nCounts <- wordCounts[which(nmatched != 0)] 
nCounts_df<-as.data.frame(nCounts)
names(nCounts_df)<-c("occurance")
nWords <- names(nCounts)
nNeg<-sum(nCounts)
#total number of words present in words vector
totalWords<-length(words)

#4.Calculate the percent positive words and negative words.
ratioPos <- nPos/totalWords
ratioPosPercent<-ratioPos*100
ratioNeg <- nNeg/totalWords
ratioNegPercent<-ratioNeg*100
#Percent Positive Words 
ratioPosPercent
#Percent Negative Words 
ratioNegPercent
ratioPos
ratioNeg

#5.	Write a block comment that summarizes what you learned from ratioPos and ratioNeg.

#So the ratio of Positive words is 0.308 which is way higher than the ratio of Negative words that is 0.079
#This shows that there are many Positive words in the Free Text column of the Dataset which shows that the Customers have given a Positive feedback.

#Part D: Visualize the results
#6.	Create a word cloud

cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts) 
wordcloud(cloudFrame$word,cloudFrame$freq)

#7.	Create a barplot of the positive and negative words that matched (at least twice)

library(ggplot2)
ggplot (mCounts_df,aes(x=rownames(mCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Positive Words in Comments")+
  ylab("Count")

ggplot (nCounts_df,aes(x=rownames(nCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Negative Words in Comments")+
  ylab("Count")

#8.	Write a block comment on what you observe from these two barplots and the wordcloud. 
#Looking at the wordcloud, I've observed that the word cloud shows most of the positive words
#which makes it visually appealing.
#Looking at the two barplots, I've observed that there are way more positive words than the negative words 
#And also that we can view the exact count of each and every positive and negative words used in FreeText.

#9.	Does these results make sense to you in terms of the kinds of emotions you see?
#Which do you think is more informative ??? barplot or the wordcloud?
#Yes, the results show that the feedback given in FreeText was positive.
#I think barplot is more informative as it gives us a clear picture as what are the positive and negative words used in comment along with their count.

#Part E: Evaluate Happy and not Happy customer responses

#10.	Create two subset of the text vectors: one for happy customers and one for not happy customers (based on overall customer satisfaction).
#11.	Redo Steps B, C & D, for these two subsets of the text strings.

happyData <- subset(hotelSurvey, overallCustSat>=7, 
                    select=c(freeText))
str(happyData)


nonHappyData <- subset(hotelSurvey, overallCustSat<7, 
                       select=c(freeText))
#Redo Part B,D for happy customers as well as non happy custpmers 

#Part B for happy customers

happy.words.vec <- VectorSource(happyData$freeText)
happy.words.corpus <- Corpus(happy.words.vec)
happy.words.corpus

happy.words.corpus <- tm_map(happy.words.corpus, content_transformer(tolower)) 
happy.words.corpus <- tm_map(happy.words.corpus,removePunctuation) 
happy.words.corpus <- tm_map(happy.words.corpus, removeNumbers) 
happy.words.corpus <- tm_map(happy.words.corpus, removeWords,stopwords("english"))


happy.tdm <- TermDocumentMatrix(happy.words.corpus)
tdm

happy.m <- as.matrix(happy.tdm)
happy.wordCounts <- rowSums(happy.m)
happy.wordCounts <- sort(happy.wordCounts, decreasing=TRUE)
happy.wordCounts

#Part D for happy customers

#Matching positive words for happy dataset

happy.words <- names(happy.wordCounts)

happy.pmatched <- match(happy.words, p, nomatch = 0)
head(happy.pmatched,100)


happy.mCounts <- happy.wordCounts[which(happy.pmatched != 0)] 
happy.mCounts_df<-as.data.frame(happy.mCounts)
names(happy.mCounts_df)<-c("occurance")
length(happy.mCounts)
happy.mWords <- names(happy.mCounts)
happy.nPos<-sum(happy.mCounts)
happy.nPos
happy.mWords

#Matching negative words for happy dataset
happy.nmatched <- match(happy.words, n, nomatch = 0)


happy.nCounts <- happy.wordCounts[which(happy.nmatched != 0)] 
happy.nCounts_df<-as.data.frame(happy.nCounts)
names(happy.nCounts_df)<-c("occurance")
length(happy.nCounts)
happy.nWords <- names(happy.nCounts)
happy.nNeg<-sum(happy.nCounts)
happy.nNeg
happy.nWords
#Total words in happy dataset freeText
happy.totalWords<-length(happy.words)

happy.ratioPos <- happy.nPos/happy.totalWords
happy.ratioNeg <- happy.nNeg/happy.totalWords
happy.ratioPosPercent<-happy.ratioPos *100
happy.ratioNegPercent<-happy.ratioNeg *100
happy.ratioPosPercent
happy.ratioNegPercent
happy.ratioPos
happy.ratioNeg

#Ratio positive is 0.4545
#Ratio negative is 0.0174
#This shows that the highly satisfied customers included way more positive words than negative words in their comment. 

ggplot (happy.mCounts_df,aes(x=rownames(happy.mCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Positive Words in Comments for Happy customers")+
  ylab("Count")

ggplot (happy.nCounts_df,aes(x=rownames(happy.nCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Negative Words in Comments for Happy customers")+
  ylab("Count")

happy.cloudFrame<-data.frame(happy.word=names(happy.wordCounts),happy.freq=happy.wordCounts) 
wordcloud(happy.cloudFrame$happy.word,cloudFrame$happy.freq)

#Part B for non-happy customers

nonHappy.words.vec <- VectorSource(nonHappyData$freeText)
nonHappy.words.corpus <- Corpus(nonHappy.words.vec)
nonHappy.words.corpus

nonHappy.words.corpus <- tm_map(nonHappy.words.corpus, content_transformer(tolower)) 
nonHappy.words.corpus <- tm_map(nonHappy.words.corpus,removePunctuation) 
nonHappy.words.corpus <- tm_map(nonHappy.words.corpus, removeNumbers) 
nonHappy.words.corpus <- tm_map(nonHappy.words.corpus, removeWords,stopwords("english"))


nonHappy.tdm <- TermDocumentMatrix(nonHappy.words.corpus)


nonHappy.m <- as.matrix(nonHappy.tdm)
nonHappy.wordCounts <- rowSums(nonHappy.m)
nonHappy.wordCounts <- sort(nonHappy.wordCounts, decreasing=TRUE)
nonHappy.wordCounts

#Part D for non-happy customers

#Matching positive words

nonHappy.words <- names(nonHappy.wordCounts)

nonHappy.pmatched <- match(nonHappy.words, p, nomatch = 0)
nonHappy.mCounts <- nonHappy.wordCounts[which(nonHappy.pmatched != 0)] 
nonHappy.mCounts_df<-as.data.frame(nonHappy.mCounts)
names(nonHappy.mCounts_df)<-c("occurance")

nonHappy.mWords <- names(nonHappy.mCounts)
nonHappy.nPos<-sum(nonHappy.mCounts)
nonHappy.nPos
nonHappy.mWords

#Matching negative words
nonHappy.nmatched <- match(nonHappy.words, n, nomatch = 0)
nonHappy.nCounts <- nonHappy.wordCounts[which(nonHappy.nmatched != 0)] 
nonHappy.nCounts_df<-as.data.frame(nonHappy.nCounts)
names(nonHappy.nCounts_df)<-c("occurance")
length(nonHappy.nCounts)
nonHappy.nWords <- names(nonHappy.nCounts)
nonHappy.nNeg<-sum(nonHappy.nCounts)
nonHappy.nNeg
nonHappy.nWords

#Total words for non Happy customers
nonHappy.totalWords<-length(nonHappy.words)

nonHappy.ratioPos <- nonHappy.nPos/nonHappy.totalWords
nonHappy.ratioNeg <- nonHappy.nNeg/nonHappy.totalWords
nonHappy.ratioPos
nonHappy.ratioNeg
nonHappy.ratioPosPercent<-nonHappy.ratioPos *100
nonHappy.ratioNegPercent<-nonHappy.ratioNeg *100
nonHappy.ratioPosPercent
nonHappy.ratioNegPercent

#Ratio positive is 0.0915
#Ratio negative is 0.1232
#This shows that the nonHappy customers included more negative than positive words in their feedback but the difference is not much.

ggplot (nonHappy.mCounts_df,aes(x=rownames(nonHappy.mCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Positive Words in Comments for nonHappy customers")+
  ylab("Count")

ggplot (nonHappy.nCounts_df,aes(x=rownames(nonHappy.nCounts_df),y=occurance))+
  geom_bar(stat='identity')+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("Negative Words in Comments for nonHappy customers")+
  ylab("Count")

nonHappy.cloudFrame<-data.frame(nonHappy.word=names(nonHappy.wordCounts),nonHappy.freq=nonHappy.wordCounts) 
wordcloud(nonHappy.cloudFrame$nonHappy.word,cloudFrame$nonHappy.freq)

# Combining the two we can say that happy customers are providing postive feedbacks whereas
#non-happy customers do not have much difference in positive ann negative words used in their comments but still
#they contain more negative words than positive ones. 



