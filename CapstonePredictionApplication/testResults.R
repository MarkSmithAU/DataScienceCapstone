# 
# testResults.R
# 
# Coursera Data Science Capstone Project
# 
# (c) 2018 Mark Smith
# 
# This script should be run after build.R to caculate test and validation data.
#
# Please see the detailed report for full information on the results
# 
# Load packages
loadNoisyPackages <- function() {
        library(data.table)
        library(dplyr)
}
suppressPackageStartupMessages(loadNoisyPackages())

# Load the prediciton functions
source("prediction.R")

# The data from the quizzes has been collected here to allow for testing of the model
# t is the text
# c is the answer choices (not currently used to constrain the results)
# a is the correct answer
t101 <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
c101 <- c("soda", "cheese", "pretzels", "beer")
a101 <- "beer"
t102 <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
c102 <- c("best", "world", "most", "universe")
a102 <- "world"
t103 <- "Hey sunshine, can you follow me and make me the"
c103 <- c("happiest", "smelliest", "saddest", "bluest")
a103 <- "happiest"
t104 <- "Very early observations on the Bills game: Offense still struggling but the"
c104 <- c("defense", "crowd", "players", "referees")
a104 <- "defense"
t105 <- "Go on a romantic date at the"
c105 <- c("beach", "movies", "mall", "grocery")
a105 <- "beach"
t106 <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
c106 <- c("phone", "horse", "way", "motorcycle")
a106 <- "way"
t107 <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
c107 <- c("thing", "weeks", "years", "time")
a107 <- "time"
t108 <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
c108 <- c("toes", "ears", "fingers", "eyes")
a108 <- "fingers"
t109 <- "Be grateful for the good times and keep the faith during the"
c109 <- c("worse", "bad", "sad", "hard")
a109 <- "bad"
t110 <- "If this isn't the cutest thing you've ever seen, then you must be"
c110 <- c("insensitive", "asleep", "insane", "callous")
a110 <- "insane"

t201 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
c201 <- c("give","die","sleep","eat")
a201 <- "die"
t202 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
c202 <- c("horticultural","financial","spiritual","marital")
a202 <- "marital"
t203 <- "I'd give anything to see arctic monkeys this"
c203 <- c("month","decade","morning","weekend")
a203 <- "weekend"
t204 <- "Talking to your mom has the same effect as a hug and helps reduce your"
c204 <- c("hunger","stress","sleepiness","happiness")
a204 <- "stress"
t205 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
c205 <- c("look","picture","minute","walk")
a205 <- "picture"
t206 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
c206 <- c("incident","matter","case","account")
a206 <- "matter"
t207 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
c207 <- c("finger","toe","hand","arm")
a207 <- "hand"
t208 <- "Every inch of you is perfect from the bottom to the"
c208 <- c("middle","side","center","top")
a208 <- "top"
t209 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
c209 <- c("weekly","outside","inside","daily")
a209 <- "outside"
t210 <- "I like how the same people are in almost all of Adam Sandler's"
c210 <- c("pictures","stories","novels","movies")
a210 <- "movies"

# Combine the data into vectors for easy use
texts <-   c(t101, t102, t103, t104, t105, t106, t107, t108, t109, t110, t201, t202, t203, t204, t205, t206, t207, t208, t209, t210)
answers <- c(a101, a102, a103, a104, a105, a106, a107, a108, a109, a110, a201, a202, a203, a204, a205, a206, a207, a208, a209, a210)
choices <- c(c101, c102, c103, c104, c105, c106, c107, c108, c109, c110, c201, c202, c203, c204, c205, c206, c207, c208, c209, c210)

# Only used during development
cacheDir <- paste0(getwd(), "/../../cache")
if (file.exists(cacheDir)) {
        setwd(cacheDir)
}

# Load the small preprocessed datasets (indexed data.table objects)
l2bt <- readRDS("l2bt.rds")
l3bt <- readRDS("l3bt.rds")
l4bt <- readRDS("l4bt.rds")
l2nt <- readRDS("l2nt.rds")
l3nt <- readRDS("l3nt.rds")
l4nt <- readRDS("l4nt.rds")
l2tt <- readRDS("l2tt.rds")
l3tt <- readRDS("l3tt.rds")
l4tt <- readRDS("l4tt.rds")

# Validation 1: Using the small prediction set, generate a match set of the prediction vs the known answer

maxResults <- 1
unigramDiscount <- 1
bigramDiscount <- 1
trigramDiscount <- 1

# Generate the prediction summary for all the quiz texts
blogPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2bt, l3bt, l4bt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])
newsPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2nt, l3nt, l4nt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])
twitterPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2tt, l3tt, l4tt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])

# Combine the results into a data frame
predictionSummary <- data.frame(Text=texts, 
                      CorrectAnswer=answers, 
                      BlogPrediction=blogPrediction, 
                      NewsPrediction=newsPrediction, 
                      TwitterPrediction=twitterPrediction,
                      row.names = NULL,
                      stringsAsFactors = F)

# Add in counts for easy comparison of results
predictionSummary$CorrectCount <- as.integer(predictionSummary$CorrectAnswer == predictionSummary$BlogPrediction) + 
        as.integer(predictionSummary$CorrectAnswer == predictionSummary$NewsPrediction) +
        as.integer(predictionSummary$CorrectAnswer == predictionSummary$TwitterPrediction)

# Save for the report
saveRDS(predictionSummary, "predictionMatchSummary.rds")


# Validation 2: Using the small prediction set, generate a match set of the top 20 predictions vs the known answer
# The question of "Was the correct result just missed?" seems like a reasonable thing to ask.
# Here a similar calculation is undertaken, but now we check if the result was within the top 20 results
maxResults <- 20
blogPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2bt, l3bt, l4bt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)
newsPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2nt, l3nt, l4nt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)
twitterPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2tt, l3tt, l4tt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)

blogCount <- c()
newsCount <- c()
twitterCount <- c()

for (i in 1:length(texts)) {
        blogCount[[i]] <- sum(blogPredictions[[i]]$Prediction == answers[[i]])
        newsCount[[i]] <- sum(newsPredictions[[i]]$Prediction == answers[[i]])
        twitterCount[[i]] <- sum(twitterPredictions[[i]]$Prediction == answers[[i]])
}


extendedPredictionSummary <- data.frame(Text=texts, 
                      CorrectAnswer=answers, 
                      BlogPrediction=blogCount, 
                      NewsPrediction=newsCount, 
                      TwitterPrediction=twitterCount,
                      row.names = NULL,
                      stringsAsFactors = F)
extendedPredictionSummary$ContainsCount <- 
        extendedPredictionSummary$BlogPrediction + 
        extendedPredictionSummary$NewsPrediction + 
        extendedPredictionSummary$TwitterPrediction

saveRDS(extendedPredictionSummary, "predictionExtendedMatchSummary.rds")

########################

rm(list=c(ls()[ls() %like% "^l.*"]))
gc()

# Load the full datasets (indexed data.table objects)
l2bt <- readRDS("l2btf.rds")
l3bt <- readRDS("l3btf.rds")
l4bt <- readRDS("l4btf.rds")
l2nt <- readRDS("l2ntf.rds")
l3nt <- readRDS("l3ntf.rds")
l4nt <- readRDS("l4ntf.rds")
l2tt <- readRDS("l2ttf.rds")
l3tt <- readRDS("l3ttf.rds")
l4tt <- readRDS("l4ttf.rds")

# Validation 3: Using the full prediction set, generate a match set of the prediction vs the known answer

maxResults <- 1
unigramDiscount <- 1
bigramDiscount <- 1
trigramDiscount <- 1

blogPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2bt, l3bt, l4bt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])
newsPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2nt, l3nt, l4nt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])
twitterPrediction <- sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2tt, l3tt, l4tt, unigramDiscount, bigramDiscount, trigramDiscount)[,1])
predictionSummary <- data.frame(Text=texts, 
                                CorrectAnswer=answers, 
                                BlogPrediction=blogPrediction, 
                                NewsPrediction=newsPrediction, 
                                TwitterPrediction=twitterPrediction,
                                row.names = NULL,
                                stringsAsFactors = F)
predictionSummary$CorrectCount <- as.integer(predictionSummary$CorrectAnswer == predictionSummary$BlogPrediction) + 
        as.integer(predictionSummary$CorrectAnswer == predictionSummary$NewsPrediction) +
        as.integer(predictionSummary$CorrectAnswer == predictionSummary$TwitterPrediction)

saveRDS(predictionSummary, "predictionFullSetSummary.rds")


# Validation 4: Using the full prediction set, generate a match set of the top 20 predictions vs the known answer
maxResults <- 20
blogPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2bt, l3bt, l4bt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)
newsPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2nt, l3nt, l4nt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)
twitterPredictions <- as.data.frame(sapply(texts, function (x) getTrigramPrediction(x, maxResults, l2tt, l3tt, l4tt, unigramDiscount, bigramDiscount, trigramDiscount)), row.names = NULL)

blogCount <- c()
newsCount <- c()
twitterCount <- c()

for (i in 1:length(texts)) {
        blogCount[[i]] <- sum(blogPredictions[[i]]$Prediction == answers[[i]])
        newsCount[[i]] <- sum(newsPredictions[[i]]$Prediction == answers[[i]])
        twitterCount[[i]] <- sum(twitterPredictions[[i]]$Prediction == answers[[i]])
}

extendedPredictionSummary <- data.frame(Text=texts, 
                                        CorrectAnswer=answers, 
                                        BlogPrediction=blogCount, 
                                        NewsPrediction=newsCount, 
                                        TwitterPrediction=twitterCount,
                                        row.names = NULL,
                                        stringsAsFactors = F)
extendedPredictionSummary$ContainsCount <- 
        extendedPredictionSummary$BlogPrediction + 
        extendedPredictionSummary$NewsPrediction + 
        extendedPredictionSummary$TwitterPrediction

saveRDS(extendedPredictionSummary, "predictionFullSetExtendedMatchSummary.rds")


# Validation 5: Within the full data set, what percentage of the ngrams do we cover
# and how may are predicted correctly (excluding backoff)

# calculateCoverage()
# Function to calculate the precentage of ngrams that will not be used
# Note that this only considers the probability within each set without backoff
# Inputs:
#       dataFile: name of RDS tbl_df dataset
#       ngSet: the name of the set for reporting
#       ngLen: the length of the ngrams for reporting
calculateCoverage <- function(dataFile, ngSet, ngLen) {
        # Get the Prediction data
        l <- readRDS(dataFile) %>% ungroup()
        # Do all the calculations
        Selected <- l %>%
                # calculate sum of the frequencies of words with rnum ==1
                filter(rnum == 1) %>% summarise(f=sum(freq), n=n())
                # calculate sum of the frequencies of words with rnum ==1
        Unselected <- l %>%
                # calculate sum of the frequencies of words with rnum ==1
                filter(rnum > 1) %>% summarise(f=sum(freq), n=n())
        
        data.frame(Corpora=ngSet, 
                   nGramLength=ngLen, 
                   SelectedFreq=Selected$f, 
                   SelectedCount=Selected$n, 
                   UnselectedFreq=Unselected$f,
                   UnselectedCount=Unselected$n,
                   PercentByFreq=Selected$f/(Selected$f+Unselected$f),
                   PercentByCount=Selected$n/(Selected$n+Unselected$n)
                   )
}

# Calculate the number of unigram bases retained
results <- calculateCoverage("l2bf.rds", "Blogs", "1")
results <- rbind(results, calculateCoverage("l2tf.rds", "Twitter", "1"))
results <- rbind(results, calculateCoverage("l2nf.rds", "News", "1"))

# Calculate the number of bigram bases retained
results <- rbind(results, calculateCoverage("l3bf.rds", "Blogs", "2"))
results <- rbind(results, calculateCoverage("l3nf.rds", "News", "2"))
results <- rbind(results, calculateCoverage("l3tf.rds", "Twitter", "2"))

# Calculate the number of trigram bases retained
results <- rbind(results, calculateCoverage("l4bf.rds", "Blogs", "3"))
results <- rbind(results, calculateCoverage("l4nf.rds", "News", "3"))
results <- rbind(results, calculateCoverage("l4tf.rds", "Twitter", "3"))

saveRDS(results, "coverageFullSet.rds")

# Cleanup
rm(list=c(ls()[ls() %like% "^l.*"]))
gc()

# Validation 6: Within the preprocessed data set, what percentage of the ngrams do we cover
# and how may are predicted correctly (excluding backoff)

# Calculate the number of unigram bases retained
results <- calculateCoverage("l2bt.rds", "Blogs", "1")
results <- rbind(results, calculateCoverage("l2tt.rds", "Twitter", "1"))
results <- rbind(results, calculateCoverage("l2nt.rds", "News", "1"))

# Calculate the number of bigram bases retained
results <- rbind(results, calculateCoverage("l3bt.rds", "Blogs", "2"))
results <- rbind(results, calculateCoverage("l3nt.rds", "News", "2"))
results <- rbind(results, calculateCoverage("l3tt.rds", "Twitter", "2"))

# Calculate the number of trigram bases retained
results <- rbind(results, calculateCoverage("l4bt.rds", "Blogs", "3"))
results <- rbind(results, calculateCoverage("l4nt.rds", "News", "3"))
results <- rbind(results, calculateCoverage("l4tt.rds", "Twitter", "3"))

saveRDS(results, "coveragePredictionSet.rds")


# Validation 7: Within the preprocessed data set, what percentage of the ngrams do we cover
# and how may are predicted correctly (excluding backoff)

fs <- readRDS("coverageFullSet.rds")
ps <- readRDS("coveragePredictionSet.rds")

fs <- tbl_df(fs) %>%
        select(Corpora, 
        nGramLength, 
        FullSetSelectedFreq=SelectedFreq,
        FullSetSelectedCount=SelectedCount,
        FullSetUnselectedFreq=UnselectedFreq,
        FullSetUnselectedCount=UnselectedCount,
        FullSetPercentByFreq=PercentByFreq,
        FullSetPercentByCount=PercentByCount)

ps <- tbl_df(ps) %>%
        select(Corpora, 
               nGramLength, 
               PartialSetSelectedFreq=SelectedFreq,
               PartialSetSelectedCount=SelectedCount,
               PartialSetUnselectedFreq=UnselectedFreq,
               PartialSetUnselectedCount=UnselectedCount,
               PartialSetPercentByFreq=PercentByFreq,
               PartialSetPercentByCount=PercentByCount)

js <- merge(fs, ps)

js <- js %>% mutate(PercentByFreq=PartialSetSelectedFreq/(FullSetSelectedFreq+FullSetUnselectedFreq),
                   PercentByCount=PartialSetSelectedCount/(FullSetSelectedCount+FullSetUnselectedCount)
)

saveRDS(js, "coverageFullVsPredictionSet.rds")

# Generate coverage stats for the report 
js2 <- js %>% select(Corpora, 
                     nGramLength, 
                     FullSetPercentByFreq, 
                     FullSetPercentByCount, 
                     PercentByFreq, 
                     PercentByCount) %>%
        mutate(LostPercentByFreq=FullSetPercentByFreq-PercentByFreq,
               LostPercentByCount=FullSetPercentByCount-PercentByCount)

saveRDS(js2, "coverageChangeSummary.rds")