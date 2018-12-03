# 
# build.R
# 
# Coursera Data Science Capstone Project
# 
# (c) 2018 Mark Smith
# 
# This is the build script to take the raw input and generate the datasets for the prediction application.
# Please see the inline comments and the report for more information.


# Load packages with no messages
loadNoisyPackages <- function() {
        library(data.table)
        library(dplyr)
        library(quanteda)
        library(knitr)
        library(stringi)
        library(ggplot2)
}
suppressPackageStartupMessages(loadNoisyPackages())

# Speed up Quanteda with multiple threads
quanteda_options(threads = 8)

# Use a common cache directory on the build system
cacheDir <- paste0(getwd(), "/../../cache")
if (file.exists(cacheDir)) {
        setwd(cacheDir)
}

# Find all the corporas and filter the list down to the en_US ones
fList <- list.files(path = "final", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)
wSet <- fList[grep("en_US", fList)]

#sourceDataLoader()
#This function a set of files into a data frame along with the source of the files.  It is unicode aware  so UTF-8 encoded files are handled correctly.  Line counts are generated but not currently printed.
#Input: wSet: list of text files to be loaded
#Output: data.frame composed of rows with entries:
#       langSet: the language of the source file i.e. en_US
#       srcSet: the source i.e. blogs, twitter
#       textSet: the source line from the file
sourceDataLoader <- function(wSet) {
        for (fileName in wSet) {
                lineCount <- 0
                #print(paste("Processing", fileName))
                con <-  file(fileName, "rb")
                while (TRUE) {
                        lines <- iconv(readLines(con, 10000, warn = TRUE, skipNul=TRUE, encoding="UTF-8"), from="UTF-8", sub="!")
                        lineCount <- lineCount + length(lines)
                        if (length(lines) == 0)
                                break
                        if (!exists("textSet")){
                                textSet <- lines
                        } else {
                                textSet <- c(textSet, lines)
                        }
                }
                id <- strsplit(strsplit(fileName, "/")[[1]][3], "\\.")
                langVal <- id[[1]][1]
                srcVal <- id[[1]][2]
                lang <- rep(langVal, lineCount)
                src <- rep(srcVal, lineCount)
                if (!exists("langSet")){
                        langSet <- lang
                } else {
                        langSet <- c(langSet, lang)
                }
                if (!exists("srcSet")){
                        srcSet <- src
                } else {
                        srcSet <- c(srcSet, src)
                }
                close(con)
                #print(paste("Line count", lineCount))
        }
data.frame(langSet, srcSet, textSet, stringsAsFactors=FALSE)
}

# Cache the loaded data if it has been done already, otherwise generate and save it.
if(file.exists("allData.rds")){
        allData <- readRDS("allData.rds")
} else {
        allData <- sourceDataLoader(wSet)
        saveRDS(allData, file = "allData.rds", compress = FALSE)
}

# Generate line count stats
stats <- allData %>% group_by(srcSet, langSet) %>% summarise(lineCount = n())
saveRDS(stats, file = "stats.rds", compress = FALSE)

# Generate line lengths for statistics
allData$len <- stri_length(allData$textSet)
saveRDS(allData$len, file = "allDataLen.rds", compress = FALSE)

# Get the list of swear word files to filter with
swearFileList <- list.files(path = "profanity", all.files = FALSE, full.names = TRUE, recursive = TRUE, ignore.case = TRUE, include.dirs = FALSE, no.. = FALSE)

# swearListLoader()
# Load the files with swearwords
# Input: wSet: list of files
# Output: data.frame of the input lines with the source language and profanity word
swearListLoader <- function(wSet) {
        for (fileName in wSet) {
                lineCount <- 0
                con <-  file(fileName, "rb")
                while (TRUE) {
                        lines <- iconv(readLines(con, 10000, warn = FALSE), from="UTF-8")
                        lineCount <- lineCount + length(lines)
                        if (length(lines) == 0)
                                break
                        if (!exists("textSet")){
                                textSet <- lines
                        } else {
                                textSet <- c(textSet, lines)
                        }
                }
                id <- strsplit(strsplit(fileName, "/")[[1]][2], "\\.")
                langVal <- id[[1]][1]
                lang <- rep(langVal, lineCount)
                if (!exists("langSet")){
                        langSet <- lang
                } else {
                        langSet <- c(langSet, lang)
                }
                close(con)
        }
        data.frame(langSet, textSet, stringsAsFactors=FALSE)
}

profanityList <- swearListLoader(swearFileList)

# Generate list of swearword filter regexes by language
# One substitutaion is required for start and mid sentence, another is rerquired to handle end of sentence to deal with periods which we do not want removed as it would join sentences together.
filterSet <- data.frame(lang=unique(profanityList$langSet),stringsAsFactors=FALSE)
for (lang in unique(profanityList$langSet)) {
        filterSet[filterSet$lang == lang, 2] <- paste0("", 
                                                       paste(profanityList[profanityList$langSet == lang, 2], collapse=' |'),
                                                       " ")
        filterSet[filterSet$lang == lang, 3] <- paste0("", 
                                                       paste(profanityList[profanityList$langSet == lang, 2], collapse='[.]|'),
                                                       "[.]")}
names(filterSet) <- c("langSet", "mainPattern", "endSentencePattern")

workingSet <- allData
rm(allData) # Cached pristine data, saved to RDS earlier in case the later processing changes
# Process the dataset and filter it, but only if we're going to use it later (not cached)
if(!file.exists("workingSet.rds")){
        for (lang in filterSet$langSet) {
                # Handle normal words in the sentence
                workingSet[workingSet$lang == lang, 3] <- gsub(filterSet[filterSet$lang == lang, 2], "", workingSet[workingSet$lang == lang, 3], ignore.case = TRUE)
                # Handle words that come before a period
                workingSet[workingSet$lang == lang, 3] <- gsub(filterSet[filterSet$lang == lang, 3], ".", workingSet[workingSet$lang == lang, 3], ignore.case = TRUE)
        }
}

# filterString():
# This uses a set of regexes to remove all the unwanted predictors from the dataset
# Input: string(s)
# Output: string(s)
filterString <- function(s) {
        hashTagRegex <- "(?:\\s|^)#[A-Za-z0-9\\-\\.\\_]+(?:\\s|$)"
        s <- gsub(hashTagRegex, "", s, ignore.case = TRUE)

        emailRegex <- "[^\\s]+@[^\\s]+"
        s <- gsub(emailRegex, "", s, ignore.case = TRUE)

        # Based on https://stackoverflow.com/questions/3809401/what-is-a-good-regular-expression-to-match-a-url#3809435
        fullUrlRegex <- "(https?:\\/\\/)?(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{2,256}\\.[a-z]{2,4}\\b([-a-zA-Z0-9@:%_\\+.~#?&//=]*)"
        s <- gsub(fullUrlRegex, "", s, ignore.case = TRUE, perl = TRUE)

        # Remove numbers.  Combined with the "remove punctuation step" this will effectively strip out numbers with comma or period separators.
        numberRegex <- "[0-9]+"
        s <- gsub(numberRegex, "", s, ignore.case = TRUE, perl = TRUE)

        # Remove all punctuation except full stops.
        punctuationRegex <- "[^\\w\\s\\.]"
        s <- gsub(punctuationRegex, "", s, ignore.case = TRUE, perl = TRUE)

        s
}

# Save/load the main dataset
if(!file.exists("workingSet.rds")){
        workingSet[, 3] <- filterString(workingSet[, 3])
        saveRDS(workingSet, file = "workingSet.rds", compress = FALSE)
} else {
        workingSet <- readRDS("workingSet.rds")
}

# Split the main dataset and generate the three corpuses
if (!file.exists("corpTwitter.rds")) {
        corpTwitter <- corpus(workingSet[workingSet$srcSet == "twitter",]$textSet)
        saveRDS(corpTwitter, file = "corpTwitter.rds", compress = FALSE)
} else {
        corpTwitter <- readRDS("corpTwitter.rds")
}
if (!file.exists("corpNews.rds")) {
        corpNews <- corpus(workingSet[workingSet$srcSet == "news",]$textSet)
        saveRDS(corpNews, file = "corpNews.rds", compress = FALSE)
} else {
        corpNews <- readRDS("corpNews.rds")
}
if (!file.exists("corpBlogs.rds")) {
        corpBlogs <- corpus(workingSet[workingSet$srcSet == "blogs",]$textSet)
        saveRDS(corpBlogs, file = "corpBlogs.rds", compress = FALSE)
} else {
        corpBlogs <- readRDS("corpBlogs.rds")
}
rm(workingSet)


# Generate and save wordcounts or load them if already cached
if (!file.exists("wordCounts.rds")) {
        wordCounts <- data.frame(TwitterWordCount=sum(ntoken(texts(corpTwitter), remove_punct = TRUE)),
           NewsWordCount=sum(ntoken(texts(corpNews), remove_punct = TRUE)),
           BlogWordCount=sum(ntoken(texts(corpBlogs), remove_punct = TRUE)))
        saveRDS(wordCounts, file = "wordCounts.rds", compress = FALSE)
} else {
        wordCounts <- readRDS("wordCounts.rds")
}

if (!file.exists("dfmTwitter4.rds")) { # If the last file is present all processing must have been completed previously
        # Reshape the corpuses into sentences as that's what we're going to build predictions against
        scorpTwitter <- corpus_reshape(corpTwitter, to = c("sentences"), use_docvars = FALSE)
        saveRDS(scorpTwitter, file = "scorpTwitter.rds", compress = FALSE)
        scorpNews <- corpus_reshape(corpNews, to = c("sentences"), use_docvars = FALSE)
        saveRDS(scorpNews, file = "scorpNews.rds", compress = FALSE)
        scorpBlogs <- corpus_reshape(corpBlogs, to = c("sentences"), use_docvars = FALSE)
        saveRDS(scorpBlogs, file = "scorpBlogs.rds", compress = FALSE)

        # Convert the corpuses into tokens and cache the results
        toksTwitter <- tokens(scorpTwitter, remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               verbose = quanteda_options("verbose"), include_docvars = FALSE)
        saveRDS(toksTwitter, file = "toksTwitter.rds", compress = FALSE)
        toksNews <- tokens(scorpNews, remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               verbose = quanteda_options("verbose"), include_docvars = FALSE)

        saveRDS(toksNews, file = "toksNews.rds", compress = FALSE)
        toksBlogs <- tokens(scorpBlogs, remove_numbers = TRUE, remove_punct = TRUE,
               remove_symbols = TRUE, remove_separators = TRUE,
               verbose = quanteda_options("verbose"), include_docvars = FALSE)
        saveRDS(toksBlogs, file = "toksBlogs.rds", compress = FALSE)

        # Generate and save all the Document Feature Matricies
        dfmNews2 <- dfm(tokens_ngrams(toksNews, n = 2))
        saveRDS(dfmNews2, file = "dfmNews2.rds", compress = FALSE)
        dfmNews3 <- dfm(tokens_ngrams(toksNews, n = 3))
        saveRDS(dfmNews3, file = "dfmNews3.rds", compress = FALSE)
        dfmNews4 <- dfm(tokens_ngrams(toksNews, n = 4))
        saveRDS(dfmNews4, file = "dfmNews4.rds", compress = FALSE)
        dfmBlogs2 <- dfm(tokens_ngrams(toksBlogs, n = 2))
        saveRDS(dfmBlogs2, file = "dfmBlogs2.rds", compress = FALSE)
        dfmBlogs3 <- dfm(tokens_ngrams(toksBlogs, n = 3))
        saveRDS(dfmBlogs3, file = "dfmBlogs3.rds", compress = FALSE)
        dfmBlogs4 <- dfm(tokens_ngrams(toksBlogs, n = 4))
        saveRDS(dfmBlogs4, file = "dfmBlogs4.rds", compress = FALSE)
        dfmTwitter2 <- dfm(tokens_ngrams(toksTwitter, n = 2))
        saveRDS(dfmTwitter2, file = "dfmTwitter2.rds", compress = FALSE)
        dfmTwitter3 <- dfm(tokens_ngrams(toksTwitter, n = 3))
        saveRDS(dfmTwitter3, file = "dfmTwitter3.rds", compress = FALSE)
        dfmTwitter4 <- dfm(tokens_ngrams(toksTwitter, n = 4))
        saveRDS(dfmTwitter4, file = "dfmTwitter4.rds", compress = FALSE)
}

# Clean out all the objects that aren't required and garbage collect
rm(list=c(ls()[ls() %like% "^corp.*"]))
rm(list=c(ls()[ls() %like% "^scorp.*"]))
rm(list=c(ls()[ls() %like% "^toks.*"]))
rm(list=c(ls()[ls() %like% "^dfm.*"]))
gc()

# preCalculateBackoffData()
# This is the workhorse of the prediction algorithm, with many steps which have been documented in-line.
# dplyr is used extensively to create the data pipeline.
# Input: srcDfm: Document Feature Matrix to create predictions from
# Output: tbl_df:
#               base: ngram base i.e. "i want to"
#               tail: ngram tail/prediction i.e. "eat"
#               freq: frequency of the ngram
#               disc: Good-turing calculated discount
#               iprob: Individual ngram/tail probability in the set
#               pleftover: probability left over after discounting
#               rnum: row number within ngrams for easily filtering the dataset
preCalculateBackoffData <- function(srcDfm) {
        # Generate column sums of the ngrams from the DFM to the working set ('ws')
        ws <- tbl_df(colSums(srcDfm))
        # The previous step left us with the ngrams in the row names; convert to a column
        ws$ngram <- rownames(ws)
        # Sort the values and save
        ws <- ws %>% arrange(desc(value))
        # Generate the Good-Turing discount and save the result in Ngram Frequency Set ('ngfs') 
        ngfs <- ws %>% 
                # Group by the frequency
                group_by(value) %>% 
                # Get the count i.e. frequency of each frequency 
                tally() %>% 
                # Good-Turing requires the current and next freqency for the calculation, so use lead to get the next
                mutate( nextValue = lead(value), 
                        nNext = lead(n),
                        # Calculate the discount
                        dProposed = (nextValue/value)*(nNext/n),
                        # Determine if we use the discount or not.  5 has been chosen a the reliability cut-off.
                        d = case_when(value > 5 ~ 1, TRUE ~ dProposed))
        
        # In ngfs we have a summary table of all the discounts for ngrams.
        # We now join the discounts against all the actual ngrams
        l <- left_join(ws, ngfs, by="value") %>% 
                select(ngram, value, d)
        # Free up RAM!
        rm(ws,ngfs)
        # Split the ngram into the base and tail
        set <- strcapture(pattern = "(.*)\\_(.+$)", x = l$ngram, proto = list(key = character(), value = character()))
        l$base <- set$key
        l$tail <- set$value
        # Free up RAM!
        rm(set)
        
        # Perform the calculation of the backoff probabilities
        l <- l %>% 
                # Sort the ngrams by the base and descending frequency, with tail as a tiebreaker
                arrange(base, desc(value), tail) %>% 
                # We need to work within the base set, so group by it
                group_by(base) %>% 
                # Add the row number for quick sorting by the original order
                mutate(rnum = row_number()) %>% 
                # Rename some variables left over form the join, and trim the set to only what is necessary
                select(base, tail, freq=value, disc=d, rnum) %>% 
                # Ensure the set order hasn't changed after the select
                arrange(base, rnum) %>%
                # Re-group by base
                group_by(base) %>%
                # Calculate the values for Good-turing: row probability
                mutate(rprob = sum(freq * disc),
                       # Get the sum across the base set
                       sumfreq = sum(freq),
                       # Get the leftover probability for the entire base set
                       pleftover = 1 - rprob/sumfreq,
                       # Calculate the individual probability for each base/tail
                       iprob = (freq * disc) / sumfreq
                       # The next line can be used for testing to confirm that the calculated probabilites
                       # sum correctly, which they did during development.  It has been left out to cut
                       # processing in the final algorithm
                       #,s=sum(iprob)
                ) %>% 
                # Select the columns to be returned later
                select(base, tail, freq, disc, iprob, pleftover, rnum)
        # Return the set
        l
}

# cachePredictions()
# Take the dfm and save the full predictions sets
# Input: dfmFile: Document Feature Matrix from previous processing
# Input: fullFile: The full dataset unfiltered
# Output: none
cachePredictions <- function(dfmFile, fullFile) {
        # Get the DFM data
        dfm <- readRDS(dfmFile)
        # Do all the calculations
        l <- preCalculateBackoffData(dfm)
        # Save the unfiltered data
        saveRDS(l, file = fullFile, version = 3, compress = FALSE)
        # Filter the data to make it small enough to load onto ShinyApps.io
        l <- filter(l, freq >= keepThreshold)
        # Convert to an indexed data.table for speed
        lt <- data.table(l, key = c("base", "tail", "rnum"))
        # Save the file out.  Compression is off as the upload is compressed and there are no IO constraints.
        saveRDS(lt, file = targetFile, version = 3, compress = FALSE)
}

# Process each of the data files and cache the results.
cachePredictions("dfmBlogs2.rds", "l2bf.rds")
cachePredictions("dfmNews2.rds", "l2nf.rds")
cachePredictions("dfmTwitter2.rds", "l2tf.rds")
cachePredictions("dfmBlogs3.rds", "l3bf.rds")
cachePredictions("dfmNews3.rds", "l3nf.rds")
cachePredictions("dfmTwitter3.rds", "l3tf.rds")
cachePredictions("dfmBlogs4.rds", "l4bf.rds")
cachePredictions("dfmNews4.rds", "l4nf.rds")
cachePredictions("dfmTwitter4.rds", "l4tf.rds")


# filterPredictions()
# Take the unfiltered predictions and generate the data ready for predictions in the production environment
# Input: dfmFile: Document Feature Matrix from previous processing
# Input: targetFile: The data filtered by keepThreshold
# Input: fullFile: The full dataset unfiltered
# Output: none
filterPredictions <- function(targetFile, fullFile, keepThreshold) {
        # Get the DFM data
        l <- readRDS(fullFile)
        # Filter the data to make it small enough to load onto ShinyApps.io
        l <- filter(l, freq >= keepThreshold)
        # Convert to an indexed data.table for speed
        lt <- data.table(l, key = c("base", "tail", "rnum"))
        # Save the file out.  Compression is off as the upload is compressed and there are no IO constraints.
        saveRDS(lt, file = targetFile, version = 3, compress = FALSE)
}

# Process each of the data files and cache the results.
# The thresholds have been chosen to keep the maximum amount of data so a prediction can usually
# be returned to the user.
keepThreshold <- 3
filterPredictions("l2bt.rds", "l2bf.rds", keepThreshold)
filterPredictions("l2nt.rds", "l2nf.rds", keepThreshold)
filterPredictions("l2tt.rds", "l2tf.rds", keepThreshold)
keepThreshold <- 4
filterPredictions("l3bt.rds", "l3bf.rds", keepThreshold)
filterPredictions("l3nt.rds", "l3nf.rds", keepThreshold)
filterPredictions("l3tt.rds", "l3tf.rds", keepThreshold)
keepThreshold <- 5
filterPredictions("l4bt.rds", "l4bf.rds", keepThreshold)
filterPredictions("l4nt.rds", "l4nf.rds", keepThreshold)
filterPredictions("l4tt.rds", "l4tf.rds", keepThreshold)

# Generate full test sets for checking the prediction results
keepThreshold <- 1
filterPredictions("l2btf.rds", "l2bf.rds", keepThreshold)
filterPredictions("l2ntf.rds", "l2nf.rds", keepThreshold)
filterPredictions("l2ttf.rds", "l2tf.rds", keepThreshold)
filterPredictions("l3btf.rds", "l3bf.rds", keepThreshold)
filterPredictions("l3ntf.rds", "l3nf.rds", keepThreshold)
filterPredictions("l3ttf.rds", "l3tf.rds", keepThreshold)
filterPredictions("l4btf.rds", "l4bf.rds", keepThreshold)
filterPredictions("l4ntf.rds", "l4nf.rds", keepThreshold)
filterPredictions("l4ttf.rds", "l4tf.rds", keepThreshold)


# Generate some plots for the report
plotFunc <- function(dataFile, outputFile, title) {
        p <- readRDS(dataFile) %>%
                textstat_frequency(n = 15) %>% 
                ggplot(aes(x = reorder(feature, frequency), y = frequency)) +
                geom_point() +
                coord_flip() +
                labs(x = NULL, y = "Frequency") +
                theme_minimal() +
                ggtitle(title)
        saveRDS(p, outputFile)
}

plotFunc("dfmBlogs2.rds", "pb2.rds", "Blogs, 2-grams")
plotFunc("dfmNews2.rds", "pn2.rds", "News, 2-grams")
plotFunc("dfmTwitter2.rds", "pt2.rds", "Twitter, 2-grams")

plotFunc("dfmBlogs3.rds", "pb3.rds", "Blogs, 3-grams")
plotFunc("dfmNews3.rds", "pn3.rds", "News, 3-grams")
plotFunc("dfmTwitter3.rds", "pt3.rds", "Twitter, 3-grams")

plotFunc("dfmBlogs4.rds", "pb4.rds", "Blogs, 4-grams")
plotFunc("dfmNews4.rds", "pn4.rds", "News, 4-grams")
plotFunc("dfmTwitter4.rds", "pt4.rds", "Twitter, 4-grams")

allData <- readRDS("allData.rds")
stats <- allData %>% group_by(srcSet, langSet) %>% summarise(lineCount = n())
saveRDS(stats, "allDataLineCounts.rds")

allData$len <- stri_length(allData$textSet)
s <- summary(allData$len)
saveRDS(s, "allDataLineLength.rds")
