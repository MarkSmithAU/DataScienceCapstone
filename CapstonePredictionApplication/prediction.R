# 
# prediction.R
# 
# Coursera Data Science Capstone Project
# 
# (c) 2018 Mark Smith
# 
# This his file contains the prediction routines for the shiny Shiny Application, but is is split out as a library as it's also used in testing.

# FilterString()
# This function processes the input text string that the user enters.  This is important as by processing it in exactly the same way as in the proginal data processing ensures the data will match if at all possible!
# Input: s: string or vector of strings
# Output: string or vector of strings
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
        gsub(punctuationRegex, "", s, ignore.case = TRUE, perl = TRUE)
}

# getTrigramPrediction()
# This is the workhorse of the processing for the prediction algorithm.  The pre-calculated Good-Turing discounts and probabilities are used by this algorithm to generate the results.
# Matches from the highest oders ngrams are taken first and accumulated with lower order results, then the set is returned at the end.
# Comments have been provided in-line for brevity.
#
# Input:
# input: string to process (only the last three words will be used)
# maxResults: contrain the results table to this number of rows
# l2, l3, l4: data.table of pre-processed counts, probabilities, discounts, etc.
# bigramDiscount, trigramDiscount: user supplied discounts to scale the backed-off probabilities by
#
# Output: data.table with columns "Prediction", "Probability", "Type"
#
getTrigramPrediction <- function(input, maxResults, l2, l3, l4, unigramDiscount, bigramDiscount, trigramDiscount) {
        # Set up an empty resultset
        type <- c()
        prediction <- c()
        probability <- c()
        
        # Split the text string (keep the base, no tail)
        filteredInput <- strsplit(tolower(filterString(input)), " ")[[1]]
        # Remove empty tokens
        filteredInput <- filteredInput[filteredInput != ""]
        # Get the last three words and combine to the match string
        triBase <- paste(filteredInput[max(length(filteredInput)-2, 0):length(filteredInput)], collapse="_")
        # Get matches from the trigram prediction set
        l4rows <- tbl_df(l4[base == triBase & rnum <= maxResults]) %>%
                mutate(iprob = iprob * trigramDiscount)
        # If found, add the results to the output set
        if (nrow(l4rows) != 0) { 
                type <- c(type, rep("Trigram", nrow(l4rows)))
                prediction <- c(prediction, l4rows$tail)
                probability <- c(probability, l4rows$iprob)
        }
        
        # Put the two terms for the bigram match into biBase (keep the base, no tail)
        biBase <- paste(filteredInput[max(length(filteredInput)-1, 0):length(filteredInput)], collapse="_")
        # Save set of bigrams and tails but exclude tails from {triBase, tail}
        l3rows <- l3[base == biBase & !(tail %in% l4rows$tail)]
        # Get the backoff remaining probability from l4 if available, else use 1
        boRemainingProb <- min(l4[base == triBase]$pleftover, 1)
        # Process the set to recalculate the backed-off probabilities
        l3rows <- tbl_df(l3rows) %>%
                # Group by the base
                group_by(base) %>%
                mutate( # Get the discounted row probability 
                        rprob = sum(freq * disc),
                        # Sum the frequencies across the base
                        sumfreq = sum(freq),
                        # Calculate the discounted individual probability
                        iprob = bigramDiscount * boRemainingProb * (freq * disc) / sum(rprob),
                        # Calculate the leftover probability in the set
                        pleftover = boRemainingProb - sum(iprob)
                ) %>% 
                # Sort by decreasing individual probability
                arrange(desc(iprob))
        
        # If there are any bigram rows available add them to the result set
        if (nrow(l3rows) != 0) {
                type <- c(type, rep("Bigram", nrow(l3rows)))
                prediction <- c(prediction, l3rows$tail)
                probability <- c(probability, l3rows$iprob)
        }
        
        # Get the last word and look for unigram matchesSave the last term of biBase into uniBase (tail remains the same)
        uniBase <- paste(filteredInput[length(filteredInput)], collapse="_")
        # Save set of bigrams and bases and exclude tails from {triBase, tail} and {biBase, tail}
        l2rows <- l2[base == uniBase & !(tail %in% l3rows$tail) & !(tail %in% l4rows$tail)]
        # Get the backoff remaining probability from l3, but default to 1 if not found
        boRemainingProb <- min(l4[base == triBase]$pleftover, l3[base == biBase]$pleftover, 1)
        # Process the set to recalculate the backed-off probabilities
        l2rows <- tbl_df(l2rows) %>%
                # Group by the base
                group_by(base) %>%
                mutate( # Get the discounted row probability 
                        rprob = sum(freq * disc),
                        # Sum the frequencies across the base
                        sumfreq = sum(freq),
                        # Calculate the discounted individual probability
                        iprob = unigramDiscount * boRemainingProb * (freq * disc) / sum(rprob),
                        # Calculate the leftover probability in the set
                        pleftover = boRemainingProb - sum(iprob)
                ) %>% 
                # Sort by decreasing individual probability
                arrange(desc(iprob))
        
        # Add any unigram rows
        if (nrow(l2rows) != 0) {
                type <- c(type, rep("Unigram", nrow(l2rows)))
                prediction <- c(prediction, l2rows$tail)
                probability <- c(probability, l2rows$iprob)
        }
        
        # Make a data.frame to return the result for display/graphing, sort it by probability and trim the result set down to the maximum requested
        results <- data.frame(type, prediction, probability, stringsAsFactors = FALSE)
        if (nrow(results) > 0) {
                as.data.frame(tbl_df(results) %>% 
                                      select(Prediction=prediction, Probability=probability, Type=type) %>% 
                                      arrange(desc(Probability))
                ) %>% 
                        slice(1:maxResults)
        } else {
                # if nothign was found, return a single row with this result
                data.frame(Prediction="none", Probability=0, Type="NotFound", stringsAsFactors = FALSE)
        }
}
