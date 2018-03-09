library(dplyr)
library(stringr)

load("ngrams2.Rdata")
load("dict2.Rdata")

predict_words2 <- function(sentence) {
    line <- paste0(". ", sentence)
    # first, process the sentence the same way as the tokens
    line <- gsub("[.]{3,}", " ", line)
    # make all sentences end in "."
    line <- gsub("[\\?\\!\\.]", " . ", line)
    # make all characters lower case (we can correct lower/uppercase spelling later)
    line <- tolower(line)
    # remove all characters we don't want to see
    line <- gsub("[^a-z' \\.-]", " ", line)
    # replace multiple whitespaces with a single whitespace
    line <- gsub("[ ]{1,}", " ", line)
    # remove leading and trailing whitespace characters
    line <- str_trim(line)

    # split cleaned line to words
    words <- str_split(line, " ")[[1]]

    # check if user is currently typing the word to be predicted
    stillInLastWord <- any(tolower(substr(sentence, nchar(sentence), nchar(sentence))) == c(letters, "'", "-"))
    if (stillInLastWord) {
        # user is still typing next word, use this information to improve suggestions
        s <- str_split(line, " ")[[1]]

        # remove last partial word and only look at the last three full words
        currentWord <- s[length(s)]
        words <- words[1:(length(words)-1)]
        if (length(words) > 3) {
            words <- words[(length(words)-2):length(words)]
        }
        tokens <- as.integer(sapply(words,
                                    FUN = function(x) { a <- word_to_token[[x]];
                                    ifelse(is.null(a), -1L, a) }))
        
        # find best predictions that match the partial word
        rc <- tbl_df(data_frame(wordn = integer(), count = numeric()))
        if (length(tokens) >= 3) {
            offset <- 1 + (tokens[length(tokens) - 2] != -1) + (tokens[length(tokens) - 1] != -1) + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- four_tbl %>%
                    filter(word1 == tokens[length(tokens) - 2] | tokens[length(tokens) - 2] == -1) %>%
                    filter(word2 == tokens[length(tokens) - 1] | tokens[length(tokens) - 1] == -1) %>%
                    filter(word3 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    mutate(count = count/max(count) + offset)
                rc <- rbind(rc, rc_)
            }
        }
        if (length(tokens) >= 2) {
            offset <- 1 + (tokens[length(tokens) - 1] != -1) + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- three_tbl %>%
                    filter(word1 == tokens[length(tokens) - 1] | tokens[length(tokens) - 1] == -1) %>%
                    filter(word2 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    mutate(count = count/max(count) + offset)
                rc <- rbind(rc, rc_)
            }
        }
        if (length(tokens) >= 1) {
            offset <- 1 + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- two_tbl %>%
                    filter(word1 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    mutate(count = count/max(count) + 2)
                rc <- rbind(rc, rc_)
            }
        }
        rc <- rc %>%
            group_by(wordn) %>%
            summarize(count = max(count)) %>%
            arrange(desc(count))
        rc <- rc[which(substr(token_to_word[rc$wordn],
                              1,
                              nchar(currentWord)) == tolower(currentWord)),]
        if (dim(rc)[1] < 3) {
            # add additional words from dictionary
            index <- which(substr(token_to_word,
                                  1,
                                  nchar(currentWord)) == currentWord)
            rc <- rbind(rc, tbl_df(data.frame(wordn = index, count = (20000 - index) / 20000)))
            rc <- rc %>%
                group_by(wordn) %>%
                summarize(count = max(count)) %>%
                arrange(desc(count)) %>%
                top_n(3, wt = count)
        }
        if (dim(rc)[1] < 3) {
            # we still have less than 3 suggestions, so lets just add the typed
            # word from the user
            words <- c(token_to_word[rc$wordn], replicate(3 - dim(rc)[1], currentWord))
            list(replace = TRUE, words = words)
        } else {
            list(replace = TRUE, words = token_to_word[rc$wordn[1:3]])
        }
    } else {
        # user has finished typing last word. suggest the best 3 follow up words
        if (length(words) > 3) {
            words <- words[(length(words)-2):length(words)]
        }
        tokens <- as.integer(sapply(words,
                                    FUN = function(x) { a <- word_to_token[[x]];
                                    ifelse(is.null(a), -1L, a) }))
        
        # find the 4 matches (to be able to remove . of sentence is empty)
        rc <- tbl_df(data_frame(wordn = integer(), count = numeric()))
        if (length(tokens) >= 3) {
            offset <- 1 + (tokens[length(tokens) - 2] != -1) + (tokens[length(tokens) - 1] != -1) + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- four_tbl %>%
                    filter(word1 == tokens[length(tokens) - 2] | tokens[length(tokens) - 2] == -1) %>%
                    filter(word2 == tokens[length(tokens) - 1] | tokens[length(tokens) - 1] == -1) %>%
                    filter(word3 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    top_n(12, wt = count) %>%
                    mutate(count = count/max(count) + offset)
                rc <- rbind(rc, rc_)
            }
        }
        if (length(tokens) >= 2 & dim(rc)[1] < 12) {
            offset <- 1 + (tokens[length(tokens) - 1] != -1) + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- three_tbl %>%
                    filter(word1 == tokens[length(tokens) - 1] | tokens[length(tokens) - 1] == -1) %>%
                    filter(word2 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    top_n(12, wt = count) %>%
                    mutate(count = count/max(count) + offset)
                rc <- rbind(rc, rc_)
            }
        }
        if (length(tokens) >= 1 & dim(rc)[1] < 12) {
            offset <- 1 + (tokens[length(tokens) - 0] != -1)
            if (offset > 1) {
                rc_ <- two_tbl %>%
                    filter(word1 == tokens[length(tokens) - 0] | tokens[length(tokens) - 0] == -1) %>%
                    select(wordn, count) %>%
                    top_n(12, wt = count) %>%
                    mutate(count = count/max(count) + offset)
                rc <- rbind(rc, rc_)
            }
        }
        if (dim(rc)[1] < 12) {
            rc <- rbind(rc, tbl_df(data.frame(wordn = 1L:3L, count = c(1, 2/3, 1/3))))            
        }
        # remove . from results
        rc <- rc %>%
            group_by(wordn) %>%
            summarize(count = max(count)) %>%
            arrange(desc(count)) %>% 
            filter(wordn != word_to_token[["."]])
        list(replace = FALSE, words = token_to_word[rc$wordn[1:3]])
    }
}