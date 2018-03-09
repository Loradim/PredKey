library(dplyr)
library(stringr)

reduce_ngrams <- function() {
    load("ngrams.Rdata")
    load("dict.Rdata")
    
    four_tbl <- tbl_df(four) %>%
        filter(count > 2) %>%
        arrange(word1, word2, word3, desc(count)) %>%
        rename(wordn = word4)
    three_tbl <- tbl_df(three) %>%
        filter(count > 2) %>%
        arrange(word1, word2, desc(count)) %>%
        rename(wordn = word3)
    two_tbl <- tbl_df(two) %>%
        filter(count > 3) %>%
        arrange(word1, desc(count)) %>%
        rename(wordn = word2)
    save(four_tbl, three_tbl, two_tbl, file = "ngrams2.Rdata")
    
    word_to_token <- l
    token_to_word <- names(d2)
    save(word_to_token, token_to_word, file = "dict2.Rdata")
}
