source("predict2.R")
library(parallel)

measureImprovement <- function(sentence, verboose = FALSE) {
    sentence_lower <- tolower(sentence)
    s <- ""
    keypresses <- 0
    
    while (s != sentence_lower) {
        # get predictions
        pre <- predict_words2(s)
        p_match <- 0
        if (pre$replace == TRUE) {
            # check if suggested word is correct replacement for particial word
            s2 <- s
            while(tolower(substr(s2, nchar(s2), nchar(s2))) %in% c(letters, "'", "-"))
            { s2 <- substr(s2, 1, nchar(s2)-1) }
            for (i in 1:3) {
                s1 <- paste0(s2, pre$words[i])
                if (s1 == substr(sentence_lower, 1, nchar(s1))) {
                    # at least it's a partial match, check if correct match
                    if (!substr(sentence_lower, nchar(s1)+1, nchar(s1)+1) %in% c(letters, "-", "'")) {
                        # yes, word is a full match
                        p_match <- i
                    }
                }
            }
        } else {
            # check if new word is what the user hasn't started typing yet
            for (i in 1:3) {
                s1 <- paste0(s, pre$words[i])
                if (s1 == substr(sentence_lower, 1, nchar(s1))) {
                    # at least it's a partial match, check if correct match
                    if (!substr(sentence_lower, nchar(s1)+1, nchar(s1)+1) %in% c(letters, "-", "'")) {
                        # yes, word is a full match
                        p_match <- i
                    }
                }
            }
        }
        
        if (p_match > 0) {
            # match was found, add keypress, check if we need to remove additional space
            if (pre$replace == TRUE) {
                while(tolower(substr(s, nchar(s), nchar(s))) %in% c(letters, "'", "-"))
                { s <- substr(s, 1, nchar(s)-1) }
            }
            s <- paste0(s, pre$words[p_match], " ")
            if (verboose) {
                print(s)
            }
            keypresses <- keypresses + 1

            # check if added space was too much
            if (s != substr(sentence_lower, 1, nchar(s))) {
                # we need to remove added space
                s <- substr(s, 1, nchar(s)-1)
                if (verboose) {
                    print(s)
                }
                keypresses <- keypresses + 1
                if (nchar(sentence_lower) > nchar(s)) {
                    s <- paste0(s, substr(sentence_lower, nchar(s)+1, nchar(s)+1))
                    if (verboose) {
                        print(s)
                    }
                    keypresses <- keypresses + 1
                }
            }
        } else {
            # no match found. add one letter
            s <- paste0(s, substr(sentence_lower, nchar(s)+1, nchar(s)+1))
            if (verboose) {
                print(s)
            }
            keypresses <- keypresses + 1
        }
    }
    keypresses / nchar(sentence)
}

# download infinite jest, great book from David Foster Wallace. One modern author
# with probably one of the biggest active vocabularies in any modern authors
# if we can reduce keypresses for this book/author, we have a "gold standard"

url <- "https://github.com/iamciera/infiniteJest/blob/master/data/bookText/David-Foster-Wallace-Infinite-Jest-v2.0.chptags.txt?raw=true"
if (!file.exists("infinitejest.txt")) {
    download.file(url = url, destfile = "infinitejest.txt")
}
con <- file("infinitejest.txt")
infinitejest <- readLines(con = con)
close(con = con)
if (file.exists("measurement.Rdata")) {
    load("measurement.Rdata")
} else {
    nbrcores <- detectCores() - 1
    clu <- makeCluster(nbrcores, type = "FORK")
    measurement <- as.numeric(parSapply(cl = clu, X = infinitejest, FUN = function(x) measureImprovement(x), simplify = TRUE))
    stopCluster(clu)
    save(measurement, file = "measurement.Rdata")
}

#
# measure improvements for sample of blogs, news and twitter
#
all_files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
out_file <- c("blogs.Rdata", "news.Rdata", "twitter.Rdata")

for (i in 1:3) {
    if (!file.exists(out_file[i])) {
        filename <- all_files[i]
        # load file
        cat(paste0("Loading ", filename, "\n"))
        con <- file(paste0("final/en_US/", filename), open = "rb")
        lines <- readLines(con = con, skipNul = TRUE)
        close(con = con)
        
        # sample 1000 examples
        cat(paste0("Sampling ", filename, "\n"))
        set.seed(12345)
        lines <- sample(lines, 1000, FALSE)
        
        # check how much we save with predkey
        cat(paste0("Measuring ", filename, "\n"))
        nbrCores <- detectCores() - 1
        clu <- makeCluster(nbrCores, type = "FORK")
        measurement2 <- as.numeric(parSapply(cl = clu, X = lines, FUN = function(x) measureImprovement(x), simplify = TRUE))
        stopCluster(clu)
        
        # save measurements
        cat(paste0("Saving ", out_file[i], "\n"))
        save(measurement2, file = out_file[i])
    }
}
