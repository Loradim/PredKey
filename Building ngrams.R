library(plyr)
library(dplyr)
library(stringr)
library(parallel)

#------------------------------------------------------------------------------
# read some test data to work on pre-processing
#------------------------------------------------------------------------------
print("Loading file")
# con <- file("final/en_US/en_US.blogs.txt", open = "rb")
# all_lines <- readLines(con = con, skipNul = TRUE, n = 1000000)
# close(con = con)
# # all_lines <- iconv(all_lines, from = "UTF-8", "Latin1", "")
# rm(con)

if (!exists("all_lines")) {
    all_files <- c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt")
    all_lines <- character(0)
    for (filename in all_files) {
        cat(paste0("Reading file: ", filename, "\n"))
        con <- file(paste0("final/en_US/", filename), open = "rb")
        lines <- readLines(con = con, skipNul = TRUE)
        close(con = con)
        all_lines <- c(all_lines, lines)
    }
    # => "rb", because of encoding
    # => skipNul because there are a few textstrings that contain \0

    # simple effort to fix encoding
    # all_lines <- iconv(all_lines, from = "UTF-8", "Latin1", "")

    # clean up
    rm(lines, con, filename, all_files)
}

#------------------------------------------------------------------------------
# pre-precessing to clean up data
#------------------------------------------------------------------------------
print("Preprocessing strings")
# remove ...... in lines
all_lines <- gsub("[.]{3,}", " ", all_lines)

# make all sentences end in "."
all_lines <- gsub("[\\?\\!\\.]", " . ", all_lines)

# make all characters lower case (we can correct lower/uppercase spelling later)
all_lines <- tolower(all_lines)

# remove all characters we don't want to see
all_lines <- gsub("[^a-z' \\.-]", "", all_lines)

# replace multiple whitespaces with a single whitespace
all_lines <- gsub("[ ]{1,}", " ", all_lines)

# remove leading and trailing whitespace characters
all_lines <- str_trim(all_lines)

#------------------------------------------------------------------------------
# build a dictionary of words
#------------------------------------------------------------------------------

# this will split the millions of sentences into small chunks that can be
# processed with R build in tools. this makes it pretty fast compared to any
# "hand-build" solutions of building a dictionary and counting words
print("Building dictionary")
dict <- structure(integer(0), names=character(0))
blocklength <- 50000
for (startindex in seq(from = 1, to = length(all_lines), by = blocklength)) {
    endindex <- min(startindex + blocklength - 1, length(all_lines))

    # get word counts per block
    dict_add <- table(unlist(str_split(all_lines[startindex:endindex], " ")))
    
    # find words common in both dict and dict_add
    nt1 <- names(dict)
    nt2 <- names(dict_add)
    i <- intersect(nt1, nt2)
    
    d1 <- dict[nt1 %in% i]
    d1 <- d1[order(names(d1))]
    d2 <- dict_add[nt2 %in% i]
    d2 <- d2[order(names(d2))]
    # part_1 <- dict[nt1 %in% i] + dict_add[nt2 %in% i]
    part_1 <- d1 + d2
    
    # find words only in dict
    part_2 <- dict[nt1 %in% setdiff(nt1, nt2)]

    # find words only in dict_add
    part_3 <- dict_add[nt2 %in% setdiff(nt2, nt1)]
    
    # combine to new dictionary
    dict <- c(part_1, part_2, part_3)
}
dict <- sort(dict, decreasing = TRUE)
rm(d1, d2, dict_add, startindex, endindex, blocklength, i, nt1, nt2)
rm(part_1, part_2, part_3)

#------------------------------------------------------------------------------
# reduce dictionary to the most used words
#------------------------------------------------------------------------------
print("Reducing Dictionary")
d2 <- dict[1:20000]
d2[1:length(d2)] <- 1:length(d2)
l <- as.list(d2)

#------------------------------------------------------------------------------
# tokenize strings
#------------------------------------------------------------------------------
print("Tokenizing strings")
nbr_cores <- detectCores() - 1
cl <- makeCluster(nbr_cores, type = "FORK")
tokens <- parLapply(cl, str_split(all_lines, " "),
                 fun = function(x) { as.integer(sapply(x,
                                            FUN = function(x) { a <- l[[x]];
                                            ifelse(is.null(a), -1, a) } ) ) })
stopCluster(cl)

#------------------------------------------------------------------------------
# create ngrams
#------------------------------------------------------------------------------
building_ngrams <- function(tokens, ngram) {
    all_sentences <- unlist(lapply(tokens, FUN = function(x) as.integer(c(x,0))))
    all_sentences <- c(all_sentences, rep(0L, ngram - length(all_sentences) %% ngram))
    all <- integer(0)
    for (i in 0:(ngram-1)) {
       all <- c(all, rep(0L, i), all_sentences, rep(0L, ngram - i)) 
    }
    df <- as.data.frame(matrix(data = all, ncol = ngram, byrow = TRUE))
    names(df) <- paste0("word", 1:ngram)
    for (n in names(df)) {
        df <- df[df[n] != 0,]
    }
    sort_list <- paste0("word", 1:ngram)
    df <- df[do.call(order, df[, match(sort_list, names(df))]), ]
    df <- rbind(df, c(0L, 0L, 0L))
    i <- nrow(df)
    a <- rep(TRUE, (i - 1))
    for (n in names(df)) {
        a <- a & (df[[n]][1:(i-1)] == df[[n]][2:i])
    }
    b <- which(a == FALSE)
    counts <- b - c(0L, b[1:(length(b)-1)])
    df2 <- df[b,]
    df2$count <- counts
    sort_list <- c(1:(ngram-1), (ngram+1))
    sort_decr <- c(rep(FALSE, length(sort_list)-1), TRUE)
    
    # df <- df[do.call(order, df[, match(sort_list, names(df))]), ]
    # df2 <- df2[do.call(order, df2[, sort_list], sort_decr), ]
    if (ngram == 2) {
        df2 <- df2[order(df2$word1, df2$count, decreasing = c(F, T)),]
    } else if (ngram == 3) {
        df2 <- df2[order(df2$word1, df2$word2, df2$count, decreasing = c(F, F, T)),]
    } else if (ngram == 4) {
        df2 <- df2[order(df2$word1, df2$word2, df2$word3, df2$count, decreasing = c(F, F, F, T)),]
    } else {
        stop("currently only 2-grams, 3-grams or 4-grams are supported")
    }
    df2
}

print("Building ngrams")
four_grams <- building_ngrams(tokens, 4)
three_grams <- building_ngrams(tokens, 3)
two_grams <- building_ngrams(tokens, 2)

four <- four_grams[four_grams$word1 != -1,]
four <- four[four$word2 != -1,]
four <- four[four$word3 != -1,]
four <- four[four$word4 != -1,]
four <- four[order(four$count, decreasing = TRUE),]
four <- four[four$count > 1,]
rownames(four) <- c()

three <- three_grams[three_grams$word1 != -1,]
three <- three[three$word2 != -1,]
three <- three[three$word3 != -1,]
three <- three[order(three$count, decreasing = TRUE),]
three <- three[three$count > 1,]
rownames(three) <- c()

two <- two_grams[two_grams$word1 != -1,]
two <- two[two$word2 != -1,]
two <- two[order(two$count, decreasing = TRUE),]
two <- two[two$count > 2,]
rownames(two) <- c()

save(two, three, four, file = "ngrams.Rdata")
save(dict, l, d2, file = "dict.Rdata")
