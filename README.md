# PredKey - predictive keyboard prototype

by Heiko Lange

Capstone project for the Data Science Course on Coursera
by Johns Hopkins University

# Introduction

PredKey is a keyboard that help users typing english sentences.
Based on ngrams (last typed words) and potentially the partial
word already typed, PredKey will suggest the next words.

# List of files
```
README.md                 - this file
KeyboardPresentation.Rmd  - pitch presentation in as R markdown
KeyboardPresentation.html - pitch presentation knitted into html format

download.R                - downloader for examples given by
Building ngrams.R         - loads, analyses, tokenizes texts, builds dictionary and ngrams
reduceVariables.R         - further reduces ngrams and dictionary and converts into tibbles
predict2.R                - suggest next words based on sentence
app.R                     - shiny app client/server app as live demonstration
measurement.R             - measures performance of prediction algorith vs regular keyboard

dict2.Rdata               - contains translations from words to tokens and back
ngrams2.Rdata             - contains 4-grams, 3-grams and 2-grams based on tokens
measurement.Rdata         - contains % keypresses of PredKey vs regular keyboard for 'Infinite Jest'
blogs.Rdata               - contains % keypresses of PredKey vs regular keyboard for 1.000 sample blog entries
news.Rdata                - contains % keypresses of Predkey vs regular keyboard for 1.000 sample news articles
twitter.Rdata             - contains % keypresses of Predkey vs regular keyboard for 1.000 twitter tweets
```

