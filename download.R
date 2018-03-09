url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"

download.file(url, "Coursera-SwiftKey.zip")
unzip(zipfile = "Coursera-SwiftKey.zip", exdir = ".")
