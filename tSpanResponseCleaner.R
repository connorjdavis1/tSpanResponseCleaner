#=====================================================================================#
# Tone Span Response Cleaner
#=====================================================================================#
# This script will take the raw tSpan data, extract the responses with participant 
# numbers, remove duplicates, "clean" those responses, bind them with emailed responses, 
# bind responses to original data set, insert "no response" in for non-responders and 
# e-prime errors
#=====================================================================================#
# Library
library(tidyverse)
#=====================================================================================#
# Extract responses with subject number and remove duplicates
rawest <- read.csv("tSpanRawData_1_31_18.csv")
no.dup <- unique(select(rawest, contains("Subject"), contains("Strategy.RESP")))
#=====================================================================================#
# "clean" the responses
raw <- gsub("SPACE", " ", no.dup$Strategy.RESP)
raw <- gsub("SHIFT", "", raw)
raw <- gsub("ENTER", "", raw)
clean <- as.data.frame(gsub("[{}]", "", raw))
clean <- cbind(no.dup$Subject, clean)
colnames(clean) <- c("Subject", "Strategy.RESP")
#=====================================================================================#
# Bind emailed responses to "cleaned" responses
# More on this later!
cleaned <- clean
#=====================================================================================#
# NEED TO ADD "no response" to appropriate participants
#=====================================================================================#
# bind responses to original data set
final <- subset(rawest, !duplicated(rawest[,2]))
final <- cbind(final, cleaned$Strategy.RESP)
# Remove old "dirt" Strategy.RESP column
final <- final[,-36] 
colnames(final)[139] <- ("Strategy.RESP")
#=====================================================================================#
write.csv(final, "cleanTSpanResponse.csv")

