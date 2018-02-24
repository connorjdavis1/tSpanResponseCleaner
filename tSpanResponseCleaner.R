#=====================================================================================#
# Tone Span Response Cleaner
#=====================================================================================#
# This script will take the raw tSpan data, extract the responses with participant 
# numbers, remove duplicates, "clean" those responses, bind them with emailed responses, 
# bind responses to original data set, insert column for notes to identify "no 
# response", "emailed response, "response", and "e-prime error" 
#=====================================================================================#
# Library
library(tidyverse)
library(data.table)
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
# NEED TO ADD "Response" to all responder rows
clean <- as.data.table(clean)
clean <- clean[, ResponseType := (Strategy.RESP != "")] 
# Change TRUE to "responded"
clean[, ResponseType := as.character(ResponseType)]
clean$ResponseType <- ifelse(test = clean$ResponseType == "TRUE",yes = "RESPONSE", no = "")


#=====================================================================================#
# Bind emailed responses to "cleaned" responses. Make sure to include "email" in notes
email <- fread("emailedStrategies.csv")
email$Subject <- as.character(email$Subject)
clean$Subject <- as.character(clean$Subject)
names(email)
names(clean)

gianotable <- email[clean, on="Subject"] #bind email and clean by subject number
gianotable$Strata <- paste(gianotable$i.Strategy.RESP,gianotable$Strategy.RESP) #combine response columns to Strata
gianotable <- gianotable[, !c(2:4)] # eliminate unnecessary/duplicate columns from previous line
gianotable[gianotable$Strata == " NA"] <- "" # get rid of NA's

# ADD "EMAIL" to response type

gianotable <- as.data.table(gianotable)
gianotable <- gianotable[, ResponseType := (Strata != "")] 

# cleaned <- clean
#=====================================================================================#
# NEED TO ADD "no response" 
#=====================================================================================#
# bind responses to original data set
final <- subset(rawest, !duplicated(rawest[,2]))
final <- cbind(final, cleaned$Strategy.RESP)
# Remove old "dirt" Strategy.RESP column
final <- final[,-36] 
colnames(final)[139] <- ("Strategy.RESP")
#=====================================================================================#
write.csv(final, "cleanTSpanResponse.csv")

