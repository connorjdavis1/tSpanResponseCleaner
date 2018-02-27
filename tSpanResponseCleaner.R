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
raw <- gsub("CAPSLOCK", "", raw)
raw <- gsub("LEFTARROWLEFTARROWLEFTARROWLEFTARROWRIGHTARROWRIGHTARROWRIGHTARROWRIGHTARROW", "", raw)
clean <- as.data.frame(gsub("[{}]", "", raw))
clean <- cbind(no.dup$Subject, clean)
colnames(clean) <- c("Subject", "Strategy.RESP")
#=====================================================================================#
clean <- as.data.table(clean)
clean <- clean[, Response.Type := (Strategy.RESP != "")] 
# Change TRUE to "responded"
clean[, Response.Type := as.character(Response.Type)]
clean$Response.Type <- ifelse(test = clean$Response.Type == "TRUE",yes = "RESPONSE", no = "")


#=====================================================================================#
# Bind emailed responses to "cleaned" responses. Make sure to include "email" in notes
email <- fread("emailedStrategies.csv")
email$Subject <- as.character(email$Subject)
clean$Subject <- as.character(clean$Subject)
names(email)
names(clean)

gianotable <- email[clean, on="Subject"] #bind email and clean by subject number
gianotable$Strata <- paste(gianotable$i.Strategy.RESP,gianotable$Strategy.RESP) #combine response columns to Strata
gianotable <- gianotable[, !c(3:4)] # eliminate unnecessary/duplicate columns from previous line
gianotable[gianotable$`Type of Strategy Response` == ""] <- ""
gianotable$Response.Type <- paste(gianotable$'Type of Strategy Response', gianotable$Response.Type) #add "emailed" to response type column
gianotable[gianotable == " NA"] <- "" # get rid of NA's
gianotable$Response.Type[gianotable$Response.Type == "NA RESPONSE"] <- "RESPONSE" # Fix error from paste
gianotable[gianotable == "NA "] <- "" # Delete some more NA's
gianotable$Response.Type[gianotable$Response.Type == ""] <- "NO RESPONSE" # Add "NO RESPONSE" to response.type 
gianotable <- gianotable[, !c(2)] #eliminate unnecessary column from paste
gianotable$Strata <- str_replace_all(gianotable$Strata, "NA", "") # remove more blasted NA's from the end of the string
cleaned <- gianotable # Prepped for binding back to original dataset
#=====================================================================================#
# bind responses to original data set
final <- subset(rawest, !duplicated(rawest[,2]))
final <- cbind(final, cleaned) #bind original and cleaned dataset
final <- final[,-36] # Remove old Strategy.RESP column
colnames(final)[142] <- ("Strategy.RESP")
#=====================================================================================#
write.csv(final, "cleanTSpanResponse.csv")