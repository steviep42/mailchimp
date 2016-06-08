setwd("~/Downloads")
library(readxl)

# Here we pick out the tweets that are known to relate to the Mandrill app. This is 
# spreadsheet 1 in the workbook

mandrill1 <- read_excel("Mandrill.xlsx",1)

mandrill2 <- mandrill1[,1]
mandrill2 <- mandrill2[complete.cases(mandrill2),]   # Get only complete cases

# Let's create a function to do cleanup. There are some packages that have twitter specific
# functions to do this but let's mimic what we see in the Mandrill example

cleaners <- function(x) {
  x <- tolower(x)
  x <- gsub("\\. "," ",x)
  x <- gsub("\\: "," ",x)
  x <- gsub("\\?"," ",x)
  x <- gsub("\\!"," ",x)
  x <- gsub("\\;"," ",x)
  x <- gsub("\\,"," ",x)
#  x <- gsub("[[:punct:]]","",x)
  return(x)
}

mandrill2 <- sapply(mandrill2,cleaners)
  
tokens_per_tweet <-  apply(as.matrix(mandrill2),1,strsplit," ")

total_tokens <- unlist(tokens_per_tweet)

token_table <- table(total_tokens)

# Let's find the count for a given token and check it against
# the spreadsheet as a spot check

token_table[which(names(token_table)=="your")]

# Now strip out the tokens that are <= 3 characters long

final_token_table <- token_table[nchar(names(token_table)) > 3] 

final_token_table[1:20]

probs <- as.vector((final_token_table+1)/sum(final_token_table+1))
names(probs) <- names(final_token_table)

# According to the text the probability for the "support" token is .002074689
# Let's do a spot check 

probs[which(names(probs)=="support")]

# support 
# 0.002074689 

# Now take the natural log of these values to take into consideration the 
# additive effects

ln_probs <- log(probs)

app_prob_df <- data.frame(token=names(probs),
                      token_count=as.vector(final_token_table),
                      probs=as.vector(probs),
                      ln_probs=as.vector(ln_probs),
                      stringsAsFactors = FALSE)

# So now we need to look at the data in the second spreadsheet which corresponds to tweets
# that don't relate to the Mandrill app. We can reuse the code above - might should make it
# into a function but we'll do that later.


mandrill1 <- read_excel("Mandrill.xlsx",2)

mandrill2 <- mandrill1[,1]
mandrill2 <- mandrill2[complete.cases(mandrill2),]


mandrill2 <- sapply(mandrill2,cleaners)

tokens_per_tweet <-  apply(as.matrix(mandrill2),1,strsplit," ")

total_tokens <- unlist(tokens_per_tweet)

token_table <- table(total_tokens)

# Let's find the count for a given token and check it against
# the spreadsheet as a spot check

token_table[which(names(token_table)=="#nameanamazingband")]

# Now strip out the tokens that are <= 3 characters long

final_token_table <- token_table[nchar(names(token_table)) > 3] 

final_token_table[1:20]

probs <- as.vector((final_token_table+1)/sum(final_token_table+1))
names(probs) <- names(final_token_table)


ln_probs <- log(probs)

other_prob_df <- data.frame(token=names(probs),
                          token_count=as.vector(final_token_table),
                          probs=as.vector(probs),
                          ln_probs=as.vector(ln_probs),
                          stringsAsFactors = FALSE)

# Okay now we are ready to read in the tweets that we want to classify

test_tweets <- read_excel("Mandrill.xlsx",7)

test_tweets <- test_tweets[,2:3]

# Normalize the test tweets like above.

just_tweets <- as.list(sapply(test_tweets[,2],cleaners))

# Split up the tweets into a list
split_tweets <- sapply(just_tweets,strsplit," ")

# Next we write a function to process each of the tokens within each tweet

token_probs <- function(y,df) {
  tmp_probs <- vector()
  length(tmp_probs) <- length(y)
  for (ii in 1:length(y)) {
    if (nchar(y[ii]) <= 3) {                        # If the token is 3 chars or less then set prob to 0
        tmp_probs[ii] <- 0
    } else {
        tmp <- df[df$token==y[ii],]$ln_probs
        if ( length(tmp) == 0 ) {                   # If token is not in data frame then apply formula given from text
            tmp_probs[ii] <- log(1/sum(df$token_count+1))
        } else {
            tmp_probs[ii] <- tmp                    # We found the token and pull out the associated log prob
        }
    }
  } 
  return(sum(tmp_probs))
}

approbs    <- sapply(split_tweets,token_probs,app_prob_df)
otherprobs <- sapply(split_tweets,token_probs,other_prob_df)

# Here are the predictions

ifelse(approbs > otherprobs,"APP","OTHER")
  