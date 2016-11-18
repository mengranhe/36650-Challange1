#!/usr/local/bin/Rscript
suppressMessages(library(testthat))
source("autocomplete.R")

############## unit testing ################
input <- read.terms("pokemon.txt")
test_that("Error in number of suggestions", {
  expect_error(autocomplete("Ar", input,-1), "Invalid k")
})

test_that("query doesn't match any of the words", {
  expect_error(autocomplete("?", input, 3), "argument lengths differ")
})

test_that("number of suggestion exceeds number of matches", {
  expect_error(autocomplete("Ha", input, 12), "queue is empty!")
})


############ Randomization test ######################
#establish prefix:
prefix1 <- letters[round(runif(1,1,25),0)]
prefix2 <- letters[round(runif(1,1,25),0)]
prefix3 <- letters[round(runif(1,1,25),0)]
prefix <- paste0(prefix1, prefix2, prefix3, collapse = "")  #generate random prefix string

#construct random suffix
randomstring <- function(x){
  suffix <- c()
  charlength <- round(runif(1,1,6),0)
  i <- 0
  while(i < charlength){
    char <- letters[round(runif(1,1,25),0)]
    suffix <- c(suffix, char)
    i <- i + 1
  }
  sufstring <- paste0(suffix, collapse = "")
  word <- paste0(x, sufstring, collapse = "")
  return(word)
}

#store all the words with random weight into a dataframe and save as randomtest.csv for randomizaiton testing
text <- data.frame()
for (w in 1:30){
  text[w,1] <- round(runif(1,1,200),0)  #weight
  text[w,2] <- randomstring(prefix)   #word
}
names(text) <- c("weight", "word")
write.table(text, file = "randomtest.txt", sep = "\t", row.names = FALSE, quote = FALSE)

input2 <- read.terms("randomtest.txt")
test_that("Weights are in order", {
  k <- round(runif(1,2,13),0) #generate random number of suggestions
  result <- matrix(unlist(strsplit(autocomplete(prefix, input2, k), " ")), byrow = TRUE, ncol = 2)
  weightlist <- as.numeric(result[,1])
  orderweight <- function(x){ 
    for (i in 1: length(x)){
      if(x[i] > x[i+1]){return(TRUE)}  #return TRUE if weight is descending ordered
      else(return(FALSE))
    }
  }
  expect_true(orderweight(weightlist))
})

test_that("Length of result = k suggestions", {
  k <- round(runif(1,2,13),0)
  result <- matrix(unlist(strsplit(autocomplete(prefix, input2, k), " ")), byrow = TRUE, ncol = 2)
  expect_equal(dim(result)[1], k)
})


