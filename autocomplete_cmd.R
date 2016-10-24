#!/usr/local/bin/Rscript
args <- commandArgs(trailingOnly = TRUE) #Passing arguments to an R script from command lines
prefix <- args[1]
textfile <- args[2]
k <- as.numeric(args[3])
#On shell, write: Rscript "the" "wiktionary.txt" 5

suppressMessages(library(hash))
suppressMessages(library(liqueueR))
suppressMessages(library(profvis))
suppressMessages(library(testthat))

#constructor function for "trie" class:
trie <-
  function(maxweight = -1,
           letterkey = NA,
           children = hash(),
           isword = FALSE,
           fullword = NULL) {
    #initialze the attributes of node
    object <- list(
      maxweight = maxweight,
      letterkey = letterkey,
      children = children,
      isword = isword,
      fullword = fullword
    )   #fullword is to check if current input is full word
    attr(object, "class") <- "trie"
    return(object)
  }

##The name of the functions have two parts separated by a ”.”
##where the prefix is the function name and the suffix is the name of a class.

#Insert:
insert.trie <- function(node, weight, word, full) {
    #each node has weight(self/max), aggregation of letters(word), and status of fullword
    if (is.null(word)) {
      #if no letters left(leaf node), restletter = NULL passed to word
      node$isword <- TRUE
      node$fullword <- full
      node$maxweight <- weight
      return(node)
    }
    else{
      letter <-
        unlist(strsplit(word, split = ""))   #split aggregation of letters into pieces
      if (!has.key(letter[1], node$children)) {
        #if current letter is not a key to children, create a new node
        node$children[[letter[1]]] = trie(letterkey = letter[1])   # assign key value pair to this current node, default rest of attributes
      }
      if (length(letter) > 1) {
        restletter <-
          paste0(letter[2:length(letter)], collapse = "") #collapse rest of letters as new word to next iteration
      } else{
        restletter <-
          NULL #leaf node has length(letter) == 1, no more letters left
      }
      node$children[[letter[1]]] = insert.trie(node$children[[letter[1]]], weight, restletter, full)
      node$maxweight = max(node$maxweight, node$children[[letter[1]]]$maxweight)
      return(node)
    }
  }

read.terms <- function(textfile) {
    #read text file and initialize node
    dtf <-
      read.csv(
        textfile,
        header = FALSE,
        as.is = TRUE,
        skip = 1,
        strip.white = TRUE,
        sep = "\t",
        col.names = c("weight", "word")
      )
    
    #initialize root, then insert every word to build trie
    node = trie()
    for (i in 1:nrow(dtf)) {
      node = insert.trie(node, dtf[i, 1], dtf[i, 2], dtf[i, 2])
    }
    return(node)
  }

autocomplete <- function(prefix, node, k) {
  if (k < 0) {
    stop("Invalid k")
  }
  
  prefixletter <- unlist(strsplit(prefix, split = ""))
  queue <-
    PriorityQueue()   #assign queue as class of priority queue
  count = 0
  
  for (i in 1:length(prefixletter)) {
    sub <- node$children[[prefixletter[i]]]
    node <- sub
  }
  
  queue$push(node, node$maxweight) #push root on queue first
  while (count < k) {
    pop <- queue$pop()  #pop first node on queue according to weight
    if (pop$isword) {
      count = count + 1
      answer = pop$fullword # get word
      print(paste0(c(pop$maxweight, " ", answer), collapse = ""))
    }
    
    for (key in keys(pop$children)) {
      queue$push(pop$children[[key]], pop$children[[key]]$maxweight) #push all children's keys on queue and their weights
    }
  }
}

# run automcomplete from command line arg
words <- read.terms(textfile)
autocomplete(prefix, words, k)



#######################################Testing#######################################
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

#Randomization test
for (j in 1:3) {
  #The results correctly matach queries.
  random <- round(rnorm(1, 5, 3), 0)
  print(paste("Number of k suggestion is", random, sep = " "))
  if (j == 1) {
    autocomplete("C", input, random)
  }
  else if (j == 2) {
    autocomplete("T", input, random)
  }
  else{
    autocomplete("B", input, random)
  }
}

#########################################Profiling######################################
#code:
#     Rprof("autocompelte.out")
#     -------function and arguments-------
#     Rprof(NULL)
#     summaryRprof("autocompelte.out")

####autocomplete("The", "movies.txt", 5):
# $by.self
#                      self.time self.pct total.time total.pct
# "<Anonymous>"         0.02    14.29       0.08     57.14
# "callSuper"           0.02    14.29       0.04     28.57
# "is"                  0.02    14.29       0.04     28.57
# ".identC"             0.02    14.29       0.02     14.29
# "as.environment"      0.02    14.29       0.02     14.29
# "initialize"          0.02    14.29       0.02     14.29
# "print.default"       0.02    14.29       0.02     14.29
#
# $by.total
#                               total.time total.pct self.time self.pct
# "autocomplete"                   0.14    100.00      0.00     0.00
# "queue$push"                     0.10     71.43      0.00     0.00
# "<Anonymous>"                    0.08     57.14      0.02    14.29
# "methods:::.setDummyField"       0.06     42.86      0.00     0.00
# "callSuper"                      0.04     28.57      0.02    14.29
# "is"                             0.04     28.57      0.02    14.29
# ".identC"                        0.02     14.29      0.02    14.29
# "as.environment"                 0.02     14.29      0.02    14.29
# "initialize"                     0.02     14.29      0.02    14.29
# "print.default"                  0.02     14.29      0.02    14.29
# "new"                            0.02     14.29      0.00     0.00
# "print"                          0.02     14.29      0.00     0.00
# "PriorityQueue"                  0.02     14.29      0.00     0.00
#
# $sample.interval
# [1] 0.02
#
# $sampling.time
# [1] 0.14

####autocomplete("The", "movies.txt", 10):
# $by.self
#                     self.time self.pct total.time total.pct
# "queue$push"          0.08    30.77       0.20     76.92
# "callSuper"           0.06    23.08       0.06     23.08
# "queue$pop"           0.04    15.38       0.06     23.08
# ".class1"             0.02     7.69       0.02      7.69
# "as.environment"      0.02     7.69       0.02      7.69
# "get"                 0.02     7.69       0.02      7.69
# "getClassDef"         0.02     7.69       0.02      7.69
#
# $by.total
#                               total.time total.pct self.time self.pct
# "autocomplete"                   0.26    100.00      0.00     0.00
# "queue$push"                     0.20     76.92      0.08    30.77
# "<Anonymous>"                    0.08     30.77      0.00     0.00
# "callSuper"                      0.06     23.08      0.06    23.08
# "queue$pop"                      0.06     23.08      0.04    15.38
# "methods:::.setDummyField"       0.06     23.08      0.00     0.00
# ".class1"                        0.02      7.69      0.02     7.69
# "as.environment"                 0.02      7.69      0.02     7.69
# "get"                            0.02      7.69      0.02     7.69
# "getClassDef"                    0.02      7.69      0.02     7.69
# ":::"                            0.02      7.69      0.00     0.00
# "as"                             0.02      7.69      0.00     0.00
# "is"                             0.02      7.69      0.00     0.00
#
# $sample.interval
# [1] 0.02
#
# $sampling.time
# [1] 0.26

##Answer:
#For autocomplete("The", "movies.txt", 10), the function takes 0.26 seconds to match
#prefix query. In particular, the slowest part is queue$push(), it takes 0.20 seconds to run, which is 76.92%
#of overall time. The way to improve priority queue is to try other queue packages or
#implement priority queue during trie insertion instead of autocomplete() function.