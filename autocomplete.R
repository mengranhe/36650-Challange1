suppressMessages(library(liqueueR))
suppressMessages(library(hash))
#constructor function for "trie" class:
trie <- function(maxweight = -1, letterkey = NA, children = hash(), isword = FALSE, fullword = NULL){
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

#Insert:
insert.trie <- function(node, weight, word, full) {
  if (is.null(word)) {
    #if no letters left(leaf node), restletter = NULL passed to word
    node$isword <- TRUE
    node$fullword <- full
    node$maxweight <- weight
    return(node)
  }
  else{
    letter <- unlist(strsplit(word, split = ""))   
    if (!has.key(letter[1], node$children)) {
      #if current letter is not a key to children, create a new node
      node$children[[letter[1]]] = trie(letterkey = letter[1])   # assign key value pair to this current node, default rest of attributes
    }
    if (length(letter) > 1) {
      restletter <- paste0(letter[2:length(letter)], collapse = "") #collapse rest of letters as new word to next iteration
    } else{
      restletter <- NULL #leaf node has length(letter) == 1, no more letters left
    }
    node$children[[letter[1]]] <- insert.trie(node$children[[letter[1]]], weight, restletter, full)
    node$maxweight <- max(node$maxweight, node$children[[letter[1]]]$maxweight)
    return(node)
  }
}

read.terms <- function(textfile) {
  #read text file and initialize node
  dtf <- read.csv(textfile, header = FALSE, as.is = TRUE, skip = 1, strip.white = TRUE, sep = "\t", col.names = c("weight", "word"))
  
  #initialize root, then insert every word to build trie
  node <- trie()
  for (i in 1:nrow(dtf)) {
    node <- insert.trie(node, dtf[i, 1], dtf[i, 2], dtf[i, 2])
  }
  return(node)
}

autocomplete <- function(prefix, node, k) {
  if (k < 0) {
    stop("Invalid k")
  }
  
  prefixletter <- unlist(strsplit(prefix, split = ""))
  queue <- PriorityQueue()   
  count <- 0
  
  for (char in prefixletter) {
    sub <- node$children[[char]]
    node <- sub
  }
  
  queue$push(node, node$maxweight) #push root on queue first
  answerlist <- c()
  while (count < k) {
    pick <- queue$pop() 
    if (pick$isword) {
      count = count + 1
      answer = pick$fullword 
      answerlist[count] <- paste0(c(pick$maxweight, " ", answer), collapse = "")
    }
    
    for (key in keys(pick$children)) {
      queue$push(pick$children[[key]], pick$children[[key]]$maxweight) #push all children's keys on queue and their weights
    }
  }
  return(answerlist)
}

