#!/usr/local/bin/Rscript

#On shell, write: Rscript autocomplete_cmd.R "Ar "pokemon.txt" 5
source("autocomplete.R")  #load autocomplete function from autocomplete.R
args <- commandArgs(trailingOnly = TRUE) #Passing arguments to an R script from command lines
prefix <- args[1]
textfile <- args[2]
k <- as.numeric(args[3])
# run automcomplete from command line arg
words <- read.terms(textfile)
autocomplete(prefix, words, k)

