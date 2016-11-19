suppressMessages(library(profvis))
source("autocomplete.R")
input <- read.terms("movies.txt")

Rprof("autocompelte.out")
autocomplete("The", input, 5)
Rprof(NULL)
summaryRprof("autocompelte.out")

Rprof("autocompelte.out")
autocomplete("The", input, 10)
Rprof(NULL)
summaryRprof("autocompelte.out")
