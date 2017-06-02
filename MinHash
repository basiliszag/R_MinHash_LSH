library(stats)
library(dplyr)
library(proxy)
library(stringr)
library(data.table)
library(proto) ## needed for next library
library(gsubfn) #read multiple times <BODY>
library(R.methodsS3)
#library(R.oo)
library(digest)
setwd("C:/Users/zagklaras/Desktop/data_mining_assignment1/input_data")


options(max.print=1000000)
doc <- lapply( list.files(), readLines )





# if I had only one <BODY>
# docNew<- gsub(".*<BODY>|</BODY>.*", "", doc)


# parse files
docNew <- strapply(doc, "<BODY>(.*?)</BODY>", simplify = c)






# clear files, and turn each document into a list of its words
doc1 <- lapply(docNew, function(x) {
 
  text <- gsub("[[:punct:]]", "", x) %>% tolower()
  text <- gsub("reuter 3","",text)
  text <- gsub("\\s+", " ", text) %>% str_trim()
  word <- strsplit(text, " ") %>% unlist()
  return(word)  
# return(text)
  })


# export in files
for (i in 1:length(doc1)) {
  cat(doc1[[i]], file = paste0("C:/Users/zagklaras/Desktop/data_mining_assignment1/output_data",i, ".txt"))
}



# creating shingles
Shingling <- function(document, k) {
  
  k2 = if (length(document)<k) length(document) else k
  
  shingles <- character( length = length(document) - k2 + 1 )
  
  for( i in 1:( length(document) - k2 + 1 ) ) {
    shingles[i] <- paste( document[ i:(i + k2 - 1) ], collapse = " " )
  }
  
  return( unique(shingles) )  
}


#Shingling <- function(document, k) {
#  shingles <- character( length = length(document) - k + 1 )
#  for( i in 1:( length(document) - k + 1 ) ) {
#    shingles[i] <- paste( document[ i:(i + k - 1) ], collapse = " " )
#  }
  
#  return( unique(shingles) )  
#}



doc1[[34]]
Shingling(doc1[[34]],k=3)

which(lengths(doc1)<=4)



# "k- shingles"  Convert each document intp k-shingles

kapa=5  # choose K

doc2 <- lapply(doc1, function(x) {
  Shingling(x, k = kapa)
       
})



# function that converts hex (base 16) to decimal 
hex_to_int = function(h) {
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16^(rev(seq_along(xx) - 1)))
}



start.time <- Sys.time()


doc3 <- for (i in 1:length(doc2)) {
  for (j in 1: length(doc2[[i]])) {
    digest(doc2[[i]][j], algo = 'xxhash32')
  }
}

end.time <- Sys.time()
end.time - start.time







################### old try ###############################

# find hashcodes for each shingle
hashCodeFunction <- function(y) {
  lapply(doc2[[y]], function(x) { 
  hex_to_int(digest(x, algo = 'xxhash32'))
  })
}

#doc2hashed <- lapply(doc2[[2]], function(x) { 
#  hex_to_int(digest(x, algo = 'xxhash32'))
#})


doc2Hashed <- for(i in 1:length(doc2)) {
  lapply(doc2[[i]], function(x) { 
    hex_to_int(digest(x, algo = 'xxhash32'))
    })
}
  
