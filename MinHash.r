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
library(numbers) # find next prime
setwd("/Users/vasiliszagklaras/Dropbox/MSc_Business_Analytics/Spring_Quarter_2016-2017/Data_Mining/Assignment1/input_data")



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


# clean document
docClean <- lapply(docNew, function(x) {
  
  text <- gsub("[[:punct:]]", "", x) %>% tolower()
  text <- gsub("reuter 3","",text)
  text <- gsub("\\s+", " ", text) %>% str_trim()
  #word <- strsplit(text, " ") %>% unlist()
  #return(word)  
  return(text)
})


# export in files
#for (i in 1:length(doc1)) {
#  cat(doc1[[i]], file = paste0("C:/Users/zagklaras/Desktop/data_mining_assignment1/output_data",i, ".txt"))
#}



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





# "k- shingles"  Convert each document intp k-shingles

kapa=2  # choose K

doc2 <- lapply(doc1, function(x) {
  Shingling(x, k = kapa)
       
})



# function that converts hex (base 16) to decimal 
hex_to_int = function(h) {
  xx = strsplit(tolower(h), "")[[1L]]
  pos = match(xx, c(0L:9L, letters[1L:6L]))
  sum((pos - 1L) * 16^(rev(seq_along(xx) - 1)))
}



# hashCode to each element of doc2 
# then convert hex to int
myFunction <- function(x,y) {
  hex_to_int(digest( doc2[[x]][y] , algo = 'xxhash32' ))
}




# apply that to entire doc2
# red is a list, where each element is a list of hashcodes
red <- vector(mode='list', length =length(doc2))
for (i in 1:length(doc2)) {
  for (j in 1: length(doc2[[i]])) {
    red[[i]][j] <- myFunction(i,j)
  }
}



# total number of shingles

# I need that because I need to pick random parameters in-between

totalShingles = 0
for (i in 1:length(red)) { 
  totalShingles = totalShingles + length(red[[i]])
  }

# I need the next prime number to total shingles, in order to feed my hash functions (minHash)
c = nextPrime(totalShingles)



# hash


mylist = vector(mode='list', length =1)
start.time <- Sys.time()
for (i in 1:19000) {
  mylist = unlist(append(mylist,red[[i]]))
}
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


length(mylist)
length(unique(mylist))


# number of hash functions (signature number)
signature_num <- 40

# prime number
prime <- c

# generate the unique coefficients  
set.seed(12345)
coeff_a <- sample( length(unique(mylist)), signature_num )
coeff_b <- sample( length(unique(mylist)), signature_num )


# If i call dim(red[[1]]) i get null, but using the script bellow I get actual length

red1 <- vector(mode='list', length =1)
for (i in 1:length(red)) {
  red1[[i]] <- as.array(red[[i]])
}
  




#?? this is a list, where each element is a table that holds as many columns as number of hash functions
# I need, and for each column I get a hash function applied to shingles that have been converted to
# integers

myFinalList <- lapply (red1, function(x) {
   mymatrix = matrix(, nrow = nrow(as.matrix(x)), ncol = signature_num)
   
   for (i in 1:nrow(as.matrix(x)) ) {
     for (j in 1:signature_num) {
       mymatrix[i,j] =  ( coeff_a[j] * x[i] + coeff_b[j] ) %% prime
     }
   }
   
   return(mymatrix)
 })




# Calculating signatures for each document
# using minHash
mySignaturesList <- lapply (myFinalList, function(x) {
  
  minHash <- apply(x,2,min)
  return(minHash)
})


# testing similarity

for (i in 2:900) {
  print(mySignaturesList[[1]]==mySignaturesList[[i]])
}


# function to compate x to y

#total number of documents
length(red1) #holds hashcodes
length(docNew) # holds text

signatureSimilarity <- function(x,y) {
  temp=length(intersect(mySignaturesList[[x]],mySignaturesList[[y]]))/signature_num
  return(temp)
}


#temp = table(mySignaturesList[[1]]==mySignaturesList[[2]])
#temp[[2]]

# Jaccard Similarity
# doc2 holds k-shingles


jaccardSimilarity <- function(x, y) {
 temp = length(intersect(doc2[[x]],doc2[[y]]))/length(union(doc2[[x]],doc2[[y]]))
 return(temp)
}


# testing area

document1 = 2
document2 = 2

for (i in 1:500) {
print( signatureSimilarity(document1,i))
print(jaccardSimilarity(document1,i))
print("-")
}



rowsPerBand= 4

a <- 1:signature_num
n <- length(a)
 ## your LEN
bandMapper = split(a, rep(1:ceiling(n/rowsPerBand), each=rowsPerBand)[1:n])

totalBands= length(bandMapper)


# create a matrix with all signatures
mySignatureMatrix =do.call(rbind, mySignaturesList)
# columns = all my documents
# rows = elements of signatures
mySignatureMatrix= t(mySignatureMatrix)


# create bands

# bucketList has all documents as columns, and each minisignature hashed
bucketList = matrix(nrow =  totalBands, ncol = ncol(mySignatureMatrix))
for (i in 1: totalBands) {
  for (j in 1: ncol(mySignatureMatrix)) {
    bucketList[i,j] = hex_to_int(digest( mySignatureMatrix[bandMapper[[i]],j] , algo = 'xxhash32' ))
  }
}

# first method
#setNames(lapply(unique(bucketList), function(i) 
#  as.vector(which(bucketList==i, arr.ind = TRUE)[,2])), unique(bucketList)) 

# this list holds all buckets, and which documents belong to each one
bucketDocumentsList =split(col(bucketList), bucketList)
bucketDocumentsList   # H lista auti exei ola ta buckets kai poia documents einai se kathe bucket

# for input document get buckets

inputDocument= 485

# this list holds in which buckets are inputDocument's minisignatures hased
bucketsContainingInputDoc = as.list(bucketList[,inputDocument])
bucketsContainingInputDoc    #  edw vriskw se poia buckets einai topothetimeno to input document




# This list holds the positions on buckeτDocumentsList on which inputDocument can be found
# i use it to see if this bucket holds other documents in order to compare them

positionsInputDocCanBeFound = list()
for (i in 1:totalBands) {
  positionsInputDocCanBeFound[i] = which( names(bucketDocumentsList) == bucketsContainingInputDoc[[i]])
}

positionsInputDocCanBeFound


################################## #################################

########### prepei na dw poia buckets exoun >1 docs


# 
listsOfDocumentsInsideBucketsWhereInputDocCanBeFound = lapply (positionsInputDocCanBeFound, function(x) {
  bucketDocumentsList[[x]]
})


# H idea einai pws pairnw tin lista me ola ta buckets poy einai mesa to input document
# kai pairnw pio sugkekrimena ta docs pou periexoun auta ta buckets
# anti na psaksw poia buckets exoun mesa >1 docs, dld na exoun mesa ki alla pithanws omoia me to
# input doc, ta pairnw ola mazi uniqly omws, afou allwstw thelw to zitoymeno na to sugkrinw me ola

# apo tin nea lista pou dimiourgw (xrisimopoiontaw to unlist) pairnw ta unique, kai meta afairw
# apla to arxiko mou. Epeidi den kserw tin thesi tou, tin briskw kai afairw apo tin lista to doc pou einai
# stin thesi poy brika

#kataligw stin lista parakatw poy exei ola ta docs me ta opoia thelw na sugkrinw pairwise to input mou

listOfDocumentsTestingForSimilarity = unique( unlist(listsOfDocumentsInsideBucketsWhereInputDocCanBeFound))[-which( unique( unlist(listsOfDocumentsInsideBucketsWhereInputDocCanBeFound)) == inputDocument)]
listOfDocumentsTestingForSimilarity



# print all neigbhors and their similarity with
#input document
lapply(listOfDocumentsTestingForSimilarity, function(i) {
     myInitiallyNearNeighbors = c(i,signatureSimilarity(inputDocument, i))
   })


######################################################################################################
# print input document
docClean[[inputDocument]]


bucketDocumentsList[[485]]
jaccardSimilarity(34,69)

