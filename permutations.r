install.packages("readxl")

# import my peptide
my_seq <- "GTTGT"
my_id <- 'P0DOY5'

# create permutations of my_seq
# https://stackoverflow.com/a/34287541/13249714

getPerms <- function(x) {
  if (length(x) == 1) {
    return(x)
  }
  else {
    res <- matrix(nrow = 0, ncol = length(x))
    for (i in seq_along(x)) {
      res <- rbind(res, cbind(x[i], Recall(x[-i])))
    }
    return(res)
  }
}

my_seq_unlisted <- unlist(strsplit(my_seq, ""))
perms <- getPerms(my_seq_unlisted)

# delete duplicates from perms
not_duplicates <- unique(perms) 
prms <- apply(not_duplicates, 1, paste, collapse = "")

# import the EXCEL file with the data downloaded from UniProt (+filters)
library(readxl)
file_name <- 'uniprot-length_[5+TO+5].xlsx'
data <- read_excel(file_name)
data_not_my_id <- data[data$Entry != my_id,]

# get the sequences
seqs <- data_not_my_id$Sequence

# check if any of my sequence permutations any of the sequencies in seqs
for (perm in prms){
  for (seq in seqs){
    if (perm == seq){
      print(paste("The permutation", perm, "has been detected in nature!"))
    }
  }
}
