getCiteCounts <- function(citekeys, bibtex){
  ## This function takes a list of citation keys, for ex. from lit review notes
  ## and a master bibtex file with citekeys and DOIs
  ## Returns a vector of number of times each article was cited from CrossRef
  
  
  # Check for and load/install required packages
  # --> stringr, bibtex, rcrossref
  if (!'stringr' %in% installed.packages()) install.packages('stringr')
  require(stringr, quietly=TRUE)
  if (!'rcrossref' %in% installed.packages()){
    if(!'devtools' %in% installed.packages()) install.packages('devtools')
    devtools::install_github('ropensci/rcrossref')
  }
  require(rcrossref)
  
  # Load and parse bibtex file pulling out citation keys and DOIs only
  keys <- c()
  dois <- c()
  pass <- FALSE
  lib <- file(bibtex, open='r')
  for(line in readLines(lib, warn=F)){
    if(str_sub(str_trim(tolower(line)), 1, ) == '@preamble'){
      pass <- TRUE
    } else if (str_trim(line) == '"}'){
      pass <- FALSE
    }
    if(pass) next
    if(str_sub(line, 1, 1) == '@'){
      key <- str_match(line, "@\\w+[\\{]([^\\,]+)[\\,]")[1,2]
      doi <- ''
    }
    if(str_sub(str_trim(line), 1,3) == 'doi'){
      doi <- str_split(line, '\\{|\\}')[[1]][2]
    }
    if(line == '}'){
      keys <- c(keys, key)
      dois <- c(dois, doi)
    }
  }
  close(lib)
  biblib <- dois
  names(biblib) <- keys
  
  getCites <- function(key){
    # Helper function to look up citation key on CrossRef
    # Does its best to handle blank keys and missing DOIs
    if(key == ''){
      return(NA)
    }
    doi <- biblib[key]
    if(is.na(doi)){
      print(paste('Citekey not found in bibfile:', key))
      return(NA)
    } else if (doi == ''){
      return(NA)
    }
    cites <- cr_citation_count(doi)
    if(!is.numeric(cites)){
      return(NA)
    } else {
      return(cites)
    }
  }
  
  cites <- rep(NA, length(citekeys))
  
  for(i in 1:length(citekeys)){
    cites[i] <- getCites(citekeys[i])
  }
  
  return(cites)
}

####---- How To Use ----####
## Load a csv with your research notes (indexed by citekey)
# litdata <- read.csv('path/to/your/file', stringsAsFactors = FALSE)

## Point R at your Master BibTeX file
# MasterBibtex <- 'path/to/bibtex.lib'

## Use getCiteCounts to add citation counts as a new column
# litdata$Citations <- getCiteCounts(litdata$Citekey, MasterBibtex)

## Win!


####---- Demo ----####
extable <- read.csv('test.csv', stringsAsFactors = FALSE)
MasterBibtex <- 'test.bib'
extable$Citations <- getCiteCounts(extable$Citekey, MasterBibtex)


