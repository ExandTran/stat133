# Midterm 3

# Write a function called numBracElements. Your function should take the following
# arguments
#   <chvec>: A character vector containing strings of letters, possibly 
#     with the "[" symbol
#
# and return the following
#   <num.brac>: an integer indicating how many elements of <chvec> contain the "["
#     symbol. For example: numBracElements(c('digit', '[:digit:]', '[]')) should return 2

numBracElements <- function(chvec){
  return(length(grep("[",chvec,fixed=TRUE)))
}


# Write a function called maxDigits that return the maximum of all (single) digits in
# a string.  The function should return 0 if there is no digit in the
# string. Your function should take the following arguments:
#   <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and return the following
#   <total>: A single number (the maximum of all digits in chvec)

maxDigits <- function(chvec){
  chvec=str_split(chvec,"")[1][[1]]
  chvec=sapply(chvec,as.numeric)
  numbers=chvec[is.na(chvec)==FALSE]
  if (length(numbers)==0){
    return(0)
  } else {
    return(max(numbers))
  }
}

# Some test cases:
all.equal(maxDigits("1z3p ! 28"), 8)
all.equal(maxDigits("abcdefg"), 0)



# Write a function called hisToHer that converts every instance of 
# him in a string to her; every instance of he to she and every instance 
# of his to her. You can assume everything is lower case. Be careful not 
# to replace words that contain him/he/his (eg you don't want to
# replace the with ther). Your function should take the argument
#   <chvec>: A character vector
#
# and return
#   <herchvec>: The same character vector with the required substitutions.

hisToHer <- function(chvec){
  chvec <- gsub(" he ", " she ",chvec)
  chvec <- gsub(" his ", " her ",chvec)
  chvec <- gsub(" him", " her",chvec)
  if (substring(chvec,1,2)=="he"){
    chvec <- paste("she",substring(chvec,4))
  }
  if (substring(chvec,1,3)=="his"|substring(chvec,1,3)=="him"){
    chvec <- paste("her",substring(chvec,5))
  }
  return(chvec)
}

# A test case
all.equal(
  hisToHer("he went to the store his mother gave him"), 
  "she went to the store her mother gave her"
)


# Write a function called mostCommonLetter that finds the most common 
# letter in a string. If there is a tie for most common letter return 
# all of the letters that were most common. Your function should 
# convert all letters in the string to *lower case* and you should 
# remove  everything other than letters. 
# Your function has the argument
#  <chvec>: A character vector of length 1 (chvec contains only 1 string)
#
# and it should return
#  <letter> The most common letter or letters in the string.
# For example mostCommonLetter("aabbccccdddd") should return 
# [1] "c" "d"

mostCommonLetter <- function(chvec){
  chvec=tolower(chvec)
  chvec=str_split(chvec,"")[1][[1]]
  chvec=table(chvec)
  max.letter=c(1)
  letter.max=c()
  for (i in 1:length(chvec)){
    if (chvec[i]==max(max.letter)){
      max.letter=append(chvec[i],max.letter)
      letter.max=append(names(chvec[i]),letter.max,after=0)
    }
    if (chvec[i]>max(max.letter)){
      max.letter=c(chvec[i])
      letter.max=c(names(chvec[i]))
    }
  }
  return(letter.max)
}
