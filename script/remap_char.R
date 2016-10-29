# Function that replaces elements of a character variable using a mapping table.
# The mapping table is in the form of a data frame where the variable "from" 
# indicates the value to be matched and "to" is the value to use as a replacement.
# vector is the character vector with values to be remapped
# mapping is the data frame with the mapping to be used
# from is the variable name or index of the column to be matched
# to is the variable name or index of the column with replacement values

require(stringr)

map_char <- function(vector, mapping, from, to) {
     if(sum(duplicated(mapping[,from])) > 0) print("Warning: duplicate values in from vector")
     for(i in 1:length(vector)) {
          for(j in 1:nrow(mapping)) {
               if(is.na(vector[i])) next
               if(str_trim(vector[i]) == str_trim(mapping[j, from])) {
                    vector[i] <- mapping[j, to]
               }
               # vector[i] <- gsub(paste("^",mapping[j, from],"$",""), 
               #                   mapping[j, to], vector[i], fixed = TRUE)
          }
     }
     return(unlist(vector))
}
