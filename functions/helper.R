

# Translate ICPC-2 codes to chapters ==========================================

# Translate the ICPC-2 codes to chapters
translate_icpc2_chapter <- function(icpc2) {
  chapter <- str_sub(icpc2, 1, 1)
  chapter[chapter == "A"] <- "General and unspecified"
  chapter[chapter == "B"] <- "Blood and Immune Mechanism"
  chapter[chapter == "D"] <- "Digestive"
  chapter[chapter == "F"] <- "Eye"
  chapter[chapter == "H"] <- "Ear"
  chapter[chapter == "K"] <- "Cardiovascular"
  chapter[chapter == "L"] <- "Musculoskeletal"
  chapter[chapter == "N"] <- "Neurological"
  chapter[chapter == "P"] <- "Psychosocial"
  chapter[chapter == "R"] <- "Respiratory"
  chapter[chapter == "S"] <- "Skin"
  chapter[chapter == "T"] <- "Endocrine"
  chapter[chapter == "U"] <- "Urological"
  chapter[chapter == "W"] <- "Pregnancy, childbearing"
  chapter[chapter == "X"] <- "Female genital"
  chapter[chapter == "Y"] <- "Male genital"
  chapter[chapter == "Z"] <- "Social problems"
  
  
  return(chapter)
  
}


# Create date from interger =========================

# This function is used to take an integer representation of a date as an input
# and transform it into R's Date format
# The function may take dates in the format of yyyy, yyyymm, and yyyymmdd

trans_int_date <- function(int) {
  #if(!is.integer(int)) stop("Input must be integer. Check!")
  if(length(unique(nchar(int)))!=1) warning("Different input lengths across elements!")
  
  
  # Account for missing values
  missings <- is.na(int)
  
  # Create a result vector
  result <- character(length=length(int))
  result[missings] <- NA
  result <- as.Date(result)
  
  # Keep the non-missing elemnts
  int <- int[!missings]
  
  # Tranform the integer into a character string,
  # where year, month and day are seperated by a hyphon
  if (unique(nchar(int)) == 8) { # Full date information
    date_char <- paste(str_sub(int, 1, 4), str_sub(int, 5, 6), str_sub(int, 7, 8), sep="-")
  } else if (unique(nchar(int)) == 6) { # Date without day
    date_char <- paste(str_sub(int, 1, 4), str_sub(int, 5, 6), "15", sep="-")
  } else if  (unique(nchar(int)) == 4) { # Only year
    warning("Do you really want to transform a year into a date?")
    date_char <- paste(str_sub(int, 1, 4), "06", "15", sep="-")
  }
  
  # Transform the character into a Date-variable
  result[!missings] <- as.Date(date_char)
  
  return(result)
}


# Get the number of repetitions for regression table ========================


### END ########################################################################
