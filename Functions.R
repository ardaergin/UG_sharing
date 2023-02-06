
############## Single Replacement ##############
replace.single <- function(column1, column2, value, rep.value) {
  
  # Number of Changes that will be made:
  a <- data %>% dplyr::filter(
    !!sym(column1) == value)
  
  if (is.na(rep.value)) {
    changed <- 
      sum(a[,column1] == value, na.rm=T) - 
      sum(is.na(a[,column2]))
  } else {
    changed <- 
      sum(a[,column1] == value, na.rm=T) -
      sum(a[,column2] == rep.value, na.rm=T)
  }
  
  # The Actual Assigning
  data[which(data[,column1] == value), column2] <<- rep.value
  
  # Output 
  cat("=====================", "\n")
  cat("Single Replacement:", "\n")
  cat(changed,"changes made.", "\n")
  cat("=====================", "\n")
}


############## Recode Numbers ##############
# This is a function for recoding numbers; 
#* It extracts number form text entries. Examples:
#* "I join for about 16 hours" is converted into "16"
#* When people indicate "between 16-18 hours", it takes the average, so, 17.
#* when people indicate "more than 20" its takes the exact number, 20.

# IMPORTANT: 
#* Data frame name needs to be "data"
#* It is better if Column 2, the new column, is already initialized

recode.numbers <- function(
    column1, 
    column2, # Column to be initialized
    maximum) {
  
  # Error Statement
  if (is.numeric(maximum) == FALSE){
    stop("The 'maximum' value has to be a numeric value.")
  }
  
  for (i in 1:nrow(data)){
    
    a <- as.character(data[i,column1])
    
    # NAs:
    if (is.na(a)) {
      data[i,column2] <<- NA
    }
    
    # Normal Numeric Values:
    else if (is(tryCatch(as.numeric(a),warning=function(w) w),
                "warning")==FALSE){
      if (as.numeric(a) < maximum)
        {data[i,column2] <<- as.numeric(a)} 
      else 
        {data[i,column2] <<- NA}
    }
    
    # No Numbers at all:
    else if (grepl(paste(0:9, collapse="|"), a) == FALSE)
      {data[i,column2] <<- NA}
    
    # Text that has a number within
    else if (grepl(paste(0:9, collapse="|"), a) == TRUE){
      
      #1: " - "
      if (grepl("-", paste(a), fixed = TRUE)){
        d <- as.data.frame(strsplit(a, "-"))
        
        if(is(tryCatch(as.numeric(d[,1]),
                       warning=function(w) w), "warning")==FALSE) 
          {data[i,column2] <<- mean(as.numeric(d[,1]))} 
        
        else if (any(d[,1] == 0)) 
          {data[i,column2] <<- 0}
        
        else {
          d.list <- data.frame(number.range = 0:maximum)
          dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
          dist_m$minID <- apply(dist_m, 1, which.min)
          d$correct <- d.list$number.range[dist_m$minID]
          data[i,column2] <<- mean(d$correct[d$correct > 1])
        }
      }
      
      #2: no " - "
      else {
        
        # 2a) With spaces
        if (grepl(" ", paste(a), fixed = TRUE)) {
          
          d <- as.data.frame(strsplit(a, " "))
          if (any(d[,1]==0)) {
            data[i,column2] <<- 0
          } else {
            d.list <- data.frame(number.range = 0:maximum)
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            data[i,column2] <<- mean(d$correct[d$correct > 1])
          }
        } 
        
        # 2b) No spaces at all (e.g., 8h)
        else if (grepl(" ", paste(a), fixed = TRUE)==FALSE) {
          d <- as.data.frame(a)
          if (any(d[,1]==0)) {
            data[i,column2] <<- 0
          } else {
            d.list <- data.frame(number.range = 0:maximum)
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            data[i,column2] <<- mean(d$correct[d$correct > 1])
          }
        }
      }
      
    } # ending of: Text that has a number within
    else { NULL }
  }
}

  
############## Jaro-winkler Distance ##############
# This is a function for recoding text entries; 

# Compares text and checks for overlap/similarities; 
# for example for NAME_CEI this checks 
# if people filled out same name even with different wording 
# (water vs waterr is counted similar) 

# Data frame name needs to be "data"
# Column 2 (destination column) needs to have already been initalized
# Empty responses (" ") needs to have already been converted to NA's 


############## Recode Text 1 ##############
recode.text_1 <- function (
    column1, column2, 
    vector,
    language = "english",
    JW.threshold = 0.9) {
  
  ########## Language ##########
  # English
  if (language == "english") {
    unwanted_array <- list(
      NULL
    )
  }
  # Spanish
  if (language == "spanish") {
    unwanted_array <- list(
      'á'='a','à'='a',
      'é'='e','è'='e',
      'ò'='o','ó'='o',
      'ù'='u','ú'='u','ü'='u',
      'ì'='i','í'='i',
      'ñ'='n'
    )
  }
  
  ########## Counters ##########
  counter.categorization_step1 <- 0
  counter.categorization_step2 <- 0
  counter.uncategorized <- 0
  counter.NA <- 0
  
  
  ########## Setup ##########
  # Vector as is
  b0 <- vector
  # Vector with:
  #* 1 - Lower case, 
  #* 2 - Special characters removed
  b1 <- chartr(paste(names(unwanted_array), collapse=''),
               paste(unwanted_array, collapse=''),
               as.vector(tolower(vector)))
  
  
  ########## Loop ##########
  for (i in 1:nrow(data)) {
    
    # Setup
    a0 <- as.character(data[i,column1])
    a1 <- chartr(paste(names(unwanted_array), collapse=''),
                 paste(unwanted_array, collapse=''),
                 tolower(data[i,column1]))
    
    # NAs
    if (is.na(a0)) {
      data[i,column2] <<- NA
      
      counter.NA <- counter.NA + 1
    } 
    
    # Vector to Compare
    
    # Step 1
    else if (max(jarowinkler(a0, b0)) > 0.95){
      data[i,column2] <<- b0[which.max(jarowinkler(a0, b0))]
      
      # Counter:
      counter.categorization_step1 <- 
        counter.categorization_step1 + 1
    } 
    
    # Step 2
    else if (max(jarowinkler(a1, b1)) > JW.threshold){
      data[i,column2] <<- b0[which.max(jarowinkler(a1, b1))]
      
      # Counter:
      counter.categorization_step2 <- 
        counter.categorization_step2 + 1
    } 
    
    # Rest of the answers
    else {
      data[i,column2] <<- NA
      
      # Counter:
      counter.uncategorized <- counter.uncategorized + 1
    }
  }
  
  ########## Output ##########
  cat("=====================", "\n")
  cat("Text Recode Output", "\n")
  cat("--", "\n")
  cat(counter.NA, "cases were missing", "\n")
  cat(counter.categorization_step1, "cases were categorized on Step 1", "\n")
  cat(counter.categorization_step2, "cases were categorized on Step 2", "\n")
  cat(counter.uncategorized, "cases could not be categorized", "\n")
  cat("--", "\n")
  cat("Jaro-winkler Distance Threshold", JW.threshold, "\n")
  cat("=====================")
}







############## Recode Text 2 ##############
recode.text.2 <- function (column1, column2, 
                           vector1, 
                         # fixing = TRUE,
                         vector1.assign = NA, 
                         # rest.assign = NULL, 
                         vector2 = NULL,
                         vector2.assign = NULL,
                         j.threshold1 = 0.9,
                         j.threshold2 = 0.9) {
  
  for (i in 1:nrow(data)) {
    
    a <- tolower(data[i,column1])
    b <- as.vector(tolower(vector1))
    c <- as.vector(tolower(vector2))
    
    # NAs
    if (is.na(a)) {
      data[i,column2] <<- NA
    } 
    
    # First Vector to Compare
    else if (max(jarowinkler(a, b)) > j.threshold1){
      data[i,column2] <<- vector1.assign
    } 
    
    # Second Vector to Compare
    else if (!is.null(vector2) & 
             max(jarowinkler(a, c)) > j.threshold2){
      data[i,column2] <<- vector2.assign
    } # also possible: paste(c[which.max(jarowinkler(a, c))])
    
    # Rest of the answers
    else {
      data[i,column2] <<- a
    }
  }
}



############## Recode Text: List Format ##############
recode.text.list <- function (
    column1, column2, 
    List, 
    Excluding.Vector = NULL,
    JW.threshold_List = 0.85,
    JW.threshold_Excluding = 0.85) {
  
  # Counters for the Output:
  counter.exclusion <- 0
  counter.categorization <- 0
  counter.uncategorized <- 0
  counter.NA <- 0
  
  # Setup Outside the for loop:
  b <- as.vector(tolower(unlist(List,use.names=FALSE)))
  c <- as.vector(tolower(Excluding.Vector))
  
  for (i in 1:nrow(data)) {
    
    # Setup inside the for loop:
    a <- tolower(data[i,column1])
    
    # NAs
    if (is.na(a)) {
      data[i,column2] <<- NA
      counter.NA <- counter.NA + 1
    } 
    
    # Excluding Vector
    else if (!is.null(Excluding.Vector) & 
             max(RecordLinkage::jarowinkler(a, c)) > JW.threshold_Excluding){
      data[i,column2] <<- NA
      counter.exclusion <- counter.exclusion + 1
    }
    
    # First Vector to Compare
    else if (max(RecordLinkage::jarowinkler(a, b)) > JW.threshold_List){
      
      # Step 1: Getting which list elements have the thing
      step1 <- lapply(tolower(List), function(y) 
        grep(b[which.max(jarowinkler(a, b))], y)
        )
      
      # Step 2: Indexing our List Name
      step2 <- sapply(step1, function(y) 
        length(y) > 0)
      
      # Step 3: Assigning
      data[i,column2] <<- names(List[which(step2)])
      
      counter.categorization <- counter.categorization + 1
    }
    
    # Rest of the answers
    else {
      data[i,column2] <<- NA
      
      counter.uncategorized <- counter.uncategorized + 1
    }
  }
  
  cat("=====================", "\n")
  cat("Text Recode Output", "\n")
  cat("--", "\n")
  cat(counter.NA, "cases were missing", "\n")
  cat(counter.exclusion, "cases were excluded (based on the exclusion vector)", "\n")
  cat(counter.categorization, "cases were categorized (based on the given list)", "\n")
  cat(counter.uncategorized, "cases could not be categorized (based on the given list)", "\n")
  cat("--", "\n")
  cat("Jaro-winkler Distance Thresholds:", "\n")
  if (!is.null(Excluding.Vector)) {
    cat("1. Exclusion:", JW.threshold_Excluding, "\n")
  }
  cat("2. Categorization:", JW.threshold_List, "\n")
  cat("=====================")
  
}

########## Confidence Intervals ##########
lower.CI <- function(x){
  CI_L = mean(x) - (sd(x) * 1.96)/sqrt(length(x))
  CI_L
}
upper.CI <- function(x){
  CI_U = mean(x) + (sd(x) * 1.96)/sqrt(length(x))
  CI_U
}

