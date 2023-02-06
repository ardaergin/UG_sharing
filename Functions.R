
########## Single Replacement ##########
replace.single <- function(
    column1, column2, 
    value, rep.value) {
  
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





########## Confidence Intervals ##########
lower.CI <- function(x){
  CI_L = mean(x) - (sd(x) * 1.96)/sqrt(length(x))
  CI_L
}
upper.CI <- function(x){
  CI_U = mean(x) + (sd(x) * 1.96)/sqrt(length(x))
  CI_U
}





########## Recode Numbers ##########

recode.numbers <- function(
    data,
    column, 
    maximum = 999) {
  
  ##### Error Statement #####
  if (is.numeric(maximum) == FALSE){
    stop("The 'maximum' value has to be a numeric value.")
  }
  
  ##### Data #####
  new_data <- data
  
  
  ##### Setup: New Column #####
  new_column <- paste(
    column, 
    "recode", sep="_")
  new_data[, new_column] <- NA
  
  
  ##### Loop #####
  for (i in 1:nrow(new_data)){
    
    a <- as.character(new_data[i, column])
    
    # NAs:
    if (is.na(a)) {
      new_data[i, new_column] <- NA
    }
    
    # Normal Numeric Values:
    else if (is(tryCatch(as.numeric(a),warning = function(w) w),
                "warning")==FALSE){
      if (as.numeric(a) < maximum)
      {new_data[i,new_column] <- as.numeric(a)} 
      else 
      {new_data[i,new_column] <- NA}
    }
    
    # No Numbers at all:
    else if (grepl(paste(0:9, collapse="|"), a) == FALSE)
    {new_data[i,new_column] <- NA}
    
    # Text that has a number within
    else if (grepl(paste(0:9, collapse="|"), a) == TRUE){
      
      #1: " - "
      if (grepl("-", paste(a), fixed = TRUE)){
        d <- as.data.frame(strsplit(a, "-"))
        
        if(is(tryCatch(as.numeric(d[,1]),
                       warning=function(w) w), "warning")==FALSE) 
        {new_data[i,new_column] <- mean(as.numeric(d[,1]))} 
        
        else if (any(d[,1] == 0)) 
        {new_data[i,new_column] <- 0}
        
        else {
          d.list <- data.frame(number.range = 0:maximum)
          dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
          dist_m$minID <- apply(dist_m, 1, which.min)
          d$correct <- d.list$number.range[dist_m$minID]
          new_data[i,new_column] <- mean(d$correct[d$correct > 1])
        }
      }
      
      #2: no " - "
      else {
        
        # 2a) With spaces
        if (grepl(" ", paste(a), fixed = TRUE)) {
          
          d <- as.data.frame(strsplit(a, " "))
          if (any(d[,1]==0)) {
            new_data[i,new_column] <- 0
          } else {
            d.list <- data.frame(number.range = 0:maximum)
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            new_data[i,new_column] <- mean(d$correct[d$correct > 1])
          }
        } 
        
        # 2b) No spaces at all (e.g., 8h)
        else if (grepl(" ", paste(a), fixed = TRUE)==FALSE) {
          d <- as.data.frame(a)
          if (any(d[,1]==0)) {
            new_data[i,new_column] <- 0
          } else {
            d.list <- data.frame(number.range = 0:maximum)
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            new_data[i,new_column] <- mean(d$correct[d$correct > 1])
          }
        }
      }
      
    } # ending of: Text that has a number within
    else { NULL }
  }
  return(new_data)
}



########## Recode Text 1 ##########

recode.text.vector <- function (
    data,
    column, 
    vector, 
    JW.threshold = 0.9) {
  
  ##### Language Fix #####
  unwanted_array = list(
    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 
    'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 
    'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 
    'È'='E', 'É'='E','Ê'='E', 'Ë'='E', 
    'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 
    'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 
    'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
    'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 
    'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 
    'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 
    'æ'='a', 'ç'='c','è'='e', 'é'='e', 
    'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 
    'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 
    'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
    'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 
    'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 
    'ÿ'='y')
  
  
  ##### Counters #####
  counter.categorization_step1 <- 0
  counter.categorization_step2 <- 0
  counter.uncategorized <- 0
  counter.NA <- 0
  
  
  ##### Setup: Data #####
  new_data <- data
  
  
  ##### Setup: New Column #####
  new_column <- paste(
    column, 
    "recode", sep="_")
  new_data[, new_column] <- NA
  
  
  ##### Setup: Vector #####
  # Vector as is
  b0 <- vector
  # Vector with:
  #* 1 - Lower case, 
  #* 2 - Special characters removed
  b1 <- chartr(paste(names(unwanted_array), collapse=''),
               paste(unwanted_array, collapse=''),
               as.vector(tolower(vector)))
  
  
  ##### Loop #####
  for (i in 1:nrow(new_data)) {
    
    # Setup
    a0 <- as.character(new_data[i, column])
    a1 <- chartr(paste(names(unwanted_array), collapse=''),
                 paste(unwanted_array, collapse=''),
                 tolower(new_data[i, column]))
    
    # NAs
    if (is.na(a0)) {
      new_data[i, new_column] <- NA
      
      counter.NA <- counter.NA + 1
    } 
    
    # Vector to Compare
    
    # Step 1
    else if (max(jarowinkler(a0, b0)) > 0.95){
      new_data[i, new_column] <- b0[which.max(jarowinkler(a0, b0))]
      
      # Counter:
      counter.categorization_step1 <- 
        counter.categorization_step1 + 1
    } 
    
    # Step 2
    else if (max(jarowinkler(a1, b1)) > JW.threshold){
      new_data[i, new_column] <- b0[which.max(jarowinkler(a1, b1))]
      
      # Counter:
      counter.categorization_step2 <- 
        counter.categorization_step2 + 1
    } 
    
    # Rest of the answers
    else {
      new_data[i, new_column] <- NA
      
      # Counter:
      counter.uncategorized <- counter.uncategorized + 1
    }
  }
  
  ##### Output #####
  cat("=====================", "\n")
  cat("Text Recode Output:", new_column, "\n")
  cat("--", "\n")
  cat(counter.NA, "cases were missing", "\n")
  cat(counter.categorization_step1, "cases were categorized on Step 1", "\n")
  cat(counter.categorization_step2, "cases were categorized on Step 2", "\n")
  cat(counter.uncategorized, "cases could not be categorized", "\n")
  cat("--", "\n")
  cat("Jaro-winkler Distance Threshold", JW.threshold, "\n")
  cat("=====================")
  return(new_data)
}





########## Recode Text 2 ##########

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
