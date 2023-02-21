
########## Single Replacement ##########
replace.single <- function(
    data,
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
  data[which(data[,column1] == value), column2] <- rep.value
  
  # Output 
  cat("=====================", "\n")
  cat("Single Replacement:", "\n")
  cat(changed,"changes made.", "\n")
  cat("=====================", "\n")
  return(data)
}



########## Multiple Replacement ##########


replace.m <- function(
    data,
    column1, column2,
    vector, rep.value) {
  
  vector = meaningless
  rep.value = NA
  data = data
  column1 = "RENT_OWN_4_TEXT"
  column2 = "RENT_OWN_recode.2"
  
  # Number of Changes that will be made:
  a <- data[
    grep(paste(tolower(vector), collapse="|"),
         tolower(data[[column1]])), ]
  
  a[[column1]]==
  
  if (is.na(rep.value)) {
    changed <- nrow(a) - sum(is.na(a[,column2]))
  } else {
    changed <- nrow(a) - sum(a[[column2]] == rep.value, na.rm=T)
  }
  
  # The Actual Assigning
  data[
    grep(paste(vec, collapse="|"),
         as.vector(y)), column2] <- rep.value
  
  # Output 
  cat("=====================", "\n")
  cat("Single Replacement:", "\n")
  cat(changed,"changes made.", "\n")
  cat("=====================", "\n")
  return(data)
}


replacing <- function(
    data,
    column1,column2,
    vector, rep.value) {
  
  data_new <- data
  
  for (i in 1:nrow(data_new)) {
    
    x <- as.character(data_new[i,column1])
    
    if (!is.na(x)) {
      if(any(x == vector)){
        data_new[i,column2] <- rep.value
      }
    }
  }
  
  changed <- sum(
    data_new!=data, na.rm=T) + 
    abs(
      sum(is.na(data)) - sum(is.na(data_new))
      )
  # Output 
  cat("=====================", "\n")
  cat("Single Replacement:", "\n")
  cat(changed,"changes made.", "\n")
  cat("=====================", "\n")

  return(data_new)
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
    maximum = 1000) {
  
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
    
    a <- gsub(",", ".", as.character(new_data[i, column]))
    
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
          d.list <- data.frame(number.range = c(0:maximum,0.5:(maximum+0.5)))
          dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
          dist_m$minID <- apply(dist_m, 1, which.min)
          d$correct <- d.list$number.range[dist_m$minID]
          new_data[i,new_column] <- mean(d$correct[d$correct > 0])
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
            d.list <- data.frame(number.range = c(0:maximum,0.5:(maximum+0.5)))
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            new_data[i,new_column] <- mean(d$correct[d$correct > 0])
          }
        } 
        
        # 2b) No spaces at all (e.g., 8h)
        else if (grepl(" ", paste(a), fixed = TRUE)==FALSE) {
          d <- as.data.frame(a)
          if (any(d[,1]==0)) {
            new_data[i,new_column] <- 0
          } else {
            d.list <- data.frame(number.range = c(0:maximum,0.5:(maximum+0.5)))
            dist_m <- data.frame(stringdist::stringdistmatrix(d[,1], d.list$number.range))
            dist_m$minID <- apply(dist_m, 1, which.min)
            d$correct <- d.list$number.range[dist_m$minID]
            new_data[i,new_column] <- mean(d$correct[d$correct > 0])
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
    x, 
    Listing = FALSE,
    JW.threshold_step1 = 0.98,
    JW.threshold_step2 = 0.90) {
  
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
  if (Listing == FALSE) {
    # Vector as is
    b0 <- x
    # Vector with:
    #* 1 - Lower case, 
    #* 2 - Special characters removed
    b1 <- chartr(
      paste(names(unwanted_array), collapse=''),
      paste(unwanted_array, collapse=''),
      as.vector(tolower(x)))
  }
  
  
  ##### Setup: List #####
  if (Listing == TRUE) {
    # List as is
    b0 <- unlist(x, use.names=FALSE)
    # List with:
    #* 1 - Lower case, 
    #* 2 - Special characters removed
    b1 <- chartr(
      paste(names(unwanted_array), collapse=''),
      paste(unwanted_array, collapse=''),
      as.vector(tolower(unlist(x, use.names=FALSE))))
  }
  
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
    else if (max(jarowinkler(a0, b0)) > JW.threshold_step1){
      if (Listing == FALSE) {
        new_data[i, new_column] <- b0[which.max(jarowinkler(a0, b0))]
      }
      
      if (Listing == TRUE){
        # Step 1: Getting which list elements have the thing
        step1 <- lapply(x, function(y) 
          grep(b0[which.max(jarowinkler(a0, b0))], y)
        ) 
        
        # Step 2: Indexing our List Name
        step2 <- sapply(step1, function(y) 
          length(y) > 0)
        
        # Step 3: Assigning
        new_data[i,new_column] <- names(x[which(step2)])
      }
      
      # Counter:
      counter.categorization_step1 <- 
        counter.categorization_step1 + 1
    }
     
    
    # Step 2
    else if (max(jarowinkler(a1, b1)) > JW.threshold_step2){
      
      if (Listing == FALSE) {
        new_data[i, new_column] <- b0[which.max(jarowinkler(a1, b1))]
      }
      
      if (Listing == TRUE) {
        # Step 1: Getting which list elements have the thing
        step1 <- lapply(x, function(y) 
          grep(b0[which.max(jarowinkler(a1, b1))], y)
        )
        
        # Step 2: Indexing our List Name
        step2 <- sapply(step1, function(y) 
          length(y) > 0)
        
        # Step 3: Assigning
        new_data[i,new_column] <- names(x[which(step2)])
      }
      
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
  cat("Jaro-winkler Distance Threshold (Step 1):", JW.threshold_step1, "\n")
  cat("Jaro-winkler Distance Threshold (Step 2):", JW.threshold_step2, "\n")
  cat("=====================")
  return(new_data)
}





########## Recode Text 2 ##########

recode.text.list <- function (
    data,
    column, 
    List, 
    Excluding.Vector = NULL,
    JW.threshold_List = 0.85,
    JW.threshold_Excluding = 0.85) {
  
  ##### Counters #####
  counter.exclusion <- 0
  counter.categorization <- 0
  counter.uncategorized <- 0
  counter.NA <- 0
  
  
  ##### Setup: New Column #####
  new_column <- paste(
    column, 
    "recode", sep="_")
  data[, new_column] <- NA
  
  ##### Setup: Vector #####
  b <- as.vector(tolower(unlist(x, use.names=FALSE)))
  # c <- as.vector(tolower(Excluding.Vector))
  
  ##### Loop #####
  for (i in 1:nrow(data)) {
    
    # Setup inside the for loop:
    a <- tolower(data[i, column])
    
    # NAs
    if (is.na(a)) {
      data[i, new_column] <- NA
      counter.NA <- counter.NA + 1
    } 
    
    # Excluding Vector
    else if (!is.null(Excluding.Vector) & 
             max(RecordLinkage::jarowinkler(a, c)) > JW.threshold_Excluding){
      data[i, new_column] <- NA
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
      data[i,new_column] <- names(List[which(step2)])
      
      counter.categorization <- counter.categorization + 1
    }
    
    # Rest of the answers
    else {
      data[i,new_column] <- NA
      
      counter.uncategorized <- counter.uncategorized + 1
    }
  }
  return(data)
  
  ##### Output #####
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

########## Converting Dataframe ##########
###* Converting the Data Frame Variables
## 
convert.dataframe <- function(data) {
  
  ##### Factor Columns #####
  factor.columns <- 
    names(data)[which(sapply(data, is.factor))]
  
  ##### Loop #####
  for (names in names(data)) {
    
    if (any(names %in% factor.columns)) {
      data[names] <- 
        labelled::to_factor(data[[names]])
    }
    
    else {
      data[names] <- 
        as.numeric(data[[names]])
    }
  }
  return(data)
}

##### Reliability #####

# Either putting a vector of many scales
# or list containing different dimensions of a scale

investigate.r <- function(
    data,
    list,
    many.scales = FALSE) {
  
  ##### Setup #####
  # Data Frame
  reliable <- as.data.frame(matrix(
    nrow = length(list),
    ncol = 3))
  colnames(reliable) <- c(
    "Scale","Alpha","Spearman_Brown")
  
  
  ##### Loop #####
  for (i in 1:length(list)) {
    
    # Setup
    ##### Vector #####
    if (many.scales == TRUE) {
      x <- eval(
        parse(text = as.character(list[i])))
      # Naming
      reliable[i, "Scale"] <- 
        paste(names(list)[i])
    }
    
    ##### List #####
    else {
      x <- names(
        data[
          ,as.vector(unlist(list[[i]]))])
      # Naming
      reliable[i, "Scale"] <- 
        paste(names(list[i]))
    }
    
    # N = 1
    if (length(x) == 1) {
      NULL
    } 
    
    ##### Reliability #####
    # SPEARMAN-BROWN
    else if (length(x) == 2) {
      reliable[i, "Spearman_Brown"] <- 
        round(splithalfr::spearman_brown(
          as.vector(na.omit(data[x[1]])),
          as.vector(na.omit(data[x[2]]))),3)
    } 
    
    # ALPHA
    else {
      reliable[i, "Alpha"] <- 
        round(psych::alpha(
          data[,x]
        )[["total"]][["raw_alpha"]],3)
    }
  }
  return(knitr::kable(reliable) %>% 
           kableExtra::kable_styling())
}



##### Formula #####
create.formula <- function(
    list,
    elements = 1:length(list)) {
  
  ##### Setup #####
  formulize <- formula()
  
  ##### Loop #####
  for (i in 1:length(elements)) {
    formulize[i] <- paste(
      names(list[i]), 
      "=~", 
      paste0(list[[elements[i]]], 
             collapse = " + "))
  }
  return(paste(formulize, collapse = " \n "))
  
}


########## Loadings Plot ##########
loading.plot <- function(
    data, 
    model) {
  
  pairs(model$loadings, 
        col = 1:ncol(data), 
        upper.panel = NULL, 
        main = "Factor Loadings")
  par(xpd = TRUE) 
  legend('topright', 
         bty = 'n',
         pch = 'o', 
         col = 1:ncol(behave), 
         attr(model$loadings, 'dimnames')[[1]], 
         title = "Variables",
         cex = 0.6)
}


########## Best Fit ##########
investigate.efa <- function(
    data, 
    from = 1, to = 5,
    rotation,
    items.named = FALSE){
  
  # Correlation Matrix
  corr_matrix <- stats::cor(data_fa)
  
  # Data Frame
  df <- as.data.frame(matrix(nrow = 4,
                             ncol = length(from:to)))
  colnames(df) <- paste(seq(from, to),"Factor")
  rownames(df) <- c("RMSEA","BIC","STATISTIC","PVAL")
  
  # Loop
  for (i in from:to){
    
    # Model Name
    model_name <- paste("Exploratory Model:", 
                        i, "Factors",
                        sep = " ")
    
    # Model
    EXPLORATORY <- psych::fa(
      r = corr_matrix,
      nfactor = i,
      n.obs = nrow(data),
      fm = 'ml', rotate = rotation)
    
    # Plotting
    load <- as.data.frame(
      loadings(EXPLORATORY)[, from:i])
    colnames(load) <- colnames(loadings(EXPLORATORY))
    
    if(items.named == FALSE) {
      item.names <- paste(
        "Item", 1:ncol(data),
        sep = "_")
      load$Item <- item.names
    } else if (items.named == TRUE) {
      load$Item <- colnames(data)
    }
    
    loadings.m <- reshape2::melt(
      load, 
      id = "Item", 
      measure = paste("ML", 
                      from:i, 
                      sep=""), 
      variable.name = "Factor", 
      value.name = "Loading")
    
    ##### Loading Plot 1 #####
    
    print(
      ggplot(loadings.m, aes(Item, abs(Loading), fill=Loading)) + 
        facet_wrap(~ Factor, nrow = 1) + # placing the factors in separate facets
        geom_bar(stat="identity") + # making the bars
        coord_flip() + #flip the axes so the test names can be horizontal  
        # define the fill color gradient: blue = positive, red = negative
        scale_fill_gradient2(name = "Loading", 
                             high = "blue", mid = "grey92", low = "red", 
                             midpoint = 0, guide = "none") +
        ylab("Loading Strength") + #improve y-axis label
        theme_bw(base_size = 10) + 
        labs(title = bquote("Loadings for" ~ bold(.(paste(model_name)))))
    )
    
    ##### Loading Plot 2 #####
    if(i != 1) {
      print(
        ggplot(loadings.m, 
               aes(Item, 
                   abs(Loading), 
                   fill = Factor)) + 
          geom_bar(stat = "identity") + coord_flip() + 
          ylab("Loading Strength") + theme_bw(base_size = 10) + 
          #remove labels and tweak margins for combining with the correlation matrix plot
          theme(axis.text.y = element_blank(), 
                axis.title.y = element_blank(), 
                plot.margin = unit(c(3,1,39,-3), "mm")) + 
          labs(title = bquote("Loadings for" ~ bold(.(paste(model_name)))))
      )
    }
    
    # The Data Frame
    if (is.null(EXPLORATORY[["RMSEA"]][1])) {
      warning(paste(i,"-Factor model does not make sense, returning NA column."))
    } else {
      df[1,i] <- EXPLORATORY[["RMSEA"]][1]
      df[2,i] <- EXPLORATORY[["BIC"]]
      df[3,i] <- EXPLORATORY[["STATISTIC"]]
      rownames(df)[3] <- "Chi_squared"
      df[4,i] <- EXPLORATORY[["PVAL"]]
      rownames(df)[4] <- "P_value"
    }
  }
  
  df.round <- data.frame(lapply(df, round, 3))
  rownames(df.round) <- rownames(df)
  colnames(df.round) <- colnames(df)
  return(knitr::kable(df) %>% 
           kableExtra::kable_styling())
}


########## CFA ##########

investigate.cfa <- function(
    data, 
    model, 
    models = FALSE) {
  
  if(models == FALSE) {
    # Name
    nam <- paste(
      "FITCFA", 
      deparse(substitute(model)), 
      sep = "_")
    # CFA
    assign(
      nam,
      lavaan::cfa(
        model = model, 
        data = data, 
        sample.nobs = nrow(data)),
      envir = parent.env(environment())
    )
  }
  
  else if (models == TRUE) {
    # Names
    nam <- character()
    for (i in 1:length(model)){
      nam[i] <- paste(
        "FITCFA", "model", i,
        sep = "_")
    }
    
    results <- sapply(
      model, function(model) {
        lavaan::cfa(
          model = model, 
          data = data, 
          sample.nobs = nrow(data))
      })
    names(results) <- nam
    return(results)
  }
}

########## Fix Labels ##########
fix.Qualtrics.labels <- function(data){
  
  list <- vector("list", 100)
  counter <- 1
    
    for (col_name in colnames(data)) {
      
      # For Assigning a Label
      x <- strsplit(attributes(
        data[[col_name]])$label," - ")[[1]][2]
      
      # For List
      y <- strsplit(attributes(
        data[[col_name]])$label," - ")[[1]][1]
      
      if (haven::is.labelled(data[[col_name]]) & 
          !is.na(x) & 
          x != "Selected Choice"){
        
        # Assigning
        attributes(data[[col_name]])$label <- x
        
        
        # List 
        names(list)[counter] <- col_name
        list[[counter]] <- y
        counter <- counter + 1
        
      }
      
    }
  list.2 <- vector("list",
                   length(unique(list))-1)
  
  for (i in 1:length(list.2)){
    names(list.2)[i] <- unique(list)[[i]]
    list.2[[i]] <- names(list[list==unique(list)[[i]]])
  }
  return(list(data,list.2))
}


##### give.label_mean #####
give.label_mean <- function(
    list,
    sub_scale = FALSE,
    sub_scale_index = NULL){
  
  if (sub_scale == TRUE & is.null(sub_scale_index)){
    stop("You have selected 'sub_scale = TRUE, but the 'sub_scale_index' is unspecified.")
  }
  paste()
  if (sub_scale == TRUE){
    # Scale Name 
    new_name <- paste(
      names(list)[sub_scale_index], 
      collapse = " & ")
    
    # Number of items 
    number_of_items <- vector()
    for (i in 1:length(sub_scale_index)){
      number_of_items[i] <- length(list[[sub_scale_index[i]]])
    }
    
    scale_name <- paste(
      new_name,
      " (",
      sum(number_of_items),
      " items aggregated)",
      sep = "")
  } 
  
  else {
    if (is.null(attributes(list)[["scale_name"]]) ){
      stop("Please assign attributes(.)$scale_name to your specified list.",
           "\n",
           "  Or, if you intend to name a sub-scale, specify 'sub_scale = TRUE'.")
    } else {
      scale_name <- paste(
        attributes(list)[["scale_name"]],
        " (",
        length(unlist(list, use.names=FALSE)),
        " items aggregated)",
        sep = "")
    }
  }
  return(scale_name)
}



