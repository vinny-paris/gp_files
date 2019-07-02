



make_design <- function(data_summary){
  groups <- data_summary$Group
  
  
  grp_size <- length(unique(groups))
  
  #make the 0/1 factorial design
  fact <- balloon(grp_size)
  
  #expand the factorial design so every variable in the
  # same group has the same column
  exp_01_char <- fact[,groups]
  exp_01      <- apply(exp_01_char, 1:2, as.numeric)
  
  #transform the 0/1's to p20/p80's
  exp_0big <- sweep(exp_01, 2, data_summary$p80, "*")
  exp_lit1 <- sweep(abs(exp_01 - 1), 2, data_summary$p20, "*")
  exp_correct <- exp_0big + exp_lit1
  
  #name the cols
  namey <- apply(data_summary[,2:4], 1, paste, collapse = ",")
  colnames(exp_correct) <- namey
  
  return(exp_correct)
}





make_groups <- function(file = NULL, group_size = 10, data = NULL){
  
  if(length(data) == 0 & length(file) == 0) {stop("Please pass a file or
  a dataframe to this function")}
  
  #read in the file
  if(length(data) == 0){
    fil     <-  read.csv(file, header = TRUE)
  } else {fil <- data}
  
  rs      <- dim(fil)[1]
  #attach 20th and 80th percentiles
  fil$p20 <-  (fil$Maximum - fil$Minimum)*.2 + fil$Minimum 
  fil$p80 <-  (fil$Maximum - fil$Minimum)*.8 + fil$Minimum
  
  #combine and then break apart names
  Nam <- data.frame(apply( fil[ , 2:4 ] , 1 , paste , collapse = "_" ))
  
  #For every possible combination of rows, find word correlation, 
  #feild/class/boject matriches and if the 80th percentiles are 
  #the same
  matching <- jing(Nam, fil) 
  
  #give scores to each pair for "closeness" and organize it as a 
  #data frame
  cooky <- t(apply(matching, 1, points))
  colnames(cooky)[8] <- 'corr_points'
  cooky <- data.frame(cooky)
  lcook <- cooky[,c(1,2,8)]
  
  
  #prepare a similarity matrix for the clustering algorithm
  
  #assign to the diagnols the highest possible score
  diags <- data.frame(1:rs, 1:rs, 15)
  names(diags) <- c("i", "j", "corr_points")
  
  #Create a *similarity* matrix that is symetric
  #The + .001 is for an inversion in the next step
  ook <- rbind(lcook, diags)
  L <- dcast(ook, i ~ j, value.var = "corr_points")[,-1]
  L[is.na(L)] <- 0
  L <- L + t(L) + .001
  
  #Create a *disimilarity* matrix by inverting the above
  m <- 1/L
  
  #create groups and attach to the original data frame
  agnes(m, diss = TRUE) -> hope
  heep <- as.dendrogram(hope)
  sub_grp <- cutree(as.hclust(heep), k = min(dim(data)[1], group_size))
  fil$Group <- sub_grp
  
  #done
  return(fil)
}  

#variables match in that category, 0 if not. Lastly, param_match
#is 1 if the two variables have the same 80th percentile
jing <- function(nam, data){
  idx <- combn(1:dim(data)[1], 2)
  i <- idx[1,]
  j <- idx[2,]
  nam <- apply(data.frame(nam), 1, as.character)
  jaxon <- cbind(nam[i], nam[j])
  word_cors <-  apply(jaxon, 1, word_cor)
  param_match <- (data[i, 'p80'] == data[j, 'p80']) + 0
  class_match <- (data[i, 'Class'] == data[j, 'Class']) + 0
  object_match <- (data[i, 'Object'] == data[j, 'Object']) + 0
  field_match <- (data[i, 'Field'] == data[j, 'Field']) + 0
  bb  <- data.frame(i, j, word_cors, class_match, object_match, field_match, param_match)
  return(bb)
}



#Applies the points function to the above dataframe to create
#a rough metric for how closely related the pairs are.
#The metric is 3*word_cor + sum of class/object/field_match +
#6*param_match
points <- function(data){
  pnts <- 6*data[3] + sum(data[4:6]) + 6*data[7]
  data[8] <- pnts
  return(data)
}




#builds a full factorial design based on how many variables you
#passing to it (for us that is the number of groups)
balloon <- function(x) { 
  
  u <- unlist(base::strsplit(R.utils::intToBin(0:(2^x - 1)),
                             split = ""))
  
  v <- matrix(u, byrow = TRUE, ncol = x)
  
  return(v)
}





#finds the proportion of overlap between names of variables
word_cor <- function(j){
  x <- unlist(strsplit(as.character(j[1]), split = "[[:punct:][:space:]]+"))
  y <- unlist(strsplit(as.character(j[2]), split = "[[:punct:][:space:]]+"))
  sum(chmatch(x, y, nomatch = 0) != 0)/max(length(x), length(y))
}


#effects cancelling.


create_grouping_exp <- function(file = NULL, num_of_groups = 10, data = NULL){
  if(length(data) == 0 & length(file) == 0) {stop("Please pass a file or
  a dataframe to this function")}
  if(length(data) != 0 & length(file) != 0) {stop("Please pass only a single
  file or dataframe to this function")}
  
  exp_data <- make_groups(file = file, group_size = num_of_groups, data) 
  design_matrix <- make_design(exp_data)
  final <- list(exp_data, design_matrix)
  names(final) <- c("param_data", "design_matrix")
  return(final)
}




design_with_defaults <- function(data_summary, data){
  
  
  new_groups <- data_summary
  groups <- new_groups$Group
  grp_size <- length(unique(groups))
  
  #make the 0/1 factorial design
  fact <- balloon(grp_size)
  
  #expand the factorial design so every variable in the
  # same group has the same column
  exp_01_char <- fact[,groups]
  exp_01      <- apply(exp_01_char, 1:2, as.numeric)
  
  #transform the 0/1's to p20/p80's
  exp_0big <- sweep(exp_01, 2, data_summary$p80, "*")
  exp_lit1 <- sweep(abs(exp_01 - 1), 2, data_summary$p20, "*")
  exp_correct <- exp_0big + exp_lit1
  
  #name the cols
  namey <- apply(data_summary[,3:5], 1, paste, collapse = "_")
  colnames(exp_correct) <- namey
  
  d <- dim(exp_correct)[1]
  useless_groups <- subset(data, !data$Group %in% sig_groups)
  
  x <- dim(useless_groups)[1]
  y <- dim(exp_correct)[1]
  J_1 <- matrix(rep(1, x * y), nrow = y, ncol = x)
  def <- useless_groups$Default
  defin <- t(t(J_1) * def)
  other_namey <- apply(useless_groups[,3:5], 1, paste, collapse = "_")
  colnames(defin) <- other_namey
  
  
  expment <- data.frame(exp_correct, defin)
  
  return(expment)
}

grouping_wrapper <- function(data, num_of_groups = 10, new_groups = 6, fractional_report = 25, groups_interest = 6){
  
  #warning message
  if(num_of_groups <= new_groups){stop("The number of groups (num_of_groups)
  must be larger than the number of groups chosen for the next 
                                       iteration (new_groups)")}
  
  
  #create initial experiment's data and design_matrix
  exper <- create_grouping_exp(data = data, num_of_groups = num_of_groups)
  data  <- exper[[1]]
  design_matrix <- exper[[2]]
  
  #prepare the holding list and indexer
  final <- NULL
  i <- 1
  
  #loop to cut down the data
  while(dim(data)[1] > num_of_groups) {
    #response <- eplusr(design_matrix)
    response <- rnorm(dim(design_matrix)[1])
    g <- iterative_grouping(response, data, design_matrix, num_of_groups, new_groups)
    data <- g[[1]]
    design_matrix <- g[[2]]
    if(.75*fractional_report <= dim(data)[1] && dim(data)[1] <= 1.5*fractional_report){final[[2]] <- data}
    if(length(unique(data$Class)) > groups_interest){final[[3]] <- data}
    cat(paste("Current Iteration: ", i, "\n", sep = ""))
    i <- i + 1
  }
  {
    #collect first section
    final[[1]] <- data
    
    names(final) <- c("Proposal for Pure Grouping Orthogonal Experiment", "Proposal for Fractional Experiment", "Proposal for Large Class Fractional Experiment") 
    
    cat("Grouping Experiment Completed")
    return(final)
  }
}




iterative_grouping <- function(response, data, design_matrix, num_of_groups = 10, new_groups = 6){
  actual_design <- design_matrix[,which(apply(design_matrix, 2, fu) > 1)]
  r <- Lenth(response, design_matrix = actual_design, data = data, new_groups = new_groups)
  new_data <- grouping_it(data, sig_groups = r, num_of_groups = num_of_groups)
  return(new_data)
}

Lenth <- function(response, design_matrix, data, new_groups = 6) {
  ind <- order(data$Group)[!duplicated(sort(data$Group))] 
  des <- design_matrix[,ind]
  x10 <- apply(des, 2, ma)
  rownames(x10) <- NULL
  colnames(x10) <- NULL
  
  
  #build the full design matrix with interactions built in
  f <- as.matrix(x10[,1])
  colnames(f) <- LETTERS[1]
  for(i in 2:dim(x10)[2]){
    j  <- cbind(1, f)
    colnames(j)[1] <- ""
    cc <- colnames(j)
    f_t <- (j + x10[,i]) %% 2
    colnames(f_t) <- paste(cc, sep = "", LETTERS[i])
    f <- cbind(f, f_t)
  }
  
  
  #calculate the betas and use lenths to find the inital sig. betas
  b <- fastLmPure(f, response)$coefficients
  
  s_initial <- 3.75 * median(abs(b))
  
  b_star <- subset(b, abs(b) < s_initial)
  pse <- 1.5 * median(abs(b_star))
  sig <- subset(b, abs(b) > 2*pse)
  
  q <- choosey(sig, new_groups)
  
  #cut down group size to make sure each step is useful
  while(length(q) > new_groups){
    stop <- FALSE
    j <- sig[abs(sig) > min(abs(sig))]
    if(length(j) == 0){
      q <- choosey(sig[1], new_groups)
      stop <- TRUE
      break} 
    if(stop){break}
    sig <- sig[abs(sig) > min(abs(sig))]
    q <- choosey(sig, new_groups)
  }
  
  if(length(sig) == 0){
    q <- choosey(sig[abs(sig) == max(abs(sig))], new_groups)
  }
  
  #convert letters into groups
  r <- unique(grep(paste(q, collapse = "|"), LETTERS, value = FALSE))
  
  return(r)
}




grouping_it <- function(data, sig_groups = NULL, num_of_groups = 10){
  if(length(sig_groups) == 0) {
    sig_groups <- 1:max(data$Group)
  }
  new_groupss <- subset(data, data$Group %in% sig_groups)
  
  
  
  exp_data <- make_groups(group_size = num_of_groups, data = new_groupss) 
  design_matrix <- design_with_defaults(data_summary = exp_data, data = data) 
  
  new_groupss <- list(exp_data, design_matrix)
  
  names(new_groupss) <- c("param_data", "design_matrix")
  
  return(new_groupss)
}




ma <- function(x){
  ma <- max(x)
  mi <- min(x)
  mz <- x == ma
  mze <- x == mi
  x[mz] <- 1
  x[mze] <- 0
  return(x)
}


fu <- function(x) length(unique(x))

choosey <- function(sig, new_groups){
  k <- names(sig)
  l <- str_split(k, pattern = "")
  m <- map(l, length)
  n <- which(m < new_groups)
  ifelse(length(n) == 0, o <- l[1], o <- l[n])
  p <- map(o, function(x) x[-1])
  q <- unique(unlist(p))
  return(q)
}

balloon <- function(x) { 
  
  u <- unlist(base::strsplit(R.utils::intToBin(0:(2^x - 1)),
                             split = ""))
  
  v <- matrix(u, byrow = TRUE, ncol = x)
  
  return(v)
}

sm <- function(x){
  x[1:6, 1:6]
}
