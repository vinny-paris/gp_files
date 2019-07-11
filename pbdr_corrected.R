source("/Users/vinny/cades_upload/pbdr_all_functions.R")

dir <- read.csv('/Users/vinny/cades_upload/prac_params')


dir <-  dir
num_of_groups <- 12 
new_groups <- 6 
fractional_report <- 25 
groups_interest <- 6
fields_interest <- 6
  
  files <- list.files(dir)
  
  
  #split the files among the nodes
  if (comm.size() > 1) {
    files <- split(files , cut(seq_along(files),
                               comm.size()))
  } else  {
    files <- list(files)
  }
  
  result <- NULL
  for (file in files[[comm.rank()+1]]) {
    data <- fread(paste0(dir, "/", file))
    out <- grouping_wrapper(data = data, num_of_groups = num_of_groups, 
                            new_groups = new_groups, fractional_report = fractional_report, 
                            groups_interest = groups_interest, fields_interest = fields_interest)
    
    
    
    pure <- data.frame(comm.rank(), file ,out[[1]])
    frac <- data.frame(file ,out[[2]])
    class_frac <- data.frame(file ,out[[3]])
    field_frac <- data.frame(file, out[[4]])
    
  }
  if(comm.rank() == 0){
  pures <- gather(pure)
  
  fracs <- gather(frac)
  
  class_fracs <- gather(class_frac)
  fields_fracs <- gather(fields_frac)
  
  final <- list(pures, fracs, class_fracs, fields_fracs)
  names(final) <- c("Full Factorial Experiment", "Fractional Factorial Experiment", "Class Fractional Factorial Experiment")
  
  #return the final product




write.csv(holding[[1]], "pbdr_pure_orthogonal_exp.csv")

write.csv(holding[[2]], "pbdr_fractional_var_exp.csv")

write.csv(holding[[3]], "pbdr_fractional_class_exp.csv")

write.csv(holding[[4]], "pbdr_fractional_field_exp.csv")

}
