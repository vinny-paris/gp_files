source("./pbdr_all_corrected.R")

dir <- "./pk"

library(stringr)
library(data.table)
library(Matrix)
library(RcppEigen)
library(cluster)
library(reshape2)
library(purrr)
library(pbdMPI)


dir <-  dir
num_of_groups <- 12 
new_groups <- 6 
fractional_report <- 25 
groups_interest <- 6
fields_interest <- 6
  
  files <- list.files(dir)


  init()


 cat("\n \n \n", "read files", comm.rank(), "\n", "\n", "\n")   
  
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
    
 cat("\n \n \n", "grouping done", comm.rank(), "\n", "\n", "\n")   
    
    pure <- data.frame(comm.rank(), file ,out[[1]])
    frac <- data.frame(file ,out[[2]])
    class_frac <- data.frame(file ,out[[3]])
    field_frac <- data.frame(file, out[[4]])
    barrier()
  }


 cat("\n \n \n", "barrier done", comm.rank(), "\n", "\n", "\n")   


  if(comm.rank() == 0){

 cat("\n \n \n", comm.rank(), "\n", "\n", "\n")   
  pures <- gather(pure)
  cat(head(pures))
  fracs <- gather(frac)
  
  class_fracs <- gather(class_frac)
  fields_fracs <- gather(fields_frac)
  
  final <- list(pures, fracs, class_fracs, fields_fracs)
  names(final) <- c("Full Factorial Experiment", "Fractional Factorial Experiment", "Class Fractional Factorial Experiment")
  
  #return the final product

cat(names(final))


write.csv(final[[1]], "pbdr_pure_orthogonal_exp.csv")

write.csv(final[[2]], "pbdr_fractional_var_exp.csv")

write.csv(final[[3]], "pbdr_fractional_class_exp.csv")

#write.csv(final[[4]], "pbdr_fractional_field_exp.csv")

}


finalize()
