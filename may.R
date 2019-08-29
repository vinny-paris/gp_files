source("./pbdr_all_corrected.R")

dir <- "./prac_params"

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
set.seed(10)

 cat("\n \n \n", "read files", comm.rank(), "\n", "\n", "\n")   
  
  #split the files among the nodes
  if (comm.size() > 1) {
    files <- split(files , cut(seq_along(files),
                               comm.size()))
  } else  {
    files <- list(files)
  }

  h1 <- NULL 
  h2 <- NULL
  h3 <- NULL
  h4 <- NULL
  result <- NULL
  for (file in files[[comm.rank()+1]]) {
    data <- fread(paste0(dir, "/", file))
    holding <- grouping_wrapper(data = data, num_of_groups = num_of_groups, 
                            new_groups = new_groups, fractional_report = fractional_report, 
                            groups_interest = groups_interest, fields_interest = fields_interest)
       holding <- cbind(file, holding)
h1 <- rbind(h1, holding[[1]])
h2 <- rbind(h2, holding[[2]])
h3 <- rbind(h3, holding[[3]])
h4 <- rbind(h4, holding[[4]])


cat("\n", "\n", "\n", comm.rank(), file, "finished", "\n", "\n", "\n")
}


 cat("\n \n \n", "grouping done", comm.rank(), "\n", "\n", "\n")   
    
    pure <- data.frame(comm.rank(), h1)
    frac <- data.frame(comm.rank(), h2)
    class_frac <- data.frame(comm.rank(), h3)
    field_frac <- data.frame(comm.rank(), h4)
    barrier()
  


 cat("\n \n \n", "barrier done", comm.rank(), "\n", "\n", "\n")   



  
  

  #final <- list(pures, fracs, class_fracs, fields_fracs)
 # cat("\n", "\n", "length of final: ", length(final), "\n", "\n")


    barrier()
  
 cat("\n \n \n", "gather done", comm.rank(), "\n", "\n", "\n")   
#names(final) <- c("Full Factorial Experiment", "Fractional Factorial Experiment", "Class Fractional Factorial Experiment", "fields")
  
  #return the final product



pures <- as.list(gather(pure))
fracs <- as.list(gather(frac))
cf <- as.list(gather(class_frac))
ff <- as.list(gather(field_frac))




cat('gathered')

j <- do.call(rbind.data.frame, pures)
k <- do.call(rbind.data.frame, fracs)
l <- do.call(rbind.data.frame, cf)
m <- do.call(rbind.data.frame, ff)




if(comm.rank() == 0){
write.csv(j, "./output/full_exp.csv")
write.csv(k, "./output/frac_exp.csv")
write.csv(l, "./output/class_exp.csv")
write.csv(m, "./output/field_exp.csv")
}

#pures <- rbind(pures)

#comm.write.csv(pures, "pbdr_pure_orthogonal_exp.csv")

cat("\n", "\n", 'pures done', "\n", "\n")

barrier()

#comm.write.csv(final[[2]], "pbdr_fractional_var_exp.csv")

#comm.write.csv(final[[3]], "pbdr_fractional_class_exp.csv")

#comm.write.csv(final[[4]], "pbdr_fractional_field_exp.csv")

cat("\n", "\n", "finished!", "\n", "\n")

finalize()
