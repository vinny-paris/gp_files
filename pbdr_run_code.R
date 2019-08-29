<<<<<<< HEAD

source("./pbdr_all_functions.R")

library(data.table)
library(reshape2)
library(stringr)
library(cluster)
library(Matrix)
library(RcppEigen)
library(purrr)
library(pbdMPI)


dir <- './pk'

holding <- dir_grouping(dir = dir, num_of_groups = 10, 
                            new_groups = 6, fractional_report = 25, 
                            groups_interest = 6, fields_interest = 6)
=======
source("/Users/vinny/cades_upload/pbdr_all_functions.R")

dir <- read.csv('/Users/vinny/cades_upload/prac_params')

holding <- dir_grouping(dir = dir, num_of_groups = 10, 
                            new_groups = 6, fractional_report = 25, 
                            groups_interest = 6)
>>>>>>> 0701341ce5b84075c8c3e6dd62ff19567aaaccf1

write.csv(holding[[1]], "pbdr_pure_orthogonal_exp.csv")

write.csv(holding[[2]], "pbdr_fractional_var_exp.csv")

write.csv(holding[[3]], "pbdr_fractional_class_exp.csv")

<<<<<<< HEAD
write.csv(holding[[4]], "pbdr_fractional_field_exp.csv")



finalize()
=======

>>>>>>> 0701341ce5b84075c8c3e6dd62ff19567aaaccf1

