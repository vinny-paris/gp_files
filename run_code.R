t <- Sys.time()


source("./all_functions.R")

library(data.table)
library(reshape2)
library(cluster)
library(stringr)
library(RcppEigen)
library(Matrix)
library(purrr)

data <- read.csv("data_update.csv", header = TRUE)

holding <- grouping_wrapper(data = data, num_of_groups = 13, 
                            new_groups = 9, fractional_report = 25, 
                            groups_interest = 8)

write.csv(holding[[1]], "pure_orthogonal_exp.csv")

write.csv(holding[[2]], "fractional_var_exp.csv")

write.csv(holding[[3]], "fractional_class_exp.csv")

u <- Sys.time()

cat("\n", u - t)


