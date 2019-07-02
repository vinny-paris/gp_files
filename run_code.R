source("/Users/vinny/cades_upload/all_functions.R")

data <- read.csv(csv, header = TRUE)

holding <- grouping_wrapper(data = data, num_of_groups = 10, 
                            new_groups = 6, fractional_report = 25, 
                            groups_interest = 6)

write.csv(holding[[1]], "pure_orthogonal_exp.csv")

write.csv(holding[[2]], "fractional_var_exp.csv")

write.csv(holding[[3]], "fractional_class_exp.csv")



