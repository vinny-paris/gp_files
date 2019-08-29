library(data.table)
library(reshape2)
library(cluster)
library(stringr)
library(RcppEigen)
library(Matrix)
library(stringr)
library(purrr)

data <- read.csv(".cades_files/data_update.csv", header = TRUE)
data <- data[,-c(13,14)]
data <- data[,-c(11)]


j <- grouping_wrapper(data = data)






