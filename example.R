library(data.table)
library(reshape2)
library(cluster)
library(stringr)
library(RcppEigen)
library(Matrix)
library(stringr)
library(purrr)

data <- read.csv("/Users/vinny/cades_upload/data_update.csv", header = TRUE)
design <- read.csv("/Users/vinny/cades_upload/design_matrix.csv", header = TRUE)
data <- data[,-c(13,14)]
data <- data[,-c(11)]


file <- "/Users/vinny/cades_upload/data_update.csv"
k <- grouping_wrapper(file)
j <- grouping_wrapper(data = data)






