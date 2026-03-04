library(tidyverse)

read.table("FoodData.txt")

View("FoodData.txt")

file.info("FoodData.txt")

FoodData <- haven::read_xpt("FoodData.txt")
FoodData

str(FoodData)
names(FoodData)
nrow(FoodData)


fastfood <- read.csv("https://www.openintro.org/data/csv/fastfood.csv")

set.seed(123)
