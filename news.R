library(tidyverse)
library(DataExplorer)
library(caret)
library(e1071)

#### Fake News

## load in data 
fake.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/train.csv")
fake.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/test.csv")

fake <- bind_rows(fake.test, fake.train)


## clean data
plot_missing(fake)

sum(is.na(fake.train$label))
