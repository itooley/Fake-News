library(tidyverse)
library(DataExplorer)
library(caret)
install.packages("naniar")
library(naniar)

#### Fake News

## load in data 
fake.train <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/train.csv")
fake.test <- read_csv("/Users/graceedriggs/Documents/Stat 495/Fake News/fake-news/test.csv")

fake <- bind_rows(fake.test, fake.train)


## clean data
plot_missing(fake.train)

fake.train <- fake.train %>% replace_with_na(replace = list(author = 'nan'))

fakeTrainComplete<- na.omit(fake.train) 
plot_missing(fakeTrainComplete)
