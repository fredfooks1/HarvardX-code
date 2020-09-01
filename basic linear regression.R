#---------Basic Linear regression excercise-------------------------#
#---------completed as part of the havardx ds accreditiation--------#

#import packages
library(tidyverse)
library(HistData)
library(caret)

#select father and son heights
galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

head(galton_heights,10)

#use the caret package to create a train and test set
y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)


#if we ignored the fathers height and just gets the sons height
# we could use the average
av <- mean(train_set$son)
av
#calculated the squared loss:
av_loss <- mean((av - test_set$son)^2)
av_loss

# fit linear regression model
fit <- lm(son ~ father, data = train_set)
fit$coef

y_hat <- fit$coef[1] + fit$coef[2]*test_set$father
lin_loss <-mean((y_hat - test_set$son)^2)
lin_loss
1
