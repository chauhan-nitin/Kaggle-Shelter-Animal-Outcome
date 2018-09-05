# Kaggle_ShelterAnimalOutcome
Every year, approximately 7.6 million companion animals end up in US shelters. Many animals are given up as unwanted by their owners, while others are picked up after getting lost or taken out of cruelty situations. Many of these animals find forever families to take them home, but just as many are not so lucky. 2.7 million dogs and cats are euthanized in the US every year.

Using a dataset of intake information including breed, color, sex, and age from the Austin Animal Center, we're asking Kagglers to predict the outcome for each animal.

Submissions are evaluated using the multi-class logarithmic loss. Each incident has been labeled with one true class. For each animal, you must submit a set of predicted probabilities (one for every class). The formula is then,

logloss=−1N∑i=1N∑j=1Myijlog(pij),

where N is the number of animals in the test set, M is the number of outcomes, log is the natural logarithm, yij is 1 if observation i is in outcome j and 0 otherwise, and pij is the predicted probability that observation i belongs to outcome j.

In this repository, I've provided my approach towards the problem and how I've predicted the outcomes of various animals in the shelter.
