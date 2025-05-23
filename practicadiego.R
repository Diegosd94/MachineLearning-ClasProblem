rm(list = ls())
library("naivebayes")
library("tm")
library("SnowballC")
library("gmodels")

sms_raw <- read.csv ("sms_spam.csv", encoding = "UTF-8", stringsAsFactors = F)
sms_raw$type <- as.factor(sms_raw$type)
sms_raw$type <- factor(sms_raw$type)
table(sms_raw$type)
prop.table(table(sms_raw$type))*100
sms_corpus <- VCorpus(VectorSource(sms_raw$text))
inspect(sms_corpus[1:2])

sms_corpus_clean <- tm_map (sms_corpus,content_transformer(tolower))
sms_corpus_clean <- tm_map (sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map (sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map (sms_corpus_clean, removeWords, stopwords())
sms_corpus_clean <- tm_map (sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map (sms_corpus_clean, stripWhitespace)

inspect(sms_corpus_clean[1:2])
sms_dtm <- DocumentTermMatrix(sms_corpus_clean)

inspect(sms_dtm[1:5, 1:10])
ratio <- 0.75
p_index <- round(nrow(sms_dtm)*ratio)
sms_data_train <- sms_dtm[1:p_index,]
sms_data_test <- sms_dtm[(p_index+1):nrow(sms_dtm),]
sms_data_train_labels <- sms_raw[1:p_index,]$type
sms_data_test_labels <- sms_raw[(p_index+1):nrow(sms_raw),]$type


sms_freq_words <- findFreqTerms(sms_data_train,5)
sms_freq_words_train <- sms_data_train[,sms_freq_words]
sms_freq_words_test <- sms_data_test[,sms_freq_words]

convert_counts <- function(x) {
  x <- ifelse(x > 0, "Yes", "No")
}

sms_train <- apply(sms_freq_words_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_freq_words_test, MARGIN = 2, convert_counts)

sms_classifier <- naive_bayes(sms_train,sms_data_train_labels, laplace = 1)
sms_test_pred <- predict(sms_classifier,sms_test)

CrossTable(sms_test_pred,sms_data_test_labels,prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE, dnn = c('predicted','actual'))
