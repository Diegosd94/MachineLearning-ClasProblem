# -----------------------------------
# 1. LIBRARIES & DATA LOADING
# -----------------------------------
rm(list = ls())
library(tidyverse)
library(lubridate)
library(corrgram)
library(corrplot)
library(VIM)
library(mice)
library(caret)
library(ggplot2)
library(wordcloud)
library(pROC)
library(gmodels)
library(C50)
library(randomForest)
library(writexl)

# Load data
data <- read.csv("LUBS5990M_courseworkData_202425.csv")
ico_data <- read_csv("LUBS5990M_courseworkData_202425.csv")

# -----------------------------------
# 2. DATA CLEANING & PREPROCESSING
# -----------------------------------

# Remove invalid country entries
ico_data <- ico_data %>% filter(!(is.na(country) | country == 'Available' | country == "Unknown"))
ico_data$success <- as.factor(ico_data$success)

# Date parsing and duration calculation
ico_data <- ico_data %>%
  mutate(
    ico_start = dmy(ico_start),
    ico_end = dmy(ico_end),
    days = as.numeric(difftime(ico_end, ico_start, units = "days")),
    pre_ico_start = dmy(pre_ico_start),
    pre_ico_end = dmy(pre_ico_end),
    days_pre = as.numeric(difftime(pre_ico_end, pre_ico_start, units = "days"))
  )

# Clean 'whitelist' column
ico_data$whitelist <- case_when(
  toupper(ico_data$whitelist) == "YES" ~ 1,
  toupper(ico_data$whitelist) == "NO" ~ 0,
  TRUE ~ NA_real_
)

# Adjust values > 1 in 'distributed_in_ico'
ico_data$distributed_in_ico <- ifelse(ico_data$distributed_in_ico > 1, NA, ico_data$distributed_in_ico)

# Create binary indicators for online presence
ico_data <- ico_data %>%
  mutate(
    has_white_paper = ifelse(!is.na(link_white_paper) & link_white_paper != "", 1, 0),
    has_github = ifelse(!is.na(github_link) & github_link != "", 1, 0),
    has_website = ifelse(!is.na(website) & website != "", 1, 0),
    has_linkedin = ifelse(!is.na(linkedin_link) & linkedin_link != "", 1, 0)
  ) %>%
  select(-link_white_paper, -github_link, -website, -linkedin_link)

# Clean MVP column
ico_data$mvp <- ifelse(!is.na(ico_data$mvp) & ico_data$mvp == "Available", 1, 0)

# -----------------------------------
# 3. CURRENCY CONVERSION SETUP
# -----------------------------------

# Identify price entries with non-USD currencies
Price_non_numeric <- data %>%
  filter(!grepl("^[0-9\.eE\-]+$", gsub(".*=\\s*", "", price_usd))) %>%
  mutate(
    currency = gsub(".*\\s([A-Z]+)$", "\\1", price_usd),
    currency = ifelse(currency == price_usd, NA, currency)
  ) %>%
  filter(!is.na(currency))

# Define exchange rates (example values)
exchange_rates <- data.frame(
  currency = toupper(trimws(unique(Price_non_numeric$currency))),
  rate_to_usd = c(1800, 1.1, 0.09, 1.24, 1.3, 28000, 0.05, 0.02, 0.67, 0.01, 0.00076, 0.065, 0.003, 0.74, 0.0001, 0.02, 0.5, 2.0)
)

# -----------------------------------
# 4. TEXT ANALYSIS: WORDCLOUDS
# -----------------------------------

# Function for wordcloud generation
create_wordcloud <- function(text_column, min_freq = 2, max_words = 50) {
  text_data <- text_column %>%
    na.omit() %>%
    strsplit(",") %>%
    unlist() %>%
    trimws() %>%
    toupper()
  freq <- sort(table(text_data), decreasing = TRUE)
  set.seed(123)
  wordcloud(names(freq), freq, min.freq = min_freq, max.words = max_words, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), random.order = FALSE)
}

create_wordcloud(ico_data$restricted_areas)
create_wordcloud(ico_data$accepting)
create_wordcloud(ico_data$country)

# -----------------------------------
# 5. FEATURE ENGINEERING
# -----------------------------------

ico_data <- ico_data %>%
  mutate(
    restricted_top5 = ifelse(str_detect(toupper(restricted_areas), "USA|UNITED STATES|UK|UNITED KINGDOM|SINGAPORE|RUSSIA|ESTONIA"), 1, 0),
    accepting_ETH = ifelse(str_detect(toupper(accepting), "ETH"), 1, 0),
    country_USA = ifelse(str_detect(toupper(country), "USA|UNITED STATES"), 1, 0)
  ) %>%
  select(-accepting, -restricted_areas, -country)

# -----------------------------------
# 6. CLEAN PRICE FIELDS
# -----------------------------------

ico_data <- ico_data %>%
  mutate(
    price_usd_clean = toupper(trimws(as.character(price_usd))),
    value = suppressWarnings(case_when(
      grepl("=.*[0-9\\.eE\\-]+", price_usd_clean) ~ as.numeric(gsub(".*=\\s*([0-9\\.eE\\-]+).*", "\\1", price_usd_clean)),
      grepl("^[0-9\\.eE\\-]+$", price_usd_clean) ~ as.numeric(price_usd_clean),
      grepl("^[0-9\\.eE\\-]+\\s+[A-Z]{2,6}$", price_usd_clean) ~ as.numeric(str_extract(price_usd_clean, "^[0-9\\.eE\\-]+")),
      TRUE ~ NA_real_
    )),
    currency = str_extract(price_usd_clean, "([A-Z]{2,6})$"),
    rate_to_usd = exchange_rates$rate_to_usd[match(currency, exchange_rates$currency)],
    price_usd = case_when(
      !is.na(value) & !is.na(rate_to_usd) ~ value * rate_to_usd,
      !is.na(value) & is.na(currency) ~ value,
      TRUE ~ NA_real_
    )
  ) %>%
  select(-price_usd_clean, -value, -currency, -rate_to_usd)

# -----------------------------------
# 7. OUTLIER REMOVAL
# -----------------------------------

ico_data <- ico_data %>%
  filter(!token_for_sale %in% sort(token_for_sale, decreasing = TRUE)[1:2],
         !days %in% sort(days, decreasing = TRUE)[1:3],
         !price_usd %in% sort(price_usd, decreasing = TRUE)[1:3]) %>%
  select(-price_usd)

# -----------------------------------
# 8. MULTIPLE IMPUTATION (MICE)
# -----------------------------------

vars_para_imputar <- ico_data %>%
  select(distributed_in_ico, teamsize, token_for_sale, days, accepting_ETH, ERC20, rating) %>%
  mutate(across(c(accepting_ETH, ERC20), as.factor))

metodos <- make.method(vars_para_imputar)
metodos[c("distributed_in_ico", "teamsize", "days", "rating")] <- "cart"
metodos[c("token_for_sale")] <- "pmm"
metodos[c("accepting_ETH", "ERC20")] <- "logreg"

imi <- mice(vars_para_imputar, method = metodos, m = 5, seed = 123)
imputado <- complete(imi, 1)
imputado$accepting_ETH <- as.numeric(as.character(imputado$accepting_ETH))
imputado$ERC20 <- as.numeric(as.character(imputado$ERC20))
ico_data_final <- ico_data
ico_data_final[names(imputado)] <- imputado

# -----------------------------------
# 9. MODELING: KNN, NAIVE BAYES, DECISION TREE, RANDOM FOREST
# -----------------------------------

  # Prepare data
ico_data_final$success <- factor(ico_data_final$success, levels = c(1, 0), labels = c("Yes", "No"))

# Normalize numeric variables for KNN
vars_continuas <- c("distributed_in_ico", "token_for_sale", "teamsize", "days", "rating")
normalize <- function(x) (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE))
ico_data_final[vars_continuas] <- lapply(ico_data_final[vars_continuas], normalize)

# Partition the data
set.seed(123)
index <- createDataPartition(ico_data_final$success, p = 0.9, list = FALSE)
train_data <- ico_data_final[index, ]
test_data <- ico_data_final[-index, ]

# Cross-validation setup
ctrl <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary, savePredictions = "final")

  
  
# ---- KNN MODEL ----
  
  library(caret)
  library(gmodels)
  library(class) 
  library(pROC)

  
k_values <- data.frame(k = seq(1, 51, 2))
set.seed(123)
knn_model <- train(success ~ ., data = train_data, method = "knn", trControl = ctrl, tuneGrid = k_values, metric = "ROC")
final_pred <- predict(knn_model, newdata = test_data)
prob_pred <- predict(knn_model, newdata = test_data, type = "prob")
roc_obj <- roc(response = test_data$success, predictor = prob_pred$Yes, levels = c("No", "Yes"))
    
    # Best K
    cat("\n✅ Best value for K:", knn_model$bestTune$k, "\n")
    
    ggplot(knn_model$results, aes(x = k, y = ROC)) +
      geom_line() +
      geom_point() +
      labs(
        title = "KNN Model Performance (ROC AUC vs. K)",
        x = "Number of Neighbours (K)",
        y = "ROC AUC Score"
      ) +
      theme_minimal()
    
    #  test 
    final_pred <- predict(knn_model, newdata = test_data)
    prob_pred <- predict(knn_model, newdata = test_data, type = "prob")
  
  # confusionMatrix
  conf_mat <- confusionMatrix(final_pred, test_data$success, positive = "Yes")
  print(conf_mat)
  
  # CrossTable 
  CrossTable(x = test_data$success, y = final_pred,
             prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
             dnn = c("Actual", "Predicted"))
  
  # ROC y AUC
  roc_obj <- roc(response = test_data$success, predictor = prob_pred$Yes, levels = c("No", "Yes"))
  plot(roc_obj, col = "blue", main = "ROC Curve - KNN", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "blue")
  
  # Manual Metrics+
  accuracy <- conf_mat$overall['Accuracy']
  precision <- conf_mat$byClass['Pos Pred Value']
  recall <- conf_mat$byClass['Sensitivity']
  f1 <- conf_mat$byClass['F1']
  auc_val <- auc(roc_obj)
  
  cat("\n\U0001F4CA Evaluación en el Test Set:\n")
  cat("Accuracy:", round(accuracy, 4), "\n")
  cat("Precision:", round(precision, 4), "\n")
  cat("Recall:", round(recall, 4), "\n")
  cat("F1-Score:", round(f1, 4), "\n")
  cat("AUC:", round(auc_val, 4), "\n")
  
  
  
  ################ Check distribution   ##############################################

    library(ggplot2)
  
  numeric_vars <- c("distributed_in_ico", "teamsize", "token_for_sale", "days", "rating")
  
  # Histogram  
  for (var in numeric_vars) {
    p <- ggplot(ico_data_final, aes(x = .data[[var]])) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
      stat_function(fun = dnorm,
                    args = list(mean = mean(ico_data_final[[var]], na.rm = TRUE),
                                sd = sd(ico_data_final[[var]], na.rm = TRUE)),
                    color = "red", linewidth = 1) +
      labs(title = paste("Distribution of", var),
           x = var, y = "Density") +
      theme_minimal()
    print(p)
  }
  
  
# ---- NAIVE BAYES (w/o and w/ Laplace) ----

  nb_data <- ico_data_final
  table(nb_data$success)

#Binning
  nb_data$teamsize <- cut(nb_data$teamsize, breaks = 4, labels = FALSE)
  nb_data$token_for_sale <- cut(nb_data$token_for_sale, breaks = 2, labels = FALSE)
  nb_data$days <- cut(nb_data$days, breaks = 3, labels = FALSE)
  nb_data$teamsize <- as.factor(nb_data$teamsize)
  nb_data$token_for_sale <- as.factor(nb_data$token_for_sale)
  nb_data$days <- as.factor(nb_data$days)
  set.seed(123)
  index <- createDataPartition(nb_data$success, p = 0.9, list = FALSE)
  train_data_nb <- nb_data[index, ]
  test_data_nb <- nb_data[-index, ]
  



  
  set.seed(123)
    nb_model_nolap <- train(
    success ~ ., 
    data = train_data_nb,
    method = "naive_bayes",  # usa el paquete 'naivebayes'
    trControl = ctrl,
    metric = "ROC",
    tuneGrid = expand.grid(laplace = 0, usekernel = FALSE, adjust = 1)
  )
    # Predicción en test
    final_pred <- predict(nb_model_nolap, newdata = test_data_nb)
    prob_pred <- predict(nb_model_nolap, newdata = test_data_nb, type = "prob")
    
    # confusionMatrix
    conf_mat <- confusionMatrix(final_pred, test_data_nb$success, positive = "Yes")
    print(conf_mat)
    
    # CrossTable 
    CrossTable(x = test_data_nb$success, y = final_pred,
               prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
               dnn = c("Actual", "Predicted"))
    
    # ROC y AUC
    roc_obj <- roc(response = test_data_nb$success, predictor = prob_pred$Yes, levels = c("No", "Yes"))
    plot(roc_obj, col = "darkorange", main = "ROC Curve - Naive Bayes", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "darkorange")
    
    # Metrics
    accuracy <- conf_mat$overall['Accuracy']
    precision <- conf_mat$byClass['Pos Pred Value']
    recall <- conf_mat$byClass['Sensitivity']
    f1 <- conf_mat$byClass['F1']
    auc_val <- auc(roc_obj)
    
    cat("\n📊 Test Set (Naive Bayes):\n")
    cat("Accuracy:", round(accuracy, 4), "\n")
    cat("Precision:", round(precision, 4), "\n")
    cat("Recall:", round(recall, 4), "\n")
    cat("F1-Score:", round(f1, 4), "\n")
    cat("AUC:", round(auc_val, 4), "\n")
  
  #W  laplace 
    
    set.seed(123)
    nb_model_lap <- train(
      success ~ ., 
      data = train_data_nb,
      method = "naive_bayes",  # usa el paquete 'naivebayes'
      trControl = ctrl,
      metric = "ROC",
      tuneGrid = expand.grid(laplace = 1, usekernel = FALSE, adjust = 1)
    )
    # Pred test
    final_pred_lap <- predict(nb_model_lap, newdata = test_data_nb)
    prob_pred_lap <- predict(nb_model_lap, newdata = test_data_nb, type = "prob")
    
    # confusionMatrix
    conf_mat_lap <- confusionMatrix(final_pred_lap, test_data_nb$success, positive = "Yes")
    print(conf_mat_lap)
    
    # CrossTable 
    CrossTable(x = test_data_nb$success, y = final_pred_lap,
               prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
               dnn = c("Actual", "Predicted"))
    
    # ROC y AUC
    roc_obj_lap <- roc(response = test_data_nb$success, predictor = prob_pred_lap$Yes, levels = c("No", "Yes"))
    plot(roc_obj_lap, col = "darkorange", main = "ROC Curve - Naive Bayes Laplace", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "darkorange")
    
    # Metrics
    accuracy_lp <- conf_mat_lap$overall['Accuracy']
    precision_lp <- conf_mat_lap$byClass['Pos Pred Value']
    recall_lp <- conf_mat_lap$byClass['Sensitivity']
    f1_lp <- conf_mat_lap$byClass['F1']
    auc_val_lp <- auc(roc_obj_lap)
    
    cat("\n📊 Test Set (Naive Bayes LP):\n")
    cat("Accuracy:", round(accuracy_lp, 4), "\n")
    cat("Precision:", round(precision_lp, 4), "\n")
    cat("Recall:", round(recall_lp, 4), "\n")
    cat("F1-Score:", round(f1_lp, 4), "\n")
    cat("AUC:", round(auc_val_lp, 4), "\n")


# ---- DECISION TREE ----
  
  
  library(C50)
  library(caret)
  library(pROC)
  library(gmodels)
  
  data_tree <- ico_data_final
  data_tree$success <- factor(data_tree$success, levels = c(0,1), labels = c("No", "Yes"))
  
  set.seed(123)
  index <- createDataPartition(data_tree$success, p = 0.9, list = FALSE)
  train_data <- data_tree[index, ]
  test_data <- data_tree[-index, ]
  
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  #  C5.0  (trials = 1)
  set.seed(123)
  tree_model_noboost <- train(
    success ~ ., 
    data = train_data,
    method = "C5.0",
    trControl = ctrl,
    tuneGrid = expand.grid(model = "tree", trials = 1, winnow = FALSE),
    metric = "ROC"
  )
  
  pred_noboost <- predict(tree_model_noboost, newdata = test_data)
  prob_noboost <- predict(tree_model_noboost, newdata = test_data, type = "prob")
  
  conf_mat_noboost <- confusionMatrix(pred_noboost, test_data$success, positive = "Yes")
  roc_noboost <- roc(response = test_data$success, predictor = prob_noboost[,"Yes"])
  
  cat("\n🔍 Decision Tree W Boosting:\n")
  print(conf_mat_noboost)
  cat("🔵 AUC:", auc(roc_noboost), "\n")
  
  plot(roc_noboost, col = "red", main = "ROC Curve - Decision Tree SIN Boosting", legacy.axes = TRUE, percent = TRUE, print.auc = TRUE, print.auc.col = "red")
  text(50, 30, labels = paste("AUC:", round(auc(roc_noboost), 3)), col = "red")
  
  CrossTable(x = test_data$success, y = pred_noboost,
             prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
             dnn = c("Actual", "Predicted"))
  
  cat("Accuracy:", conf_mat_noboost$overall["Accuracy"], "\n")
  cat("Precision:", conf_mat_noboost$byClass["Precision"], "\n")
  cat("Recall:", conf_mat_noboost$byClass["Recall"], "\n")
  cat("F1 Score:", conf_mat_noboost$byClass["F1"], "\n")

  
  library(C50)
  library(rpart)
  library(rpart.plot)



  
 set.seed(123)
 tree_model_direct <- C5.0(
   success ~ ., 
   data = train_data,
   trials = 1,
   control = C5.0Control(winnow = FALSE)
 )
 
 # Graph Tree
                # Reset to default text size
 
 summary(tree_model_direct)



 library(caret)
 imp <- varImp(tree_model_noboost)
 
 print(imp)

  
 imp_df <- data.frame(
   Variable = rownames(imp$importance),
   Importance = imp$importance$Overall
 )
 
 imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
 
 # Graph Attribute usage
 ggplot(imp_df, aes(x = reorder(Variable, Importance), y = Importance)) +
   geom_bar(stat = "identity", fill = "steelblue") +
   coord_flip() +
   labs(
     title = "Attribute usage - Decision Tree C5.0",
     x = "Attribute",
     y = "usage (Overall) %"
   ) +
   theme_minimal() +
   theme(
     plot.title = element_text(size = 14, face = "bold"),
     axis.text.y = element_text(size = 10)
   )
  
  #### W / BOOSTING
  
  # Model (trials = 20 or ejemplo)
  set.seed(123)
  tree_model_boost <- train(
    success ~ ., 
    data = train_data,
    method = "C5.0",
    trControl = ctrl,
    tuneGrid = expand.grid(model = "tree", trials = 15, winnow = FALSE),
    metric = "ROC"
  )
  
  # Ppredict
  pred_boost <- predict(tree_model_boost, newdata = test_data, type = "raw")
  prob_boost <- predict(tree_model_boost, newdata = test_data, type = "prob")
  
  conf_mat_boost <- confusionMatrix(pred_boost, test_data$success, positive = "Yes")
  roc_boost <- roc(response = test_data$success, predictor = prob_boost[,"Yes"])
  
  cat("\n⚡ Tree W Boosting:\n")
  print(conf_mat_boost)
  cat("🔵 AUC:", auc(roc_boost), "\n")
  par(mar = c(1, 1, 1, 1))  # Reduce margins (bottom, left, top, right)
  par(cex = 0.8)           # Reduce text size
  
  plot(roc_boost, col = "blue", main = "ROC Curve - Decision Tree CON Boosting", legacy.axes = TRUE, percent = TRUE, print.auc = TRUE, print.auc.col = "blue")
  text(50, 30, labels = paste("AUC:", round(auc(roc_boost), 3)), col = "blue")



  CrossTable(x = test_data$success, y = pred_boost,
             prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
             dnn = c("Actual", "Predicted"))
  
  cat("Accuracy:", conf_mat_boost$overall["Accuracy"], "\n")
  cat("Precision:", conf_mat_boost$byClass["Precision"], "\n")
  cat("Recall:", conf_mat_boost$byClass["Recall"], "\n")
  cat("F1 Score:", conf_mat_boost$byClass["F1"], "\n")
  
  
  set.seed(123)
  tree_model_boost_direct <- C5.0(
    success ~ ., 
    data = train_data,
    trials = 15,  # Boosting activado
    control = C5.0Control(winnow = FALSE)
  )
  
  # Graph Tree
  
tree_model_boost$results
  summary(tree_model_boost_direct)
  tree_model_boost_direct
  par(mar = c(1, 1, 1, 1))  # Reduce margins (bottom, left, top, right)
  par(cex = 0.2)          # Reduce text size
  plot(tree_model_boost_direct)
  plot(tree_model_boost)
  
  par(mar = c(5.1, 4.1, 4.1, 2.1))  # Reset to default margins
  par(cex = 1)                       # Reset to default text size
  # Mostrar los valores
  imp2<- varImp(tree_model_boost)
  
  print(imp2)
  
  
  imp2_df <- data.frame(
    Variable = rownames(imp2$importance),
    Importance = imp2$importance$Overall
  )
  

  imp2_df <- imp2_df[order(imp2_df$Importance, decreasing = TRUE), ]
  
  # Graph Attribute usage
  ggplot(imp2_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Attribute usage - C5.0 boosting",
      x = "Attribute",
      y = "usage (Overall) %"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 10)
    )
  
   
  
# ---- RANDOM FOREST ----

  
  library(randomForest)
  
  set.seed(123)
  rf_model <- train(
    success ~ ., 
    data = train_data,
    method = "rf",
    trControl = ctrl,
    tuneLength = 5,  # Probar 5 valores de mtry
    metric = "ROC"
  )
  
  print(rf_model)
  pred_rf <- predict(rf_model, newdata = test_data)
  prob_rf <- predict(rf_model, newdata = test_data, type = "prob")  
  
  conf_mat_rf <- confusionMatrix(pred_rf, test_data$success, positive = "Yes")
  roc_rf <- roc(response = test_data$success, predictor = prob_rf$Yes)
  par(mar = c(5.1, 4.1, 4.1, 2.1))  # Reset to default margins
  par(cex = 1)                       # Reset to default text size
  
  plot(roc_rf, 
       col = "forestgreen", 
       main = "ROC Curve - Random Forest", 
       legacy.axes = TRUE, 
       print.auc = TRUE, 
       print.auc.col = "forestgreen")
  

  text(50, 30, labels = paste("AUC:", round(auc(roc_rf), 3)), col = "forestgreen")
  
  cat("\n🔵 AUC Random Forest:", auc(roc_rf), "\n")
  


  cat("\n⚡ Decision Tree RANDONM FOREST:")
  print(conf_mat_rf)
  
  cat("🔵 AUC:", auc(roc_rf), "\n")
  
  par(mar = c(1, 1, 1, 1))  # Reduce margins (bottom, left, top, right)
  par(cex = 0.8)           # Reduce text size
  
  
  
  
  CrossTable(x = test_data$success, y = pred_rf,
             prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
             dnn = c("Actual", "Predicted"))
  
  cat("Accuracy:", conf_mat_rf$overall["Accuracy"], "\n")
  cat("Precision:", conf_mat_rf$byClass["Precision"], "\n")
  cat("Recall:", conf_mat_rf$byClass["Recall"], "\n")
  cat("F1 Score:", conf_mat_rf$byClass["F1"], "\n")
  
  par(mar = c(5.1, 4.1, 4.1, 2.1))  # Reset to default margins
  par(cex = 1)                       # Reset to default text size
  
  # Graph Tree
  rf_model$results
  getTree(rf_model$finalModel, k = 1, labelVar = TRUE)
  par(mar = c(1, 1, 1, 1))  # Reduce margins (bottom, left, top, right)
  par(cex = 0.2)          # Reduce text size
  plot(rf_model)
  par(mar = c(5.1, 4.1, 4.1, 2.1))  # Reset to default margins
  par(cex = 1)                       # Reset to default text size
  # Mostrar los valores
  imp3<- varImp(rf_model)
  
  print(imp3)
  
  
  imp3_df <- data.frame(
    Variable = rownames(imp3$importance),
    Importance = imp3$importance$Overall
  )
  

  imp3_df <- imp3_df[order(imp3_df$Importance, decreasing = TRUE), ]
  
# Graph Attribute usage
  ggplot(imp3_df, aes(x = reorder(Variable, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(
      title = "Attribute usage - Ranmdom Forest",
      x = "Attribute ",
      y = "usage (Overall) %"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      axis.text.y = element_text(size = 10)
    )
  rf_model$finalModel$ntree
  
  
