rm(list = ls())
data <- read.csv("LUBS5990M_courseworkData_202425.csv")
library(writexl)

library(corrgram)
library(faraway)
library(forecast)
library(ggplot2)
library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tm)
library(wordcloud)
library(corrplot)
library("VIM")
library("mice")

#dATA CLEANNING
ico_data <- read_csv("LUBS5990M_courseworkData_202425.csv")
ico_data <- ico_data %>% filter(!(is.na(country) | country == 'Available' | country == "Unknown"))
ico_data1 <- ico_data
mean(is.na(data)) * 100

ico_data$success <- as.factor(ico_data$success)
ico_data$ico_start <- dmy(ico_data$ico_start)
ico_data$ico_end <- dmy(ico_data$ico_end)
ico_data$days <- abs(as.numeric(difftime(ico_data$ico_end, ico_data$ico_start, units="days")))
ico_data$days[is.na(ico_data$days)] <-NA

ico_data$pre_ico_start <- dmy(ico_data$pre_ico_start)
ico_data$pre_ico_end <- dmy(ico_data$pre_ico_end)
ico_data$days_pre <- abs(as.numeric(difftime(ico_data$pre_ico_end, ico_data$pre_ico_start, units="days")))
ico_data$days_pre[is.na(ico_data$days_pre)] <-NA
ico_data1 <- ico_data

hist(ico_data$token_for_sale)
hist(ico_data$sold_tokens)
hist(ico_data$distributed_in_ico)
hist(ico_data$rating)

unique(ico_data$whitelist)
unique(data$mvp)

unique(data$whitelist)
ico_data$whitelist <- case_when(
  toupper(ico_data$whitelist) == "YES" ~ 1,
  toupper(ico_data$whitelist) == "NO" ~ 0,
  TRUE ~ NA_real_
)
unique(ico_data$whitelist)
ico_data1 <- ico_data
ico_data$distributed_in_ico <- ifelse(ico_data$distributed_in_ico > 1, NA, as.numeric(ico_data$distributed_in_ico))
summary(ico_data)
hist(ico_data$distributed_in_ico)

ico_data <- ico_data %>%
  mutate(
    has_white_paper = ifelse(!is.na(link_white_paper) & link_white_paper != "", 1, 0),
    has_github       = ifelse(!is.na(github_link) & github_link != "", 1, 0),
    has_website      = ifelse(!is.na(website) & website != "", 1, 0),
    has_linkedin     = ifelse(!is.na(linkedin_link) & linkedin_link != "", 1, 0)
  )
ico_data <- ico_data %>% select(-link_white_paper, -github_link, -linkedin_link, -website)


unique(ico_data$mvp)
ico_data$mvp <- ifelse(!is.na(ico_data$mvp) & ico_data$mvp == "Available", 1, 0)
unique(ico_data$mvp)

summary(ico_data)

#AJUSTAR USD_USD

Price_non_numeric <- data %>%
  filter(!grepl("^[0-9\\.eE\\-]+$", gsub(".*=\\s*", "", price_usd))) %>%  # Filtrar no numéricos
  mutate(
    currency = gsub(".*\\s([A-Z]+)$", "\\1", price_usd),  # Extraer moneda
    currency = ifelse(currency == price_usd, NA, currency) # Asignar NA si no hay moneda
  ) %>%
  filter(!is.na(currency))

unique_price_cripto <- unique(Price_non_numeric$currency)
unique_price_cripto
exchange_rates <- data.frame(
  unique_price_cripto,
  rate_to_usd = c(1800, 1.1, 0.09, 1.24, 1.3, 28000, 0.05, 0.02, 0.67, 0.01, 
                  0.00076, 0.065, 0.003, 0.74, 0.0001, 0.02, 0.5, 2.0) # Ajusta las tasas reales
)

# Normalizar la tabla de tasas de cambio
exchange_rates <- exchange_rates %>%
  rename(currency = unique_price_cripto) %>% # Asegurar que la columna se llame 'currency'
  mutate(currency = toupper(trimws(currency))) # Normalizar: mayúsculas y eliminar espacios

exchange_rates



### WORLD CLOUD
# 1. Limpiar los datos: juntar todo y separar países
text_data <- ico_data$restricted_areas %>%
  na.omit() %>%                    # quitar NA
 # paste(collapse = ",") %>%        # unir todos los textos separados por coma
  strsplit(split = ",") %>%        # separar por coma
  unlist() %>%                     # convertir a vector
  trimws() %>%                     # quitar espacios
  toupper()                        # convertir a mayúsculas (para unificar)

# 2. Contar frecuencia de cada país
freq <- table(text_data)
freq <- sort(freq, decreasing = TRUE)

# 3. Crear el wordcloud
set.seed(123)
wordcloud(words = names(freq),
          freq = freq,
          min.freq = 2,
          max.words = 50,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)




# 1. Limpiar los datos: juntar todo y separar países
text_data1 <- ico_data$accepting %>%
  na.omit() %>%                    # quitar NA
 # paste(collapse = ",") %>%        # unir todos los textos separados por coma
  strsplit(split = ",") %>%        # separar por coma
  unlist() %>%                     # convertir a vector
  trimws() %>%                     # quitar espacios
  toupper()                        # convertir a mayúsculas (para unificar)

# 2. Contar frecuencia de cada país
freq1 <- table(text_data1)
freq1 <- sort(freq1, decreasing = TRUE)

# 3. Crear el wordcloud
set.seed(123)
wordcloud(words = names(freq1),
          freq = freq,
          min.freq = 2,
          max.words = 50,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)

# 1. Limpiar los datos: juntar todo y separar países
text_data2 <- ico_data$country %>%
  na.omit() %>%                    # quitar NA
  # paste(collapse = ",") %>%        # unir todos los textos separados por coma
  strsplit(split = ",") %>%        # separar por coma
  unlist() %>%                     # convertir a vector
  trimws() %>%                     # quitar espacios
  toupper()                        # convertir a mayúsculas (para unificar)

# 2. Contar frecuencia de cada país
freq2 <- table(text_data2)
freq2 <- sort(freq2, decreasing = TRUE)

# 3. Crear el wordcloud
set.seed(123)
wordcloud(words = names(freq2),
          freq = freq,
          min.freq = 2,
          max.words = 50,
          scale = c(3, 0.5),
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE)

ico_data <- ico_data %>%
  mutate(
    restricted_top5 = ifelse(
      str_detect(toupper(restricted_areas), "USA|UNITED STATES|UK|UNITED KINGDOM|SINGAPORE|RUSSIA|ESTONIA"),
      1, 0
    )
  )

ico_data <- ico_data %>%
  mutate(
    accepting_ETH = ifelse(str_detect(toupper(accepting), "ETH"), 1, 0))

exchange_rates
unique(toupper(str_extract(ico_data$min_investment, "[A-Z]{2,4}$")))



unique(ico_data$country)
# Crear columna de clasificación
ico_data <- ico_data %>%
  mutate(
    country_USA = ifelse(
      str_detect(toupper(country), "USA|UNITED STATES"),
      1, 0
    )
  )


ico_data <- ico_data %>% select(-accepting, -restricted_areas,-country)


price_comparison <- data %>%
  select(pre_ico_price_usd, price_usd)

print(exchange_rates)


#minvest

ico_data1 <- ico_data1 %>%
  mutate(
    min_invest_clean = toupper(trimws(min_investment)),
    min_invest_clean = str_replace_all(min_invest_clean, ",", "")
  )

## para dolares
ico_data1 <- ico_data1 %>%
  mutate(
    usd_value = case_when(
      # Extrae número antes de "USD", ignorando si hay "PRE" antes
      str_detect(min_invest_clean, "(?<!PRE[-\\s]?)\\d+(\\.\\d+)?\\s*USD") ~
        as.numeric(str_extract(min_invest_clean, "(?<!PRE[-\\s]?)\\d+(\\.\\d+)?(?=\\s*USD)")),
      
      # Extrae número después de "$", ignorando si está cerca de "PRE"
      str_detect(min_invest_clean, "(?<!PRE[-\\s]?)\\$\\d+(\\.\\d+)?") ~
        as.numeric(str_extract(min_invest_clean, "(?<=\\$)\\d+(\\.\\d+)?")),
      
      TRUE ~ NA_real_
    )
  )
ico_data1$min_invest_usd <- ico_data1$usd_value
ico_data1$source_currency <- ifelse(!is.na(ico_data1$usd_value), "USD", NA)

## resto de monedas

valid_currencies <- exchange_rates$currency[exchange_rates$currency != "USD"]



ico_data <- ico_data %>% select(-ico_start, -ico_end, -pre_ico_start, -pre_ico_end)
head(ico_data)

missing_percent <- colMeans(is.na(ico_data)) * 100

# Convertir a data frame ordenado
missing_df <- data.frame(
  Column = names(missing_percent),
  Missing_Percent = round(missing_percent, 2)
) %>%
  arrange(desc(Missing_Percent))

# Ver el resultado
print(missing_df)

ico_data <- ico_data %>%
  mutate(
    success = case_when(
      as.character(success) == "Y" ~ 1,
      as.character(success) == "N" ~ 0,
      TRUE ~ NA_real_
    )
  )

total_validos <- ico_data %>% 
  filter(!is.na(whitelist)) %>% 
  nrow()

# Filas que violan la regla: whitelist == 1 pero kyc != 1
violaciones <- ico_data %>% 
  filter(!is.na(whitelist) & whitelist == 1 & kyc != 1) %>% 
  nrow()

# Calcular el porcentaje de violaciones
porcentaje_violaciones <- round((violaciones / total_validos) * 100, 2)

# Mostrar resultado
cat("Porcentaje de violaciones de la regla (whitelist=1 pero kyc≠1):", porcentaje_violaciones, "%\n")

ico_data <- ico_data %>% select(-whitelist,-sold_tokens, -pre_ico_price_usd, -min_investment, -restricted_top5, -days_pre)

missing_percent <- colMeans(is.na(ico_data)) * 100

missing_df <- data.frame(
  Column = names(missing_percent),
  Missing_Percent = round(missing_percent, 2)
) %>%
  arrange(desc(Missing_Percent))

# Ver el resultado
print(missing_df)

  corrgram(ico_data)
  ico_numeric <- ico_data %>% select(where(is.numeric))
  
  # Corrgram mejorado
  corr_matrix <- cor(ico_numeric, use = "pairwise.complete.obs")
  
  # Gráfico limpio y profesional
  corrplot(corr_matrix,
           method = "color",
           type = "upper",
           tl.cex = 0.8,           # tamaño del texto
           number.cex = 0.7,       # tamaño de los números
           addCoef.col = "black",  # mostrar coeficientes
           col = colorRampPalette(c("red", "white", "blue"))(100),
           tl.col = "black")
  aggr(ico_data, numbers=TRUE, prop=FALSE)
  

  
  ##############33
  

  
  
  ###########quitar atipicos
  
  hist(ico_data$distributed_in_ico)
  hist(ico_data$token_for_sale)
  ico_data %>% arrange(desc(token_for_sale)) %>% print(n = 20)
  
  top2_indices <- order(ico_data$token_for_sale, decreasing = TRUE)[1:2]
  
  ico_data <- ico_data[-top2_indices, ]
  options(scipen = 999)
  hist(ico_data$token_for_sale)
  
  hist(ico_data$teamsize)
  
  ico_data %>% arrange(desc(teamsize)) %>% print(n = 20)
  
  hist(ico_data$days)
  ico_data %>% arrange(desc(days)) %>% print(n = 20)
  top3_indices <- order(ico_data$days, decreasing = TRUE)[1:3]
  ico_data <- ico_data[-top3_indices, ]
  hist(ico_data$days)
  
  hist(ico_data$rating)
  
  
  ### cambiar Price usd ########
  ico_data <- ico_data %>%
    mutate(
      # 1. Limpiar el texto original
      price_usd_clean = toupper(trimws(as.character(price_usd))),
      
      # 2. Extraer el valor numérico
      value = suppressWarnings(case_when(
        grepl("=.*[0-9\\.eE\\-]+", price_usd_clean) ~ as.numeric(gsub(".*=\\s*([0-9\\.eE\\-]+).*", "\\1", price_usd_clean)),
        grepl("^[0-9\\.eE\\-]+$", price_usd_clean) ~ as.numeric(price_usd_clean),
        grepl("^[0-9\\.eE\\-]+\\s+[A-Z]{2,6}$", price_usd_clean) ~ as.numeric(str_extract(price_usd_clean, "^[0-9\\.eE\\-]+")),
        TRUE ~ NA_real_
      )),
      
      # 3. Extraer la moneda
      currency = case_when(
        grepl("([A-Z]{2,6})$", price_usd_clean) ~ str_extract(price_usd_clean, "([A-Z]{2,6})$"),
        TRUE ~ NA_character_
      ),
      
      currency = toupper(trimws(currency)),
      
      # 4. Buscar tasa de cambio
      rate_to_usd = exchange_rates$rate_to_usd[match(currency, exchange_rates$currency)],
      
      # 5. Calcular el precio final en USD
      Price_usd2 = case_when(
        !is.na(value) & !is.na(rate_to_usd) ~ value * rate_to_usd,
        !is.na(value) & is.na(currency) ~ value,  # asumimos USD
        TRUE ~ NA_real_
      ),
      
      # 6. Fuente de conversión
      source_currency_price = case_when(
        !is.na(rate_to_usd) ~ currency,
        is.na(currency) & !is.na(value) ~ "USD",
        TRUE ~ NA_character_
      )
    )
  
  
  ico_data <- ico_data %>% select(-price_usd, -price_usd_clean, -value, -currency, -rate_to_usd, -source_currency_price)
  
  ico_data <- ico_data %>%
    rename(price_usd = Price_usd2)
  
  
  ############333
  hist(ico_data$price_usd)
  
  
  
  
  
  
  ico_data %>%
    arrange(desc(price_usd)) %>%
    select(price_usd) %>%
    slice_head(n = 20)
  top4_indices <- order(ico_data$price_usd, decreasing = TRUE)[1:3]
    ico_data <- ico_data[-top4_indices, ]
    hist(ico_data$price_usd)
    
  
    ico_data <- ico_data %>% select(-price_usd)
    
  # Imputación múltiple
    
    # Selección de variables
    vars_para_imputar <- ico_data %>%
      select(distributed_in_ico, teamsize, token_for_sale, days,
             accepting_ETH, ERC20, rating)
    
    vars_para_imputar <- vars_para_imputar %>%
      mutate(
        accepting_ETH = as.factor(accepting_ETH),
        ERC20 = as.factor(ERC20)
      )
    # Asignar métodos personalizados
    metodos <- make.method(vars_para_imputar)
    metodos["distributed_in_ico"] <- "cart"
    metodos["teamsize"] <- "cart"
    metodos["token_for_sale"] <- "pmm"
    metodos["days"] <- "cart"
    metodos["accepting_ETH"] <- "logreg"
    metodos["ERC20"] <- "logreg"
    metodos["rating"] <- "cart"

    
    
  imi <- mice(vars_para_imputar, method = metodos, m = 5, seed = 123)
  
  
 
  
  imputado <- complete(imi, action = 1)
  imputado$accepting_ETH <- as.numeric(imputado$accepting_ETH) - 1
  imputado$ERC20 <- as.numeric(imputado$ERC20) - 1
  ico_data_final <- ico_data  # hacemos una copia para mantener limpio el original
  ico_data_final[names(imputado)] <- imputado  
  sapply(ico_data_final[names(imputado)], function(x) sum(is.na(x)))
  ico_data_final$accepting_ETH <- as.numeric(as.character(ico_data_final$accepting_ETH))
  ico_data_final$ERC20 <- as.numeric(as.character(ico_data_final$ERC20))
  head(ico_data_final)
  
  #write_xlsx(ico_data_final, path = "ico_data_final.xlsx")  
  
  
 
  
  
  ##############333## Modelo KNN
  ##############################################
  
  library(caret)
  library(gmodels)
  library(class) 
  library(pROC)
  # Función de normalización Min-Max
  normalize <- function(x) {
    return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE)))
  }
  
  # Preparación de datos
  ico_data_final$success <- factor(ico_data_final$success, levels = c(1, 0), labels = c("Yes", "No"))
  knn_data <- ico_data_final
  vars_continuas <- c("distributed_in_ico", "token_for_sale", "teamsize", "days", "rating")
  knn_data[vars_continuas] <- lapply(knn_data[vars_continuas], normalize)
  
  # Partición de datos
  set.seed(123)
  index <- createDataPartition(knn_data$success, p = 0.9, list = FALSE)
  train_data <- knn_data[index, ]
  test_data <- knn_data[-index, ]
  
  # Control de entrenamiento para CV
  set.seed(123)
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
    # Valores de K
    k_values <- data.frame(k = seq(1, 51, 2))
    
    # Modelo KNN
    set.seed(123)
    knn_model <- train(
      success ~ ., 
      data = train_data,
      method = "knn",
      trControl = ctrl,
      tuneGrid = k_values,
      metric = "ROC"
    )
    
    # Mejor K
    cat("\n✅ Mejor valor de K:", knn_model$bestTune$k, "\n")
    
    ggplot(knn_model$results, aes(x = k, y = ROC)) +
      geom_line() +
      geom_point() +
      labs(
        title = "KNN Model Performance (ROC AUC vs. K)",
        x = "Number of Neighbours (K)",
        y = "ROC AUC Score"
      ) +
      theme_minimal()
    
    # Evaluación en test set
    final_pred <- predict(knn_model, newdata = test_data)
    prob_pred <- predict(knn_model, newdata = test_data, type = "prob")
  
  # Matriz de confusión y métricas
  conf_mat <- confusionMatrix(final_pred, test_data$success, positive = "Yes")
  print(conf_mat)
  
  # CrossTable personalizada
  CrossTable(x = test_data$success, y = final_pred,
             prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
             dnn = c("Actual", "Predicted"))
  
  # ROC y AUC
  roc_obj <- roc(response = test_data$success, predictor = prob_pred$Yes, levels = c("No", "Yes"))
  plot(roc_obj, col = "blue", main = "ROC Curve - KNN", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "blue")
  
  # Métricas manuales+
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
  
  
  
  ################ver distribuccion
  ##############################################

    library(ggplot2)
  
  numeric_vars <- c("distributed_in_ico", "teamsize", "token_for_sale", "days", "rating")
  
  # Histograma con curva normal superpuesta
  # Loop para generar histogramas con curva normal
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
  
  
  ################v naive bayes
  ##############################################
  nb_data <- ico_data_final
  table(nb_data$success)
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
    
    # Matriz de confusión
    conf_mat <- confusionMatrix(final_pred, test_data_nb$success, positive = "Yes")
    print(conf_mat)
    
    # CrossTable personalizada
    CrossTable(x = test_data_nb$success, y = final_pred,
               prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
               dnn = c("Actual", "Predicted"))
    
    # ROC y AUC
    roc_obj <- roc(response = test_data_nb$success, predictor = prob_pred$Yes, levels = c("No", "Yes"))
    plot(roc_obj, col = "darkorange", main = "ROC Curve - Naive Bayes", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "darkorange")
    
    # Métricas
    accuracy <- conf_mat$overall['Accuracy']
    precision <- conf_mat$byClass['Pos Pred Value']
    recall <- conf_mat$byClass['Sensitivity']
    f1 <- conf_mat$byClass['F1']
    auc_val <- auc(roc_obj)
    
    cat("\n📊 Evaluación en el Test Set (Naive Bayes):\n")
    cat("Accuracy:", round(accuracy, 4), "\n")
    cat("Precision:", round(precision, 4), "\n")
    cat("Recall:", round(recall, 4), "\n")
    cat("F1-Score:", round(f1, 4), "\n")
    cat("AUC:", round(auc_val, 4), "\n")
  
  #con laplace ###################33
    ########################
    
    set.seed(123)
    nb_model_lap <- train(
      success ~ ., 
      data = train_data_nb,
      method = "naive_bayes",  # usa el paquete 'naivebayes'
      trControl = ctrl,
      metric = "ROC",
      tuneGrid = expand.grid(laplace = 1, usekernel = FALSE, adjust = 1)
    )
    # Predicción en test
    final_pred_lap <- predict(nb_model_lap, newdata = test_data_nb)
    prob_pred_lap <- predict(nb_model_lap, newdata = test_data_nb, type = "prob")
    
    # Matriz de confusión
    conf_mat_lap <- confusionMatrix(final_pred_lap, test_data_nb$success, positive = "Yes")
    print(conf_mat_lap)
    
    # CrossTable personalizada
    CrossTable(x = test_data_nb$success, y = final_pred_lap,
               prop.chisq = FALSE, prop.r = TRUE, prop.t = TRUE,
               dnn = c("Actual", "Predicted"))
    
    # ROC y AUC
    roc_obj_lap <- roc(response = test_data_nb$success, predictor = prob_pred_lap$Yes, levels = c("No", "Yes"))
    plot(roc_obj_lap, col = "darkorange", main = "ROC Curve - Naive Bayes Laplace", legacy.axes = TRUE, print.auc = TRUE, print.auc.col = "darkorange")
    
    # Métricas
    accuracy_lp <- conf_mat_lap$overall['Accuracy']
    precision_lp <- conf_mat_lap$byClass['Pos Pred Value']
    recall_lp <- conf_mat_lap$byClass['Sensitivity']
    f1_lp <- conf_mat_lap$byClass['F1']
    auc_val_lp <- auc(roc_obj_lap)
    
    cat("\n📊 Evaluación en el Test Set (Naive Bayes LP):\n")
    cat("Accuracy:", round(accuracy_lp, 4), "\n")
    cat("Precision:", round(precision_lp, 4), "\n")
    cat("Recall:", round(recall_lp, 4), "\n")
    cat("F1-Score:", round(f1_lp, 4), "\n")
    cat("AUC:", round(auc_val_lp, 4), "\n")
  ###########################3
  ############ DECISION THREE
  
  
  library(C50)
  library(caret)
  library(pROC)
  library(gmodels)
  
  # Dataset y preparación
  data_tree <- ico_data_final
  data_tree$success <- factor(data_tree$success, levels = c(0,1), labels = c("No", "Yes"))
  
  set.seed(123)
  index <- createDataPartition(data_tree$success, p = 0.9, list = FALSE)
  train_data <- data_tree[index, ]
  test_data <- data_tree[-index, ]
  
  # Configurar 10-fold cross-validation
  ctrl <- trainControl(
    method = "cv",
    number = 10,
    classProbs = TRUE,
    summaryFunction = twoClassSummary,
    savePredictions = "final"
  )
  
  # Entrenar modelo C5.0 sin boosting (trials = 1)
  set.seed(123)
  tree_model_noboost <- train(
    success ~ ., 
    data = train_data,
    method = "C5.0",
    trControl = ctrl,
    tuneGrid = expand.grid(model = "tree", trials = 1, winnow = FALSE),
    metric = "ROC"
  )
  
  # Predicción y evaluación
  pred_noboost <- predict(tree_model_noboost, newdata = test_data)
  prob_noboost <- predict(tree_model_noboost, newdata = test_data, type = "prob")
  
  conf_mat_noboost <- confusionMatrix(pred_noboost, test_data$success, positive = "Yes")
  roc_noboost <- roc(response = test_data$success, predictor = prob_noboost[,"Yes"])
  
  cat("\n🔍 Árbol de decisión SIN Boosting:\n")
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


  # Print detailed model information
 

  
 set.seed(123)
 tree_model_direct <- C5.0(
   success ~ ., 
   data = train_data,
   trials = 1,
   control = C5.0Control(winnow = FALSE)
 )
 
 # Graficar el árbol
                # Reset to default text size
 
 summary(tree_model_direct)



 library(caret)
 imp <- varImp(tree_model_noboost)
 
 # Mostrar los valores
 print(imp)

  
 imp_df <- data.frame(
   Variable = rownames(imp$importance),
   Importance = imp$importance$Overall
 )
 
 # Ordenar de mayor a menor
 imp_df <- imp_df[order(imp_df$Importance, decreasing = TRUE), ]
 
 # Gráfico
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
  
  #### BOOSTING
  
  # Entrenar modelo con boosting (trials = 20 or ejemplo)
  set.seed(123)
  tree_model_boost <- train(
    success ~ ., 
    data = train_data,
    method = "C5.0",
    trControl = ctrl,
    tuneGrid = expand.grid(model = "tree", trials = 15, winnow = FALSE),
    metric = "ROC"
  )
  
  # Predicción y evaluación
  pred_boost <- predict(tree_model_boost, newdata = test_data, type = "raw")
  prob_boost <- predict(tree_model_boost, newdata = test_data, type = "prob")
  
  conf_mat_boost <- confusionMatrix(pred_boost, test_data$success, positive = "Yes")
  roc_boost <- roc(response = test_data$success, predictor = prob_boost[,"Yes"])
  
  cat("\n⚡ Árbol de decisión CON Boosting:\n")
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
  
  # Graficar el árbol
  # Reset to default text size
  
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
  
  # Ordenar de mayor a menor
  imp2_df <- imp2_df[order(imp2_df$Importance, decreasing = TRUE), ]
  
  # Gráfico
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
  
  
  
  # Combinar dos curvas ROC en un mismo plot
  # Dibujar la primera curva ROC (sin boosting)
  # Graficar la primera curva ROC (No Boost)
  plot(roc_noboost, 
       col = "red", 
       main = "ROC Curve - Decision Tree With and Without Boosting", 
       legacy.axes = TRUE, 
       percent = FALSE, 
  )
  
  # Añadir la segunda curva (Boost)
  lines(roc_boost, col = "blue",      legacy.axes = TRUE, 
        percent = TRUE, 
        print.auc = TRUE, 
        print.auc.col = "blue")
  # Sin percent aquí, solo la curva
  
  # Añadir líneas de la grilla
  abline(h = seq(0, 100, by = 20), v = seq(0, 100, by = 20), col = "gray90", lty = 2)
  
  # Añadir AUCs manualmente
  text(60, 30, labels = paste("AUC No Boost:", round(auc(roc_noboost), 3)), col = "red")
  text(70, 15, labels = paste("AUC Boost:", round(auc(roc_boost), 3)), col = "blue")
  
  # Añadir leyenda
  legend("bottomright",
         legend = c("No Boosting", "With Boosting"),
         col = c("red", "blue"),
         lwd = 2)
  
  #### RAMDOM FORESTT ######################3
  #############################################3
  
  
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
  
  # Opcional: añadir texto manual extra si querés
  text(50, 30, labels = paste("AUC:", round(auc(roc_rf), 3)), col = "forestgreen")
  
  cat("\n🔵 AUC Random Forest:", auc(roc_rf), "\n")
  


  cat("\n⚡ Árbol de decisión CON RANDONM FOREST:")
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
  
  # Graficar el árbol
  # Reset to default text size
  
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
  
  # Ordenar de mayor a menor
  imp3_df <- imp3_df[order(imp3_df$Importance, decreasing = TRUE), ]
  imp3_df
  # Gráfico
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
  
  
