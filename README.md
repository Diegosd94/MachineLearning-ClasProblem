# 🔍 ICO Success Prediction Project (R)

This R-based project aims to predict the success of Initial Coin Offerings (ICOs) by applying comprehensive data cleaning, imputation, and machine learning classification models. The dataset was provided as part of the **LUBS5990M** coursework.

## 🧠 Technologies & Libraries Used

- Language: R
- Key packages: `dplyr`, `lubridate`, `ggplot2`, `mice`, `caret`, `C50`, `randomForest`, `e1071`, `wordcloud`, `corrplot`, `tm`

## 📦 Project Workflow

### 1. Data Cleaning
- Parsing and transforming ICO/pre-ICO start and end dates
- Standardizing fields such as `whitelist`, `kyc`, `mvp`, and `price_usd`
- Currency normalization using manually defined exchange rates
- Detection and removal of extreme values and outliers

### 2. Visualization
- Histograms with overlaid normal distribution curves
- Wordclouds of countries for `restricted_areas`, `accepting`, and `country`
- Correlation matrices (`corrplot`, `corrgram`)

### 3. Missing Data Imputation
- Performed via `mice` with tailored methods:
  - `cart` for numeric
  - `logreg` for binary/factor
  - `pmm` for continuous with natural distributions

### 4. Predictive Modeling
The project evaluates and compares multiple classification algorithms:

- **K-Nearest Neighbors (KNN)**  
  Grid search over K values using 10-fold cross-validation

- **Naive Bayes**  
  With and without Laplace smoothing

- **Decision Trees (C5.0)**  
  Both with and without boosting (trials = 15)

- **Random Forest**  
  Tuned via `caret` and evaluated on test data

- **Support Vector Machines (SVM)**  
  Implemented using `e1071`, tuned with `caret`, evaluated with ROC and confusion matrix

### 5. Model Evaluation
All models were evaluated using the following metrics:

- Accuracy
- Precision
- Recall
- F1-Score
- ROC Curve and AUC

## 📊 Key Outputs
- ROC curves for all models side-by-side
- Feature importance analysis for tree-based methods
- Comparison of performance across all models, including boosted and non-boosted versions
