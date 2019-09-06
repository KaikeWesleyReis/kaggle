# Scania APS Analysis through Inference and Prediction Model Approach

**Overview**: The dataset consist of a challenge presented by **Scania** in a conference in 2016. There is some scientific work related as presented in **Conference References**. Nowadays this dataset is host in kaggle's website. This is a good dataset to work, because present several problems to solve:
- A high number of NA values
- High correlated variables
- Unbalanced label categories

**Problem Objective**: Develop a model capable to predict if a truck will present problems in **APS (Air Pressure System)**.

**Description**: For more information of how I managed to solve this challenge and why I decided this route to solve this problem read my notebook on *Kaggle*, link bellow. The code present here doesn't change in comparison to my notebook.

## Links
**Kaggle dataset**: [here](https://www.kaggle.com/uciml/aps-failure-at-scania-trucks-data-set)

**PS**: For this code I used the original dataset (not the 8-bit presented in kaggle website). Because of dimension, I did not upload it here, so acess the the link *Kaggle dataset* and download it!

**My Kaggle Notebook**: [here](https://www.kaggle.com/kaikewreis/scania-feature-selection-random-forest-xgboost)

## Results
### 1 - Preprocessing Phase :: Correlation Grid
![corrPlot](https://user-images.githubusercontent.com/32513366/63640000-47388000-c671-11e9-9327-dc41c1f08bdc.png)

### 2 - Preprocessing Phase :: PCA Variable Explained
![pca](https://user-images.githubusercontent.com/32513366/63640003-515a7e80-c671-11e9-9bd9-88872254d83a.PNG)


### 3 - Prediction info & Final Cost for Random Forest Model
![rfResults](https://user-images.githubusercontent.com/32513366/63640052-1f95e780-c672-11e9-9637-708ee1306784.PNG)

### 4 - Prediction info & Final Cost for XGBoost Model
![xgResults](https://user-images.githubusercontent.com/32513366/63640053-1f95e780-c672-11e9-9459-04c29b0c423d.PNG)

### 5 - Inference Analysis
![inf](https://user-images.githubusercontent.com/32513366/63640073-6a176400-c672-11e9-828d-e95b566e4210.PNG)

## Conference References
•	Camila Ferreira Costa, Mario A. Nascimento; Advances in Intelligent Data Analysis XV: 15th International Symposium – página 381; IDA 2016 Industrial Challenge: Using Machine Learning for Predicting Failures; 2016; 

•	Ezgi Can Ozan, Ekaterina Riabchenko, Serkan Kiranyaz, and Moncef Gabbouj; Advances in Intelligent Data Analysis XV: 15th International Symposium – página 381; An Optimized k-NN Approach for Classification on Imbalanced Datasets with Missing Data; 2016 ;

•	Vítor Cerqueira, Fábio Pinto, Claudio Sá, and Carlos Soares; Advances in Intelligent Data Analysis XV: 15th International Symposium – página 393; Combining Boosted Trees with Metafeature Engineering for Predictive Maintenance; 2016;

•	Christopher Gondek, Daniel Hafner, and Oliver R. Sampson; Advances in Intelligent Data Analysis XV: 15th International Symposium – página 398; Prediction of Failures in the Air Pressure System of Scania Trucks Using a Random Forest and Feature Engineering; 2016; 
