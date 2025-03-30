
#### Libraries ####
library(tidymodels)
library(patchwork)
library(tidyverse)

## Load the data 
training_data <- read_rds("clean_train")

# Create temporary sample to speed up the initial process
training_data <- training_data %>%
  sample_n(size = 10000)

test_data <- read_rds("clean_test")

# Create a validation set from training set 80 / 20 split
validation_split <- initial_split(data = training_data,prop = 0.8,strata = habitability_score)

# Training set 
model_training <- training(validation_split)
model_validation <- testing(validation_split)

#### Feature Engineering ####

## Recipe with no interactions 
recipe_ni <- recipe(habitability_score ~ .,data = model_training) %>%
  
  # Isolate id from transformations 
  add_role(id ,new_role = "id") %>%
  
  # Create bagged tree models to impute missing data.
  step_impute_bag(all_predictors()) %>%
  
  # Standardize all numeric predictors
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Normalize all numeric predictors 
  step_normalize(all_numeric_predictors()) %>%
  
  # Dummy encode all nominal predictors
  step_dummy(all_nominal_predictors()) %>%
  
  # Remove near zero variance features
  step_nzv(all_predictors()) 
  
## Recipe with interactions 
recipe_in <- recipe(habitability_score ~ .,data = model_training) %>%
  
  # Isolate id from transformations 
  add_role(id ,new_role = "id") %>%
  
  # Create bagged tree models to impute missing data.
  step_impute_bag(all_predictors()) %>%
  
  # Dummy encode all nominal predictors
  step_dummy(all_nominal_predictors()) %>%
  
  # Remove near zero variance features
  step_nzv(all_predictors()) %>%
  
  # Add interactions
  step_interact(~ matches("^furnishing_.*"):matches("^power_backup_.*")) %>%
  step_interact(~ matches("^furnishing_.*"):neighborhood_review) %>%
  
  # Standardize all numeric predictors
  step_YeoJohnson(all_numeric_predictors()) %>%
  
  # Normalize all numeric predictors 
  step_normalize(all_numeric_predictors()) 

#### Models specifications  ####

# NuLL Model
null_model <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("regression")

# RF Untuned
untuned_rf <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

# XGB Untuned
untuned_xgb <- boost_tree()%>%
  set_engine("xgboost") %>%
  set_mode("regression")

## Create a workflow set to compare different models with different pre-Proc and evaluate the tuning
models_workflow_set <- workflow_set(
  preproc = list(no_interact = recipe_ni,interact = recipe_in),
  models = list(
    Null = null_model,
    RF = untuned_rf,
    XGB = untuned_xgb),
  cross = TRUE)

# Fit all the models 
models_results <- models_workflow_set %>%
  workflow_map(
    fn = "fit_resamples",
    resamples = vfold_cv(data = model_validation,v = 10,strata = "habitability_score"),
    metrics = metric_set(rmse),
    verbose = TRUE,
    seed = 123)

# Extract the results against the validation set
results_extract <- models_results %>%
  collect_metrics()

df_results <- as.data.frame(results_extract)

# Viz the results
df_plot <- df_results %>%
  mutate(preproc = if_else(condition = str_detect(wflow_id,"no_interact"),true = "interactions",false = "no_interactions"))
  
# Create a Heat map plot
plot_1 <- ggplot(data = df_plot,aes(x = preproc,y = model,fill = mean))+
  geom_tile()+
  scale_fill_viridis_c(option = "B",direction = -1,begin = 0.1,end = 0.7)+
  theme_minimal()+
  labs(
    title = "Heatmap",
    y = "",
    x = " Preprocessing",
    fill = "RMSE")+
  theme(axis.text = element_text(face = "italic"))

# Create a Bar chart
plot_2 <- ggplot(data = df_plot,aes(x = mean,y = fct_reorder(factor(wflow_id),.fun = sum,.x = mean,.desc = TRUE),fill = model))+
  geom_col()+
  scale_fill_viridis_d(option = "A",,begin = 0.1,end = 0.7)+
  theme_minimal()+
  labs(
    title = "Bar Chart",
    x = "RMSE",
    y = "",
  )+
  theme(
    axis.text = element_text(face = "italic"),
    legend.position = "none"
  )

# Create a scatter plot
plot_3 <- ggplot(data = df_plot,aes(x = std_err, y = mean,color = model,shape = preproc)) +
  geom_point(size = 3) +
  scale_color_viridis_d(option = "A",begin = 0.1,end = 0.7)+
  theme_minimal()+
  ylim(c(1,12))+
  labs(
    title = "Scatter Plot",
    x = "Standart Error",
    y = "RMSE",
    color = "Models",
    shape = "Preprocessing")+
  theme(
    axis.text = element_text(face = "italic")
  )

# Combine all plots 
final_plot <- (plot_2 + plot_1) / plot_3 &
  plot_annotation(
    title = "Models & Preprocessing",
    subtitle = "all of the models are not tuned",
    theme = theme_minimal())&
  theme(
    title = element_text(size = 10,face = "bold")
  )

final_plot



