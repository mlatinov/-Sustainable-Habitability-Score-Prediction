
#### Libraries ####
library(tidymodels)
library(patchwork)
library(tidyverse)
library(baguette)
library(future)
library(doFuture)
library(finetune)
library(stacks)

# Set up the env

# Register doFuture as the parallel backend
registerDoFuture()

# Set up the parallel plan 
plan(multisession, workers = 2)

# Confirm the plan
print(plan())

## Load the data 
training_data <- read_rds("clean_train")

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

# Linear regression 
linear_regression <- linear_reg()%>%
  set_engine("lm") %>%
  set_mode("regression")

# MARS Untuned
mars <- mars(
  prod_degree = 2) %>%
  set_engine("earth") %>%
  set_mode("regression")

# Bagged MARS
bagged_mars <- bag_mars(
  prod_degree = 2) %>%
  set_engine("earth") %>%
  set_mode("regression")

# RF Untuned
untuned_rf <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

# XGB Untuned
untuned_xgb <- boost_tree(
  trees = 1000 )%>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Neural Network Model Untuned
nn_model <- mlp(
  hidden_units = 10)%>%
  set_engine("nnet")%>%
  set_mode("regression")

# Bagged Neural Network Model Untuned
bagged_nn_model <- bag_mlp(
  hidden_units = 10)%>%
  set_engine("nnet") %>%
  set_mode("regression")

# KNN Untuned
knn_model <- nearest_neighbor(neighbors =  5 )%>%
  set_engine("kknn") %>%
  set_mode("regression")

## Create a workflow set to compare different models with different pre-Proc and evaluate the tuning
models_workflow_set <- workflow_set(
  preproc = list(no_interact = recipe_ni,interact = recipe_in),
  models = list(
    Null = null_model,
    RF = untuned_rf,
    XGB = untuned_xgb,
    MARS = mars,
    Bagged_mars = bagged_mars,
    Neural_Network = nn_model,
    Bagged_Neural_Network = bagged_nn_model,
    Linear_Model = linear_regression,
    KNN = knn_model),
  cross = TRUE)

# Fit all the models 
models_results <- models_workflow_set %>%
  workflow_map(
    fn = "fit_resamples",
    resamples = vfold_cv(data = model_validation,v = 5,strata = "habitability_score"),
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
  scale_fill_viridis_c(option = "D",direction = -1,begin = 0.1,end = 0.7)+
  theme_minimal()+
  labs(
    title = "Heatmap",
    y = "",
    x = " Preprocessing",
    fill = "RMSE")+
  theme(
    axis.text = element_text(face = "italic"))

# Create a Bar chart
plot_2 <- ggplot(data = df_plot,aes(x = mean,y = fct_reorder(factor(wflow_id),.fun = sum,.x = mean,.desc = TRUE),fill = model))+
  geom_col()+
  scale_fill_viridis_d(option = "D",,begin = 0.1,end = 0.7)+
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
  scale_color_viridis_d(option = "D",begin = 0.1,end = 0.7)+
  theme_minimal()+
  ylim(c(5,8))+
  xlim(c(0,0.3))+
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
final_plot <- (plot_2 + plot_1) / plot_3  &
  plot_annotation(
    title = "Models & Preprocessing",
    subtitle = "all of the models are not tuned",
    theme = theme_minimal())&
  theme(
    title = element_text(size = 10,face = "bold")
  )

# Plot
final_plot

#### Tuning Models ####

# Parameters for tuning
resamples <- vfold_cv(data = model_validation,v = 5)
metrics <- metric_set(rmse)

# Control for BO optimization
control <- control_bayes(
  verbose = TRUE,
  no_improve = 5,
  seed = 123,
  time_limit = 3600,
  parallel_over = "resamples",
  save_pred = TRUE,
  save_workflow = TRUE)

# Tune control for initial tuning
control_tune <- control_grid(
  verbose = TRUE,
  save_pred = TRUE,
  save_workflow = TRUE,
  parallel_over = "everything")

## Bag MLP

# Bag mlp spec
bad_mlp_tune <- bag_mlp(
  hidden_units = tune(),
  epochs = tune(),
  penalty = tune()) %>%
  set_mode("regression") %>%
  set_engine("nnet")
  
# Create a workflow
bag_mlp_workflow <- 
  workflow() %>%
  add_recipe(recipe_ni) %>% # Recipe with interactions
  add_model(bad_mlp_tune)

# Create a Search grid LHC
bag_mlp_lhc <-
  grid_space_filling(
    hidden_units(range = c(10, 20)),   
    epochs(range = c(50,80)),         
    penalty(range = c(-1, 0))
    )

# Tune the Bag mlp
initial_bag_mlp <- bag_mlp_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = bag_mlp_lhc,
    metrics = metrics,
    control = control_grid(
      parallel_over = "everything",
      verbose = TRUE))

# bag mlp BO
bag_mlp_bo <- bag_mlp_workflow %>%
  tune_bayes(
    resamples = resamples,
    iter = 10,
    metrics = metrics,
    initial = initial_bag_mlp,
    control = control)

# Extract the best results
bag_mlp_best <- bag_mlp_bo %>% select_best(metric = "rmse")

# Finalize the model
bag_mlp_final_workflow <- finalize_workflow(bag_mlp_workflow,bag_mlp_best)

# Fit the final model on the training data 
bag_mlp_final <- fit(bag_mlp_final_workflow,data = training_data)

## RF 

# RF Model spec
rf_tuned <- rand_forest(
  mtry = tune(),
  min_n = tune(),
  trees = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")
  
# Create a workflow
rf_workflow <- 
  workflow() %>%
  add_model(rf_tuned)%>%
  add_recipe(recipe_in)
  
extract_parameter_set_dials(rf_workflow)

# Set parameters RF
rf_param <- 
  rf_workflow %>%
  extract_parameter_set_dials() %>%
  finalize(training_data) %>%
  update(
    mtry = mtry(c(1L,14L)),
    min_n = min_n(c(3L,40L))
  )

# Create a Grid LHC
rf_lhc <- grid_space_filling(
  rf_param
)

# Initial tuning
rf_tuning <- rf_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = rf_lhc,
    metrics = metrics,
    control = control_grid(
      parallel_over = "everything",
      verbose = TRUE))

# RF BO
rf_bo <- rf_workflow %>%
  tune_bayes(
    resamples = resamples,
    metrics = metrics,
    param_info = rf_param,
    initial = rf_tuning,
    iter = 15,
    control = control)

## XGB

# XGB spec
xgb_tune <- boost_tree(
  mtry = tune(),
  trees = tune(),
  min_n = tune(),
  learn_rate = tune(),
  sample_size = tune(),
  tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# XGB workflow 
xgb_workflow <- 
  workflow() %>%
  add_model(xgb_tune) %>%
  add_recipe(recipe_in)

# Set params 
xgb_params <- 
  xgb_workflow %>%
  extract_parameter_set_dials()%>%
  finalize(training_data) %>%
  update(
    mtry = mtry(c(1L,14L))
  )

# Create a LHC Grid
xgb_lhc <- xgb_params %>% grid_space_filling()
  
# Initial tune
xgb_initial <- xgb_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = xgb_lhc,
    metrics = metrics,
    control = control_grid(
      verbose = TRUE,
      parallel_over = "everything"))

# XGB BO 
xgb_bo <- xgb_workflow %>%
  tune_bayes(
    resamples = resamples,
    iter = 15,
    param_info = xgb_params,
    initial = xgb_initial,
    metrics = metrics,
    control = control)

# Bagged Mars 

# Bag Mars spec
bag_mars_tune <- bag_mars(
  num_terms = tune(),
  prod_degree = tune()) %>%
  set_engine("earth") %>%
  set_mode("regression")

# Create a workfow
bag_mars_workflow <- 
  workflow() %>%
  add_model(bag_mars_tune) %>%
  add_recipe(recipe_in)

# Create a LHC grid
bag_mars_lhc <- grid_space_filling(
  num_terms(range = c(20,50)),
  prod_degree(range = c(1,2))
)

# Tune the Bag_Mars initial
bag_mars_inital <- bag_mars_workflow %>%
  tune_grid(
    resamples = resamples,
    grid = bag_mars_lhc,
    metrics = metrics,
    control = control_grid(
      verbose = TRUE,
      parallel_over = "everything"))

# Bagged Mars BO
bag_mars_bo <- bag_mars_workflow %>%
  tune_bayes(
    resamples = resamples,
    metrics = metrics,
    iter = 15,
    initial = bag_mars_inital,
    control = control)

## Stack Model 
model_stack <- stacks() %>%
  add_candidates(bag_mars_bo) %>%
  add_candidates(rf_bo) %>%
  add_candidates(bag_mlp_bo)




