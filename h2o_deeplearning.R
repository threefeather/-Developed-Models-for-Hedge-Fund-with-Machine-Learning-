## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Initialise H2O Connection
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Start a local H2O cluster directly from R
## For the first experiment, start the cluster with default 1GB RAM
localH2O = h2o.init(ip = "localhost", port = 54321, startH2O = TRUE)


## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Core Settings for the Experiments
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Define the number of runs (for each model)
## Increase this for more robust comparison
n_run <- 50
n_epochs <- 500
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## [Experiment 1D]: DNN with Dropout (Inputs/Hidden = 20%/50%)
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## Create an empty data frame for results
res_tmp <- data.frame(Trial = 1:n_run, Training = NA, Test = NA, Duration = NA)

## Train model and evaluate performance for n times
cat("\n[Experiment 1D]: DNN with Dropout (Inputs/Hidden = 20%/50%) ...\n")

for (n in 1:n_run) {
  
  ## Display
  cat("Run", n, "out of", n_run, "...\n")
  
  ## Start the timer
  tt <- start_timer()
  
  ## Train the model
  model <- h2o.deeplearning(x = 1:9,  # column numbers for predictors
                            y = 10,   # column number for label
                            data = dat_h2o[row_train, ],
                            activation = "TanhWithDropout",
                            input_dropout_ratio = 0.2,
                            hidden_dropout_ratios = c(0.5,0.5,0.5),
                            balance_classes = TRUE,
                            hidden = c(50,50,50),  ## three hidden layers
                            epochs = n_epochs)
  
  ## Evaluate performance
  yhat_train <- h2o.predict(model, dat_h2o[row_train, ])$predict
  yhat_train <- as.factor(as.matrix(yhat_train))
  yhat_test <- h2o.predict(model, dat_h2o[row_test, ])$predict
  yhat_test <- as.factor(as.matrix(yhat_test))
  
  ## Store Results
  res_tmp[n, 1] <- n
  res_tmp[n, 2] <- round(confusionMatrix(yhat_train, y_train)$overall[1], 4)
  res_tmp[n, 3] <- round(confusionMatrix(yhat_test, y_test)$overall[1], 4)
  res_tmp[n, 4] <- round(stop_timer(tt), 2)
  
}

## Store overall results
res_d <- data.frame(Model = "1D", res_tmp[, -1])
print(res_d)