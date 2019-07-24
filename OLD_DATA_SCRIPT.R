dir.USB <- "/volumes/no_name/SanDisk/simulation_images/Images_500_ln/"
setwd(dir.USB)
estimated_parameter <- "ln1"

  
  Input_data <- read.csv(file="estimators.csv")
  Input_data[,1]<- gsub("roots_image_ ","",Input_data[,1])
  Input_data[,1]<- gsub(" .tiff","",Input_data[,1])
  
  names(Input_data)[1]<-paste("id")
  
  Output_data <- read.csv(file="simulated_parameters.csv")
  params2 <- select(Output_data,id,estimated_parameter)
  
  DATA <-merge(Input_data,params2,by.x ="id")
  
  data_stop <- 21
  x <- as.matrix(DATA[,2:data_stop])
  #x <- as.matrix(x[,-81])
  MAX_MAT <- max(x)
  MIN_MAT <- min(x)
  #x <- t(as.matrix(apply(x,1, function(x) 2*((x-MIN_MAT)/(MAX_MAT-MIN_MAT))-1)))
  x <- t(as.matrix(apply(x,1, function(x) (x)/MAX_MAT )))
  #x <- t(as.matrix(apply(x,1, function(x) (x))))
  y <- as.matrix(DATA[,125])
  
  
  #vector = 1:length(y)
  # for (i in vector) {
  #   if (y[i]>=3.75) {
  #     y[i] = 1 
  #   }
  #   else if (y[i]>=2.5 && y[i]<3.75) {
  #     y[i] = 2
  #   }
  #   else if (y[i]>=1.25 && y[i]<2.5) {
  #     y[i] = 3
  #   }
  #   else if (y[i]<1.25) {
  #     y[i] = 4
  #   }
  # }
  # 
  # y <- to_categorical(as.integer(y)-1, num_classes = 4)
  # 
  
  model = keras_model_sequential()
  model %>%
    layer_dense(units= 2048, input_shape=ncol(x), activation="relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 2048, activation="relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 1024, activation="relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 1024, activation="relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 512,  activation="relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 512, activation = "relu") %>%
    layer_dense(units = 1, activation = "linear")
  
  model %>% 
    compile(
      loss = "mse",
      optimizer = "adam",
      metrics = list("mean_absolute_error")
    )
  
  summary(model)
  
  fit = model %>%
    fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 70,
      validation_split = 0.3,
      epochs = 150,
      callbacks = list(callback_early_stopping(monitor='val_loss', patience=250,verbose=1,restore_best_weights=True))
    )
  
  # plot(fit)
  
  



#--------------------------------------------------------------------------------------------
#------------------------------------------- PREDICTION  ------------------------------------ 
#--------------------------------------------------------------------------------------------

  if (prediction){
  
  #------- Col-0 -------
  setwd("/Users/thomasferon/Desktop/Col-0_10_day_images/Images_1/Images_2/estimators")
  test_a<- read.csv(file="estimators.csv")
  test_1 <- as.matrix(test_a[,2:data_stop])
  #test_1 <- as.matrix(test_1[,-81])
  #test_1 <- t(as.matrix(apply(test_1,1, function(test_1) 2*((test_1-MIN_MAT)/(MAX_MAT-MIN_MAT))-1)))
  test_1 <- t(as.matrix(apply(test_1,1, function(test_1) (test_1)/MAX_MAT )))
  #test_1 <- t(as.matrix(apply(test_1,1, function(test_1) (test_1))))
  test_predictions_1 <- model %>% predict(test_1)
  test_predictions_1[,1]
  mean_col <- mean(test_predictions_1[,1])
  interval_col <- c(min(test_predictions_1),max(test_predictions_1))
  index_1 <- sort(test_predictions_1[,1], index.return=TRUE)
  
  print("---------------------")
  print("-----   Col-0   -----")
  print("---------------------")
  print(paste0( "Length between laterals [ cm ] :  "))
  print(paste0( round(test_predictions_1[1:16],4)))
  print(paste0( "Mean :  ", round(mean_col,4)))
  print(paste0( "Interval : Min ", round(interval_col[1],4)," and Max  ",round(interval_col[2],4)))
  print(paste0("index : ",index_1[2]))
  
  #------- Mutant ------
  
  setwd("/Users/thomasferon/Desktop/DAS_10_R1/Images_3/estimators/")
  test_b<- read.csv(file="estimators.csv")
  test_2 <- as.matrix(test_b[,2:data_stop])
  #test_2 <- as.matrix(test_[,-81])
  #test_2 <- t(as.matrix(apply(test_2,1, function(test_2) 2*((test_2-MIN_MAT)/(MAX_MAT-MIN_MAT))-1)))
  test_2 <- t(as.matrix(apply(test_2,1, function(test_2) (test_2)/MAX_MAT )))
  #test_2 <- t(as.matrix(apply(test_2,1, function(test_2) (test_2))))
  test_predictions_2 <- model %>% predict(test_2)
  test_predictions_2[,1]
  mean_mut <- mean(test_predictions_2[,1])
  interval_mut <- c(min(test_predictions_2[,1]),max(test_predictions_2[,1]))
  index_2 <- sort(test_predictions_2[,1], index.return=TRUE)
  
  print("---------------------")
  print("-----  Mutant   -----")
  print("---------------------")
  print(paste0( "Length between laterals [ cm ] :  "))
  print(paste0( round(test_predictions_2[1:16],4)))
  print(paste0( "Mean :  ", round(mean_mut,4)))
  print(paste0( "Interval : Min ", round(interval_mut[1],4)," and Max  ",round(interval_mut[2],4)))
  print(paste0("index : ",index_2[2]))
  
  }  
