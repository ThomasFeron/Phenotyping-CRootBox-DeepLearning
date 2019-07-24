# Thomas FERON - UCLouvain (Belgium)
# Root systems Generator and Deep Learning training to predict phenotypes from experimental images

# The aim of this script is to run CRootBOX in a batch mode in order to create
# any number of root systems, with any combinaison of parameters.
# Once these roots have been generated and all the parameters compiled,
# this script also generates the corresponding images.
# After analyzing the images with RIAJ, this script also contains the neural network 
# and its training in order to predict the phenotype of real scans of experimental root systems 

library(tictoc)
library(tidyverse)
library(data.table)
library(stringi)
library(plyr)
library(dplyr)
library(Hmisc)
library(cowplot)
library(keras)
library(GGally)

tic.clearlog()
tic("full script")

general_options           <- T # directories, user parameters, custom functions, original parameters
delete_old                <- F # To delete all remaining files
generate_roots            <- F # To launch CRootBox and generate the roots
compile_parameters        <- F # To compile the parameter sets used to generate the roots
generate_images           <- F # To generate images corresponding to the roots already created
extract_info_from_images  <- F # Made with RIAJ : http://www.guillaumelobet.be/RIA/
deep_learning_training    <- T # To train the neural networks to predict the expected parameter
correlation               <- F # To plot the correlation graphs between the different proxies from RIAJ
prediction                <- T # To predict the phenotype from experimental images
extract_dbl_from_csv      <- T # To validate the model and to compare predicted value from ground truth data

#--------------------------------------------------------------------------------------------
#------------------------------------- GENERAL OPTIONS --------------------------------------
#--------------------------------------------------------------------------------------------

if (general_options){
  
  options(scipen=999) # Disable scientific notation
  
  # Main directories, where everthing is stored
  dir.model <- "/Users/thomasferon/Documents/UCL/MEMOIRE/phenotype_2_model"
  dir.test_dbl <- "/Users/thomasferon/Desktop/Col-0_10_day_images/csv_wild"
  setwd(dir.model)
  
  # User defined parameters
  nsimulations <- 250 
  deviation <- 3          
  estimated_parameter <-'ln1'  # lmax1, lmax2, ln1, la1, r1, r2,
  
  # Load custom functions
  source("io_function.R")
  
  # This is done to always start from the some parameters when we induce variations
  dataset_init <- read_rparam('original/param.rparam')     
  plant_init <- read_pparam('original/param.pparam')
  
}

#--------------------------------------------------------------------------------------------
#----------------------------------------- DELETE OLD ---------------------------------------
#--------------------------------------------------------------------------------------------

if(delete_old){
  tic("delete_old")
  
  ls <- list.files("simulation_data")
  for(l in ls){
    unlink(paste0("simulation_data/",l))
  }
  
  ls <- list.files("simulation_parameters/")
  for(l in ls){
    unlink(paste0("simulation_parameters/",l))
  }
  
  ls <- list.files("simulation_rsmls/")
  for(l in ls){
    unlink(paste0("simulation_rsmls/",l))
  }
  
  ls <- list.files("simulation_images/")
  for(l in ls){
    unlink(paste0("simulation_images/",l))
  }
  toc(log=TRUE)
}

#--------------------------------------------------------------------------------------------
#------------------------ WRITE NEW PARAMETER FILES AND RUN CROOTBOX ------------------------
#--------------------------------------------------------------------------------------------

if(generate_roots){
  tic("generate_roots")
  plant_index <-NULL
  all_roots <- NULL
  
  for(k in c(1:nsimulations)){
    
    unid <- stri_rand_strings(1, 10)  # Get a unique ID for the simulation
    
    plant <- plant_init
    dataset <- dataset_init
    
    print("-----------------------")
    print(paste0("Set ",k," of new parameters generated"))
    
    #--------------------------- CHANGE PARAMETERS IN THE DATASETS ---------------------------------------------
    
    if (estimated_parameter=="lmax1"){
      # tochange <- dataset %>%
      #   filter(type==1 & param == "lmax") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==1 & dataset$param == "lmax"]  <- round(runif(1, tochange/max(deviation, 1e-9), tochange*deviation), 2)
      dataset$val2[dataset$type==1 & dataset$param == "lmax"]  <- 0
    }
    
    if (estimated_parameter=="lmax2"){
      # tochange <- dataset %>%
      #   filter(type==2 & param == "lmax") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==2 & dataset$param == "lmax"]  <- round(runif(1, tochange/max(deviation, 1e-9), tochange*deviation), 2)
      dataset$val2[dataset$type==2 & dataset$param == "lmax"]  <- 0
    }
    
    if (estimated_parameter=="ln1"){
      # tochange <- dataset %>%
      #   filter(type==1 & param == "ln") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==1 & dataset$param == "ln"]  <- round(runif(1, 0.1, 5), 2)
      dataset$val2[dataset$type==1 & dataset$param == "ln"]  <- 0
    }
    
    if (estimated_parameter=="la1"){
      # tochange <- dataset %>%
      #   filter(type==1 & param == "la") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==1 & dataset$param == "la"]  <- round(runif(1, 2, 20), 2)
      dataset$val2[dataset$type==1 & dataset$param == "la"]  <- 0
    }
    
    if (estimated_parameter=="r1"){
      # tochange <- dataset %>%
      #   filter(type==1 & param == "r") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==1 & dataset$param == "r"]  <- round(runif(1, 0.2, 4), 2)
      dataset$val2[dataset$type==1 & dataset$param == "r"]  <- 0
    }
    
    if (estimated_parameter=="r2"){
      # tochange <- dataset %>%
      #   filter(type==2 & param == "r") %>%
      #   select(val1) %>%
      #   as.numeric()
      dataset$val1[dataset$type==2 & dataset$param == "r"]  <- round(runif(1, 0.2, 5), 2)
      dataset$val2[dataset$type==2 & dataset$param == "r"]  <- round(runif(1, 0.2, 5), 2)
    }
    
    # Write the data in the parameter file that will be sued nby CRootBox
    write_rparam(dataset, c("www/param.rparam", paste0("simulation_parameters/",unid,"-param.rparam")))
    write_pparam(plant, c("www/param.pparam", paste0("simulation_parameters/",unid,"-param.pparam")))
    
    plant_index<-rbind(plant_index,unid)
    
    #--------------------------- RUN CROOTBOX ---------------------------------------------
    
    system("chmod 777 www/a-mac.out") 
    system("www/a-mac.out")
    
    fls <- list.files("./")
    fls <- fls[grepl("rootsystem.txt", fls)]
    
    for(f in fls){
      temp <- fread(f, header = T)
      temp$age <- strsplit(f, "_")[[1]][1]
      temp$rep <- k
      temp$ex <- unid
      # Save the temporary data in a permanent file to use. 
      all_roots <- rbind(all_roots, temp)
    }
    
    write_csv(all_roots, "all_roots.csv")
  }
  
  toc(log=TRUE)
  
}

#--------------------------------------------------------------------------------------------
#------------------------------------ COMPILE PARAMETERS ------------------------------------ 
#--------------------------------------------------------------------------------------------

if(compile_parameters){
  setwd(dir.model)
  tic("compile_parameters")
  
  print("-------------")
  print("------ COMPILING ALL  PARAMETERS")
  ls <- list.files("simulation_parameters/")
  ls <- gsub("-param.rparam", "", ls)
  ls <- gsub("-param.pparam", "", ls)
  ls <- unique(ls)
  params <- NULL
  count <- 0
  evol <- 0
  for(l in ls){
    da <- read_rparam(paste0('simulation_parameters/',l,'-param.rparam'))
    pl <- read_pparam(paste0('simulation_parameters/',l,'-param.pparam'))  
    params <- rbind(params, data.frame(
      id = l,
      lmax1 = da$val1[da$type=="1" & da$param == "lmax"],
      lmax2 = da$val1[da$type=="2" & da$param == "lmax"],
      ln1 = da$val1[da$type=="1" & da$param == "ln"],
      la1 = da$val1[da$type=="1" & da$param == "la"],
      r1 = da$val1[da$type=="1" & da$param == "r"],
      r2 = da$val1[da$type=="2" & da$param == "r"],
      maxB = pl$val1[pl$param == "maxB"]
    )) 
    count <- count+1
    if(count >= length(ls)/nsimulations){
      evol <- evol+1
      count <- 0
      message(paste0( " Compile parameters ",evol, " / ",nsimulations))
    }
  }
  write_csv(params, "./simulation_root_estimators/simulated_parameters_1.csv")
  toc(log = TRUE)
}

#--------------------------------------------------------------------------------------------
#------------------------------------ GENERATE IMAGES ------------------------------------ 
#--------------------------------------------------------------------------------------------

if(generate_images){
  setwd("/Volumes/NO_NAME/SanDisk/simulation_images")
  tic("Generate and save images")
  count <- 1
  
  for(l in ls){
    all_roots%>%
      filter(age == "10",ex == l) %>%
      ggplot()+
      theme_classic()+
      theme(axis.line=element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            legend.position="none",
            panel.background=element_blank(),
            panel.border=element_blank(),
            panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),
            plot.background=element_blank())+
      coord_fixed() +
      xlim(-30, 30) +
      ylim(-60,0) +
      geom_segment(data = , aes(x = x1, y = z1, xend = x2, yend = z2), alpha = 0.2, size = 0.3)
    
    message(paste0( "Saving image ", count,"/",nsimulations))
    ggsave(path = "Images_250_ln",filename=paste("roots_image_",l,".tiff"),width = 17.78, height = 17.78, units = "cm",dpi=300)
    count <- count + 1
  }
  toc(log = TRUE)
}

#--------------------------------------------------------------------------------------------
#------------------------------ EXTRACT INFORMATIONS FROM IMAGES ----------------------------
#--------------------------------------------------------------------------------------------

if(extract_info_from_images){
  
  # You can use the RIA plug-in of ImageJ, to extract information directly from images files
  #  http://www.guillaumelobet.be/RIA/  
  # You have to rescale the images you have generated in the format 3OOdpi [2099 pixels = 17.78cm]  
}

#--------------------------------------------------------------------------------------------
#------------------------------------ TRAINING ALGORITHM ------------------------------------ 
#--------------------------------------------------------------------------------------------

if(deep_learning_training){
  tic("DeepLearning training")
  
  setwd(paste0("/Volumes/NO_NAME/SanDisk/simulation_images/Images_250_ln/estimators"))
  
  Input_data <- read.csv(file="estimators.csv")
  Input_data[,1]<- gsub("roots_image_ ","",Input_data[,1])
  Input_data[,1]<- gsub(" .tiff","",Input_data[,1])
  names(Input_data)[1]<-paste("id")
  
  Output_data <- read.csv(file="simulated_parameters_1.csv")
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
  
  
  vector = 1:length(y)
  for (i in vector) {
    if (y[i]<=1.25) {
      y[i] = 1
    }
    else if (y[i]>1.25 && y[i]<3.75) {
      y[i] = 2
    }
    else if (y[i]>=3.75) {
      y[i] = 3
    }
  }

  y <- to_categorical(as.integer(y)-1, num_classes = 3)

  
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
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 128, activation = "relu") %>%
    layer_dropout(rate=0.3) %>%
    layer_dense(units= 128, activation = "relu") %>%
    layer_dense(units = 3, activation = "softmax")
  
  model %>%
    compile(
      loss = "categorical_crossentropy",
      optimizer = "adadelta",
      metrics = "accuracy"
    )
  
  fit = model %>%
    fit(
      x = x,
      y = y,
      shuffle = T,
      batch_size = 35,
      validation_split = 0.3,
      epochs = 200
    )
  
  # plot(fit)
  
  toc(log = TRUE)
  
}

#--------------------------------------------------------------------------------------------
#------------------------------------------- CORRELATION ------------------------------------ 
#--------------------------------------------------------------------------------------------

if(correlation){
  tic("correlation")
  
  MAT <- as.matrix(DATA[,2:125])
  params2 <- select(Output_data,id,estimated_parameter)
  
  
  DATA_CORR <- DATA[,]
  DATA_CORR <- DATA_CORR[,-(1)]
  
  mcor<-cor(DATA_CORR)
  DATA_CORR<-DATA_CORR[,apply(DATA_CORR, 2, function(DATA_CORR) !any(is.na(DATA_CORR)))]
  
  # Use of the mtcars data proposed by R
  data=cor(DATA_CORR)
  data[is.na(data)] <- 0
  data=abs(data)
  
  # Order the correlation matrix
  ord <- order(data[1, ])
  data_ord = data[ord, ord]
  col<- colorRampPalette(c("royalblue4", "white", "red3"))(30)
  heatmap(x = data_ord, col = col, symm = TRUE)
  #ggcorr(data_ord, method = c("everything", "pearson")) 
  
  toc(log=TRUE)
}

#--------------------------------------------------------------------------------------------
#------------------------------------------- PREDICTION  ------------------------------------ 
#--------------------------------------------------------------------------------------------

if (prediction){
  tic("prediction")
  
  #------- Col-0 -------
  setwd("/Users/thomasferon/Desktop/Col-0_10_day_images/Images_val_wild/estimators")
  test_a<- read.csv(file="estimators.csv")
  test_1 <- as.matrix(test_a[,2:data_stop])
  #test_1 <- as.matrix(test_1[,-81])
  #test_1 <- t(as.matrix(apply(test_1,1, function(test_1) 2*((test_1-MIN_MAT)/(MAX_MAT-MIN_MAT))-1)))
  test_1 <- t(as.matrix(apply(test_1,1, function(test_1) (test_1)/MAX_MAT )))
  #test_1 <- t(as.matrix(apply(test_1,1, function(test_1) (test_1))))
  test_predictions_1 <- model %>% predict(test_1)
  for (u in 1:length(test_predictions_1[,1])){
    for (v in 1:3){
      if (test_predictions_1[u,v] == max(test_predictions_1[u,])){
        test_predictions_1[u,1:3] = 0
        test_predictions_1[u,v] = 1
      }
    }
  }
  
  type_1<- cbind(rep(0,length(test_predictions_1[,1] )))
  for (u in 1:length(test_predictions_1[,1])){
    if (test_predictions_1[u,2]==1){
      type_1[u,]="WILD"
    }
    else {
      type_1[u,]="MUTANT"
    }
  }
  
  
  print("---------------------")
  print("-----   Col-0   -----")
  print("---------------------")
  print(paste0( "Estimated Parameter :  ", estimated_parameter))
  print(paste0( type_1[1:16,]))
  
  #------- Mutant ------
  
  setwd("/Users/thomasferon/Desktop/DAS_10_R1/Images_val_mut/Resize/estimators")
  test_b<- read.csv(file="estimators.csv")
  test_2 <- as.matrix(test_b[,2:data_stop])
  #test_2 <- as.matrix(test_[,-81])
  #test_2 <- t(as.matrix(apply(test_2,1, function(test_2) 2*((test_2-MIN_MAT)/(MAX_MAT-MIN_MAT))-1)))
  test_2 <- t(as.matrix(apply(test_2,1, function(test_2) (test_2)/MAX_MAT )))
  #test_2 <- t(as.matrix(apply(test_2,1, function(test_2) (test_2))))
  test_predictions_2 <- model %>% predict(test_2)
  
  for (u in 1:length(test_predictions_2[,1])){
    for (v in 1:3){
      if (test_predictions_2[u,v] == max(test_predictions_2[u,])){
        test_predictions_2[u,1:3] = 0
        test_predictions_2[u,v] = 1
      }
    }
  }
  
  type_2<- cbind(rep(0,length(test_predictions_2[,1] )))
  for (u in 1:length(test_predictions_2[,1])){
    if (test_predictions_2[u,2]==1){
      type_2[u,]="WILD"
    }
    else {
      type_2[u,]="MUTANT"
    }
  }
  
  print("---------------------")
  print("-----  Mutant   -----")
  print("---------------------")
  print(paste0( "Estimated Parameter :  ", estimated_parameter))
  print(paste0( type_2[1:16,]))
  
  toc(log=TRUE)
}


#--------------------------------------------------------------------------------------------
#------------------------------------EXTRACT DBL FROM CSV------------------------------------ 
#--------------------------------------------------------------------------------------------

if(extract_dbl_from_csv){
  tic("extract_dbl_from_csv")
  setwd(dir.test_dbl)
  SR_csv <- read.csv(file="col-0-17_lr.csv")
  SR_csv_insertion_position <- SR_csv[-(1),14]
  SR_csv_insertion_position <- sort(SR_csv_insertion_position)
  len <-(length(SR_csv_insertion_position))
  
  dbl <- NULL
  vector <- 1:len
  for (i in vector) {
    dbl[i] <- SR_csv_insertion_position[i]-SR_csv_insertion_position[i-1]
  }
  dbl <- dbl[2:len]
  
  start_laterals_zone <- as.numeric(as.character(SR_csv[1,19]))
  end_laterals_zone <- as.numeric(as.character(SR_csv[1,21]))
  len_laterals_zone <- end_laterals_zone - start_laterals_zone
  TEST <- sum(dbl)-len_laterals_zone
  
  if (TEST==0){
    print("-------------------------------------")
    print("----- Distance between laterals -----")
    print("-------------------------------------")
    print(paste0(dbl))
    print(paste0( "Mean :  ", mean(dbl)))
    print(paste0( "SD :  ", sd(dbl)))
  }
  toc(log=TRUE)
}

#--------------------------------------------------------------------------------------------
#------------------------------------------ TIME --------------------------------------------
#--------------------------------------------------------------------------------------------

toc(log=TRUE)
tic.log()
