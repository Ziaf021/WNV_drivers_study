require(xgboost)
library(caTools)
library(caret)
library(ROCR)
library(pROC)
library(data.table)
library(here)
library(dplyr)
library(tidyverse)
library(tidymodels)
library(Metrics) 
library(imputeTS)
library(DMwR) #SMOTE function
library(ROSE) #ROSE function


#function to plot classification metrics for different thresholds
fn_chooseThreshold <- function(model, data,labels_test, thresholds_vec){
  df_res <- data.frame()
  
  for (j in 1: length(thresholds_vec)){
    pred_Probs <- predict(model, data)
    confMat <- ConfMatrix(pred_Probs, labels_test,thresholds_vec[j])
    df_res[j,1:7] <-c (thresholds_vec[j], round(as.numeric(confMat$overall[1]),2 ),
                       round(as.numeric(confMat$byClass[c(1,2,5,7,11)]),2) )
    
    
  }
  names(df_res) <- c("Threshold", "Accuracy","Sensitivity", "Specificity",
                     "Precision","F1.Score" ,"Balanced.Accuracy")
  
  return(df_res)
  
}



fun_df_VarsClasses <- function(varNames, model_quarter){
  
  df_names_class <- data.frame(VarName= varNames[1:length(varNames)-1],
                               VarClass= varNames[1:length(varNames)-1])
  
  
  if (model_quarter==1){
    clim_indices = 1:3
    NDVI_indices = 4
    prec_indices = 5
    NDWI_indices = 6
    bioclim_indices = 7:25
    vecotrs_indices = 26:27
    demographic_indices = 28:35
    econ_indices = 36
    mobility_indices = 37:38
    birds_indices= 39: (length(varNames)-1)
    
    df_names_class$VarClass[clim_indices] <- "Climate"
    df_names_class$VarClass[NDVI_indices] <- "NDVI"
    df_names_class$VarClass[prec_indices] <- "Climate"
    df_names_class$VarClass[bioclim_indices] <- "Bioclimatic"
    df_names_class$VarClass[NDWI_indices] <- "NDWI"
    df_names_class$VarClass[vecotrs_indices] <- "Vectors"
    df_names_class$VarClass[demographic_indices] <- "Demographic"
    df_names_class$VarClass[econ_indices] <- "Socio-Economic"
    df_names_class$VarClass[mobility_indices] <- "Mobility"
    df_names_class$VarClass[birds_indices] <- "Host Birds"
    
    
    
  }
  else if (model_quarter==2){
 
    
    clim_indices = c(1:3,6:8)
    NDVI_indices = c(4,9)
    prec_indices = c(5,10)
    NDWI_indices = c(11,12)
    bioclim_indices = 13:31
    vecotrs_indices = 32:33
    demographic_indices = 34:41
    econ_indices = 42
    mobility_indices = 43:44
    birds_indices= 45: (length(varNames)-1)
    
    df_names_class$VarClass[clim_indices] <- "Climate"
    df_names_class$VarClass[NDVI_indices] <- "NDVI"
    df_names_class$VarClass[prec_indices] <- "Climate"
    df_names_class$VarClass[bioclim_indices] <- "Bioclimatic"
    df_names_class$VarClass[NDWI_indices] <- "NDWI"
    df_names_class$VarClass[vecotrs_indices] <- "Vectors"
    df_names_class$VarClass[demographic_indices] <- "Demographic"
    df_names_class$VarClass[econ_indices] <- "Socio-Economic"
    df_names_class$VarClass[mobility_indices] <- "Mobility"
    df_names_class$VarClass[birds_indices] <- "Host Birds"
    
    
    
    
    
       
  }
  else if (model_quarter==3){
    clim_indices = c(1:3,6:8,11:13)
    NDVI_indices = c(4,9,14)
    prec_indices = c(5,10,15)
    NDWI_indices = 16:18
    bioclim_indices = 19:37
    vecotrs_indices = 38:39
    demographic_indices = 40:47
    econ_indices = 48
    mobility_indices = 49:50
    birds_indices= 51: (length(varNames)-1)
    
    df_names_class$VarClass[clim_indices] <- "Climate"
    df_names_class$VarClass[NDVI_indices] <- "NDVI"
    df_names_class$VarClass[prec_indices] <- "Climate"
    df_names_class$VarClass[bioclim_indices] <- "Bioclimatic"
    df_names_class$VarClass[NDWI_indices] <- "NDWI"
    df_names_class$VarClass[vecotrs_indices] <- "Vectors"
    df_names_class$VarClass[demographic_indices] <- "Demographic"
    df_names_class$VarClass[econ_indices] <- "Socio-Economic"
    df_names_class$VarClass[mobility_indices] <- "Mobility"
    df_names_class$VarClass[birds_indices] <- "Host Birds"
    
    
    
  }
  else if (model_quarter==4){
    
  }
  else{
    print("Enter number between 1 to 4")
  }
  
 # df_names_class$VarClass[1:12] <- "Climate"
 # df_names_class$VarClass[13:16] <- "NDVI"
 # df_names_class$VarClass[17:20] <- "Climate"
#  df_names_class$VarClass[21:39] <- "Bioclimatic"
#  df_names_class$VarClass[40:43] <- "NDWI"
 # df_names_class$VarClass[44:45] <- "Vectors"
 # df_names_class$VarClass[46:53] <- "Demographic"
#  df_names_class$VarClass[54] <- "Socio-Economic"
#  df_names_class$VarClass[55:56] <- "Mobility"
#  df_names_class$VarClass[57:length(varNames)-1] <- "Host Birds"
  
  df_names_class$VarClass <- as.factor(df_names_class$VarClass)
  return(df_names_class)
  
}










#### function to draw confusion matrix
draw_confusion_matrix <- function(cm, cm_title) {
  
  total <- sum(cm$table)
  res <- as.numeric(cm$table)
  
  # Generate color gradients. Palettes come from RColorBrewer.
  greenPalette <- c("#F7FCF5","#E5F5E0","#C7E9C0","#A1D99B","#74C476","#41AB5D","#238B45","#006D2C","#00441B")
  redPalette <- c("#FFF5F0","#FEE0D2","#FCBBA1","#FC9272","#FB6A4A","#EF3B2C","#CB181D","#A50F15","#67000D")
  getColor <- function (greenOrRed = "green", amount = 0) {
    if (amount == 0)
      return("#FFFFFF")
    palette <- greenPalette
    if (greenOrRed == "red")
      palette <- redPalette
    colorRampPalette(palette)(100)[10 + ceiling(90 * amount / total)]
  }
  
  # set the basic layout
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title(cm_title, cex.main=1.4)
  
  # create the matrix 
  classes = colnames(cm$table)
  rect(150, 430, 240, 370,  col=getColor("green", res[4]))
  text(195, 435, classes[1], cex=1.2)
  rect(250, 430, 340, 370, col=getColor("red", res[3]))
  text(295, 435, classes[2], cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col=getColor("red", res[2]))
  rect(250, 305, 340, 365,col=getColor("green", res[1]) )
  text(140, 400, classes[1], cex=1.2, srt=90)
  text(140, 335, classes[2], cex=1.2, srt=90)
  
  # add in the cm results
  text(195, 400, res[1], cex=1.6, font=2, col='black')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='black')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.2, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.2)
  text(70, 35, names(cm$byClass[11]), cex=1.2, font=2)
  text(70, 20, round(as.numeric(cm$byClass[11]), 3), cex=1.2)
}

### confusion matrix -End

##  function to  plot train-validation error curves
xgbCV_plot <- function(xgbcv){
  ## plot to visualize 
  res_df <- data.frame( xgbcv$evaluation_log[,2], 
                       xgbcv$evaluation_log[,4],
                       # Don't confuse this with the test data set. 
                       xgbcv$evaluation_log$iter) %>%
    mutate(MIN = xgbcv$evaluation_log[,4] == min(xgbcv$evaluation_log[,4]))%>%
    `colnames<-`(c("Train", "Validation",
                   "Iteration", "Min"))
  
  best_nrounds <- res_df %>%
    filter(Min) %>%
    pull(Iteration)
  
  res_df_longer <- pivot_longer(data = res_df, 
                                cols = c(1, 2), 
                                names_to = "ERROR_TYPE",
                                values_to = "ERROR")
  
  g <- ggplot(res_df_longer, aes(x = Iteration)) +        # Look @ it overfit.
    geom_line(aes(y = ERROR, group = ERROR_TYPE, colour = ERROR_TYPE)) +
    geom_vline(xintercept = best_nrounds[1], colour = "green") +
    #geom_label(aes(label = str_interp("${best_nrounds} iterations \ngive minimum validation error"),
      #             y = 0.2, x = best_nrounds[1], hjust = 0.1)) +
    labs(
      x = "nrounds",
      y = "logloss",
      title = "Xgb logloss",
      subtitle = str_interp("Iteration(s) ${best_nrounds},give the best results")
    ) +
    theme(axis.title.x = element_blank(),
          #text = element_text(size=10,  family= "Microsoft Sans Serif") ,
          panel.border = element_rect(colour = "black",fill=NA, size=0.5),
          panel.background = element_rect(fill = 'white', colour = 'white'),
          legend.position = "top" ,#c(0.5, 0.94),
          legend.direction = "horizontal",
          legend.text=element_text(size=10),
          legend.title=element_blank(),
          #axis.ticks.x  = element_blank(),
          #axis.text.x= element_blank(),
          axis.ticks.length.y =unit(-0.1, "cm"),
          # axis.text.x = element_text(margin = margin(t = .5, unit = "cm"), angle=45),
          # axis.text.x = element_text(margin = margin(t = .5, unit = "cm")),
          axis.text.y = element_text(margin = margin(r = .5, unit = "cm"),
                                     size=10),
          legend.key = element_blank(), #element_rect(fill = NA),
          legend.box = "vertical")+
    #labs(y="CFR [%]") + 
    #scale_x_discrete(  limits=c(0, best_nrounds+10),
      #                   expand = c(0, 0))+
    scale_colour_discrete("Error Type: ")
  
  g
}
## End

# Function to calculate Confusion Matrix 
ConfMatrix <- function(Prediction_prob, Reference, prob_threshold){
  pred_fct <- ifelse(Prediction_prob > prob_threshold, 1, 0)
  cm_data<- confusionMatrix( fct_rev(as.factor(pred_fct)), 
                             fct_rev(as.factor(Reference)),                             
                             positive = "1")                                                                                  
  return(cm_data)
}

birer.res <- function(y, p){
  b.01 <- mean(abs(y-p)^2)
  b.0 <- mean(abs(y[y==0]-p[y==0])^2)
  b.1 <- mean(abs(y[y==1]-p[y==1])^2)
  return(c(b.01,b.0,b.1))
}
logloss.res <- function(y, p){
  b.01 <- mean(log(p)*y + log(1-p)*(1-y) )
  p0 <- p[y==0]
  y0 <- y[y==0]
  b.0 <- mean(log(p0)*y0 + log(1-p0)*(1-y0) )
  p1 <- p[y==1]
  y1 <- y[y==1]
  b.1 <- mean(log(p1)*y1 + log(1-p1)*(1-y1) )
  return(c(b.01,b.0,b.1))
}


reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}



# function to select data according to prediction type 
#i.e. for predictions with upto 1st quarter data only , choose model_flag~1
# or predictions with upto 2nd quarter data , choose model_flag~2 and so on ..
fun.DataSelect  <- function(df, model_flag){
  if (model_flag==1){
    colNames <- names(df)
    #vectors
    #tempratrure, precipiation and ndvi, mndwi, responseVar
    df_quarterly_features <- df[c(1:2,
                                  seq(3,21,by=4),
                                  42, 
                               length(colNames))]

    # features whose values are to be used from one year before
    # bio1-19, GDP, mobility, birds, demographic
    
    features_ids = c(23:41, 46:(length(colNames)-1))
    df_yearly_features <- fun.Adjust_featureValues(df,1,features_ids)
    
    
    DF <-   merge(df_quarterly_features,df_yearly_features,
                  by.x=c("NUTS_ID","year"),
                  by.y=c("NUTS_ID","year"), all.x=T)%>%
                  dplyr::select(-wnf_case ,everything())
    
    return(DF)
  }
  
  else if (model_flag==2){
    colNames <- names(df)
    id_vars <-colNames[1:2] 
    #tempratrure, precipiation and ndvi, mndwi, responseVar
    df_quarterly_features <- df[c(1:2,seq(3,21,by=4),
                               seq(4,22,by=4),42:43, 
                               length(colNames))]
    # features whose values are used from one year before
    features_ids = c(23:41, 46:(length(colNames)-1))
    
    df_yearly_features<- fun.Adjust_featureValues(df,2,features_ids) #colNames[23:41]
    
    DF <-   merge(df_quarterly_features,df_yearly_features,
                  by.x=c("NUTS_ID","year"),
                  by.y=c("NUTS_ID","year"), all.x=T)%>%
      dplyr::select(-wnf_case ,everything())
    
    return(DF)
  }  
  else if (model_flag==3){
    colNames <- names(df)
    #id_vars <-colNames[1:2] 
    #tempratrure, precipiation and ndvi,mndwi, wnf_case
    df_quarterly_features <- df[c(1:2,
                           seq(3,21,by=4), #1st quarter
                           seq(4,22,by=4), #2nd quarter
                           seq(5,22,by=4), # 3rd qauarter
                            42:44,         #mndwi, quarter 1-3
                           length(colNames)) # response feature
                           ]
    
    # features whose values are used from one year before
    features_ids = c(23:41, 46:(length(colNames)-1))
    df_yearly_features<- fun.Adjust_featureValues(df,3,features_ids) 

    DF <-   merge(df_quarterly_features,df_yearly_features,
                  by.x=c("NUTS_ID","year"),
                  by.y=c("NUTS_ID","year"), all.x=T)%>%
      dplyr::select(-wnf_case ,everything())
    return(DF)
  } 
  
  else if(model_flag==4) {
    DF <- df 
    return(DF)
  }
  else{
    print("Enter number between 0 to 4")
  }
  
}

# function for assigning temporal features values from previous year

fun.Adjust_featureValues <- function(df, model_flag, features_ids){
  
years <-sort(unique(df$year)) #c(2010:2019)
df1 <- data.frame()
if (model_flag==1 | model_flag==2 | model_flag==2) {

  df2010 <- df%>% 
    filter(year!=2010)%>%
    group_by(NUTS_ID) %>%
    #group_by(NUTS_ID)%>%
    dplyr::select(c(1,features_ids))%>%
    #mutate_if(is.numeric, ~replace_na(., 0))%>%
    #na.omit()%>%
    dplyr::summarise(across(everything(),list(mean))) %>% #
    dplyr::mutate(year= rep(2010,length(NUTS_ID) ))%>%
    dplyr::select(NUTS_ID, year, everything()) %>%
    `colnames<-`(names(df[,c(1,2,features_ids)]) )
  df1 <- rbind(df1,df2010 )
  
for (jj in 2: length(years)){
  
  ndf <- cbind(df[which(df$year== years[jj]),c(1:2)] ,
               df[which(df$year== years[jj-1]),features_ids])
  df1 <- rbind(df1,ndf)
}
  return(df1)
  }

else if (model_flag==3 | model_flag==4) {
  df1 <- df[c(1:2,features_ids)]
  return(df1)
  
}
else{
  print("Enter number between 0 to 4")
}

}


params.xgb <- list(booster = "gbtree", 
                   objective = "binary:logistic", 
                   eta = 0.1, #0.03, 
                   gamma=1, #0.5 ,
                   min_child_weight = 5,# ,
                   #max_delta_step=1,
                   scale_pos_weight = scale_pos_weight ,
                   lambda=1
)



