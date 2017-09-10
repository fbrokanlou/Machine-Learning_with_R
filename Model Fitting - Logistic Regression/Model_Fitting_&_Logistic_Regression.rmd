---
title: "Model Fitting - Logistic Regression"
author: "Fakhraddin Jaf"
date: "February 13, 2017"
output:
  html_document: default
  pdf_document: default
---
---------

### Step 1 : Loading Data , and running numerical and graphical summaries 

#### Following libraliers are included:
#### library(dplyr) 
#### library(plotly)

```{r chunk_name,include=FALSE}
library(dplyr)
library(plotly)
```


```{r}
Data_Orig <- read.table(file="datos_icb.txt", header=TRUE)
summary(Data_Orig)
```

```{r}
relapse_code <- ifelse(Data_Orig$recid == "SI" ,1 , 0 )
plot(as.numeric(rownames(Data_Orig)), jitter(relapse_code, 0.15), 
        main = "Distribution of Relapsed/Not Relapsed condition" , 
           xlab = "Patient's row number in the original dataset" , 
                          ylab = "relapse (0 = No,    1 = Yes )" )
        
```
------------

### Step 2 : Fit an example logistic regression model and prediction based on "Tumor size" :
```{r}
Primary_Model <-  glm(recid ~ tam, data = Data_Orig, family = binomial("logit"))
summary(Primary_Model)
```


#### using __predict()__ function :
```{r}
Primary_Prediction = predict(Primary_Model,select(Data_Orig,tam),type="response")

plot(as.numeric(rownames(Data_Orig)), jitter((round (Primary_Prediction )), 0.05) , 
 main = "Probability of relapse, based on Tumor Size" , xlab = "Patient's number" , 
                            ylab = " Probability of relapse (0 = No,    1 = Yes )")
```

#### Comparing predicted cases with probability of relapse to those who actually had replase in original data set  :
```{r}
Matched_cases <- length(intersect(which(Primary_Prediction > 0.5), 
                                  which(Data_Orig$recid == "SI")))
                                  
cat(" Total Number of cases predicted with probability of relapse:"
                  , length(which(Primary_Prediction > 0.5)) ,"\n", 
                       "Number of cases matched: ", Matched_cases)
```

-------------

### Step 3 : Fit a logistic regression model and prediction based on each column :
```{r}
Prediction_1_mat = matrix(data=NA, nrow=2, ncol=7)

for(i in 1:(ncol(Data_Orig)-1)){

    Model_1 <-  glm(recid ~ eval(as.name(colnames(Data_Orig)[i])),
	data = Data_Orig, family = binomial("logit"))
	
    Predict_1 <- predict(Model_1, select(Data_Orig,eval(as.name(colnames(Data_Orig)[i])))
                                                                        ,type="response")
	
    Matched_1_cases <- length(intersect(which(Predict_1 > 0.5), 
                              which(Data_Orig$recid == "SI")))
    
    Prediction_1_mat[1,i] = length(which(Predict_1 > 0.5))
    Prediction_1_mat[2,i] = Matched_1_cases


}
rownames(Prediction_1_mat) <- c("Predcited", "Matched")
colnames(Prediction_1_mat) <- c(names(Data_Orig)[1:7])
Prediction_1_mat <- Prediction_1_mat[, order(as.character(colnames(Prediction_1_mat)))]


Predcited <- as.vector(Prediction_1_mat["Predcited",])
Matched <- as.vector(Prediction_1_mat["Matched",])
columne_names <- as.factor(colnames(Prediction_1_mat))
data1 <- data.frame(columne_names, Predcited, Matched)


Prediction_1_plot <- plot_ly(data1, x = ~columne_names, 
                     y = ~Predcited, type = 'bar', name = 'Predcited', width = 920, alpha = 0.8) %>%
                     add_trace(y = ~Matched, type = 'bar' , name = 'Matched') %>%
                     layout(yaxis = list(title = 'Relapse Count'), 
                           xaxis = list(title = "", tickangle = -45), 
                           margin = list(b = 100),
                           barmode = 'group')
Prediction_1_mat
Prediction_1_plot

```

__Predcited__: Number of predcited cases with the probability of relapse

__Matched__ : Number of items matched with cases having relapse in original dataset


-------------

### Step 4 : Fit a logistic regression model and prediction based on a combination of two columns :
```{r}
Comb2 <- combn(colnames(Data_Orig)[1:7],2,FUN=NULL)
Prediction_2_mat = matrix(data=NA, nrow=2, ncol=ncol(Comb2))

for(i in 1:(ncol(Comb2))){

    Model_2 <-  glm(recid ~ eval(as.name(Comb2[1,i]))+eval(as.name(Comb2[2,i])), 
                                  data = Data_Orig, family = binomial("logit"))
    
    Predict_2 <- predict(Model_2, select(Data_Orig, eval(as.name(Comb2[1,i])), 
                                  eval(as.name(Comb2[2,i]))),type="response") 
    
    Matched_2_cases <- length(intersect(which(Predict_2 > 0.5), 
                              which(Data_Orig$recid == "SI")))
    
    Prediction_2_mat[1,i] = length(which(Predict_2 > 0.5))
    Prediction_2_mat[2,i] = Matched_2_cases
}
rownames(Prediction_2_mat) <- c("Predcited", "Matched")
a = NULL
for (i in 1:(ncol(Comb2))) {
    a <- append(a , (paste(Comb2[1,i],Comb2[2,i],sep='+')))
}
colnames(Prediction_2_mat) <- c(a[1:ncol(Comb2)])
Prediction_2_mat <- Prediction_2_mat[, order(as.character(colnames(Prediction_2_mat)))]


Predcited <- as.vector(Prediction_2_mat["Predcited",])
Matched <- as.vector(Prediction_2_mat["Matched",])
columne_names <- as.factor(colnames(Prediction_2_mat))
data2 <- data.frame(columne_names, Predcited, Matched)


Prediction_2_plot <- plot_ly(data2, x = ~columne_names, 
                     y = ~Predcited, type = 'bar', name = 'Predcited', width = 920, alpha = 0.8) %>%
                     add_trace(y = ~Matched, type = 'bar' , name = 'Matched') %>%
                     layout(yaxis = list(title = 'Relapse Count'), 
                           xaxis = list(title = "", tickangle = -45), 
                           margin = list(b = 100),
                           barmode = 'group')
Prediction_2_mat
Prediction_2_plot

```


__Predcited__: Number of predcited cases with the probability of relapse

__Matched__ : Number of items matched with cases having relapse in original dataset

--------

### Step 5 : Fit a logistic regression model and prediction based on a combination of three columns :

```{r}
Comb3 <- combn(colnames(Data_Orig)[1:7],3,FUN=NULL)

Prediction_3_mat = matrix(data=NA, nrow=2, ncol=ncol(Comb3))

for(i in 1:(ncol(Comb3))){

    Model_3 <-  glm(recid ~ eval(as.name(Comb3[1,i]))+eval(as.name(Comb3[2,i]))
       +eval(as.name(Comb3[3,i])),data = Data_Orig, family = binomial("logit"))
    
    Predict_3 <- predict(Model_3, select(Data_Orig, eval(as.name(Comb3[1,i])), 
        eval(as.name(Comb3[2,i])),eval(as.name(Comb3[3,i]))),type="response")
    
    Matched_3_cases <- length(intersect(which(Predict_3 > 0.5), 
                              which(Data_Orig$recid == "SI")))
    
    Prediction_3_mat[1,i] = length(which(Predict_3 > 0.5))
    Prediction_3_mat[2,i] = Matched_3_cases
}

rownames(Prediction_3_mat) <- c("Predcited", "Matched")

a = NULL
for (i in 1:(ncol(Comb3))) {
    a <- append(a , (paste(Comb3[1,i],Comb3[2,i],Comb3[3,i], sep='+')))
}
colnames(Prediction_3_mat) <- c(a[1:ncol(Comb3)])

Prediction_3_mat <- Prediction_3_mat[, order(as.character(colnames(Prediction_3_mat)))]


Predcited <- as.vector(Prediction_3_mat["Predcited",])
Matched <- as.vector(Prediction_3_mat["Matched",])
columne_names <- as.factor(colnames(Prediction_3_mat))
data3 <- data.frame(columne_names, Predcited, Matched)


Prediction_3_plot <- plot_ly(data3, x = ~columne_names, 
                     y = ~Predcited, type = 'bar', name = 'Predcited', width = 920, alpha = 0.8) %>%
                     add_trace(y = ~Matched, type = 'bar' , name = 'Matched') %>%
                     layout(yaxis = list(title = 'Relapse Count'), 
                           xaxis = list(title = "", tickangle = -45), 
                           margin = list(b = 100),
                           barmode = 'group')
Prediction_3_mat
Prediction_3_plot
```

__Predcited__: Number of predcited cases with the probability of relapse

__Matched__ : Number of items matched with cases having relapse in original dataset





