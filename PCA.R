ys<-read.csv("C:/Users/Sugar/Documents/MSBA/ADV/Modeling Project/Y.csv")
xs<-read.csv('C:/Users/Sugar/Documents/MSBA/ADV/Modeling Project/result.csv')
library(xtable)
list<-list()
rmse_list<-list()
library(fpp)

for (i in unique(xs$Country.Name)){
  features<-list()
  print (i)
  xx<-subset.data.frame(xs, Country.Name==i)
  X<-as.data.frame(t(xx))
  X<-X[,1:ncol(X)]
  new_header<-X[1,]
  X<-X[2:nrow(X),]
  #colnames(X, new_header)
  
  # Save y series in memory for later modeling
  income<-subset.data.frame(ys, Country==i)[3]
  age<-subset.data.frame(ys, Country==i)[4]
  
  # PCA
  d<-X[,apply(X[2:22,], 2, var, na.rm=TRUE) != 0]
  d<-as.data.frame(d[2:22,])
  dd=apply(d, 2, as.numeric)
  pca<-prcomp(dd, scale=T, center=T)
  var<-varimax(pca$rotation)
  plot( cumsum((pca$sdev)^2) / sum(pca$sdev^2), main=paste(i,'Scree Plot'), xlab='Components')
  
  # Get 3 factors from PC1, 2 from PC2, 1 from PC3-5
  table<-xtable(unclass(var$loadings))
  list<-append(list, paste(i, 'Extracted Features:', xs[2][names(d)[which.max(abs(table$PC1))],] ,'|', xs[2][names(d)[match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[2]],] ,'|', xs[2][names(d)[match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][1]],] , '|', xs[2][names(d)[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][2]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],], '|', xs[2][names(d)[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1]],]))
  
  
  features<-paste(which.max(abs(table$PC1)) , match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[2] , match(tail(sort(abs(table$PC1)),3), abs(table$PC1))[1], match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][1], match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))[match(tail(sort(abs(table$PC2)),2+length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))), abs(table$PC2))!=ifelse(length(intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))==0, 0, intersect(x = match(tail(sort(abs(table$PC2)),2), abs(table$PC2)), y = match(tail(sort(abs(table$PC1)),3), abs(table$PC1))))][2], match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))[match(tail(sort(abs(table$PC3)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC3))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1], 
match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))[match(tail(sort(abs(table$PC4)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC4))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)),match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1], 
match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))[match(tail(sort(abs(table$PC5)),1+length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))), abs(table$PC5))!=ifelse(length(Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))==0, 0, Reduce(intersect, list(match(tail(sort(abs(table$PC1)),3), abs(table$PC1)),match(tail(sort(abs(table$PC3)),1), abs(table$PC3)),match(tail(sort(abs(table$PC4)),1), abs(table$PC4)), match(tail(sort(abs(table$PC5)),1), abs(table$PC5)), match(tail(sort(abs(table$PC2)),2), abs(table$PC2)))))][1])
  
  
  green<-NULL
  green_test<-NULL
  nom<-NULL
  
  d_test<-X[,apply(X[2:22,], 2, var, na.rm=TRUE) != 0]
  d_test<-as.data.frame(d_test[23:62,])
  dd_test=apply(d_test, 2, as.numeric)
  
  for (i in unique(as.numeric(unlist(strsplit(features, "\\s+"))))){
    green <- rbind(green, dd[,i])
    nom <- cbind(nom, as.character(xs[2][names(d)[i],]))
    row.names(green) <- nom
    
    green_test <- rbind(green_test, dd_test[,i])
    row.names(green_test) <- nom
    
  }
  green <- t(green)
  green_test <- t(green_test)
  
  # MODELING
  ## Always convert model inputs to dataframes!!!!!!!!!!!!!!!!!!!!!!!
  x_train<-as.data.frame(green)
  x_test<-as.data.frame(green_test)
  y_train<-income[1:21,]
  y_test<-income[22:nrow(income),]
  xxx<-cbind(y_train, x_train)
  y_train_age<-age[1:21,]
  xxxx<-cbind(y_train_age, x_train)
  y_test_age<-age[22:nrow(age),]
  
  #arima_income <- auto.arima(y = y_train, xreg = x_train)
  #arima_age <- auto.arima(y = y_train_age, xreg = x_train)
  arima_income<-lm(formula = y_train~.,data = xxx)
  arima_age<-lm(formula = y_train_age~.,data = xxxx)
  pred_income<-predict(arima_income, newdata = x_test)
  pred_age<-predict(arima_age, newdata = x_test)
  rmses<-matrix(0,1,2)
  rmse_income <- sqrt( mean( (pred_income[1:5] - y_test)^2 , na.rm = TRUE ) )
  rmse_age <- sqrt( mean( (pred_age[1:5] - y_test_age)^2 , na.rm = TRUE ) )
  rmses[1,1]<-rmse_income
  rmses[1,2]<-rmse_age
  rmse_list <- rbind(rmse_list, rmses)

  
}