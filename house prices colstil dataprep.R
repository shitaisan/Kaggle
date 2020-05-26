library(tidyverse)

reorder <- function(x, name_order){
  x[is.na(x)] <- 0
  for (i in 1:length(name_order))
    x[x==name_order[i]] <- i
  return (as.numeric(x))
}


df <- read_csv("train.csv")

dataprep <- function(df){
  df$Alley[is.na(df$Alley)] <- 'no access'
  df$LotFrontage[is.na(df$LotFrontage)] <- median(df$LotFrontage, na.rm = T)
  
  df$LandContour[df$LandContour=='Lvl'] <- 1
  df$LandContour[df$LandContour!='Lvl'] <- 0
  df$LandContour <- as.numeric(df$LandContour)
  
  df$Utilities <- reorder(df$Utilities, c('ELO', "NoSeWa", "NoSewr", "AllPub"))
  df$LotConfig <- reorder(df$LotConfig, c('FR3', 'FR2', "CulDSac", "Corner", "Inside"))
  df$LandSlope <- reorder(df$LandSlope, c('Sev', "Mod", "Gtl"))
  
  df$Condition1[df$Condition1=='Norm' & df$Condition1=='Norm'] <- 2
  df$Condition1[df$Condition1!='Norm' & df$Condition1=='Norm'] <- 1
  df$Condition1[df$Condition1!='Norm' & df$Condition1!='Norm'] <- 0
  df$Condition2 <- NULL
  
  df$Condition1 <- as.numeric(df$Condition1)
  
  df$BldgType <- reorder(df$BldgType, c("Twnhs", "TwnhsE", "Duplex", "2fmCon", "1Fam"))
  
  df$OverallCond <- df$OverallCond*df$OverallQual
  df$OverallQual <- NULL
  
  df$YearBuilt <- df$YearRemodAdd+0.5*df$YearBuilt-2750
  df$YearRemodAdd <- NULL
  
  df$MasVnrArea[is.na(df$MasVnrArea)] <- 0
  df$MasVnrType[is.na(df$MasVnrType)] <-"None" 
  
  df$ExterCond <- reorder(df$ExterCond, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$ExterQual <- reorder(df$ExterQual, c("Po", "Fa", "TA", "Gd", "Ex"))
  
  df$ExtreCond <- df$ExterCond*df$ExterQual
  df$ExterQual <- NULL
  
  df$BsmtCond <- reorder(df$BsmtCond, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$BsmtQual <- reorder(df$BsmtQual, c("Po", "Fa", "TA", "Gd", "Ex"))
  
  df$BsmtExposure <- reorder(df$BsmtExposure, c("No", "Mn", "Av", 'Gd'))
  
  df$BsmtFinType1 <- reorder(df$BsmtFinType1, c('Unf', "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
  df$BsmtFinType2 <- reorder(df$BsmtFinType2, c('Unf', "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
  
  df$BsmtFinSF1[is.na(df$BsmtFinSF1)] <- 0
  df$BsmtFinSF2[is.na(df$BsmtFinSF2)] <- 0
  df$BsmtUnfSF[is.na(df$BsmtUnfSF)] <- 0
  df$TotalBsmtSF[is.na(df$TotalBsmtSF)] <- 0
  
  df$BsmtFinType1 <- df$BsmtFinType1*df$BsmtFinSF1+df$BsmtFinType2*df$BsmtFinSF2
  df$BsmtFinType2 <- NULL
  df$BsmtFinSF1 <- NULL
  df$BsmtFinSF2 <- NULL
  
  df$BsmtFullBath[is.na(df$BsmtFullfBath)] <- 0
  df$BsmtHalfBath[is.na(df$BsmtHalfBath)] <- 0
  
  df$HeatingQC <- reorder(df$HeatingQC, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$CentralAir[df$CentralAir=='N'] <- 0
  df$CentralAir[df$CentralAir=='Y'] <- 1
  
  df$CentralAir <- as.numeric(df$CentralAir)
  
  df$FlrSF1st <- df$`1stFlrSF`
  df$FlrSF2nd <- df$`2ndFlrSF`
  df$`1stFlrSF` <- NULL
  df$`2ndFlrSF` <- NULL
  
  df$Electrical <- reorder(df$Electrical, c('Mix', 'FuseP', 'FuseF', 'FuseA', 'SBrkr'))
  df$KitchenQual <- reorder(df$KitchenQual, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Functional <- reorder(df$Functional, c('Sal', 'Sev', "Maj2", "Maj1", "Mod", 'Min2', 'Min1', 'Typ'))
  df$FireplaceQu <- reorder(df$FireplaceQu, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$Fireplaces <- df$Fireplaces*df$FireplaceQu
  df$FireplaceQu <- NULL
  
  df$GarageYrBlt[is.na(df$GarageYrBlt)] <- 1900
  df$GarageYrBlt <- df$GarageYrBlt-1900
  df$GarageArea[is.na(df$GarageArea)] <- 0
  df$GarageCars[is.na(df$GarageCars)] <- 0
  df$GarageFinish <- reorder(df$GarageFinish, c('Unf', 'RFn', 'Fin'))
  df$GarageCond <- reorder(df$GarageCond, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$GarageQual <- reorder(df$GarageQual, c("Po", "Fa", "TA", "Gd", "Ex"))
  df$GarageCond <- df$GarageCond*df$GarageQual
  df$GarageQual <- NULL
  
  df$SsnPorch3 <- df$`3SsnPorch`
  df$`3SsnPorch` <- NULL
  
  df$PavedDrive <- reorder(df$PavedDrive, c('N', 'P', 'Y'))
  df$PoolQC <- reorder(df$PoolQC, c('Fa', 'TA', 'Gd', 'Ex'))
  df$PoolArea <- df$PoolArea*df$PoolQC
  df$PoolQC <- NULL
  
  df$Fence <- reorder(df$Fence, c('MnWw', 'GdWo', 'MnPrv', 'GdPrv'))
  df$MiscFeature[is.na(df$MiscFeature)] <- 'No'
  
  df$MSZoning <- reorder(df$MSZoning, c("A", "C (all)", "FV", "I", "RH", 'RL', 'RP', 'RM'))
  df$LotShape <- reorder(df$LotShape, c("IR3", 'IR2', 'IR1', "Reg"))
  df$HouseStyle <- reorder(df$HouseStyle, c('1Story', "1.5Unf", '1.5Fin', 'SFoyer', 'SLvl', '2Story', '2.5Unf', '2.5Fin'))
  df$MasVnrType <- reorder(df$MasVnrType, c('None', 'Stone', 'CBlock', 'BrkFace', 'BrkCmn'))
  df$Foundation <- reorder(df$Foundation, c('CBlock', 'Wood', 'Slab', 'Stone', 'BrkTil', 'PConc'))
  df$Heating <- reorder(df$Heating, rev(c('GasA', 'GasW', 'OthW', 'Wall', 'Grav', 'Floor')))
  df$GarageType <- reorder(df$GarageType, rev(c('BuiltIn', "Attchd", 'Basment', '2Types', 'Detchd', "CarPort")))
  df$SaleType <- reorder(df$SaleType, rev(c('New', 'Con', 'CWD', 'ConLI', 'WD', 'COD', 'ConLw', 'ConLD', 'Oth')))
  df$SaleCondition <- reorder(df$SaleCondition, rev(c('Partial', 'Normal','Alloca', 'Family', 'Abnorml', "AdjLand")))
    
  df$Exterior1st <- reorder(df$Exterior1st, c('BrkComm', 'AsphShn', 'CBlock', 'AsbShng', 'MetalSd', 'Wd Sdng','WdShing', 'Stucco', 'HdBoard', 'Plywood', 'BrkFace', 'VinylSd', 'CemntBd', 'Stone', 'ImStucc'))
  df$Exterior2nd <- reorder(df$Exterior2nd, c('CBlock', 'AsbShng', 'Brk Cmn', 'AsphShn', 'Wd Sdng', 'MetalSd', 'Stucco', 'Stone', 'Wd Shng', 'HdBoard', 'Plywood', 'BrkFace', 'VinylSd', 'CmentBd', 'ImStucc', 'Other'))
  
  
  df <- df %>% mutate(Alley = factor(Alley),
                      Street = factor(Street),
                      Neighborhood = factor(Neighborhood),
                      RoofStyle = factor(RoofStyle),
                      RoofMatl = factor(RoofMatl),
                      MiscFeature = factor(MiscFeature))
  return(df)
}

df <- dataprep(df)
library(caret)
dv <- dummyVars(' ~ .', data = df)
df1 <- data.frame(predict(dv, newdata = df))


train <- sample_n(df1, nrow(df1)*.8)
test <- setdiff(df1, train)

rf <- randomForest::randomForest(SalePrice~., data = train)

prediction <- predict(rf, test)
sqrt(mean((log(prediction)-log(test$SalePrice))^2))

rf <- randomForest::randomForest(SalePrice~., data = df1)
realtest <- read_csv('test.csv')
realtest <- dataprep(realtest)

dv <- dummyVars(' ~ .', data = realtest)
realtest1 <- data.frame(predict(dv, newdata = realtest))

realtest1$RoofMatl.ClyTile <- 0
realtest1$RoofMatl.Membran <- 0
realtest1$RoofMatl.Metal <- 0
realtest1$RoofMatl.Roll <- 0
realtest1$MiscFeature.TenC <- 0

prediction <- predict(rf, realtest1)
res <- data.frame(Id = realtest$Id, SalePrice = prediction)
write.csv(res, file = 'res.csv', quote = F, na = '', row.names = F)
