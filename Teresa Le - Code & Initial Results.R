
# Packages ----------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(caret)

# Importing Code & Renaming Columns ---------------------------------------

raw.data <- read_excel("/Users/Teresa - School/Documents/CKME136 - Capstone Project/Dataset/Ames Housing Dataset.xls", 
                       sheet = 1, col_names = TRUE, na = c(""))

## Add periods and remove spaces from column names 
names(raw.data) <- str_replace_all(names(raw.data), c(" " = "."))

## Rename column names 
raw.data <- raw.data %>% rename(
  MS.Subclass = MS.SubClass,
  Zoning.Dsgn = MS.Zoning,
  Neighbourhood = Neighborhood,
  Condition1 = Condition.1,
  Condition2 = Condition.2, 
  Exterior1 = Exterior.1st, 
  Exterior2 = Exterior.2nd, 
  Year.Remod = `Year.Remod/Add`,
  Bsmt.Fin1.Area = BsmtFin.SF.1,
  Bsmt.Fin2.Area = BsmtFin.SF.2,
  Bsmt.Unf.Area = Bsmt.Unf.SF,
  Total.Bsmt.Area = Total.Bsmt.SF,
  Floor1.Area = `1st.Flr.SF`,
  Floor2.Area = `2nd.Flr.SF`, 
  Low.Qual.Fin.Area = Low.Qual.Fin.SF,
  Rms.AbvGr = TotRms.AbvGrd,
  Liv.Area.AbvGr = Gr.Liv.Area, 
  Bsmt.Fin.Type1 = BsmtFin.Type.1,
  Bsmt.Fin.Type2 = BsmtFin.Type.2,
  Garage.Yr.Built = Garage.Yr.Blt, 
  Heating.Qual = Heating.QC, 
  Fireplace.Qual = Fireplace.Qu,
  Wood.Deck.Area = Wood.Deck.SF,
  Open.Porch.Area = Open.Porch.SF,
  Enclosed.Porch.Area = Enclosed.Porch,
  ThreeSsn.Porch.Area =`3Ssn.Porch`,
  Scrn.Porch.Area = Screen.Porch,
  Val.Misc.Feature = Misc.Val,
  Pool.Qual = Pool.QC,
  Month.Sold = Mo.Sold, 
  Year.Sold = Yr.Sold, 
  Sale.Price = SalePrice 
)

# Correcting Values, Recoding Values & Removing Outliers ------------------

c_data <- raw.data

## Correct incorrect Garage Yr Built value 
c_data$Garage.Yr.Built[c_data$Garage.Yr.Built == 2207] = 2007 

## Remove outliers 
c_data1 <- c_data[c_data$Liv.Area.AbvGr < 4000,]

c_data2 <- c_data1 

## Recode NA values as None (In this case, NA does not represent missing data but not applicable)
c_data2$Alley = replace(c_data2$Alley, c_data2$Alley == "NA", "None")
c_data2$Bsmt.Qual = replace(c_data2$Bsmt.Qual, c_data2$Bsmt.Qual == "NA", "None")
c_data2$Bsmt.Cond = replace(c_data2$Bsmt.Cond, c_data2$Bsmt.Cond == "NA", "None")
c_data2$Bsmt.Exposure = replace(c_data2$Bsmt.Exposure, c_data2$Bsmt.Exposure == "NA", "None")
c_data2$Bsmt.Fin.Type1 = replace(c_data2$Bsmt.Fin.Type1, c_data2$Bsmt.Fin.Type1 == "NA", "None")
c_data2$Bsmt.Fin.Type2 = replace(c_data2$Bsmt.Fin.Type2, c_data2$Bsmt.Fin.Type2 == "NA", "None")
c_data2$Fireplace.Qual = replace(c_data2$Fireplace.Qual, c_data2$Fireplace.Qual == "NA", "None")
c_data2$Garage.Type = replace(c_data2$Garage.Type, c_data2$Garage.Type == "NA", "None")
c_data2$Garage.Finish = replace(c_data2$Garage.Finish, c_data2$Garage.Finish == "NA", "None")
c_data2$Garage.Qual = replace(c_data2$Garage.Qual, c_data2$Garage.Qual == "NA", "None")
c_data2$Garage.Cond = replace(c_data2$Garage.Cond, c_data2$Alley == "NA", "None")
c_data2$Pool.Qual = replace(c_data2$Pool.Qual, c_data2$Pool.Qual == "NA", "None")
c_data2$Fence = replace(c_data2$Fence, c_data2$Fence == "NA", "None")

c_data3 <- c_data2 


# Splitting Dataset & Handling Missing Values -----------------------------

## Split the dataset into training and test set by year sold 
ctrain <- subset(c_data3, Year.Sold == 2006 | Year.Sold == 2007 | Year.Sold == 2008 | Year.Sold == 2009)
ctest <- subset(c_data3, Year.Sold == 2010)

## 
## 1. Lot Frontage 
NA.LotFrontage <- ctrain[is.na(ctrain$Lot.Frontage),] 
Lot.Frontage.Median <- ctrain %>% group_by(ctrain$Neighbourhood) %>% summarize(median_Lot.Frontage = median(ctrain$Lot.Frontage, na.rm=T))
ctrain 

ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Blmngtn"] = Lot.Frontage.Median$median_Lot.Frontage[1]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Blueste"] = Lot.Frontage.Median$median_Lot.Frontage[2]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "BrDale"] = Lot.Frontage.Median$median_Lot.Frontage[3]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "BrkSide"] = Lot.Frontage.Median$median_Lot.Frontage[4]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "ClearCr"] = Lot.Frontage.Median$median_Lot.Frontage[5]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "CollgCr"] = Lot.Frontage.Median$median_Lot.Frontage[6]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Crawfor"] = Lot.Frontage.Median$median_Lot.Frontage[7]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Edwards"] = Lot.Frontage.Median$median_Lot.Frontage[8]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Gilbert"] = Lot.Frontage.Median$median_Lot.Frontage[9]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Greens"] = Lot.Frontage.Median$median_Lot.Frontage[10]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "IDOTRR"] = Lot.Frontage.Median$median_Lot.Frontage[12]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "MeadowV"] = Lot.Frontage.Median$median_Lot.Frontage[14]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Mitchel"] = Lot.Frontage.Median$median_Lot.Frontage[15]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "NAmes"] = Lot.Frontage.Median$median_Lot.Frontage[16]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "NoRidge"] = Lot.Frontage.Median$median_Lot.Frontage[17]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "NPkVill"] = Lot.Frontage.Median$median_Lot.Frontage[18]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "NridgHt"] = Lot.Frontage.Median$median_Lot.Frontage[19]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "NWAmes"] = Lot.Frontage.Median$median_Lot.Frontage[20]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "OldTown"] = Lot.Frontage.Median$median_Lot.Frontage[21]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Sawyer"] = Lot.Frontage.Median$median_Lot.Frontage[22]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "SawyerW"] = Lot.Frontage.Median$median_Lot.Frontage[23]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Somerst"] = Lot.Frontage.Median$median_Lot.Frontage[24]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "StoneBr"] = Lot.Frontage.Median$median_Lot.Frontage[25]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "SWISU"] = Lot.Frontage.Median$median_Lot.Frontage[26]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Timber"] = Lot.Frontage.Median$median_Lot.Frontage[27]
ctrain$Lot.Frontage[is.na(ctrain$Lot.Frontage) & ctrain$Neighbourhood == "Veenker"] = Lot.Frontage.Median$median_Lot.Frontage[28]

## 2. & 3. Masonry Veneer Area & Masonry Veneer Type 
median(ctrain$Mas.Vnr.Area, na.rm=T) 
ctrain$Mas.Vnr.Area[is.na(ctrain$Mas.Vnr.Area)] = 0
ctrain$Mas.Vnr.Type[is.na(ctrain$Mas.Vnr.Type)] = "None"

## 4. Electrical 
table(ctrain$Electrical, useNA="ifany") 
ctrain$Electrical <- replace(ctrain$Electrical, is.na(ctrain$Electrical), "SBrkr") 

## 5. Bsmt Exposure 
table(ctrain$Bsmt.Exposure, useNA = "ifany")
ctrain$Bsmt.Exposure[is.na(ctrain$Bsmt.Exposure) & !is.na(ctrain$Bsmt.Qual)] = "No"

## 6. Bsmt Fin Type 2 
table(ctrain$Bsmt.Fin.Type2, useNA="ifany") 
# Observation with missing has a Bsmt Fin Type 2 Area value i.e. cannot be None 
NA.BsmtFinType2 <- ctrain[is.na(ctrain$Bsmt.Fin.Type2) & !is.na(ctrain$Bsmt.Fin.Type1),] 
ctrain$Bsmt.Fin.Type2[is.na(ctrain$Bsmt.Fin.Type2) & !is.na(ctrain$Bsmt.Fin.Type1)] = "Rec"

# Correct Garage Values - Supposed to be 0 or None 
## 7. Garage Yr Built 
ctrain$Garage.Yr.Built[is.na(ctrain$Garage.Yr.Built)] = 0

# 8. Garage Cars 
# Observation w/ missing Garage Cars value - No Garage 
NA.GarageCars <- ctrain[is.na(ctrain$Garage.Cars) & !is.na(ctrain$Garage.Type),] 
ctrain$Garage.Cars[is.na(ctrain$Garage.Cars) & !is.na(ctrain$Garage.Type)] = 0

## 9. Garage Area 
ctrain$Garage.Area[is.na(ctrain$Garage.Area) & !is.na(ctrain$Garage.Type)] = 0

## 10. Garage Finish 
ctrain$Garage.Finish[is.na(ctrain$Garage.Finish) & !is.na(ctrain$Garage.Type)] = "None"

## 11. Garage Qual 
ctrain$Garage.Qual[is.na(ctrain$Garage.Qual) & !is.na(ctrain$Garage.Type)] = "None" 

## 12. Garage Cond 
ctrain$Garage.Cond[is.na(ctrain$Garage.Cond) & !is.na(ctrain$Garage.Type)] = "None"

## 13. Garage Type 
ctrain$Garage.Type[ctrain$PID == "0910201180"] = "None"


# Filter Rows & Columns ---------------------------------------------------

## Remove Identifier variables, remove observations from neighbourhoods that aren't in the training set, create total porch area variable 
ftrain <- ctrain[,3:82]
ftrain <- ftrain %>% filter(Neighbourhood != "GrnHill" & Neighbourhood != "Landmrk")
Total.Porch.Area <- ftrain$Wood.Deck.Area + ftrain$Open.Porch.Area + ftrain$Enclosed.Porch.Area + ftrain$Scrn.Porch.Area    

## Remove Identifier variables and sale prices 
ftest <- ctest[,3:82]
ftest$Sale.Price <- replace(ftest$Sale.Price, !is.na(ftest$Sale.Price), " ")


# Select Features ---------------------------------------------------------

## Correlation 
set.seed(1)
ftrain.numeric <- ftrain%>% select_if(is.numeric)
correlation.matrix <- cor(ftrain.numeric)
strong.correlation <- findCorrelation(correlation.matrix, cutoff=0.7)
print(strong.correlation)

### strong.correlation - Sale Price, Floor 1 Area, Living Area Above Grade, Garage Cars, and Total Porch Area 

## Dimensionality Reduction (FSelector)
library(FSelector)
weights.features.ig <- information.gain(Sale.Price~., ftrain)
cutoff.biggest.diff(weights.features.ig)
cutoff.k.percent(weights.features.ig, 0.1)
top.features.ig <- cutoff.k(weights.features.ig, 10)
print(top.features.ig)

# Models  -----------------------------------------------------------------

train <- ftrain 
test <- ftest 

set.seed(2)
model1 <- lm(Sale.Price ~ Liv.Area.AbvGr + Floor1.Area + Neighbourhood + Overall.Qual + Bsmt.Fin1.Area + Total.Bsmt.Area + 
               MS.Subclass + Floor2.Area + Bsmt.Fin.Type1 + Lot.Area, data = train)
summary(model1)

predictions1 <- model1 %>% predict(test)


# RSquared1 <- R2(predictions1, test$Sale.Price)
# RMSE1 <- RMSE(predictions1, test$Sale.Price)
# MAE1 <- MAE(predictions1, test$Sale.Price)

## Error - X must be numeric 











