setwd("./Market Response/")
# getwd()
library(readxl)
library(ggplot2)
library(skimr)
library(DataExplorer)
library(dplyr)
library(Hmisc)
library(corrplot)
library(PerformanceAnalytics)
library(coefplot)
library(gpairs)
library(lmtest)
library(caret)
library(MLmetrics)
data = read_excel("Data.xls")
str(data)
sum(is.na(data))

# What is the market share of the brand, in value and in volume?

##### Market shares in Chains 
colnames(data)
data%>%dplyr::select(3:10)%>%summarise_all(funs(sum))
pie_data=data%>% group_by(Chain)%>% summarise(total = sum(REXONASales))
pie_data_p=pie_data%>% mutate(total_p= round(total/sum(total),2))
pie_data_p
mycols <- c("#0073C2FF", "#EFC000FF", "#868686FF", "#CD534CFF","#CC79A7")
ggplot(pie_data_p, aes(x = "", y = total_p, fill = Chain)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = total_p, label = paste0(round(total_p*100), "%")),position = position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = mycols) +
  theme_void()


##### Market shares of brands in total market 

pie_share= data%>%dplyr::select(3:10)%>%colSums()
pie_share=as.data.frame(t(pie_share))
pie_share=as.data.frame(t(pie_share))
colnames(pie_share)='sales_volume'
pie_share
pie_share$sales_percent= round(pie_share$sales_volume/sum(pie_share$sales_volume),2)
pie_share <- cbind(brands = rownames(pie_share), pie_share)
rownames(pie_share) <- 1:nrow(pie_share)

mycols2 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
             "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
ggplot(pie_share, aes(x = "", y = sales_percent, fill = brands)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = sales_percent, label = paste0(round(sales_percent*100), "%")),position = position_stack(vjust = 0.5), color = "black")+
  scale_fill_manual(values = mycols2) +
  theme_void()

##################Is there an evolution in that market share?
 
odd_numbers= data%>% filter(REXONASales>200)



   
####### Evolution of sales in each retailer



data_albert= data%>% filter(Chain=="ALBERT HEIJN")%>%dplyr::select(1:10)
data_albert$WEEK=(seq(1:nrow(data_albert)))
data_c= data%>% filter(Chain=="C-1000")%>%dplyr::select(1:10)
data_c$WEEK=(seq(1:nrow(data_c)))

data_jumbo= data%>% filter(Chain=="JUMBO")%>%dplyr::select(1:10)
data_jumbo$WEEK=(seq(1:nrow(data_jumbo)))

data_edah= data%>% filter(Chain=="EDAH")%>%dplyr::select(1:10)
data_edah$WEEK=(seq(1:nrow(data_edah)))

data_super= data%>% filter(Chain=="SUPER DE BOER")%>%dplyr::select(1:10)
data_super$WEEK=(seq(1:nrow(data_super)))


data_total= rbind(data_albert,data_c,data_jumbo,data_edah,data_super) # this is market sales
data_total%>%filter(Chain=='ALBERT HEIJN')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_total%>%filter(Chain=='C-1000')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()

data_total%>%filter(Chain=='JUMBO')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_total%>%filter(Chain=='EDAH')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_total%>%filter(Chain=='SUPER DE BOER')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
####### 
####### Evolution of market share in each retailer

data_total_n= data_total%>%dplyr:: select(c(-1,-2))
data_total_nn=data_total%>%dplyr::select(c(1,2))
data_total_percent= round(data_total_n/rowSums(data_total_n),2)
data_share = cbind(data_total_nn,data_total_percent)
View(data_share)  # This is market shares

data_share%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_share%>%filter(Chain=='ALBERT HEIJN')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_share%>%filter(Chain=='C-1000')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()

data_share%>%filter(Chain=='JUMBO')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_share%>%filter(Chain=='EDAH')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()
data_share%>%filter(Chain=='SUPER DE BOER')%>%ggplot( aes(x=WEEK, y=REXONASales)) +
  geom_line()

###### ODD NUMBERS
odd_numbers= data_share%>% filter(REXONASales>0.4 |REXONASales<0.1 )
View(odd_numbers)

rexo_mean=mean(data$REXONASales)
rexo_std= sd(data$REXONASales)
odd_numbers= data%>% filter(REXONASales>rexo_mean+(3*(rexo_std)) |REXONASales< rexo_mean-(1*(rexo_std)))
View(odd_numbers)

rexo_mean=mean(data$REXONADISP)
rexo_std= sd(data$REXONADISP)
odd_numbers= data%>% filter(REXONADISP>rexo_mean+(3*(rexo_std)) |REXONADISP< rexo_mean-(1*(rexo_std)))
View(odd_numbers)

rexo_mean=mean(data$REXONAFEAT)
rexo_std= sd(data$REXONAFEAT)
odd_numbers= data%>% filter(REXONAFEAT>rexo_mean+(3*(rexo_std)) |REXONAFEAT< rexo_mean-(1*(rexo_std)))
View(odd_numbers)




#How does this market share differ between retailers?
data_share%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(REXONASales))  #Volume


data$rexona_value = (data$REXONASales*data$REXONAPrice)
data$nivea_value = (data$NIVEASales*data$NIVEAPrice)
data$dove_value = (data$DOVESales*data$DOVEPrice)
data$fa_value = (data$FASales*data$FAPrice)
data$vogue_value = (data$VOGUESales*data$VOGUEPrice)
data$axe_value = (data$AXESales*data$AXEPrice)
data$a8X4_value = (data$`8X4Sales`*data$`8X4Price`)
data$sanex_value = (data$SANEXSales*data$SANEXPrice)


## Calculations for market value 
data_value= round(data[51:58]/rowSums(data[51:58]),2)  
data_value_final= cbind(data[1:2],data_value)
data_value_final%>% group_by(Chain)%>%summarise(mean(rexona_value)) 

b=data_value_final%>%summarise(mean(rexona_value))
c=data_value_final%>%summarise(mean(axe_value))
d= data_value_final%>%summarise(mean(nivea_value))
e= data_value_final%>%summarise(mean(fa_value))
f= data_value_final%>%summarise(mean(a8X4_value))
g= data_value_final%>%summarise(mean(vogue_value))
h= data_value_final%>%summarise(mean(sanex_value))
k= data_value_final%>%summarise(mean(dove_value))



b+c+d+e+f+g+h+k #Sanity check to show sum of value shares is 1

###Evolution in total market 
data_value_final$WEEK= gsub("W","",data$WEEK)
data_value_final$WEEK=as.numeric(data_value_final$WEEK)
evolution=data_value_final%>% group_by(WEEK)%>% summarise(median(rexona_value))
colnames(evolution)=c("Week","Rexona_Market_Share")
ggplot(evolution, aes(x=Week)) + 
  geom_line(aes(y = Rexona_Market_Share), color = "darkred")  
data$sum= rowSums(data[3:10])

summarize = data%>%dplyr::select(c(2,51))%>%group_by(WEEK)%>% summarise(Score = sum(sum))
summarize$WEEK=gsub("W","",summarize$WEEK)
summarize$WEEK=as.numeric(summarize$WEEK)
colnames(summarize)=c("Week","Market_Size_Volume")

ggplot(summarize, aes(x=Week)) + 
  geom_line(aes(y = Market_Size_Volume), color = "darkred")  



#How does the brand's price level and promotional support levels compare to the
#levels observed with competing brands? Are the insights retailer specific?

##Price Display Feature Feature&Display
a= aggregate (data[,43:50],list(data$Chain),mean)
colMeans(a[,2:8])

#Can you visually detect evidence of a price war among the brands and/or
#supermarkets? If yes, explain how you got to that conclusion.

colnames(data)
data_albert= data%>% filter(Chain=="ALBERT HEIJN")
data_c= data%>% filter(Chain=="C-1000")
data_jumbo= data%>% filter(Chain=="JUMBO")
data_edah= data%>% filter(Chain=="EDAH")
data_super= data%>% filter(Chain=="SUPER DE BOER")
data_albert$WEEK=(seq(1:nrow(data_albert)))
data_c$WEEK=(seq(1:nrow(data_c)))
data_jumbo$WEEK=(seq(1:nrow(data_jumbo)))
data_edah$WEEK=(seq(1:nrow(data_edah)))
data_super$WEEK=(seq(1:nrow(data_super)))



data_cor= rbind(data_albert,data_c, data_jumbo,data_edah,data_super)

Rexona = diff(data_cor$REXONAPrice,1,1)
Dove = diff(data_cor$DOVEPrice,1,1)
Fa= diff(data_cor$FAPrice,1,1)
Nivea = diff(data_cor$NIVEAPrice,1,1)
Sanex = diff(data_cor$SANEXPrice,1,1)
Vogue = diff(data_cor$VOGUEPrice,1,1)
`8X4Price` = diff(data_cor$`8X4Price`,1,1)
df = data.frame(Rexona, Dove,Fa, Nivea, Sanex, Vogue,`8X4Price` )

cormat<- rcorr(as.matrix(df))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cormat$r, cormat$P)

res= cor(df)

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)


# Is there any evidence of seasonality in the market shares? Describe what evidence
# you use to make the seasonality assessment. Is there evidence of an up-or
# downward trend in the share of your selected brand?

### Seasonality 
#Based on time serries chart there is no seasonality.

library(data.table)

rexona_data = data[,colnames(data) %like% "REXONA" ]



############## MODEL1: LINEAR  ###################


data$WEEK=(seq(1:nrow(data)))
colnames(data)[43]='FADF'
colnames(data)[44]='NIVEADF'
colnames(data)[45]='REXONADF'
colnames(data)[46]='SANEXDF'
colnames(data)[47]='VOGUEDF'
colnames(data)[48]='a8X4DF'
colnames(data)[49]='DOVEDF'
colnames(data)[50]='AXEDF'


data_date= data%>% filter(Chain=='ALBERT HEIJN')%>% dplyr::select(c(1,2))

test = round(data[3:10]/rowSums(data[3:10]),2)
test= test[1:124,]
data_c=data%>% filter(Chain=='ALBERT HEIJN')%>% dplyr::select(11:50)

data1 = cbind(data_date,test,data_c)


View(data1)
#######PRICE WAR
colors <- c("Rexona" = "darkred", "Dove" = "steelblue")
ggplot(data1, aes(x=WEEK)) + 
  geom_line(aes(y = REXONAPrice), color = "darkred") + 
  geom_line(aes(y = DOVEPrice), color="steelblue", linetype="twodash")+ 
  labs(x = "Weeks",  y = "Price", color = "Legend")   ## Legend
  # geom_line(aes(y = NIVEAPrice), color = "#0073C2FF")+
  # geom_line(aes(y = FAPrice), color = "#EFC000FF")+
  # geom_line(aes(y = AXEPrice), color = "#868686FF")+
  # geom_line(aes(y = `8X4Price`), color = "#CD534CFF")+
  # geom_line(aes(y = VOGUEPrice), color = "#CC79A7")

plot(data1$WEEK, data1$REXONAPrice, type = "b", frame = FALSE, pch = 19, 
     col = "red", xlab = "Week", ylab = "Price")
# Add a second line
lines(data1$WEEK, data1$DOVEPrice, pch = 18, col = "blue", type = "b", lty = 2)
# Add a legend to the plot
op <- par(cex = 0.6)
legend("topright", legend=c("Rexona Price", "Dove Price"),
       col=c("red", "blue"), lty = 1:2, pch=1, bty = 'n')

cormat<- rcorr(as.matrix(data1[,11:20]))  #This shows the main price wars is for DOVE, VOGUE,SANEX
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cormat$r, cormat$P)

market_share_albert = data_share%>% filter(Chain=="ALBERT HEIJN")
cormat<- rcorr(as.matrix(market_share_albert[,3:10]))  #This shows the main competitors is AXE
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cormat$r, cormat$P)

corrplot(cor(market_share_albert[,3:10]),type= 'upper', method = 'number')

############################## MODELS##########################################


data1_log= log(data1[,c(-1,-2)])
data1_log= cbind(data1_log[1:8],data1[11:50])
data1_scaled= scale(data1[,-1])
data1_scaled_log= scale(data1_log)
data1_scaled = as.data.frame(data1_scaled)
data1_scaled_log = as.data.frame(data1_scaled_log)

gpairs(data1[3:10])
gpairs(data1_log[2:9])
gpairs(data1_scaled_log[1:8])

# Split the data into training and test set
train.data  <- data1[1:100, ]
test.data <- data1[101:124, ]
chart.Correlation(train.data[,c(6,14,29,45,37,34,19)], histogram=TRUE, pch=19)  # Transforing is necessary 

m1 = lm(REXONASales~REXONADISP+REXONAFEAT+REXONADF+REXONAPrice+DOVEPrice+AXEDISP,train.data) #R2=0.77
summary(m1)

RSS <- c(crossprod(m1$residuals))

MSE <- RSS / length(m1$residuals)

  
RMSE1 <- sqrt(MSE)


sig2 <- RSS / m1$df.residual

predictions1 <- m1 %>% predict(train.data)

data.frame(
  RMSE = RMSE(predictions1, train.data$REXONASales),
  R2 = R2(predictions1, train.data$REXONASales)
)
MAPE(predictions1,train.data$REXONASales)




coefplot(m1, intercept= F,outerCI=1.96, lwdOuter = 1.5,
         ylab= "Variables",xlab= 'Association with Rexona market share')
plot(m1)

colMeans( data[,colnames(data) %like% "AXE"])

#Multicolinearity test 
car::vif(m1)
  cormat<- rcorr(as.matrix(train.data[,c("REXONADISP","REXONAFEAT",
                                         'REXONADF','REXONAPrice',
                                         'DOVEPrice','AXEDISP')]))  
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  
  flattenCorrMatrix(cormat$r, cormat$P)
#Heterosedasticity test 
gqtest(REXONASales~REXONADISP+REXONAFEAT+REXONADF+REXONAPrice+DOVEPrice+AXEDISP,data=train.data)  
  
bptest(m1)
train.data$resi <- m1$residuals
ggplot(data = train.data, aes(y = resi, x = REXONASales)) + geom_point(col = 'blue') + geom_abline(slope = 0)
# Out of sample performance
predictions <- m1 %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$REXONASales),
  R2 = R2(predictions, test.data$REXONASales)
)
MAPE(predictions,test.data$REXONASales)




#########Multiplicative model####################
m2 = lm(log(REXONASales)~log(REXONADISP+1)+log(REXONAFEAT+1)+log(REXONADF+1)+log(AXEDISP+1)+log(DOVEPrice)+log(REXONAPrice),train.data) #R2=0.72
summary(m2)

lnsales = -1.3196 + 1.1117 * log(REXONADISP + 1) -1.4443 * log(REXONAFEAT + 1) +
  2.5353 * log(REXONADF + 1) -0.6392 * log(AXEDISP + 1) + 0.9476 * log(DOVEPrice) -1.4025 * log(REXONAPrice)

coefplot(m2, intercept= F,outerCI=1.96, lwdOuter = 1.5,
         ylab= "Variables",xlab= 'Association with Rexona market share')


#Heterosedasticity test 
gqtest(log(REXONASales)~log(REXONADISP+1)+log(REXONAFEAT+1)+log(REXONADF+1)+log(AXEDISP+1)+log(DOVEPrice)+log(REXONAPrice),data=train.data)


train.data <- train.data %>%
  mutate(lnsales = -1.3196 + 1.1117 * log(REXONADISP + 1) -1.4443 * log(REXONAFEAT + 1) +
           2.5353 * log(REXONADF + 1) -0.6392 * log(AXEDISP + 1) + 0.9476 * log(DOVEPrice) -1.4025 * log(REXONAPrice))

train.data <- train.data %>%
  mutate(predicted_sales = exp(1) ^ lnsales)

train.data <- train.data %>%
  mutate(se = (REXONASales - predicted_sales)^2)

train.data <- train.data %>%
  mutate(ape = abs((REXONASales - predicted_sales)/REXONASales))

mape_train = sum(train.data$ape)/length(train.data$ape)
mape_train

rmse_train <- sqrt(sum(train.data$se)/length(train.data$se))
rmse_train

cor(train.data$REXONASales,train.data$predicted_sales)^2


test.data <- test.data %>%
  mutate(lnsales = -1.3196 + 1.1117 * log(REXONADISP + 1) -1.4443 * log(REXONAFEAT + 1) +
           2.5353 * log(REXONADF + 1) -0.6392 * log(AXEDISP + 1) + 0.9476 * log(DOVEPrice) -1.4025 * log(REXONAPrice))

test.data <- test.data %>%
  mutate(predicted_sales = exp(1) ^ lnsales)

test.data <- test.data %>%
  mutate(se = (REXONASales - predicted_sales)^2)

test.data <- test.data %>%
  mutate(ape = abs((REXONASales - predicted_sales)/REXONASales))

mape_test = sum(test.data$ape)/length(test.data$ape)
mape_test

rmse_test <- sqrt(sum(test.data$se)/length(test.data$se))
rmse_test

cor(test.data$REXONASales,test.data$predicted_sales)^2


#Multicolinearity test 
car::vif(m2)
cormat<- rcorr(as.matrix(train.data[,c("REXONADISP","REXONAFEAT",
                                       'REXONADF','REXONAPrice',
                                       'DOVEPrice','AXEDISP')]))  
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flattenCorrMatrix(cormat$r, cormat$P)
#Heterosedasticity test 
gqtest(lm(log(REXONASales)~log(REXONADISP+1)+log(REXONAFEAT+1)+log(REXONADF+1)+log(AXEDISP+1)+log(DOVEPrice)+log(REXONAPrice),train.data)) 

bptest(m2)
train.data$resi <- m2$residuals
ggplot(data = train.data, aes(y = resi, x = REXONASales)) + geom_point(col = 'blue') + geom_abline(slope = 0)
# Out of sample performance
predictions <- m2 %>% predict(test.data)
data.frame(
  RMSE = RMSE(predictions, test.data$REXONASales),
  R2 = R2(predictions, test.data$REXONASales)
)
MAPE(predictions,test.data$REXONASales)








############  Range Constrainst
m3 = lm(log(REXONASales)~ REXONADISP+REXONAFEAT+REXONADF+REXONAPrice+DOVEPrice+AXEDISP,train.data) #R2=0.77
summary(m3)



coefplot(m3, intercept= F,outerCI=1.96, lwdOuter = 1.5,
         ylab= "Variables",xlab= 'Association with Rexona market share')


#Heterosedasticity test 
gqtest(log(REXONASales)~REXONADISP+REXONAFEAT+REXONADF+REXONAPrice+DOVEPrice+AXEDISP,data=train.data)



train.data <- train.data %>%
  mutate(lnsales_2 = -1.12278 + 0.75306 * REXONADISP -0.72086 * REXONAFEAT +
           1.83470 * REXONADF -0.76310 * REXONAPrice + 0.51854* DOVEPrice -0.44669 * AXEDISP)


train.data <- train.data %>%
  mutate(predicted_sales_2 = exp(1)^lnsales_2)

train.data <- train.data %>%
  mutate(se_2 = (REXONASales - predicted_sales_2)^2)

train.data <- train.data %>%
  mutate(ape_2 = abs((REXONASales - predicted_sales_2)/REXONASales))

mape_train_2 = sum(train.data$ape_2)/length(train.data$ape_2)
mape_train_2

rmse_train_2 <- sqrt(sum(train.data$se_2)/length(train.data$se_2))
rmse_train_2

cor(train.data$REXONASales,train.data$predicted_sales_2)^2


test.data <- test.data %>%
  mutate(lnsales_2 = -1.12278 + 0.75306 * REXONADISP -0.72086 * REXONAFEAT +
           1.83470 * REXONADF -0.76310 * REXONAPrice + 0.51854* DOVEPrice -0.44669 * AXEDISP)

test.data <- test.data %>%
  mutate(predicted_sales_2 = exp(1) ^ lnsales_2)

test.data <- test.data %>%
  mutate(se_2 = (REXONASales - predicted_sales_2)^2)

test.data <- test.data %>%
  mutate(ape_2 = abs((REXONASales - predicted_sales_2)/REXONASales))

mape_test_2 = sum(test.data$ape_2)/length(test.data$ape_2)
mape_test_2

rmse_test_2 <- sqrt(sum(test.data$se_2)/length(test.data$se_2))
rmse_test_2

cor(test.data$REXONASales,test.data$predicted_sales_2)^2

