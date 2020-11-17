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
library(MASS)
library(lmtest)
library(caret)
data = read_excel("Data.xls")
str(data)
sum(is.na(data))
# skim(data) #Choosing ###Rexona### as market leader 
# DataExplorer::create_report(data) 


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



chart.Correlation(data[,c(-1,-2)], histogram=TRUE, pch=19)

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

##For total sales of rexona
# data$WEEK=gsub("W","",data$WEEK)
# data$WEEK=as.numeric(data$WEEK)
# evolution_1=data%>% group_by(WEEK)%>% summarise(mean(rexona_value))
# colnames(evolution_1)=c("WEEK","rexona_value")
# ggplot(evolution_1, aes(x=WEEK)) + 
#   geom_line(aes(y = rexona_value), color = "darkred")


#How does the brand's price level and promotional support levels compare to the
#levels observed with competing brands? Are the insights retailer specific?



#############Price
data_price= cbind(data[1:2],data[19:26]-data[11:18])
colnames(data_price)[9]='a8X4RPrice'
data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(REXONARPrice)) 
data_price%>% summarise(mean(REXONARPrice)) 

data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(DOVERPrice))
data_price%>%dplyr::select(3:10)%>%summarise(mean(DOVERPrice)) 

data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(FARPrice)) 
data_price%>% summarise(mean(FARPrice)) 

data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(NIVEARPrice)) 
data_price%>% summarise(mean(NIVEARPrice)) 

data_price%>% group_by(Chain)%>%dplyr::select(3:10)%>%summarise(mean(SANEXRPrice))
data_price%>% summarise(mean(SANEXRPrice))


data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(VOGUERPrice))
data_price%>%summarise(mean(VOGUERPrice))

data_price%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(a8X4RPrice))
data_price%>%summarise(mean(a8X4RPrice)) 


###############Promotion
#####DISPLAY
data_promo= cbind(data[1:2],data[27:50])
colnames(data_promo)[8]='a8X4DISP'
colnames(data_promo)[16]='a8X4FEAT'
colnames(data_promo)[24]='a8X4D+F'

data_promo%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(REXONADISP)) 
data_promo%>% summarise(mean(REXONADISP)) 

data_promo%>% group_by(Chain)%>%dplyr::select(3:10)%>%summarise(mean(DOVEDISP)) 
data_promo%>%summarise(mean(DOVEDISP)) 

data_promo%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(FADISP))
data_promo%>% summarise(mean(FADISP)) 

data_promo%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(NIVEADISP)) 
data_promo%>%summarise(mean(NIVEADISP)) 

data_promo%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(SANEXDISP))
data_promo%>% summarise(mean(SANEXDISP)) 

data_promo%>% group_by(Chain)%>% summarise(mean(VOGUEDISP))
data_promo%>% summarise(mean(VOGUEDISP)) 

data_promo%>% group_by(Chain)%>% dplyr::select(3:10)%>%summarise(mean(a8X4DISP)) 
data_promo%>% summarise(mean(a8X4DISP)) 

#####FEATURE



data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(REXONAFEAT)) 
data_promo%>% summarise(mean(REXONAFEAT)) 

data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(DOVEFEAT)) 
data_promo%>%summarise(mean(DOVEFEAT)) 

data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(FAFEAT))
data_promo%>% summarise(mean(FAFEAT)) 

data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(NIVEAFEAT)) 
data_promo%>% summarise(mean(NIVEAFEAT)) 

data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(SANEXFEAT))
data_promo%>% summarise(mean(SANEXFEAT)) 

data_promo%>% group_by(Chain)%>% dplyr::select(11:18)%>%summarise(mean(VOGUEFEAT))
data_promo%>% summarise(mean(VOGUEFEAT)) 

data_promo%>% group_by(Chain)%>%dplyr::select(11:18)%>%summarise(mean(a8X4FEAT)) 
data_promo%>% summarise(mean(a8X4FEAT))

### DISP AND FEATURE 

data_promo%>% group_by(Chain)%>%summarise(mean(REXONADF)) 
data_promo%>% summarise(mean(REXONADF)) 

data_promo%>% group_by(Chain)%>%summarise(mean(DOVEDF)) 
data_promo%>% summarise(mean(DOVEDF)) 

data_promo%>% group_by(Chain)%>%summarise(mean(FADF))
data_promo%>% summarise(mean(FADF)) 

data_promo%>% group_by(Chain)%>%summarise(mean(NIVEADF)) 
data_promo%>% summarise(mean(NIVEADF)) 

data_promo%>% group_by(Chain)%>%summarise(mean(SANEXDF))
data_promo%>% summarise(mean(SANEXDF)) 

data_promo%>% group_by(Chain)%>% summarise(mean(VOGUEDF))
data_promo%>% summarise(mean(VOGUEDF)) 

data_promo%>% group_by(Chain)%>% summarise(mean(a8X4DF)) 
data_promo%>% summarise(mean(a8X4DF))

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




############################## MODELS##########################################

chart.Correlation(data1[2:10], histogram=TRUE, pch=19)  # Transforing is necessary 

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
set.seed(123)
training.samples <- data1_scaled$REXONASales%>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- data1[training.samples, ]
test.data <- data1[-training.samples, ]
m1 = lm(REXONASales~REXONADISP+REXONAFEAT+REXONADF+REXONAPrice+DOVEPrice+AXEDISP,train.data) #R2=0.77
summary(m1)
coefplot(m1, intercept= F,outerCI=1.96, lwdOuter = 1.5,
         ylab= "Variables",xlab= 'Association with Rexona market share')

#Multicolinearity test 
car::vif(m1)
cormat<- rcorr(as.matrix(train.data[,c("REXONAPrice","VOGUEPrice",'SANEXFEAT')]))  #This shows the main competitors is AXE
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
bptest(m1)
train.data$resi <- m1$residuals
ggplot(data = train.data, aes(y = resi, x = REXONASales)) + geom_point(col = 'blue') + geom_abline(slope = 0)
#########Multiplicative model####################
m2 = lm(log(REXONASales+2)~log(REXONADISP+2)+log(REXONAFEAT+2)+log(REXONADF+2)+log(AXEDISP+2),data1_scaled) #R2=0.62



summary(m2)

############  Range Constrainst 
m2 = lm(log(1/(REXONASales+2))~log(1/(REXONADISP+2))+log(1/(REXONAFEAT+2))+log(1/(REXONADF+2))+log(1/(AXEDISP+2)),data1_scaled) #R2=0.62

