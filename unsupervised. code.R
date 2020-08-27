stock = read.csv("C:/Users/Lorenzo de Sario/Desktop/unsupervised/fundamentals.csv", header = TRUE, sep = ",")
summary(stock)
#replacing missing values with the mean for each column  
stock[stock==0] = NA
for(i in 1:ncol(stock)){
  stock[is.na(stock[,i]), i] <- mean(stock[,i], na.rm = TRUE)
}

#group by Ticker symbol, removing inuseful columns  
stock = aggregate(stock[,0:ncol(stock)], list(stock$Ticker.Symbol), mean )
stock = stock[, -which(names(stock) %in% c('Period.Ending', 'X', 'Ticker.Symbol', 'Year'))]
names(stock)[1] = 'Ticker'

#Exploratory data analysis
library(ggplot2)
library(reshape2)
Margin = data.frame(stock$Operating.Margin,
                    stock$Pre.Tax.Margin,
                    stock$Profit.Margin,
                    stock$Gross.Margin)

Margin = melt(Margin)
ggplot(Margin,aes(x=value, fill=variable)) + 
  geom_density(alpha=0.25)+ 
  labs(title = 'A check over the density of the margins', x='margin')+
  scale_fill_discrete(name = 'Margin', labels = c('Operating Margin', 'Pre Tax Margin', 'Profit Margin', 'Gross Margin'))
#what this different margins could mean?? why gross margin is so different? how is calculated gross margin?

#assign ticker symbol to rowname
row.names(stock) = (stock$Ticker)
stock<-stock[,-1]

#Since there are indexes and economic values($): 
#Normalize the data within 0 mean and 1 sd
data = scale(stock)
summary(data)

##PRINCIPAL COMPONENT ANALYSIS
#split in train and test since PCA is useful in the 
#exploratory data analysis and thus it could be followed by predictive models
stock_train = data[1:nrow(data),]
stock_test = data[-(1:nrow(data)),]

# Z¹ = Φ¹¹X¹ + Φ²¹X² + Φ³¹X³ + .... +Φp¹Xp
#Z² = Φ¹²X¹ + Φ²²X² + Φ³²X³ + .... + Φp2Xp

principal <- princomp(stock_train, cor = TRUE)
principal$center[1:10] #mean of the variables
principal$scale[1:10] #standard deviation of the veariables
principal$loadings[1:10, 1:5]
dim(principal$x)
summary(principal) #in order to get a cumulative portion of variance explained by the components of 95% one should consider the first 26 components.

## biplot, 
biplot(principal)
#calculating the percentage of variance explained
pr.var=principal$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
#scree plot
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
#cumulative scree plot
lot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
abline(h=1, lwd=3, col="red")

#CLUSTERING
#hierarchical clustering
set.seed(346)
#sampling 10 stock from standardized data and selecting measures of profittability
sample_stock = data[sample(nrow(stock_train), 30), ]
sample_stock = subset(sample_stock, select = c('Cash.Ratio', 'Current.Ratio', 'Quick.Ratio', 'Pre.Tax.Margin', 'Earnings.Before.Interest.and.Tax', 'Pre.Tax.ROE'))

#cityblock vs euclidean distance- complete linkage
data_cityblock = dist(sample_stock, upper = TRUE, method="manhattan")
data_euclid = dist(sample_stock, upper = TRUE, method = 'euclidean')
par(mfrow = c(1, 2))
hc_b_c <- hclust(data_cityblock, method = 'complete')
he_d <- hclust(data_euclid)
plot(hc_b_c)
plot(he_d)

#cityblock distance with average, single and complete dissimilarity 
#citiblock comparison since is the more robust measure of distance
par(mfrow = c(1, 3))
hc_b_a <- hclust(data_cityblock, method = 'average')
hc_b_s <- hclust(data_cityblock, method = 'single')
plot(hc_b_c)
plot(hc_b_a)
plot(hc_b_s)

#number of clusters for k-mean
fviz_nbclust(sample_stock, kmeans, method = "wss") +
  geom_vline(xintercept = 7, linetype = 2)+
  labs(subtitle = "Elbow method")

cutree(hc_b_a, 7)


#ward
#simil