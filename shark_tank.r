set.seed(1)

# Step 0. Install libraries and import data ---------------------------------------
load.libraries <- c("gridExtra", "corrplot", "GGally", "ggplot2", "dplyr","car","boot","plm","lmtest","ggpubr", "methods", "stargazer", "caTools", "randomForest", "rms", "tree", "rpart", "rpart.plot", "gbm", "ggfortify")
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
for(libs in install.lib) install.packages(libs, dependences = TRUE)
sapply(load.libraries, require, character = TRUE)

shark = read.csv("shark.csv")
shark = data.frame(shark)
ref = shark
attach(shark)

# Step 1. Data Preprocessing ---------------------------------------
# 1.(a) Make "deal" and "MultipleEntrepreneurs" numerical
shark$deal = ifelse(deal == TRUE, 1, 0)
shark$Multiple.Entreprenuers = ifelse(Multiple.Entreprenuers == TRUE, 1, 0)

# 1.(b) Shrink the scale of "valuation" and "askedFor"
shark <- transform(shark, valuation = valuation / 1000)
shark <- transform(shark, askedFor = askedFor / 1000)

# 1.(c) Derived information: Has website or not "hasWeb" 
shark <- transform(shark, hasWeb = ifelse(grepl("http", website, ignore.case = TRUE), 1, 0))

# 1.(c) Derived information: Length of description "descriptionLen"
shark <- transform(shark, descriptionLen = nchar(description))

# 1.(d) Aggregate and dummify sporadic predictors: "location" -> "region" (i.e. New York, NY -> Middle Atlantic)
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}
shark <- transform(shark, location = substrRight(location,2))
locationMapping = read.csv("/Users/ylfaliang/Documents/MMA/Materials/2022 Fall/MGSC661 - Multivariate Statistical Analysis/Final/region.csv")
locationMapping = data.frame(locationMapping)
shark = merge(x = shark, y = locationMapping, by = "location")
attach(shark)

# 1.(d) Aggregate and dummify sporadic predictors: "category"
categoryMapping = read.csv("/Users/ylfaliang/Documents/MMA/Materials/2022 Fall/MGSC661 - Multivariate Statistical Analysis/Final/category.csv")
categoryMapping = data.frame(categoryMapping)
shark = merge(x = shark, y = categoryMapping, by = "category")
shark$category = shark$group
attach(shark)

# 1.(e) Dummification: "region"
regionList = unique(region)
for (i in regionList){
  colname = paste("region",i,sep = ".")
  eval(parse(text=paste("shark$",colname," <- ifelse(shark$region == i, 1, 0)", sep="")))
}
attach(shark)

# 1.(e) Dummification: "category"
categoryList = unique(category)
for (i in categoryList){
  colname = gsub(" ", "", i)
  colname = gsub("-", "", colname)
  colname = gsub("'", "", colname)
  colname = paste("category",colname,sep = ".")
  eval(parse(text=paste("shark$",colname," <- ifelse(shark$category == i, 1, 0)", sep="")))
}
attach(shark)

# 1.(e) Dummification: "shark"
sharkList <- c()
for (i in c("shark1","shark2","shark3","shark4","shark5")){
  sharkList <- append(sharkList,unique(shark[c(i)])[,1])
}
sharkList <- unique(sharkList)

for (i in sharkList){
  colname = gsub(" ", "", i)
  colname = gsub("'", "", colname)
  eval(parse(text=paste("shark <- transform(shark, shark.", colname, "= ifelse(grepl(i,shark1,ignore.case = TRUE), 1, 
                                       ifelse(grepl(i,shark2,ignore.case = TRUE), 1, 
                                              ifelse(grepl(i,shark3,ignore.case = TRUE), 1, 
                                                     ifelse(grepl(i,shark4,ignore.case = TRUE), 1, 
                                                            ifelse(grepl(i,shark5,ignore.case = TRUE), 1, 0))))))", sep="")))
}
attach(shark)

# 1.(f) Extra information: "US.viewers" and "season.progress"
viewersMapping = read.csv("viewers.csv")
viewersMapping = data.frame(viewersMapping)
shark = merge(x = shark, y = viewersMapping, by = "episode.season")
attach(shark)

# (Prepare another dataset for PCA of viewers)
dropList <- c("description","entrepreneurs","website","shark1","shark2","shark3","shark4","shark5","title","season","location","group","region")
sharkPCA = shark[,!(names(shark) %in% dropList)]

# 1.(g) Drop redundant and intuitively insignificant columns
dropList <- c("description","entrepreneurs","website","shark1","shark2","shark3","shark4","shark5","title","episode.season","season","location","category","group","region","US.viewers")
shark = shark[,!(names(shark) %in% dropList)]
attach(shark)

# 1.(h) Relationship between numerical predictors and response
numerical <- c("askedFor","exchangeForStake","valuation","descriptionLen")
tag = c("Money Asked","Exchange for Stake", "Valuation of Entrepreneurs", "Product Description Length")
for (i in c(1:length(numerical))){
  eval(parse(text=paste("plot = ggplot(shark, aes(y=deal, x=",numerical[i],"))",sep="")))
  scatter = geom_point(cex = 1,aes(colour = factor(deal)))
  line = geom_smooth(method="glm", formula=y~x, method.args=list(family=binomial), lwd = 0.5)
  plot+scatter+line+labs(color='Is it a Deal?')+scale_color_manual(labels = c("No", "Yes"),values = c("#EF5350", "#8BC34A"))+theme(text = element_text(size = 6),axis.ticks = element_line(colour = "white"))+xlab(tag[i])
  ggsave(path="plots/",filename=paste("relationship_response~",numerical[i],".png",sep=""),width = 3, height = 2)
}

# 1.(i) Run random forest to check the importance of all predictors 
shark$deal <- as.factor(shark$deal)
command <- ""
for (i in names(shark)[c(2:length(names(shark)))]){
  command = paste(command, i, sep = "+")
}
command = substr(command, 2, nchar(command))
command = paste("deal", command, sep = "~" )
eval(parse(text=paste("firstForest=randomForest(",command,", ntree=500, data=shark, importance=TRUE, na.action = na.omit)", sep="")))

imp <- importance(firstForest)
impLabel <- imp[order(imp[,3], decreasing = TRUE),]
impLabel <- impLabel[-c(31:37),]
impLabel <- impLabel[order(impLabel[,3], decreasing = FALSE),]
stargazer(imp[order(imp[,3], decreasing = TRUE),], type = "latex")
varImpPlot(firstForest, labels = c("Shark: Daymond John","Shark: John Paul de Joria","Region: New England","Shark: Barbara Corcoran","Region: West North Central","Category: Entertainment",
                                   "Multiple Entrepreneurs","Region: West South Central","Shark: Steve Tisch","Shark: Kevin O'Leary","Region: East North Central","Shark: Kevin Harrington",
                                   "Shark: Robert Herjavec","Valuation of Entrepreneurs","Shark: Nick Woodman","Episode Number","Seanson Progress","Shark: Jeff Foxworthy","Money Asked",
                                   "Shark: Mark Cuban","Category: Food and Drinks","Category: Health and Well-Being","Category: Tools","Category: Baby-Related","Region: South Atlantic",
                                   "Category: Fashion and Clothing","Category: Services","Product Description Length","Available Webpages","Exchange for Stake"), 
           main = "Variable Importance via Initial Random Forest", bg = ifelse(impLabel[-c(31:37),3]<=0,"red","black"))
dropList <- rownames(subset(imp, imp[,3] <= 0))
shark = shark[,!(names(shark) %in% dropList)]
attach(shark)

# 1.(j) Outliers
outlierList <- c()
for (i in numerical){
  eval(parse(text=paste("logit = glm(deal~",i,", family = 'binomial')",sep="")))
  print(i)
  print(outlierTest(logit))
  outlierList <- append(outlierList,names(outlierTest(logit)$bonf.p))
}
outlierList <- strtoi(outlierList[!duplicated(outlierList)])
outliers <- shark[outlierList, ]
stargazer(ref[rownames(outliers),c(17:18,1,4,6:10)],type = "latex", summary = F, rownames = FALSE)
shark <- shark[-outlierList, ]
attach(shark)

# 1.(k) Non-linearity check
logit = glm(deal~episode+askedFor+exchangeForStake+valuation+descriptionLen+season.progress, family = "binomial")
residualPlots(logit, main = "Residual Plots")

# Find optimal degree for "askedFor"
logit_a1 = glm(deal~askedFor, family = "binomial", data = shark)
logit_a2 = glm(deal~poly(askedFor,2), family = "binomial", data = shark)
logit_a3 = glm(deal~poly(askedFor,3), family = "binomial", data = shark)
logit_a4 = glm(deal~poly(askedFor,4), family = "binomial", data = shark)
logit_a5 = glm(deal~poly(askedFor,5), family = "binomial", data = shark)
anova(logit_a1, logit_a2, logit_a3, logit_a4, logit_a5) # d = 1

# Find optimal degree for "valuation"
logit_v1 = glm(deal~valuation, family = "binomial", data = shark)
logit_v2 = glm(deal~poly(valuation,2), family = "binomial", data = shark)
logit_v3 = glm(deal~poly(valuation,3), family = "binomial", data = shark)
logit_v4 = glm(deal~poly(valuation,4), family = "binomial", data = shark)
logit_v5 = glm(deal~poly(valuation,5), family = "binomial", data = shark)
anova(logit_v1, logit_v2, logit_v3, logit_v4, logit_v5) # d = 1

# Step 2. Models ---------------------------------------
# 2.(a) Logistic Regression 
command <- ""
for (i in names(shark)[c(2:length(names(shark)))]){
    command = paste(command, i, sep = "+")
}
command = substr(command, 2, nchar(command))
command = paste("deal", command, sep = "~" )
eval(parse(text=paste("mylogit=glm(",command,", family = 'binomial')", sep="")))
summary(mylogit)

# Final choice
mylogit = glm(deal~askedFor+exchangeForStake+descriptionLen+hasWeb+category.Services+category.HealthandWellBeing, family = "binomial")
summary(mylogit)
stargazer(mylogit, type = "latex")
mylogit = lrm(formula = deal~askedFor+exchangeForStake+descriptionLen+hasWeb+category.Services+category.HealthandWellBeing, data = shark)
mylogit

# 2.(b) Tree 
# Find the optimal cp using a single tree
shark$deal <- as.factor(shark$deal)
command <- ""
for (i in names(shark)[c(2:length(names(shark)))]){
  command = paste(command, i, sep = "+")
}
command = substr(command, 2, nchar(command))
command = paste("deal", command, sep = "~" )
eval(parse(text=paste("myoverfittedtree = rpart(",command,",control=rpart.control(cp=0.0001))", sep="")))
rpart.plot(myoverfittedtree)
printcp(myoverfittedtree)
plotcp(myoverfittedtree)
opt_cp = myoverfittedtree$cptable[which.min(myoverfittedtree$cptable[,"xerror"]),"CP"]

eval(parse(text=paste("mytree = rpart(",command,",control=rpart.control(cp=",opt_cp,"))", sep="")))
rpart.plot(mytree,tweak = 3)

# 2.(c) Random Forest
set.seed(1)
eval(parse(text=paste("myforest = randomForest(",command,", ntree=6000, data=shark, importance=TRUE, na.action = na.omit, do.trace=1000)", sep="")))
myforest
importance(myforest)
varImpPlot(myforest)

# 2.(d) Boosting
shark$deal <- as.character(shark$deal)
attach(shark)
set.seed(1)
eval(parse(text=paste("boosted = gbm(",command,", distribution = 'bernoulli', n.trees=3000, interaction.depth=4, verbose=TRUE)", sep="")))
summary(boosted)

predicted_deal = predict(boosted,type="response", newdata=shark, n.trees=10000)
shark$deal <- as.numeric(shark$deal)
attach(shark)
mean((predicted_deal-deal)^2)

pred = ifelse(predict(boosted,type="response", newdata=shark, n.trees=10000)>0.5,1,0)
table(pred,shark$deal)

# 2.(e) PCA - to investigate what predictors affect the views most
tablePCA = cbind(aggregate(sharkPCA[,c(2)],by=sharkPCA["episode.season"], FUN = "n_distinct"),
                 aggregate(sharkPCA[,c(30,35,38)],by=sharkPCA["episode.season"], FUN = "max"),
                 aggregate(sharkPCA[,c(3,5,6,7,40,41)],by=sharkPCA["episode.season"], FUN = "mean"))
tablePCA = data.frame(tablePCA[,!(names(tablePCA) %in% "episode.season")])
tablePCA = tablePCA[,which(apply(tablePCA, 2, var) != 0)]
names(tablePCA) <- c("Category Count","Lori Greiner","Mark Cuban","Kevin Harrington","Deals",
                     "Avg. Money Asked","Avg. Exchange for Stake","Avg. Entrepreneur Valuation","Viewers", "Season Progress")

varPCA = tablePCA[,-c(9)]
labPCA = tablePCA[,c(9)]
pca_shark = prcomp(na.omit(varPCA), scale=TRUE)

autoplot(pca_shark, data=na.omit(tablePCA), loadings=TRUE, col="Viewers", loadings.label.size=4,
         loadings.label=TRUE, loadings.colour="#607D8B", loadings.label.colour="#455A64")+scale_color_gradientn(colors=c("#0097A7","#FFE082","#FF3333"), values = c(0,0.5,1))
