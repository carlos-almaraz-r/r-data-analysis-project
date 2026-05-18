##Libraries to analyze the data##
##Visualization##
library("ggplot2")
library("ggthemes")
library("corrgram")
library("corrplot")
library("ggcorrplot")
library("VIM")
library("scales")
##Data management and order##
library("dplyr")
library("mice")
library("misty")
##Clustering and Multivariate##
library("fastcluster")
library("NbClust")
library("cluster")
##MCDA and decision making##
library("MCDA")
library("goalp")
library('lmtest')
##Load data sheets for analysis##
orders<-read.csv("order_july25.csv")
customers<-read.csv("new_customer25.csv")

##Data understanding##
#Check and clean data for the customer behavior#
#we check our data after cleaning and transforming#
str(orders)
head(orders)
glimpse(orders)
summary(orders)
summary(orders$revenue) 
summary(orders$past_spend)
table(orders$past_spend)
summary(orders$time_web)
table(orders$ad_channel)
table(orders$voucher)
prop.table(table(orders$voucher))
table(orders$ad_channel)
prop.table(table(orders$ad_channel))

# Leeds pallete color#
leeds_palette <- c(
  "ShingleFawn" = "#6C5432",  
  "GenoaLight"  = "#1FA2A6",
  "LeedsGreen"  = "#146264",
  "Emerald"     = "#1FA77C",
  "BrightGold"  = "#FFD84D",
  "AntiqueGold" = "#B8860B"
)

#Leeds theme#
theme_leeds_dark <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#FFD84D"), # Laser
    axis.title = element_text(face = "bold", color = "#FFD84D"),
    axis.text  = element_text(color = "#1FA2A6"),
    plot.caption = element_text(color = "#FFD84D"),
    panel.grid.major = element_line(color = "#1A1A1A"),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#1A1A1A", color = NA), # fondo general oscuro
    panel.background = element_rect(fill = "#1A1A1A", color = NA)  # fondo del panel
  )
theme_leeds <- theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, color = "#1FA2A6"), # Laser
    axis.title = element_text(face = "bold", color = "#1FA2A6"),
    axis.text  = element_text(color = "#1FA2A6"),
    plot.caption = element_text(color = "#1FA2A6"),
    panel.grid.major = element_line(color = "white"),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "white", color = NA), # fondo general oscuro
    panel.background = element_rect(fill = "white", color = NA)  # fondo del panel
  )

##We do histograms to detect skews and outliers##
ggplot(orders)+geom_histogram(aes(number_past_order), binwidth = NULL, bin = NULL) +
  labs(title = "Number of past orders",
       caption = "Data from SweetAroma",
       x = "Previous number of orders",y = "Number of Customers")

ggplot(orders %>% count(number_past_order) %>% filter(!is.na(number_past_order)),
       aes(x = factor(number_past_order), y = n)) +
  geom_col(fill = "#B8860B", color = "#FFD84D") +
  geom_text(aes(label = n), vjust = -0.5, color = "#FFD84D", size = 3.5) +
  geom_line(aes(group = 1), color = "#146264", size = 1.2) +
  geom_point(color = "#146264", size = 3) +
  labs(
    title   = "Number of past orders",
    caption = "Data from SweetAroma",
    x       = "Previous number of orders",
    y       = "Number of Customers"
  ) +
  theme_leeds_dark

ggplot(orders %>% count(number_past_order) %>% filter(!is.na(number_past_order)),
       aes(x = factor(number_past_order), y = n)) +
  geom_col(fill = "#1FA77C", color = "#1FA2A6") +
  geom_text(aes(label = n), vjust = -0.5, color = "#1FA2A6", size = 3.5) +
  geom_line(aes(group = 1), color = "#B8860B", size = 1.2) +
  geom_point(color = "#B8860B", size = 3) +
  labs(
    title   = "Number of past orders",
    caption = "Data from SweetAroma",
    x       = "Previous number of orders",
    y       = "Number of Customers"
  ) +
  theme_leeds

ggplot(orders)+geom_histogram(aes(time_web), binwidth = NULL, bin = NULL) +
  labs(title = "Time spent on the web before a purchase",
       caption = "Data from SweetAroma",
       x = "Time on web",y = "Number of Customers")

ggplot(orders, aes(x = time_web)) +
  geom_histogram(binwidth = 5, fill = "#1FA77C", color = "#1FA2A6", alpha = 0.6) +
  geom_density(color = "#B8860B", size = 1.2, adjust = 1.2) +
  geom_vline(xintercept = mean(orders$time_web, na.rm = TRUE),
             color = "#B8860B", linetype = "solid", size = 1) +
  annotate("text", 
           x = mean(orders$time_web, na.rm = TRUE) + 5, 
           y = -30, 
           label = paste("", round(mean(orders$time_web, na.rm = TRUE), 1), ""),
           color = "#1FA77C", hjust = 0, size = 4) +
  labs(
    title   = "Time spent on the web before a purchase",
    caption = "Data from SweetAroma",
    x       = "Time on web (minutes)",
    y       = "Number of Customers"
  ) +
  theme_leeds

ggplot(orders, aes(x=factor(voucher)))+
  geom_bar(fill="steelblue") +
  scale_x_discrete(
    labels=c("0"="NO", "1"="YES"),
    na.translate=FALSE #do not show NA results
  ) +
  labs(title = "Did the customer used a voucher?",
       caption = "Data from SweetAroma",
       x = "",y = "Number of Customers") +
  theme_economist()

ggplot(orders %>% count(voucher) %>% filter(!is.na(voucher)),
       aes(x = factor(voucher), y = n)) +
  geom_col(fill = "#1FA77C", color = "#1FA2A6") +
  geom_text(aes(label = n), vjust = -0.5, color = "#1FA2A6", size = 3.5) +
  scale_x_discrete(
    labels = c("0" = "NO", "1" = "YES"),
    na.translate = FALSE
  ) +
  labs(
    title   = "Did the customer use a voucher?",
    caption = "Data from SweetAroma",
    x       = "",
    y       = "Number of Customers"
  ) +
  theme_leeds

ggplot(orders)+geom_histogram(aes(past_spend), binwidth = NULL, bin = NULL) +
  labs(title = "How much has the customer spent in the past?",
       caption = "Data from SweetAroma",
       x = "How much have they spent",y = "Number of Customers")

ggplot(orders[!is.na(orders$past_spend), ], aes(x = past_spend)) +
  geom_histogram(binwidth = 5, fill = "#1FA77C", color = "#1FA2A6", alpha = 0.6) +
  geom_vline(xintercept = median(orders$past_spend, na.rm = TRUE),
             color = "#B8860B", linetype = "dotted", size = 1) +
  labs(
    title   = "Distribution of past customer spending",
    caption = "Data from SweetAroma",
    x       = "Past spend (GPB)",
    y       = "Number of Customers"
  ) +
  theme_leeds

table(orders$past_spend)
summary(orders$past_spend)

ggplot(orders, aes(x=factor(ad_channel)))+
  geom_bar(fill="steelblue") +
  scale_x_discrete(
    labels=c("1"="No Ads", "2"="Paid Search",
             "3"="Free SEO", "4"="Online Ads"),
    na.translate=FALSE #do not show NA results
  ) +
  labs(title = "Which advertisment medium was used?",
       caption = "Data from SweetAroma",
       x = "",y = "Number of Customers") +
  theme_economist()

ggplot(orders %>% filter(!is.na(ad_channel)), aes(x = factor(ad_channel))) +
  geom_bar(fill = "#1FA77C", color = "#1FA2A6") +
  geom_text(stat = "count", aes(label = ..count..), 
            vjust = -0.5, color = "#1FA2A6", size = 3.5) +
  scale_x_discrete(
    labels = c("1" = "No Ads", "2" = "Paid Search",
               "3" = "Free SEO", "4" = "Online Ads"),
    na.translate = FALSE
  ) +
  labs(
    title   = "Which advertisement medium was used?",
    caption = "Data from SweetAroma",
    x       = "",
    y       = "Number of Customers"
  ) +
  theme_leeds

ggplot(orders)+geom_histogram(aes(revenue), binwidth = NULL, bin = NULL) +
  labs(title = "How much revenue has the company got?",
       caption = "Data from SweetAroma",
       x = "Average revenue per transaction",y = "Number of transactions")

ggplot(orders[!is.na(orders$revenue), ], aes(x = revenue)) +
  geom_histogram(fill = "#1FA77C", color = "#1FA2A6", alpha = 0.7) +
  geom_vline(xintercept = mean(orders$revenue, na.rm = TRUE),
             color = "#B8860B", linetype = "dotted", size = 1)+
  labs(
    title   = "How much revenue has the company got?",
    caption = "Data from SweetAroma",
    x       = "Average revenue per transaction",
    y       = "Number of transactions"
  ) +
  theme_leeds

#we create a copy of the dataframe to have factors and numbers for graphs#
orders_corr<-orders
#factor de voucher#
orders_corr$voucher<- as.factor(orders_corr$voucher)
#factor the ad channel#
orders_corr$ad_channel<- as.factor(orders_corr$ad_channel)
#we create a correlation graph to study correlation levels#
corrgram(orders_corr)


#We do graphs to analyze dependent vs independent variables# 
ggplot(orders)+geom_point(aes(number_past_order,revenue), na.rm=TRUE)+
  labs(title = "Past orders vs Revenue",
       caption = "Data from SweetAroma",
       x = "Number of past orders",y = "Revenue")

ggplot(orders[!is.na(orders$number_past_order), ])+geom_boxplot(aes(x= factor(number_past_order),y=revenue),na.rm=TRUE)+
  labs(title = "Past orders vs Revenue",
       caption = "Data from SweetAroma",
       x = "Number of past orders",y = "Revenue")

ggplot(orders[!is.na(orders$number_past_order), ]) +
  geom_boxplot(
    aes(x = factor(number_past_order), y = revenue),
    fill = "#1FA77C",          
    color = "#B8860B",         
    outlier.colour = "#1FA2A6" 
  ) +
  labs(
    title   = "Past orders vs Revenue",
    caption = "Data from SweetAroma",
    x       = "Number of past orders",
    y       = "Revenue"
  ) +
  theme_leeds +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5), # grid principal
    panel.grid.minor = element_line(color = "#333333", size = 0.25) # grid secundario
  )

ggplot(orders)+geom_point(aes(time_web,revenue))+
  labs(title = "Time on the web vs Revenue",
       caption = "Data from SweetAroma",
       x = "Time on web",y = "Revenue")

ggplot(orders[!is.na(orders$time_web) & !is.na(orders$revenue), ]) +
  geom_point(
    aes(x = time_web, y = revenue),
    color = "#1FA77C",
    alpha = 0.7,      
    size = 2            
  ) +
  labs(
    title   = "Time on the web vs Revenue",
    caption = "Data from SweetAroma",
    x       = "Time on web",
    y       = "Revenue"
  ) +
  theme_leeds+
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5),
    panel.grid.minor = element_line(color = "#333333", size = 0.25),
  )

ggplot(orders)+geom_point(aes(voucher,revenue), na.rm=TRUE)+
  scale_x_discrete(na.translate = FALSE)+
  labs(title = "Voucher vs Revenue",
       caption = "Data from SweetAroma",
       x = "Voucher usage",y = "Revenue")
ggplot(orders)+geom_boxplot(aes(x= factor(voucher),y=revenue), na.rm=TRUE)+
  scale_x_discrete(na.translate = FALSE,
                   labels = c("0"="No", "1"="Yes"))+
  labs(title = "Voucher usage vs Revenue",
       caption = "Data from SweetAroma",
       x = "Use of voucher",y = "Revenue")

ggplot(orders[!is.na(orders$voucher) & !is.na(orders$revenue), ]) +
  geom_boxplot(
    aes(x = factor(voucher), y = revenue),
    fill = "#1FA77C",          
    color = "#B8860B",         
    outlier.colour = "#1FA2A6"
  ) +
  scale_x_discrete(
    na.translate = FALSE,
    labels = c("0" = "No", "1" = "Yes")
  ) +
  labs(
    title   = "Voucher usage vs Revenue",
    caption = "Data from SweetAroma",
    x       = "Use of voucher",
    y       = "Revenue"
  ) +
  theme_leeds +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5), 
    panel.grid.minor = element_line(color = "#333333", size = 0.25) 
  )

ggplot(orders)+geom_point(aes(past_spend,revenue))+
  labs(title = "Past Spend vs Revenue",
       caption = "Data from SweetAroma",
       x = "Past Spend",y = "Revenue")

ggplot(orders[!is.na(orders$past_spend) & !is.na(orders$revenue), ]) +
  geom_point(
    aes(x = past_spend, y = revenue),
    color = "#1FA77C",   
    alpha = 0.7,       
    size = 2             
  ) +
  labs(
    title   = "Past Spend vs Revenue",
    caption = "Data from SweetAroma",
    x       = "Past Spend",
    y       = "Revenue"
  ) +
  theme_leeds +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5), 
    panel.grid.minor = element_line(color = "#333333", size = 0.25) 
  )

ggplot(orders)+geom_point(aes(ad_channel,revenue), na.rm=TRUE)+
  scale_x_discrete(na.translate = FALSE)+
  labs(title = "Ad channel vs Revenue",
       caption = "Data from SweetAroma",
       x = "Ad channel used",y = "Revenue")
ggplot(orders)+geom_boxplot(aes(x= factor(ad_channel),y=revenue), na.rm=TRUE)+
  scale_x_discrete(na.translate = FALSE,
                   labels = c("1"="No Ads", "2"="Paid Search","3"="Free SEO", "4"="Online Ads"))+ #eliminar NA de la gráfica
  labs(title = "Ad Channel vs Revenue",
       caption = "Data from SweetAroma",
       x = "",y = "Revenue")

ggplot(orders[!is.na(orders$ad_channel) & !is.na(orders$revenue), ]) +
  geom_boxplot(
    aes(x = factor(ad_channel), y = revenue),
    fill = "#1FA77C",          
    color = "#B8860B",         
    outlier.colour = "#1FA2A6"
  ) +
  scale_x_discrete(
    na.translate = FALSE,
    labels = c("1" = "No Ads", 
               "2" = "Paid Search",
               "3" = "Free SEO", 
               "4" = "Online Ads")
  ) +
  labs(
    title   = "Ad Channel vs Revenue",
    caption = "Data from SweetAroma",
    x       = "",
    y       = "Revenue"
  ) +
  theme_leeds +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5), # grid principal
    panel.grid.minor = element_line(color = "#333333", size = 0.25) # grid secundario
  )


dataplot <- orders
ggplot(data=dataplot) + geom_point(aes(x=time_web, y=revenue,color=number_past_order)) +
  scale_color_viridis_c(
    option = "plasma",   # other good options: "viridis", "magma", "cividis", "turbo"
    direction = 1,
    breaks = pretty_breaks(n = 6),
    na.value = "grey80",
    name = "Past orders"
  )+
  labs(title = "Revenue and Time on the web number of past orders",
       caption = "Data from SweetAroma",tag = "Figure ",
       x = "Time on web",y = "Revenue")

##Data preparation##
#We want to check missing data#
colSums(is.na(orders))
aggr(orders_corr, numbers=TRUE, prop=FALSE, 
     labels= c("Past order", "Time", "voucher","past spend", "ad","revenue"), 
     las=2, cex.axis = 0.9)
missdata <- orders_corr
missdata$missing <- as.numeric(!complete.cases(orders_corr))
corrgram(missdata)
na.test(orders_corr)
na.test(orders)
#We create a data frame deleting the missing data#
neworders <- orders[complete.cases(orders_corr),]
dim(orders_corr)
dim(neworders)
colSums(is.na(neworders))
#We factor categorical data for new frame#
neworders$voucher<- as.factor(neworders$voucher)
neworders$ad_channel<- as.factor(neworders$ad_channel)
str(neworders) 
summary(neworders$voucher) 
summary(neworders$ad_channel)
#We create dummy variables for categorical data#
orders_dummy<-model.matrix(~voucher+ad_channel,data=neworders)
neworders <- cbind(neworders, orders_dummy[, -1])
str(neworders)
neworders$voucher <- NULL 
neworders$ad_channel <- NULL
#we check the final data frame for the model#
cor_data<-cor(neworders) 
round(cor_data, 2)
leeds_colors <- colorRampPalette(c("#B8860B", "white", "#146264"))(200)
corrplot(cor_data, method = "color")
corrplot(cor_data,
         method = "color",
         col = leeds_colors,
         tl.cex = 0.8,  
         tl.col="#146264",
         bg = "#1A1A1A",
         mar = c(0,0,1,0),
         title = "Correlation Matrix for Linear regresion model")



##Modelling##
model1 <- lm(revenue ~ number_past_order + 
               time_web + past_spend + voucher1 + 
               ad_channel2 + ad_channel3 + ad_channel4, 
             data = neworders)
summary(model1)
model2 <- lm(revenue ~ number_past_order,
             data = neworders)
summary(model2)
model3 <- lm(revenue ~ number_past_order + time_web, data =neworders)
model4 <- lm(revenue ~ number_past_order + time_web + past_spend, data =neworders)
model5 <- lm(revenue ~ number_past_order + time_web + past_spend + voucher1, data =neworders)
model6 <- lm(revenue ~ time_web, data =neworders)

summary(model1)$adj.r.squared 
summary(model2)$adj.r.squared 
summary(model3)$adj.r.squared 
summary(model4)$adj.r.squared 
summary(model5)$adj.r.squared
summary(model6)$adj.r.squared

#We test model assumptions#
plot(model1, which = 1)
ggplot(data = NULL, aes(x = model1$fitted.values, y = rstandard(model1))) +
  geom_point(color = "#B8860B", alpha = 0.7, size = 2) +
  geom_hline(yintercept = 0, color = "#FFD84D", linetype = "dashed", linewidth = 1) +
  geom_smooth(method = "loess", se = FALSE, color = "#146264", linewidth = 1) + 
  labs(
    title   = "Residuals vs Fitted Values",
    caption = "Model: revenue ~ number_past_order + time_web + past_spend + voucher1 + ad_channel",
    x       = "Fitted values",
    y       = "Standardized residuals"
  ) +
  theme_leeds_dark +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5),
    panel.grid.minor = element_line(color = "#333333", size = 0.25)
  )

plot(model1, which = 2)
ggplot(data = NULL, aes(sample = rstandard(model1))) +
  stat_qq(color = "#B8860B", alpha = 0.7, size = 2) +
  stat_qq_line(color = "#1FA77C", linetype = "dashed", linewidth = 1) +
  labs(
    title   = "Q-Q Residuals",
    caption = "Model: revenue ~ number_past_order + time_web + past_spend + voucher1 + ad_channel",
    x       = "Theoretical Quantiles",
    y       = "Standardized residuals"
  ) +
  theme_leeds_dark +
  theme(
    panel.grid.major = element_line(color = "#444444", size = 0.5),
    panel.grid.minor = element_line(color = "#333333", size = 0.25)
  )



shapiro.test(residuals(model1))
bptest(model1)
dwtest(model1)
#More robust deviant standard errors#
library(sandwich) 
coeftest(model1, vcov = vcovHC(model1, type = "HC1"))

##Evaluation##
summary(model1)$r.squared
summary(model1)$adj.r.squared
summary(model1)$fstatistic
pf(summary(model1)$fstatistic[1], 
   summary(model1)$fstatistic[2], 
   summary(model1)$fstatistic[3], 
   lower.tail = FALSE)
sqrt(mean(residuals(model1)^2))

##Prediction##
#prepare new dataset#
new_customers<-customers
names(new_customers)[names(new_customers)=="voucher"]<-"voucher1"
new_customers$ad_channel2 <- ifelse(new_customers$ad_channel == 2, 1, 0) 
new_customers$ad_channel3 <- ifelse(new_customers$ad_channel == 3, 1, 0) 
new_customers$ad_channel4 <- ifelse(new_customers$ad_channel == 4, 1, 0)
new_customers$ad_channel <- NULL
#predictions#
predictions <- predict(model1, newdata = new_customers, interval = "prediction")
new_customers$predicted_revenue <- predictions[, "fit"] 
new_customers$lower_bound <- predictions[, "lwr"] 
new_customers$upper_bound <- predictions[, "upr"] 
head(new_customers, 20)

##We do multiple imputation for variables##
set.seed(123)
imi <- mice( subset(orders_corr, select = c("number_past_order", "time_web", "voucher",
                                            "past_spend","ad_channel","revenue")), m = 5, maxit = 10)
mi<-complete(imi,action="long")
summary(orders)
summary(mi)
summary(imi)
#we fit the model to explain revenue#
fit <- with(data=imi, exp=lm(revenue ~ number_past_order + time_web + voucher + past_spend + ad_channel))
pooled <- pool(fit)
summary(pooled)
summary(pooled, conf.int = TRUE, conf.level = 0.95)
pool.r.squared(fit)
summary(orders)
#we train our prediction model#
#factor de voucher#
customers$voucher<- as.factor(customers$voucher)
#factor the ad channel#
customers$ad_channel<- as.factor(customers$ad_channel)

pred_list <- lapply(fit$analyses, function(mod) {
  predict(mod, newdata = customers)
})
print(pred_list)

pred_final <- rowMeans(do.call(cbind, pred_list))
head(pred_final, 20)
