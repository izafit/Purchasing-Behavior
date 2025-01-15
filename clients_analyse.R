library(readxl)
library(pastecs)
library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)

## Loading data:
clients <- read_excel("dane do ćwiczeń/clients.xlsx")

# Checking the data structure:
head(clients)
str(clients)
class(clients)

# Checking for missing data and Filling missing data:
!complete.cases(clients)
colSums(is.na(clients))

clients$Year_Birth[is.na(clients$Year_Birth)] <- round(median(clients$Year_Birth, na.rm = T), 0)
clients$Income[is.na(clients$Income)] <- round(mean(clients$Income, na.rm = T), 0)
clients$Recency[is.na(clients$Recency)] <- round(mean(clients$Recency, na.rm = T), 0)
clients$MntSweetProducts[is.na(clients$MntSweetProducts)] <- round(mean(clients$MntSweetProducts, na.rm = T), 0)
clients$NumWebVisitsMonth[is.na(clients$NumWebVisitsMonth)] <- round(mean(clients$NumWebVisitsMonth, na.rm = T), 0)
clients$AcceptedCmp3[is.na(clients$AcceptedCmp3)] <- round(median(clients$AcceptedCmp3, na.rm = T), 0)
clients$AcceptedCmp4[is.na(clients$AcceptedCmp4)] <- round(median(clients$AcceptedCmp4, na.rm = T), 0)
clients$Z_CostContact[is.na(clients$Z_CostContact)] <- round(mean(clients$Z_CostContact, na.rm = T), 0)
clients$Z_Revenue[is.na(clients$Z_Revenue)] <- round(mean(clients$Z_Revenue, na.rm = T), 0)


colSums(is.na(clients))
?summary

# Converting variables to "factor" type:
zmienne <- c('Response', 'Complain', 'AcceptedCmp1', 'AcceptedCmp2', 'AcceptedCmp3', 'AcceptedCmp4', 'AcceptedCmp5')

clients[zmienne] <- lapply(clients[zmienne], function(x) factor(x, levels = c(0, 1), labels = c(0, 1)))


unique(clients$Education)
clients$Education <- factor(clients$Education, 
                            levels = c("Basic","2n Cycle", "Graduation", "Master", "PhD"), 
                            labels = c( "Basic","2n Cycle",  "Graduation", "Master", "PhD"),
                            ordered = T
)

unique(clients$Marital_Status)
clients$Marital_Status <- ifelse(clients$Marital_Status == 'Alone',
                                 'Single',
                                 ifelse(clients$Marital_Status %in% c('Absurd', 'YOLO'),
                                        'Married',
                                        clients$Marital_Status))

clients$Marital_Status <- factor(clients$Marital_Status,
                                 levels = c('Single', 'Together', 'Married', 'Widow', 'Divorced', 'Cycle'))

summary(clients)

# Statistical analysis of numeric variables:
summary(clients$Year_Birth)
summary(clients$MntWines)

stat_year_birth <- stat.desc(clients$Year_Birth, basic = TRUE, desc = TRUE, norm = TRUE)
stat_mnt_wines <- stat.desc(clients$MntWines, basic = TRUE, desc = TRUE, norm = TRUE)

print(stat_year_birth)
print(stat_mnt_wines)

## Statistics for Year_Birth indicate a homogeneous and symmetrical distribution with low variability. Both parametric and non-parametric methods can be used in further analysis.

# Statistics for MntWines show high variability and skewness. In analyses, it is recommended to use methods robust to outliers, such as the median or quartiles.


#Analyzing basic statistics for selected numeric variables to assess their distributions and variability.
variables <- c('Income', 'MntWines', 'MntFruits', 'Z_Revenue', 'MntMeatProducts', 'MntFishProducts', 'MntSweetProducts', 'MntGoldProds')
results <- list()

for (var in variables) {
  results[[var]] <- stat.desc(clients[[var]], basic = TRUE, desc = TRUE)
}
results

# Colors
hashtag_colors <- c(    
  "#6baed6",
  "#2b8cbe",
  "#0868ac", 
  "#155f8a",
  "#084081",      
  "#002e5d"    
)

hashtag_colors0 <- c(    
  "#A2C8E2",
  "#357ABD",  
  "#1D4E89", 
  "#163E63", 
  "#0A2A45", 
  "#07214B"   
)

table(clients$Education, clients$Marital_Status)

par(cex.lab = 1.1, cex.axis = 0.8) 


# Comparing Categorical Variables
table(clients$Education, clients$Marital_Status)

tab <- table(clients$Education, clients$Marital_Status)
mosaicplot(tab, 
           main = "Education vs Marital Status", 
           color = hashtag_colors, 
           xlab = "Education Level", 
           ylab = "Marital Status", 
           las = 1)

par(cex.lab = 1, cex.axis = 1)
#Conclusions
# High variability in expenditures on wine, meat, and fish suggests the existence of customer segments with distinct purchasing preferences. This could be valuable for segmentation analysis.

#Income Commentary: The wide range of Income indicates customer diversity in terms of social class and lifestyle, which may affect their purchasing preferences.


cross_tab <- table(clients$Education, clients$Response)
print(cross_tab)

mosaicplot(cross_tab, 
           main = "Wykres mozaikowy: Education vs Marital Status", 
           color = TRUE, 
           shade = TRUE, 
           xlab = "Education", 
           ylab = "Response")

# The mosaic plot illustrates the relationship between education and response. Colors indicate differences between observed and expected values, highlighting variable dependencies.
# Individuals with Graduation education are more likely to respond, indicating their importance in the analysis. The lower representation of Master, PhD, 2n, and Basic education levels may explain their smaller impact on the results.

# Analysis of the relationship between education level and the number of children in the household
# Creating a contingency table for education level and number of children
hashtag_colors1 <- c("#6baed6", "#2b8cbe", "#0868ac")
hashtag_colors2 <- c("#155f8a", "#084081", "#002e5d") 
hashtag_colors3 <- c("#6baed6", "#084081", "#2b8cbe") 
hashtag_colors4 <- c("#0868ac", "#155f8a", "#002e5d") 

par(cex.lab = 1.1, cex.axis = 0.8)  

# Analysis of the impact of education on the number of children in the household
educ_kidhome <- table(clients$Education, clients$Kidhome)
mosaicplot(educ_kidhome, 
           main = 'Education vs Kid at home', 
           color = hashtag_colors1,  
           xlab = "Education", 
           ylab = "Kid at Home", 
           las = 1)

educ_teenhome <- table(clients$Education, clients$Teenhome)
mosaicplot(educ_teenhome, 
           main = 'Education vs Teen at home', 
           color = hashtag_colors2,  
           xlab = "Education", 
           ylab = "Teen at Home", 
           las = 1)

# Analysis of the impact of marital status on the number of children in the household
marital_kidhome <- table(clients$Marital_Status, clients$Kidhome)
mosaicplot(marital_kidhome, 
           main = 'Marital Status vs Kid at home', 
           color = hashtag_colors3,  
           xlab = "Marital Status", 
           ylab = "Kid at Home", 
           las = 1)

marital_teenhome <- table(clients$Marital_Status, clients$Teenhome)
mosaicplot(marital_teenhome, 
           main = 'Marital Status vs Teen at home', 
           color = hashtag_colors4,  
           xlab = "Marital Status", 
           ylab = "Teen at Home", 
           las = 1)

par(cex.lab = 1, cex.axis = 1)


# Proportional visualization of the number of children by marital status
ggplot(clients, aes(x = Marital_Status, y = Kidhome)) +
  geom_count(aes(size = after_stat(prop), group = 1, color = after_stat(prop))) +
  scale_size_area(max_size = 10) + 
  scale_color_gradient(low = "lightblue", high = "darkblue") +
  labs(title = "Marital Status vs Kidhome",
       x = "Marital Status",
       y = "Kidhome",
       size = "Proportion",
       color = "Proportion") +
  guides(size = "legend", color = "legend") + 
  theme_minimal()

par(mfrow = c(1, 2))
mosaicplot(marital_kidhome, main = 'Marital Status vs Kid at Home', color = hashtag_colors3, xlab = "Marital Status", ylab = "Kid at Home")
mosaicplot(marital_teenhome, main = 'Marital Status vs Teen at home', color = hashtag_colors4, xlab = "Marital Status", ylab = "Teen at Home")

# Key insights:
# 1. The largest group comprises individuals with Graduation education and 0 children (Kidhome).
# 2. The largest group by marital status is married individuals with 0 children (Kidhome).


# Analysis of customer groups generating the highest average incomes
clients_summary <- clients %>%
  group_by(Education, Marital_Status) %>%
  summarise(mean_income = mean(Income, na.rm = TRUE)) %>%
  slice_max(mean_income, n = 5) %>% 
  arrange(desc(mean_income)) 

# Visualization of the top 5 customer categories by average income
ggplot(clients_summary, aes(x = Education, y = mean_income, fill = Education)) +
  geom_bar(stat = 'identity', position = 'dodge') +  
  scale_fill_brewer(palette = 'GnBu') +
  labs(title = 'Top 5 Highest Income Categories by Education and Marital Status',
       x = 'Education Level',
       y = 'Average Income',
       fill = 'Education Level') +
  theme_minimal()


# Analysis of customer spending on various products by education level

# Creating a summary table for average spending by product categories
education_summary <- clients %>% 
  group_by(Education) %>%
  summarise(
    avg_vin = mean(MntWines, na.rm = TRUE),
    avg_fruits = mean(MntFruits, na.rm = TRUE),
    avg_meat = mean(MntMeatProducts, na.rm = TRUE),
    avg_fish = mean(MntFishProducts, na.rm = TRUE),
    avg_sweet = mean(MntSweetProducts, na.rm = TRUE),
    avg_gold = mean(MntGoldProds, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with('avg_'),
    names_to = 'Product_Category',
    values_to = 'Average_Spending'
  ) %>%
  arrange(desc(Average_Spending), Education) %>%
  head(10)

education_summary

# Wine spending distribution plots by education level

# Displaying average wine spending for each education level
clients %>%
  group_by(Education) %>%
  summarise(avg_wine = mean(MntWines, na.rm = TRUE)) %>%
  print()

options(scipen = 999)

# Boxplot showing the distribution of wine spending based on education level
ggplot(clients, aes(x=Education, y=MntWines, fill = Education)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = "Distribution of Wine Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Wine") +
  theme_minimal()

# Without otuliners
clients_clean <- clients %>%
  filter(MntWines <= (quantile(MntWines, 0.75) + 1.5 * IQR(MntWines)))

# Boxplot without outliners
ggplot(clients_clean, aes(x = Education, y = MntWines, fill = Education)) +
  geom_boxplot(notch = TRUE) +
  stat_summary(fun = "mean", geom = "point", shape = 18, size = 3, color = "black") +
  scale_fill_brewer(palette = 'GnBu') +
  labs(title = "Distribution of Wine Spending by Education Level (No Outliers)",
       x = "Education Level",
       y = "Amount Spent on Wine") +
  theme_minimal() 

# Comparing average wine spending for different education levels
ggplot(clients, aes(x = Education, y = MntWines, fill = Education)) +
  geom_col() + 
  scale_fill_manual(values = hashtag_colors0) +
  labs(title = 'Average Wine Spending by Education Level',
       x = 'Education Level', y = 'Average Wine Spending') +
  theme_minimal()

# Average spending in different products category
ggplot(education_summary, aes(x = Education, y = Average_Spending, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'YlGnBu') +
  labs(title = 'Average Spending on Different Product Categories by Education Level',
       x = 'Education Level',
       y = 'Average Spending',
       fill = 'Product Category') +
  theme_minimal() 

# Analysis shows that people with higher education (PhD, Master) spend more on wine than those with lower education.
# "Graduation" level people spend the most on wine, followed by PhD.

# Analysis of fruit spending by education level
ggplot(clients, aes(x=Education, y=MntFruits, fill=Education)) +
  geom_col() +
  scale_fill_brewer(palette = 'Blues') +
  labs(title = 'Distribution of Fruits Spending by Education Level',
       x = 'Education Level',
       y = 'Amount Spent on Fruits') +
  theme_minimal()

ggplot(clients, aes(x= Education, y= MntFruits, fill= Education)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_brewer(palette = "Blues") +
  labs(title = "Distribution of Fruits Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Fruits") +
  theme_minimal()

# Analysis of meat spending by education level
ggplot(clients, aes(x=Education, y=MntMeatProducts, fill=Education)) +
  geom_col() +
  scale_fill_brewer(palette = 'GnBu') +
  labs(title = 'Distribution of Meat Spending by Education Level',
       x = 'Education Level',
       y = 'Amount Spent on Meat') +
  theme_minimal()

ggplot(clients, aes(x= Education, y= MntMeatProducts, fill= Education)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_brewer(palette = "GnBu") +
  labs(title = "Distribution of Meat Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Meat") +
  theme_minimal()

# People with "Graduation" education spend significantly more on meat, but average spending is similar across groups.

# Analysis of fish spending by education level
ggplot(clients, aes(Education, MntFishProducts, fill = Education)) +
  geom_col() +
  scale_fill_brewer(palette = 'PuBu') +
  labs(title = 'Distribution of Fish Spending by Education Level',
       x = 'Education Level',
       y = 'Amount Spent on Fish') +
  theme_minimal()

ggplot(clients, aes(x= Education, y= MntFishProducts, fill= Education)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "PuBu") +
  labs(title = "Distribution of Fish Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Fish") +
  theme_minimal()

# Analysis of sweet spending by education level
ggplot(clients, aes(Education, MntSweetProducts, fill = Education)) +
  geom_col() +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = 'Distribution of Sweet Spending by Education Level',
       x = 'Education Level',
       y = 'Amount Spent on Sweet') +
  theme_minimal()

ggplot(clients, aes(x= Education, y= MntSweetProducts, fill= Education)) +
  geom_boxplot() +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = "Distribution of Sweet Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Sweet") +
  theme_minimal()

# Analysis of gold spending based on education level
ggplot(clients, aes(Education, MntGoldProds, fill = Education))+
  geom_col()+
  scale_fill_manual(values = hashtag_colors0) +
  labs(title = 'Distribution of Gold Spending by Education Level',
       x = 'Education Level',
       y = 'Amount Spent on Gold') +
  theme_minimal()

ggplot(clients, aes(x= Education, y= MntGoldProds, fill= Education)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_manual(values = hashtag_colors0) +
  labs(title = "Distribution of Gold Spending by Education Level",
       x = "Education Level",
       y = "Amount Spent on Gold") +
  theme_minimal()

# The "Graduation" group spends the most on gold, and they also have the highest average spending on gold.

# Analysis of spending distribution on gold and meat products using histograms and density plots
par(mfrow = c(1, 2))

hist(clients$MntGoldProds, 
     main = "Distribution of Golds Products", 
     xlab = "Amount spent on Gold Products", 
     col = '#c1e2d6', 
     border = "black", 
     probability = TRUE)
lines(density(clients$MntGoldProds), 
      col = "black")

hist(clients$MntMeatProducts, 
     main = "Distribution of Meat Products", 
     xlab = "Amount spent on Meat Products", 
     col = "#0868ac", 
     border = "black", 
     probability = TRUE)
lines(density(clients$MntMeatProducts), 
      col = "black")

# Interpretation:
# 1. Similarity of both plots: Spending on gold and meat shows a similar distribution. In both cases, most people spend small amounts, and only a few customers make higher purchases.
# 2. Small number of high spenders: For both gold and meat products, only a small group of customers tends to spend more, suggesting that these products are less popular for large expenditures.
# 3. Further steps:
#    - Marketing strategy: It suggests targeting a wider audience with lower spending rather than those already making larger purchases.
#    - Further analysis: It could be useful to explore if there are customer segments more inclined to spend on gold or meat, and tailor offers to them.


## Summary of spending by marital status
marital_summary <- clients %>%
  group_by(Marital_Status) %>%
  summarise(
    avg_wine = mean(MntWines, na.rm = TRUE),
    avg_fruits = mean(MntFruits, na.rm = TRUE),
    avg_meat = mean(MntMeatProducts, na.rm = TRUE),
    avg_fish = mean(MntFishProducts, na.rm = TRUE),
    avg_sweet = mean(MntSweetProducts, na.rm = TRUE),
    avg_gold = mean(MntGoldProds, na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with('avg_'),
    names_to = 'Product_Category',
    values_to = 'Average_Spending'
  ) %>%
  arrange(desc(Average_Spending), Marital_Status) 
# %>%
# head(10)
marital_summary

# Average spending by marital status and product category
ggplot(marital_summary, aes(x = Marital_Status, y = Average_Spending, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'GnBu') +
  labs(title = 'Average Spending by Marital Status and Product Category',
       x = 'Marital Status',
       y = 'Average Spending',
       fill = 'Product Category') +
  theme_minimal() +
  geom_text(aes(label = round(Average_Spending, 0)), position = position_dodge(width = 0.9), vjust = -0.5)


# Spending on wine by marital status
ggplot(clients, aes(x=Marital_Status, y=MntWines, fill=Marital_Status)) +
  geom_col() +
  scale_fill_brewer(palette = 'PuBu') +
  labs(title = 'Distribution of Wines Spending by Marital Status',
       x = 'Marital Status',
       y = 'Amount Spent on Wines') +
  theme_minimal()

ggplot(clients, aes(x= Marital_Status, y= MntWines, fill= Marital_Status)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_brewer(palette = 'PuBu') +
  labs(title = "Distribution of Wines Spending by Marital Status",
       x = "Marital Status",
       y = "Amount Spent on Wines") +
  theme_minimal() 

# Conclusion: The "Widow" group spends the most on wine, averaging 369, showing a clear preference for wine.
# Then we have "Divorced", "Together", "Married", and "Single" groups.
# Differences in spending are noticeable but moderate within these categories.

# Spending on meat by marital status
ggplot(clients, aes(x=Marital_Status, y=MntMeatProducts, fill=Marital_Status)) +
  geom_col() +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = 'Distribution of Meat Spending by Marital Status',
       x = 'Marital Status',
       y = 'Amount Spent on Meat') +
  theme_minimal()

ggplot(clients, aes(x= Marital_Status, y= MntMeatProducts, fill= Marital_Status)) +
  geom_boxplot(notch = TRUE) +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = "Distribution of Meat Spending by Marital Status",
       x = "Marital Status",
       y = "Amount Spent on Meat") +
  theme_minimal()

# The "Widow" group spends the most on meat (189), followed by "Single" at 181.
# This may suggest that single and widow individuals tend to spend more on meat products than other groups.

# Spending on fruits by marital status
ggplot(clients, aes(x=Marital_Status, y=MntFruits, fill=Marital_Status)) +
  geom_col() +
  scale_fill_manual(values = hashtag_colors0) +
  labs(title = 'Distribution of Fruits Spending by Marital Status',
       x = 'Marital Status',
       y = 'Amount Spent on Fruits') +
  theme_minimal()

ggplot(clients, aes(x= Marital_Status, y= MntFruits, fill= Marital_Status)) +
  geom_boxplot() +
  scale_fill_manual(values = hashtag_colors0) +
  labs(title = "Distribution of Fruits Spending by Marital Status",
       x = "Marital Status",
       y = "Amount Spent on Fruits") +
  theme_minimal()


### Correlation
cor_vars <- clients %>%
  select(Income, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds, Recency)

correlation_matrix <- cor(cor_vars, use = "complete.obs", method = "pearson")
print(correlation_matrix)

# Convert the correlation matrix to a long format
cor_melted <- melt(correlation_matrix)

ggplot(cor_melted, aes(Var1, Var2, fill = value)) +
  geom_tile() +  
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  scale_fill_gradient2(low = "white", high = "#084081", mid = "white", midpoint = 0) +
  labs(title = "Correlation Heatmap", x = "", y = "") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Wine spending (MntWines) is moderately correlated with:
#   
#   Income (Income): A correlation of 0.58 suggests that people with higher incomes tend to spend more on wine.
# Meat spending (MntMeatProducts): A correlation of 0.56 indicates a strong relationship between spending on wine and meat, implying that people who spend on one of these products are also likely to spend on the other.
# Fruit spending (MntFruits) is positively correlated with:
#   
#   Meat spending (MntMeatProducts) and Fish spending (MntFishProducts), suggesting that people who spend on meat or fish are more likely to spend on healthy foods like fruits.
# Gold spending (MntGoldProds) has a relatively weak correlation with other variables:
#   
#   The correlation with Income (0.32) is low, indicating that people who spend on gold are not necessarily higher-income earners.
# The correlation with Recency (0.02) is very low, suggesting that gold spending is not strongly linked to recent purchases, which may indicate a more long-term investment nature for gold purchases.
# Recency (time since last purchase) shows very low correlation with most variables, which could imply that the frequency of purchases is not a major factor determining spending levels on food products and other categories.


# Analysis of the impact of promotions on purchase decisions
# Investigating how different promotions affect customers' purchase decisions based on variables: education, marital status, income.
sales_summary_education <- clients %>%
  group_by(Education) %>%
  summarise(
    accepted_cmp1 = sum(as.numeric(as.character(AcceptedCmp1)), na.rm = TRUE),
    accepted_cmp2 = sum(as.numeric(as.character(AcceptedCmp2)), na.rm = TRUE),
    accepted_cmp3 = sum(as.numeric(as.character(AcceptedCmp3)), na.rm = TRUE),
    accepted_cmp4 = sum(as.numeric(as.character(AcceptedCmp4)), na.rm = TRUE),
    accepted_cmp5 = sum(as.numeric(as.character(AcceptedCmp5)), na.rm = TRUE),
    accepted_response = sum(as.numeric(as.character(Response)), na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = starts_with('accepted_'),
    names_to = 'Product_Category',
    values_to = 'Accepted_Count'
  ) %>% group_by(Education) %>% 
  mutate(total = sum(Accepted_Count), percent = round(100*(Accepted_Count/total), digits = 2)) %>% 
  mutate(percent = ifelse(Accepted_Count==0,NA,percent)) %>% 
  mutate(Education = factor(Education , levels = c("Basic","2n Cycle", "Graduation", "Master", "PhD"), 
                            labels = c( "Basic","2n Cycle",  "Graduation", "Master", "PhD"),
                            ordered = T))


head(sales_summary_education, 10)

windowsFonts(A = windowsFont("Arial"))

# Creating a plot showing the percentage of accepted promotions by education level
ggplot(sales_summary_education, aes(x = Education, y = percent, fill =  Product_Category ))+
  geom_bar(position="stack", stat="identity")+ 
  theme_classic()+
  geom_text(aes(label = paste0(percent, "%")), position = position_stack(vjust = 0.5),
            color="white",
            size=3,
            fontface = "bold",
            family="A")+
  ggtitle("Percentage of Accepted Promotions by Education Level")+
  labs(
    x = "Education Level",
    y = "Percentage of Accepted Promotions",
    fill = "Product Category"
  ) +
  theme(axis.text = element_text(face="bold", color="black", angle =0,
                                 size=10, family="A"),
        axis.text.x = element_text(face="bold", color="black", angle =90,
                                   size=10, family="A"),
        axis.text.y = element_text(face="bold", color="black",
                                   size=10, family="A"),
        axis.title.x = element_text(face="bold", color="black", angle =0,
                                    size=10, family="A"),
        axis.title.y = element_text(face="bold", color="black",
                                    size=10, family="A"),
        plot.title = element_text(family = "A", face = "bold", size = (15), color="black"),
        plot.subtitle = element_text(family = "A", face = "bold", size = (10), color="black"),
        legend.text =  element_text(face="bold", color="black", angle =0,
                                    size=10, family="A"),
        legend.title = element_text(face="bold", color="black", angle =0,
                                    size=10, family="A")
  )+
  scale_fill_manual(values = hashtag_colors)

# Education level analysis
ggplot(sales_summary_education, aes(x = Education, y = Accepted_Count, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'stack') +
  scale_fill_brewer(palette = 'GnBu') +
  labs(title = 'Total Accepted Promotions by Education Level',
       x = 'Education',
       y = 'Number of Accepted Promotions',
       fill = 'Product Category') +
  theme_minimal()

# The highest number of accepted promotions came from people with a "Graduation" education, followed by those with a PhD.
# The last campaign had the highest acceptance compared to others.

# Marital status analysis
sales_summary_marital <- clients %>%
  group_by(Marital_Status) %>%
  summarise(
    accepted_cmp1 = sum(as.numeric(as.character(AcceptedCmp1)), na.rm = TRUE),
    accepted_cmp2 = sum(as.numeric(as.character(AcceptedCmp2)), na.rm = TRUE),
    accepted_cmp3 = sum(as.numeric(as.character(AcceptedCmp3)), na.rm = TRUE),
    accepted_cmp4 = sum(as.numeric(as.character(AcceptedCmp4)), na.rm = TRUE),
    accepted_cmp5 = sum(as.numeric(as.character(AcceptedCmp5)), na.rm = TRUE),
    accepted_response = sum(as.numeric(as.character(Response)), na.rm = TRUE)
  ) %>% 
  pivot_longer(
    cols = starts_with('accepted_'),
    names_to = 'Product_Category',
    values_to = 'Accepted_Count'
  ) %>%
  arrange(desc(Accepted_Count), Marital_Status)

head(sales_summary_marital, 10)

# Plot showing the number of accepted promotions by marital status
ggplot(sales_summary_marital, aes(x = Marital_Status, y = Accepted_Count, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = hashtag_colors) +
  labs(title = 'Number of Accepted Promotions by Marital Status',
       x = 'Marital Status',
       y = 'Number of Accepted Promotions',
       fill = 'Product Category') +
  geom_text(aes(label = Accepted_Count), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +
  theme_minimal()

# Single people show the highest number of accepted promotions, indicating greater susceptibility to promotions.
# Divorced individuals have the lowest acceptance, which may suggest less interest in engaging with promotions.
# Married and partnered individuals show moderate acceptance.

# Income analysis
clients$Income_Category <- cut(clients$Income, breaks = c(0, 20000, 50000, 100000, Inf), labels = c("Low", "Medium", "High", "Very High"))

sales_summary_income <- clients %>%
  group_by(Income_Category) %>%
  summarise(
    accepted_cmp1 = sum(as.numeric(as.character(AcceptedCmp1)), na.rm = TRUE),
    accepted_cmp2 = sum(as.numeric(as.character(AcceptedCmp2)), na.rm = TRUE),
    accepted_cmp3 = sum(as.numeric(as.character(AcceptedCmp3)), na.rm = TRUE),
    accepted_cmp4 = sum(as.numeric(as.character(AcceptedCmp4)), na.rm = TRUE),
    accepted_cmp5 = sum(as.numeric(as.character(AcceptedCmp5)), na.rm = TRUE),
    accepted_response = sum(as.numeric(as.character(Response)), na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with('accepted_'),
    names_to = 'Product_Category',
    values_to = 'Accepted_Count'
  ) %>%
  arrange(Income_Category, desc(Accepted_Count))

sales_summary_income

# Plot illustrating the number of accepted promotions based on income category
ggplot(sales_summary_income, aes(x = Income_Category, y = Accepted_Count, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'YlGnBu') +
  labs(title = 'Number of Accepted Promotions by Income Category',
       x = 'Income Category',
       y = 'Number of Accepted Promotions',
       fill = 'Product Category') +
  geom_text(aes(label = Accepted_Count), position = position_dodge(width = 0.8), vjust = -0.5, color = "black") +
  theme_minimal()

# Higher income customers show a higher number of accepted promotions, particularly for overall response (Response).
# People with higher incomes tend to engage more in promotions, suggesting a greater likelihood of making purchase decisions tied to promotions.

# Analysis of the impact of the number of children in the household on purchasing behavior
purchases_summary_kidhome <- clients %>%
  group_by(Kidhome) %>%
  summarise(
    num_web_purchases = sum(NumWebPurchases, na.rm = TRUE),
    num_store_purchases = sum(NumStorePurchases, na.rm = TRUE),
    num_catalog_purchases = sum(NumCatalogPurchases, na.rm = TRUE),
    numbers_purchases = sum(as.numeric(as.character(Response)), na.rm = TRUE)  
  ) %>%
  pivot_longer(
    cols = starts_with('num_'),
    names_to = 'Product_Category',
    values_to = 'Numbers_Purchases'
  ) %>%
  arrange(desc(Numbers_Purchases), Kidhome)

head(purchases_summary_kidhome, 10)

# Bar plot displaying the total number of purchases based on the number of children in the household
ggplot(purchases_summary_kidhome, aes(x = Kidhome, y = Numbers_Purchases, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_manual(values = hashtag_colors1) + 
  labs(title = 'Influence of Children in Household on Purchases',
       x = 'Number of Children in Household',
       y = 'Total Purchases',
       fill = 'Purchase Category') +
  theme_minimal() +
  geom_text(aes(label = Numbers_Purchases), position = position_dodge(width = 0.8), vjust = -0.5, color = "black")

# Analysis of the impact of teenagers in the household on purchasing behavior
purchases_summary_teenhome <- clients %>%
  group_by(Teenhome) %>%
  summarise(
    num_web_purchases = sum(NumWebPurchases, na.rm = TRUE),
    num_store_purchases = sum(NumStorePurchases, na.rm = TRUE),
    num_catalog_purchases = sum(NumCatalogPurchases, na.rm = TRUE),
    numbers_purchases = sum(as.numeric(as.character(Response)), na.rm = TRUE)
  ) %>%
  pivot_longer(
    cols = starts_with('num_'),
    names_to = 'Product_Category',
    values_to = 'Numbers_Purchases'
  ) %>%
  arrange(desc(Numbers_Purchases), Teenhome) %>%
  head(10)

head(purchases_summary_teenhome, 10)

# Bar plot displaying the total number of purchases based on the number of teenagers in the household
ggplot(purchases_summary_teenhome, aes(x = Teenhome, y = Numbers_Purchases, fill = Product_Category)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  scale_fill_brewer(palette = 'PuBu') +
  labs(title = 'Impact of Teenagers in Household on Purchase Behavior',
       x = 'Number of Teenagers in Household',
       y = 'Total Purchases',
       fill = 'Purchase Category') +
  theme_minimal() +
  geom_text(aes(label = Numbers_Purchases), position = position_dodge(width = 0.8), vjust = -0.5, color = "black")

# The number of children in the household has a significant impact on purchasing behavior. 
# People without children (Kidhome = 0) make considerably more purchases, especially in physical stores, compared to those with children. 
# Families with more children (Kidhome = 2) show much lower purchasing activity, particularly in online and catalog shopping. 
# Similarly, in the case of teenagers (Teenhome), people without teenagers make more purchases, especially online and in physical stores, while families with teenagers make fewer purchases.

# Recommendations:
#   Adjust sales strategy: Physical stores can target promotions to households with children and teenagers.
# Increase online sales: Direct marketing efforts to households with one child or teenagers, as they are more likely to shop online.

# Analysis of the impact of recency of purchases on expenditure
purchases_summary_recency <- clients %>%
  group_by(Recency) %>%
  summarise(
    sum_wine = sum(MntWines, na.rm = TRUE),
    sum_fruits = sum(MntFruits, na.rm = TRUE),
    sum_meat = sum(MntMeatProducts, na.rm = TRUE),
    sum_fish = sum(MntFishProducts, na.rm = TRUE),
    sum_sweet = sum(MntSweetProducts, na.rm = TRUE),
    sum_gold = sum(MntGoldProds, na.rm = TRUE)
  ) %>%
  arrange(Recency)
head(purchases_summary_recency, 10)

# Bar plot displaying the expenditure on different product categories based on the recency of purchases
ggplot(purchases_summary_recency, aes(x = factor(Recency))) +
  geom_bar(aes(y = sum_wine, fill = "Wine"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  geom_bar(aes(y = sum_meat, fill = "Meat"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  geom_bar(aes(y = sum_fruits, fill = "Fruits"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  geom_bar(aes(y = sum_fish, fill = "Fish"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  geom_bar(aes(y = sum_sweet, fill = "Sweet Products"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  geom_bar(aes(y = sum_gold, fill = "Gold Products"), stat = 'identity', alpha = 0.7, position = 'dodge') +
  labs(title = 'Expenditure on Products Based on Recency of Purchases',
       x = 'Recency of Purchases (Days Since Last Purchase)',
       y = 'Total Expenditure',
       fill = 'Product Category') +
  theme_minimal() +
  scale_x_discrete(breaks = seq(0, max(purchases_summary_recency$Recency, na.rm = TRUE), by = 5)) +
  scale_fill_manual(values = c('Wine' = 'purple', 'Meat' = 'blue', 'Fruits' = 'lightblue', 
                               'Fish' = 'green', 'Sweet Products' = 'pink', 'Gold Products' = 'gold')) +
  scale_y_continuous(limits = c(0, 12500))


############# General conclusions
# Marital status: Single people show the highest acceptance of offers, suggesting they are more susceptible to promotions, while divorced individuals show the least acceptance. Married people and those in relationships accept offers less frequently, indicating the need for more tailored offerings for these groups.
# 
# Income: Higher-income groups (High) are more responsive to promotions, especially in terms of accepting offers in campaigns and making purchases with discounts. Lower-income groups show less activity in this area.
# 
# Children in the household: The presence of children impacts purchasing behavior, with families without children showing higher purchasing activity, especially in physical stores. An increased number of children, particularly teenagers, leads to a decrease in the number of purchases, especially online and catalog purchases.
# 
# Purchasing behavior with discounts: It is clear that customers who purchase with discounts are more active in accepting offers and making more purchases. Higher-income groups and those without children are more likely to take advantage of promotions and discounts.

# Recommendations:
#   Personalize offers: Offer different types of promotions based on marital status and income. For singles and higher-income individuals, prepare more exclusive offers, while for married people and those with lower incomes, offer more general promotions.
# 
# Target families with children: Provide special discounts or promotions for families with children, especially for products that may be attractive to this group (e.g., children's items, toys, home furnishings).
# 
# Increase online promotions: Focus on online promotions and discounts for customers who have already shown more activity in online shopping, and for higher-income individuals who are more likely to take advantage of online offers.