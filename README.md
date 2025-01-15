# Purchasing-Behavior
# Analysis of Purchasing Behavior

This repository contains an analysis of purchasing behavior based on various factors, such as the number of children and teenagers in the household, the recency of purchases, and customer demographics. The analysis uses R programming language to perform data manipulation, summary statistics, and visualizations using `ggplot2`.

## Project Overview

The analysis includes the following key factors:
1. **Impact of Children in the Household**: The analysis examines how the number of children in the household influences total purchases, both in physical stores and online.
2. **Impact of Teenagers in the Household**: Similar to children, the presence of teenagers in the household is analyzed to determine its effect on purchasing behavior.
3. **Recency of Purchases**: The analysis explores how the recency of purchases correlates with expenditure across different product categories, such as wine, meat, fruits, and more.
4. **General Conclusions and Recommendations**: Insights from the data are used to provide recommendations for tailoring sales strategies and targeting specific customer segments.

## Key Analysis Details

### 1. **Children in the Household (Kidhome)**

- Customers with children in their household (Kidhome = 0, 1, 2) show varying levels of purchase activity across different product categories.
- Physical store purchases are more frequent in households without children, while families with more children tend to make fewer purchases, especially online.

### 2. Teenagers in the Household (Teenhome)
Similarly, households with teenagers (Teenhome = 0, 1, 2) demonstrate specific purchasing patterns that differ from those without teenagers.

### 3. Recency of Purchases
Analyzing the recency of purchases helps to understand how recently a customer has made a purchase and how that correlates with their expenditure across various product categories.


## Example Code

### Example analysis for children in household
```r
# Example analysis for children in household
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
```

## Visualization
The visualizations generated by ggplot2 help to represent the data in an easy-to-understand format. Bar plots are used to compare the total number of purchases based on household characteristics (e.g., number of children, number of teenagers) and the recency of purchases.
```r
ggplot(purchases_summary_kidhome, aes(x = Kidhome, y = num_store_purchases, fill = as.factor(Kidhome))) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(title = 'Purchases by Household Composition', x = 'Number of Children', y = 'Store Purchases')
```

## Conclusions & Recommendations
General Findings:
Marital Status: Singles show the highest acceptance of promotional offers, while divorced individuals show the least. Married people and those in relationships have a lower response rate.
Income: Higher-income individuals are more likely to accept promotional offers, while lower-income groups show less activity.
Children & Teenagers: Families with children or teenagers tend to make fewer purchases, especially online.
Discount Purchases: Customers who buy with discounts are more likely to engage with offers and make more purchases.

## Recommendations:
Personalization of Offers: Tailor offers to marital status and income. Exclusive offers for singles and high-income groups, while more general offers for married individuals and lower-income groups.
Targeting Families with Children: Offer discounts or promotions on products relevant to families with children (e.g., toys, baby products, home items).
Increase Online Promotions: Focus on online promotions for customers who show more interest in online shopping, particularly those with higher incomes.

## Prerequisites
```r
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tidyr")
install.packages("RColorBrewer")

```

