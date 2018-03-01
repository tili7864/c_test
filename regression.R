## load required packages
if(!require('xlsx')) {install.packages('xlsx')}
library('xlsx')

if(!require('ggplot2')) {install.packages('ggplot2')}
library('ggplot2')

## import sales data
my_dir <- "C:/Users/Marcus/Documents/R/Regression"
df_data <- read.xlsx(paste0(my_dir, "/data/toy_sales_data.xlsx"), sheetName = "data")

## plot of Sales vs TV and Digital Investments 
ggplot(df_data, aes(x = tv_spend, y = sales)) + 
  geom_point(aes(x = tv_spend, colour = "tv_spend")) +
  geom_point(aes(x = digital_spend, colour = "digital_spend")) + 
  theme_minimal() + 
  scale_colour_manual(values = c("darkorange", "dodgerblue"), name = "Investment Type", 
                      labels = c("Digital Investment", "TV Investment")) +
  xlab("Digital/TV Investment") + ylab("Sales") +
  ggtitle("Sales vs Digital & TV Investments") + 
  theme(plot.title = element_text(hjust=0.4), legend.position = "bottom") 

ggsave(paste0(my_dir, "/sales_vs_investments.png"))


## correlations among Sales, TV and Digital Investments
cor(df_data[, c("sales", "tv_spend", "digital_spend")])
#                   sales  tv_spend digital_spend
# sales         1.0000000 0.4406862     0.6647654
# tv_spend      0.4406862 1.0000000     0.0720594
# digital_spend 0.6647654 0.0720594     1.0000000

## fit regression model on the data
fit <- lm(sales ~ tv_spend + digital_spend, data = df_data)

## adjusted R-squared
summary(fit)$adj.r.squared
# [1] 0.5586161
plot(df_data$trend, fit$residuals)

## p-value and significance of each regressor 
summary(fit)$coefficients
# Estimate   Std. Error  t value     Pr(>|t|)
# (Intercept)   6.767146e+06 9.685341e+05 6.986998 6.717198e-07
# tv_spend      1.731542e+00 6.091071e-01 2.842755 9.746777e-03
# digital_spend 4.469165e+00 9.755057e-01 4.581383 1.619200e-04


## calculate the contribution from TV Spend to Sales in percentage and absolute dollar value
tv_coef <- unname(fit$coefficients)[2]
tv_contribution <- sum(df_data$tv_spend) * tv_coef
tv_contribution
# [1] 44108224
tv_contribution_percentage <- tv_contribution / sum(df_data$sales) * 100
tv_contribution_percentage
# [1] 16.60073

## calculate the TV return on investment (ROI)
tv_investment <- sum(df_data$tv_spend)
tv_ROI <- (tv_contribution - tv_investment) /  tv_investment
tv_ROI
# [1] 0.7315423

## Using the planned spend values for the first 3 months of 2018 and your regressions model, 
## calculate the expected sales value for the first 3 months of 2018
df_planned_spend <- read.xlsx(paste0(my_dir, "/data/toy_sales_data.xlsx"), sheetName = "planned_spend")
base_intercept <- unname(fit$coefficients)[1]
digital_coef <- unname(fit$coefficients)[3]
df_planned_spend[, "predicted_sales"] <- base_intercept + df_planned_spend$tv_spend * tv_coef + 
                                            df_planned_spend$digital_spend * digital_coef
df_planned_spend$predicted_sales
# [1]  8334056  9082486 10892394