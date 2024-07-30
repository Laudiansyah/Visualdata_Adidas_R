#Tujuan : Melakukan Visualisasi Data Penjualan Adidas yang sudah di modifikasi
#Target : Grafik Deskriptif, Hasil Korelasi, dan Hasil Regresi
#Sumber : https://www.kaggle.com/datasets/heemalichaudhari/adidas-sales-dataset/data

install.packages("readxl")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("ggcorrplot")
install.packages("tidyverse")
install.packages("scales")
install.packages("lubridate")

library(readxl)
library(dplyr)
library(ggplot2)
library(corrplot)
library(ggcorrplot)
library(tidyverse)
library(scales)
library(lubridate)

data <- read_excel("E:/project github/(R) Adidas/Adidas_Datasets1.xlsx")
head(data)

#Top Product by total sales
total_sales_by_product <- aggregate(Total_Sales ~ Product, data = data, sum)
total_sales_by_product <- total_sales_by_product[order(total_sales_by_product$Total_Sales), ]

ggplot(total_sales_by_product, aes(x = reorder(Product, Total_Sales), 
  y = Total_Sales, fill = Product)) + geom_bar(stat = "identity") +
  geom_text(aes(label = comma(Total_Sales)), vjust = -0.5, size = 3) + 
  labs(title = "Top Product by Total Sales", x = "Product", y = "Total Sales") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +    
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette = "Set3")

#Top Product by units sold
total_product_by_unitsold <- aggregate(Units_Sold ~ Product, data = data, sum)
total_product_by_unitsold <- total_product_by_unitsold[order(total_product_by_unitsold$Units_Sold), ]

ggplot(total_product_by_unitsold, aes(x = reorder(Product, Units_Sold), 
  y = Units_Sold, fill = Product)) + geom_bar(stat = "identity") +
  geom_text(aes(label = comma(Units_Sold)), vjust = -0.5, size = 3) + 
  labs(title = "Top Product by Units Sold", x = "Product", y = "Units Sold") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  
  scale_y_continuous(labels = comma) + scale_fill_brewer(palette = "Set3")

#Total Sales by Retailer
data <- data[order(-data$Total_Sales), ]
Retail <- data %>% group_by(Retailer) %>% summarise(Total_Sales = sum(Total_Sales))

top_name <- Retail %>% top_n(10, Total_Sales)
top_name <- top_name %>% arrange(desc(Total_Sales))

ggplot(top_name, aes(x = reorder(Retailer, Total_Sales), y = Total_Sales, fill = Retailer)) +
  geom_bar(stat = "identity") + geom_text (aes(label=comma(Total_Sales), vjust = -0.5))+
  labs(title = "Total Sales by Retailer", x = "Retailer", y = "Total Sales") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3") + scale_y_continuous(labels = comma)

#Monthly Total Sales Trend
data <- data %>%  mutate(Invoice_Month = format(Invoice_Date, "%Y-%m"))

monthly_sales <- data %>% group_by(Invoice_Month) %>% summarise(Total_Sales = sum(Total_Sales))

monthly_sales$Invoice_Month <- factor(monthly_sales$Invoice_Month, 
  levels = unique(monthly_sales$Invoice_Month))

ggplot(monthly_sales, aes(x = Invoice_Month, y = Total_Sales, group = 1)) +
  geom_line(color = "blue") + geom_text (aes(label=comma(Total_Sales), vjust = -0.5))+
  geom_point(color = "blue") + labs(x = "Month", y = "Total Sales", 
  title = "Monthly Total Sales Trend") + scale_y_continuous(labels = comma)+
  scale_x_discrete(labels = function(x) format(as.Date(paste(x, "01", sep="-")), "%b-%Y")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Sales Method Distribution
sales_count <- table(data$Sales_Method)

ggplot(data = data.frame(sales_count), aes(x = "", y = Freq, 
  fill = names(sales_count))) + geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) + labs(title = "Sales Method Distribution",
  fill = "Sales Method", x = NULL, y = NULL) + theme_void() +
  geom_text(aes(label = scales::percent(Freq/sum(Freq)), 
  y = Freq), position = position_stack(vjust = 0.5)) + scale_fill_brewer(palette = "Set3")

#Total 10 Sales by State
data <- data[order(-data$Total_Sales), ]
Negara <- data %>%  group_by(State) %>% summarise(Total_Sales = sum(Total_Sales))

top_name <- Negara %>% top_n(10, Total_Sales)
top_name <- top_name %>% arrange(desc(Total_Sales))

ggplot(top_name, aes(x = reorder(State, Total_Sales), y = Total_Sales, fill = State)) +
  geom_bar(stat = "identity") + labs(title = "Total 10 Sales by State",
  x = "State", y = "Total Sales") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_text(aes(label = scales::comma(Total_Sales)), vjust = -0.5, size = 3) + 
  scale_fill_brewer(palette = "Set3") + scale_y_continuous(labels = comma)

#tidak ada data cleaning dengan asumsi data sudah bagus
#Korelasi
head (data)
selected_data <- select(data,Product, Price_per_Unit, Units_Sold, Total_Sales,
                        Operating_Profit, Operating_Margin)
head(selected_data)

cor_matrix <- cor(selected_data[, c("Price_per_Unit", "Units_Sold", 
                              "Total_Sales", "Operating_Profit",
                              "Operating_Margin")])

ggcorrplot(cor_matrix, type = "lower", lab = TRUE, method = "circle", 
           outline.color = "white", ggtheme = ggplot2::theme_minimal() )

#Regresi
# Analisis regresi linear sederhana
lm_model <- lm(Operating_Profit ~ Total_Sales, data = data)

# Summary dari model regresi
summary(lm_model)