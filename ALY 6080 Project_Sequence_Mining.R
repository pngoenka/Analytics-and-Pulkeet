# Loading the required libraries
library(tidyverse) # data manipulation
library(arulesSequences) # run the sequence mining algorithm
library(readr)

## Loading the csv file
X20220413_Northeastern_AwanTunai_Capstone_Data <- read_csv("Documents/Personal/Northeastern University /Academic Matters/Spring_22 Quarter/ALY 6080/Project/20220413_Northeastern_AwanTunai_Capstone_Data.csv")

# View the loaded data
View(X20220413_Northeastern_AwanTunai_Capstone_Data)

# backup the data into a new dataframe object
mba <- X20220413_Northeastern_AwanTunai_Capstone_Data

## convert date (Year-Month-Day) column in "mba" to 3 separate columns (Year, Month, Day)
mba = mba %>% mutate(date = ymd(date)) %>% mutate_at(vars(date), funs(year, month, day))

# subset merchant id, purchase date and sku id into a new dataframe
df <- mba %>% dplyr::select(merchant_id, date, sku_id)

# name the columns of "df" properly 
names(df)[1] <- 'customer.identifier'
names(df)[2] <- 'purchase.date'
names(df)[3] <- 'product'

## EDA 
df1 <- df %>% 
  group_by(customer.identifier) %>% 
  arrange(purchase.date) %>% 
  # Remove Instances where the same product appears repeatedly
  distinct(customer.identifier, product, .keep_all = TRUE) %>%
  # Create Item ID Within Customer ID
  dplyr::mutate(item_id = row_number()) %>% 
  select(customer.identifier, purchase.date, item_id, product) %>% 
  ungroup() %>% 
  #Convert Everything to Factor
  mutate(across(.cols = c("customer.identifier", "product"), .f = as.factor))

df1 <- df1[order(df1$customer.identifier),] # ascending order

# handle the special case where one person purchased multiple products on the same date  --------
df2 <- df1
# create unique id for each customer-date pair
df2$unique<-paste0(as.character(df2$customer.identifier)," ", as.character(df2$purchase.date)) 
df2 <- df2 %>% 
  # if a customer purchased multiple products on the same date, 
  # we need to merge these products into a basket like (A,B) on a single row
  # otherwise, cspade will throw an error 
  dplyr::group_by(unique) %>%
  dplyr::summarise(product = paste(product, collapse = ","))

# restore customer identifier that was lost in the last step
df2$customer.identifier <- word(df2$unique, 1) 

# restore purchase date that was lost in the last step
df2$purchase.date <- word(df2$unique, 2)  

df2 <- df2 %>% 
  group_by(customer.identifier) %>% 
  arrange(purchase.date) %>% 
  dplyr::mutate(item_id = row_number()) %>% #Create Item ID Within customer identifier
  select(customer.identifier, purchase.date, item_id, product) %>% 
  ungroup()

df2 <- df2 %>% dplyr::arrange(customer.identifier)

save(df2,file="/Users/emmanuelnasamu/Documents/Personal/Northeastern University /Academic Matters/Spring_22 Quarter/ALY 6080/Project/R Code/df2.Rda")

# c-spade pre-process -----------------------------------------------------

load("/Users/emmanuelnasamu/Documents/Personal/Northeastern University /Academic Matters/Spring_22 Quarter/ALY 6080/Project/R Code/df2.Rda")
#backup df2
df_bk <- df2

df2 %>% head(5) %>% knitr::kable()
#df2$product <-as.factor(df2$product)
sessions <-  as(df2 %>% dplyr::transmute(items = product), "transactions")
transactionInfo(sessions)$sequenceID <- df2$customer.identifier
transactionInfo(sessions)$eventID <- df2$item_id
itemLabels(sessions) <- str_replace_all(itemLabels(sessions), "items=", "")
inspect(head(sessions,10))
# cspade ------------------------------------------------------------------

itemsets <- cspade(sessions, 
                   parameter = list(support = 0.001), 
                   control = list(verbose = FALSE))
inspect((itemsets))
df3 <- itemsets

# output all results
df3 <- as(df3, "data.frame") %>% as_tibble()
df3$pattern <- (str_count(df3$sequence, ",") + 1)
df3 <- df3[order(-df3$support),] # descending
write.csv(x=df3, file="/Users/emmanuelnasamu/Documents/Personal/Northeastern University /Academic Matters/Spring_22 Quarter/ALY 6080/Project/R Code/all_results.csv", row.names=FALSE)

# output top results
c <- df3 %>% group_by(pattern) %>% slice_max(order_by = support, n = 20)
write.csv(x=c, file="/Users/emmanuelnasamu/Documents/Personal/Northeastern University /Academic Matters/Spring_22 Quarter/ALY 6080/Project/R Code/top_results.csv", row.names=FALSE)
