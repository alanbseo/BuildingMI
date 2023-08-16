# install.packages("data.table")
library(data.table)


# only UTF-8
# my_data <- fread("~/Downloads/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt", encoding = "EUC-KR")


my_data <- readLines("~/Downloads/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt", encoding = "EUUTF")
str(my_data)                  

Encoding(my_data) = "CP949"

str(my_data)                  


my_data2 <- readLines("~/Downloads/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt")
str(my_data)                  
str(my_data2)                  


my_data3 <- readLines(textConnection("~/Downloads/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt", encoding = "bytes"), encoding="CP949")
str(my_data3)                  


my_data4 <- read.csv("~/Dropbox/Sustainable AI/MFA workshop/MART_DJY.csv")
str(my_data4)                  


# Apache Parquet
library(arrow)


arrow::write_parquet(my_data4, sink = "MART_DJY.parquet")


pq = arrow::read_parquet("~/Dropbox/Sustainable AI/Data/건축물대장 표제부/MART_DJY.parquet")
 