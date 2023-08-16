# install.packages("dplyr")
# install.packages("data.table")
# install.packages("bit64")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("stringr")


library(dplyr)
library(data.table)
library("bit64")
library(openxlsx)
library(tidyr)
library(stringr)


# read an external R script
source("RScripts/WD.R")


setwd(path_data) # set working directory (to the folder where data files stored)

getwd()


# buildingregister <- fread(file = "D:/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt",header = FALSE,sep = "|" ,encoding = "unknown")

# buildingregister <- data.table::fread(file = "mart_djy_03.txt",header = FALSE,sep = "|" ,encoding = "unknown")

 
# too slow (126 sec)
# system.time({
  # buildingregister <- read.csv("MART/MART_DJY_June2023.csv")
# })

# Use Apache Parquet 
# install.packages("arrow")
library(arrow)

system.time({ # 0.7 sec
buildingregister_df = arrow::read_parquet("MART/MART_DJY_June2023.parquet")
})

# base::data.frame to data.table::data.table 
buildingregister = data.table(buildingregister_df)

# column names 
colnames(buildingregister)

# assign new colnames 
colnames(buildingregister) <- paste0("V", 1:ncol(buildingregister))

colnames(buildingregister)

print(buildingregister)
buildingregister$year <- str_sub(buildingregister$V61, 1, 4)

# 데이터프레임 정리
summarized_data <- buildingregister %>%
  group_by(year, V32) %>%
  summarise(total_value = sum(V29))

summarized_data$year <- as.character(summarized_data$year)
summarized_data$V32 <- as.character(summarized_data$V32)
summarized_data <- summarized_data %>%
  filter(year != "" & V32 != "")

print(summarized_data)

# 피벗 테이블 생성
pivot_data <- tidyr::pivot_wider(summarized_data, names_from = V32, values_from = total_value)

# 피벗 테이블의 year 열을 행 이름으로 변경
pivot_data <- as.data.frame(pivot_data)
rownames(pivot_data) <- pivot_data$year

pivot_data <- lapply(pivot_data, function(col) {
  if(is.character(col)) iconv(col, to = "UTF-8", sub = "byte")
  else col
})

# 엑셀 파일로 저장
# write.xlsx(pivot_data, "summarized_data.xlsx")
 


