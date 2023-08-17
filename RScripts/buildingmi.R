# install.packages("dplyr")
# install.packages("data.table")
# install.packages("bit64")
# install.packages("openxlsx")
# install.packages("tidyr")
# install.packages("stringr")
# install.packages("arrow")
# install.packages("latticeExtra")

library(dplyr)
library(data.table)
library("bit64")
library(openxlsx)
library(tidyr)
library(stringr)

# Use Apache Parquet 
library(arrow)

# Levelplot
library(latticeExtra)

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


system.time({ # 0.7 sec
  buildingregister_df = arrow::read_parquet("MART/MART_DJY_June2023.parquet")
})

# base::data.frame to data.table::data.table 
buildingregister = data.table(buildingregister_df)

class(buildingregister_df)

class(buildingregister)
str(buildingregister)

# column names 
colnames(buildingregister)

# assign new colnames 
colnames(buildingregister) <- paste0("V", 1:ncol(buildingregister))

colnames(buildingregister)

print(buildingregister)

# V61 사용승인_일

head(buildingregister$V61)


buildingregister$year <- str_sub(buildingregister$V61, start = 1, end = 4)

summary(buildingregister$year)
summary(as.numeric(buildingregister$year))

buildingregister$year2 = as.numeric(buildingregister$year)
# buildingregister$year3 = buildingregister$year %>% as.numeric

# Drop meaningless year2 values
buildingregister$year2[buildingregister$year2 > 2023] = NA
buildingregister$year2[buildingregister$year2 < 1000] = NA


table(buildingregister$year2)
# buildingregister$year2 %>% table

buildingregister$year_permission =  as.numeric(buildingregister$V62)
class(buildingregister$year_permission)

table(buildingregister$year_permission > 2023)

buildingregister$year_permission[buildingregister$year_permission > 2023] = NA
buildingregister$year_permission[buildingregister$year_permission < 1000] = NA

table(buildingregister$year_permission > 2023)

table(buildingregister$year_permission < 1000)

# cross tabulation 
crosstb1 = table(buildingregister$year2, buildingregister$year_permission)

str(crosstb1)
crosstb1_m = as.matrix(crosstb1)


levelplot(crosstb1_m)


rownames(crosstb1_m )
rname = rownames(crosstb1_m ) %>% as.numeric 
cname = colnames(crosstb1_m ) %>% as.numeric 

rname_lt1900_idx = which(rname > 1900)
cname_lt1900_idx = which(cname > 1900)

crosstb1_m_small = crosstb1_m[410:532, ]
crosstb1_m_small = crosstb1_m[rname_lt1900_idx, cname_lt1900_idx]

str(crosstb1_m_small) # structure of the object
dim(crosstb1_m_small) # dimension of an matrix-like object 

# @todo xlab ylab 
levelplot(crosstb1_m_small, col.regions = hcl.colors) # provide a function name 
levelplot(crosstb1_m_small, col.regions = terrain.colors(n = 10)) # or a colour vector


# color code 0 by white 
# @todo set desired number of breaks
levelplot(crosstb1_m_small, col.regions = c("white", terrain.colors(30) ), )


# Another cross-tabulation to find out how many NAs in each column

table(is.na(buildingregister$year2))
table(is.na(buildingregister$year_permission))

crosstb2 = table(!is.na(buildingregister$year2), !is.na(buildingregister$year_permission))
crosstb2

# Replace NA of the extracted year column by the year of permission 
buildingregister$year_final = buildingregister$year2

# locate non-NA values from the year of permission data
idx_2064 = (is.na(buildingregister$year2)) & (!is.na(buildingregister$year_permission))
table(idx_2064)

# summary of the 
summary(buildingregister$year_permission[idx_2064])
summary(buildingregister$year_final[idx_2064])

# index of index 
idx_2064_short = which(idx_2064)
str(idx_2064_short)

buildingregister$year_final[idx_2064] = buildingregister$year_permission[idx_2064]
# buildingregister$year_final[idx_2064_short ] = buildingregister$year_permission[idx_2064_short]

# see how many year values are still missing
table(is.na(buildingregister$year_final))

summary(buildingregister$year_final)

# Box plot
boxplot(buildingregister$year_final)

# > help(levelplot)
# > ??levelplot


# Target years (1960 ~ 2023)
buildingregister$year_target = buildingregister$year_final
buildingregister$year_target[buildingregister$year_target < 1960] = NA


# 데이터프레임 정리
summarized_data <- buildingregister %>%
  group_by(year_target, V32) %>% # year and 구조코드(V32)
  summarise(total_value = sum(V29)) # V29 연면적(㎡)


summarized_data

summarized_data$year_target <- as.character(summarized_data$year_target)
summarized_data$V32 <- as.character(summarized_data$V32)
summarized_data <- summarized_data %>%
  filter(year_target != "" & V32 != "")

print(summarized_data)

# 피벗 테이블 생성
pivot_data <- tidyr::pivot_wider(summarized_data, names_from = V32, values_from = total_value)

# 피벗 테이블의 year 열을 행 이름으로 변경
pivot_data <- as.data.frame(pivot_data)
rownames(pivot_data) <- pivot_data$year_target

pivot_data <- lapply(pivot_data, function(col) {
  if(is.character(col)) iconv(col, to = "UTF-8", sub = "byte")
  else col
})

# 엑셀 파일로 저장
# 
# paste0("0", "aaa")
# paste("0", "aaa")
# paste("0", "aaa", "bbb", sep = "_")


write.xlsx(pivot_data, paste0("summarized_data_", Sys.Date(), ".xlsx"))

# or 
fname_out =  paste0(path_out, "summarized_data_", Sys.Date(), ".xlsx")
write.xlsx(pivot_data, fname_out)

file.exists(fname_out)




# V26	대지_면적(㎡)
# V27	건축_면적(㎡)
# V28	건폐_율(%)
# V29	연면적(㎡)
# V30	용적_률_산정_연면적(㎡)
# V31	용적_률(%)
# V32	구조_코드
# V33	구조_코드_명
# V34	기타_구조
# V35	주_용도_코드
# V36	주_용도_코드_명
# V37	기타_용도
# V40	기타_지붕
# V41	세대_수(세대)
# V42	가구_수(가구)
# V43	높이(m)

target_cols = c("V26", "V27") 
target_cols = paste0("V", c(26, 27, 28, 29, 30, 31, 32, 43))

# see if all target colums found in the MART data 
target_cols %in% colnames(buildingregister)


buildingregister_small = buildingregister[, paste0("V", c(26, 27, 28, 29, 30, 31, 32, 43))]

# for (colname_idx in 1:8 ) {
# for (colname_idx in 1:length(target_cols)) {
for (colname_idx in seq_along(target_cols)) {
  
  print(target_cols[colname_idx])
  dt_tmp = buildingregister_small[, ..colname_idx]
  tb1 = table(is.na(dt_tmp))
  print(tb1)
}




dt_tmp = buildingregister_small[, "V31"]
dt_tmp2 = as.numeric(dt_tmp$V31)
summary(dt_tmp2)
table(dt_tmp2 > 0)


table(as.numeric(buildingregister_small$V27)>0)

table(as.numeric(buildingregister_small$V28)>100)
table(as.numeric(buildingregister_small$V28)<100)

