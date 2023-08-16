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


data_fname = "D:/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt"

# or 

wd = "D:/국토교통부_건축물대장_표제부+(2023년+06월)/" # woogy114
wd = "~/Dropbox/Sustainable AI/Data/" # alanbseo 

setwd(wd) # 

getwd()


# buildingregister <- fread(file = "D:/국토교통부_건축물대장_표제부+(2023년+06월)/mart_djy_03.txt",header = FALSE,sep = "|" ,encoding = "unknown")
buildingregister <- data.table::fread(file = "mart_djy_03.txt",header = FALSE,sep = "|" ,encoding = "unknown")


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
write.xlsx(pivot_data, "summarized_data.xlsx")
##############################################################################################################################################################

buildingregister <- read.csv("D:/국토교통부_건축물대장_표제부+(2023년+06월)/MART_DJY_202306.csv")
print(buildingregister)



