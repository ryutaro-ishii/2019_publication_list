library("magrittr")
library("dplyr")
library("data.table")  
library("readxl")
library("xlsx")
 
# マスクされていますの対応: https://note.com/eiko_dokusho/n/n462c2145f55c
# remove.packages(package:data.table)
# remove.packages(package:stats)
# remove.packages(package:stats(package:base))
  
#業績集のcsvファイルの読み込み
# data <- read.csv("/Users/work/Documents/GitHub/2019年業績集/IF集計用.csv", sep = ",", header = TRUE)
 
# Remove unncessary column
# 参考文献: https://www.geeksforgeeks.org/import-only-selected-columns-of-data-from-csv-in-r/
# library("data.table")  
myData <- fread("IF集計用.csv", select = c("域", "IF", "責任著者"))

# Make summary table
# 説明: https://qiita.com/Quantas/items/c06388039010c28a3e12

域 <- c(rep("生命医科学域",16), rep("臨床医学域",16), rep("保健医療学域",16))
IFrange <- rep(c(rep("0-1", 2), rep("1-2.5", 2), rep("2.5-5", 2), rep("5-7.5", 2), rep("7.5-10", 2), rep("10-15", 2), rep("15-20", 2), rep("20+", 2)), 3)
Aut <- rep((c("責任著者", "その他")), 24)
Cnt <- rep(0,48)

# "%>%"の意味: https://www.medi-08-data-06.work/entry/paipe_r
# library("magrittr")
# library("dplyr")
Cnt[1] = nrow(myData %>% filter(域=="生命医科学域", IF>=0, IF<1, 責任著者==1))
Cnt[2] = nrow(myData %>% filter(域=="生命医科学域", IF>=0, IF<1, 責任著者==0))
Cnt[3] = nrow(myData %>% filter(域=="生命医科学域", IF>=1, IF<2.5, 責任著者==1))
Cnt[4] = nrow(myData %>% filter(域=="生命医科学域", IF>=1, IF<2.5, 責任著者==0))
Cnt[5] = nrow(myData %>% filter(域=="生命医科学域", IF>=2.5, IF<5, 責任著者==1))
Cnt[6] = nrow(myData %>% filter(域=="生命医科学域", IF>=2.5, IF<5, 責任著者==0))
Cnt[7] = nrow(myData %>% filter(域=="生命医科学域", IF>=5, IF<7.5, 責任著者==1))
Cnt[8] = nrow(myData %>% filter(域=="生命医科学域", IF>=5, IF<7.5, 責任著者==0))
Cnt[9] = nrow(myData %>% filter(域=="生命医科学域", IF>=7.5, IF<10, 責任著者==1))
Cnt[10] = nrow(myData %>% filter(域=="生命医科学域", IF>=7.5, IF<10, 責任著者==0))
Cnt[11] = nrow(myData %>% filter(域=="生命医科学域", IF>=10, IF<15, 責任著者==1))
Cnt[12] = nrow(myData %>% filter(域=="生命医科学域", IF>=10, IF<15, 責任著者==0))
Cnt[13] = nrow(myData %>% filter(域=="生命医科学域", IF>=15, IF<20, 責任著者==1))
Cnt[14] = nrow(myData %>% filter(域=="生命医科学域", IF>=15, IF<20, 責任著者==0))
Cnt[15] = nrow(myData %>% filter(域=="生命医科学域", IF>=20, 責任著者==1))
Cnt[16] = nrow(myData %>% filter(域=="生命医科学域", IF>=20, 責任著者==0))

Cnt[17] = nrow(myData %>% filter(域=="臨床医学域", IF>=0, IF<1, 責任著者==1))
Cnt[18] = nrow(myData %>% filter(域=="臨床医学域", IF>=0, IF<1, 責任著者==0))
Cnt[19] = nrow(myData %>% filter(域=="臨床医学域", IF>=1, IF<2.5, 責任著者==1))
Cnt[20] = nrow(myData %>% filter(域=="臨床医学域", IF>=1, IF<2.5, 責任著者==0))
Cnt[21] = nrow(myData %>% filter(域=="臨床医学域", IF>=2.5, IF<5, 責任著者==1))
Cnt[22] = nrow(myData %>% filter(域=="臨床医学域", IF>=2.5, IF<5, 責任著者==0))
Cnt[23] = nrow(myData %>% filter(域=="臨床医学域", IF>=5, IF<7.5, 責任著者==1))
Cnt[24] = nrow(myData %>% filter(域=="臨床医学域", IF>=5, IF<7.5, 責任著者==0))
Cnt[25] = nrow(myData %>% filter(域=="臨床医学域", IF>=7.5, IF<10, 責任著者==1))
Cnt[26] = nrow(myData %>% filter(域=="臨床医学域", IF>=7.5, IF<10, 責任著者==0))
Cnt[27] = nrow(myData %>% filter(域=="臨床医学域", IF>=10, IF<15, 責任著者==1))
Cnt[28] = nrow(myData %>% filter(域=="臨床医学域", IF>=10, IF<15, 責任著者==0))
Cnt[29] = nrow(myData %>% filter(域=="臨床医学域", IF>=15, IF<20, 責任著者==1))
Cnt[30] = nrow(myData %>% filter(域=="臨床医学域", IF>=15, IF<20, 責任著者==0))
Cnt[31] = nrow(myData %>% filter(域=="臨床医学域", IF>=20, 責任著者==1))
Cnt[32] = nrow(myData %>% filter(域=="臨床医学域", IF>=20, 責任著者==0))

Cnt[33] = nrow(myData %>% filter(域=="保健医療学域", IF>=0, IF<1, 責任著者==1))
Cnt[34] = nrow(myData %>% filter(域=="保健医療学域", IF>=0, IF<1, 責任著者==0))
Cnt[35] = nrow(myData %>% filter(域=="保健医療学域", IF>=1, IF<2.5, 責任著者==1))
Cnt[36] = nrow(myData %>% filter(域=="保健医療学域", IF>=1, IF<2.5, 責任著者==0))
Cnt[37] = nrow(myData %>% filter(域=="保健医療学域", IF>=2.5, IF<5, 責任著者==1))
Cnt[38] = nrow(myData %>% filter(域=="保健医療学域", IF>=2.5, IF<5, 責任著者==0))
Cnt[39] = nrow(myData %>% filter(域=="保健医療学域", IF>=5, IF<7.5, 責任著者==1))
Cnt[40] = nrow(myData %>% filter(域=="保健医療学域", IF>=5, IF<7.5, 責任著者==0))
Cnt[41] = nrow(myData %>% filter(域=="保健医療学域", IF>=7.5, IF<10, 責任著者==1))
Cnt[42] = nrow(myData %>% filter(域=="保健医療学域", IF>=7.5, IF<10, 責任著者==0))
Cnt[43] = nrow(myData %>% filter(域=="保健医療学域", IF>=10, IF<15, 責任著者==1))
Cnt[44] = nrow(myData %>% filter(域=="保健医療学域", IF>=10, IF<15, 責任著者==0))
Cnt[45] = nrow(myData %>% filter(域=="保健医療学域", IF>=15, IF<20, 責任著者==1))
Cnt[46] = nrow(myData %>% filter(域=="保健医療学域", IF>=15, IF<20, 責任著者==0))
Cnt[47] = nrow(myData %>% filter(域=="保健医療学域", IF>=20, 責任著者==1))
Cnt[48] = nrow(myData %>% filter(域=="保健医療学域", IF>=20, 責任著者==0))

mySum <- as.data.frame(cbind(域=域, IFrange=IFrange, Aut = Aut, Cnt=Cnt))

# Excelファイル出力
# library("readxl")
# library("xlsx")
file <- paste0(getwd(), "/SummaryTable.xlsx")
write.xlsx2(mySum, file, col.names = TRUE, row.names = TRUE, append = FALSE)

