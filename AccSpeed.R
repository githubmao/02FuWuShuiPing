# 加载相关package
library(reshape2)
library(data.table)
library(ggplot2)


# 导入数据
setwd(dir = "E:/R/MaWan/02FuWuShuiPing/AccSpeed")  # 设置工作目录
kAccSpeedFileList <- list.files(pattern = "*.txt")  # 分析数据文件名
kAccSpeedDataName <- gsub(".txt", "", kAccSpeedFileList)  # 导入数据集命名

for(i in 1:length(kAccSpeedDataName)){  # 导入数据
  
  kColName <- c("Speed", "Acc")
  
  tmp.file2data <- fread(kAccSpeedFileList[i],
                         header = T,
                         sep = "auto",
                         stringsAsFactors = FALSE,
                         data.table = FALSE,
                         skip = "Vx",
                         col.names = kColName)
  
  # 添加车辆比功率
  tmp.file2data$PowerWeight <- substr(kAccSpeedDataName[i], 9, 11)
  # 加速度单位转化
  tmp.file2data$Acc <- tmp.file2data$Acc * 9.8
  
  # 导入数据集
  assign(kAccSpeedDataName[i], tmp.file2data)
}

# 计算单个速度区间的最大最小加速度等
CalcAccSpeed <- function(data,
                         kSpeedSection = 10){
  # data: 原始数据
  # kSpeedSection：划分的速度区间
  
  df.result <- data.frame(Speed = 0,
                          MaxAcc = 0,
                          P85Acc = 0,
                          MeanAcc = 0,
                          MedianAcc = 0,
                          MinAcc = 0)
  
  kMaxSpeed <- (max(data$Speed) %/% kSpeedSection + 1) * kSpeedSection
  kSpeedSectionNum <- kMaxSpeed / kSpeedSection
  
  for(i in 1:kSpeedSectionNum){
    tmpdf.speedsection <- subset(data,
                                 Speed >= (i-1) * kSpeedSection &
                                   Speed <= i * kSpeedSection)
    
    tmpvector.speed <- c(max(tmpdf.speedsection$Acc),
                         quantile(tmpdf.speedsection$Acc, 0.85),
                         mean(tmpdf.speedsection$Acc),
                         median(tmpdf.speedsection$Acc),
                         ifelse(min(tmpdf.speedsection$Acc) < 0,
                                0,
                                min(tmpdf.speedsection$Acc)))
    
    df.result <- rbind(df.result, c(i*kSpeedSection, tmpvector.speed))
  }
  
  df.result <- rbind(df.result, c((i+1)*kSpeedSection, 0, 0, 0, 0, 0, 0))
  
  return(df.result[-1, ])
}


# 计算不同比功率的速度加速度数据框
df.pw4.5 <- CalcAccSpeed(AccSpeed4.5)
df.pw6.0 <- CalcAccSpeed(AccSpeed6.0)
df.pw8.0 <- CalcAccSpeed(AccSpeed8.0)
df.pw9.3 <- CalcAccSpeed(AccSpeed9.3)

# 宽数据长数据转换
df.pw4.5 <- melt(df.pw4.5, id = c("Speed"))
df.pw6.0 <- melt(df.pw6.0, id = c("Speed"))
df.pw8.0 <- melt(df.pw8.0, id = c("Speed"))
df.pw9.3 <- melt(df.pw9.3, id = c("Speed"))


# 比功率4.5车型速度-加速度曲线
plot.pw4.5 <- ggplot(df.pw4.5, aes(x = Speed, y = value))+
  geom_line(aes(colour = variable), size = 1.2)+
  labs(title = "比功率4.5", x = "速度，km/h", y = "加速度，m/s2")
plot.pw4.5

# 比功率6.0车型速度-加速度曲线
plot.pw6.0 <- ggplot(df.pw6.0, aes(x = Speed, y = value))+
  geom_line(aes(colour = variable), size = 1.2)+
  labs(title = "比功率6.0", x = "速度，km/h", y = "加速度，m/s2")
plot.pw6.0

# 比功率8.0车型速度-加速度曲线
plot.pw8.0 <- ggplot(df.pw8.0, aes(x = Speed, y = value))+
  geom_line(aes(colour = variable), size = 1.2)+
  labs(title = "比功率8.0", x = "速度，km/h", y = "加速度，m/s2")
plot.pw8.0

# 比功率9.3车型速度-加速度曲线
plot.pw9.3 <- ggplot(df.pw9.3, aes(x = Speed, y = value))+
  geom_line(aes(colour = variable), size = 1.2)+
  labs(title = "比功率9.3", x = "速度，km/h", y = "加速度，m/s2")
plot.pw9.3


