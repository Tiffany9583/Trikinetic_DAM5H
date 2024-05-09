#################################################################################
# Outline (please use Find function to search the codes)
# Function 1 : Output txt file to CSV files
# Function 2 : Plot a line chart for all time step
# Function 3 : Plot a line chart for 24 hours (zeitgeber)
# |__Function 3.1: Plot a line chart for zeitgeber (five group)
# |__Function 3.2: Plot a line chart for zeitgeber (singel group)
# |__Function 3.3: Statistical Analysis for zeitgeber
# Function 4 : customize for combind hours (average count (count/30min))
# |__Function 4.1: Statistical Analysis for combind hours
#    |__Function 4.1.1: TukeyHSD analysis among groups
#    |__Function 4.1.2: TukeyHSD analysis among ratio
# Function 5 : customize for combind hours (total count (count/day))
# |__Function 5.1: Statistical Analysis for combind hours
# Function 6 : Compute CT data for heatmap (Average CT; bind every day)
#
# Author: Yi-Ting Hung (tiffany9583@gmail.com)
#################################################################################



## Load libraries
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)
library(chron)
library(tidyverse)

# ================== Function 1 : Output txt file to CSV files ==================
# Loading Monitor data

m1 <- read.table("Run05/Run05CTM001.txt")
# m1$monitor<-rep(1,length(m1$V1))
head(m1)
m2 <- read.table("Run05/Run05CTM002.txt")
m3 <- read.table("Run05/Run05CTM003.txt")
m4 <- read.table("Run05/Run05CTM004.txt")

Data <- list(m1, m2, m3, m4)
namelist <- c("m1", "m2", "m3", "m4")

for (x in 1:4) {
  data <- data.frame(Data[x])
  data$V3 <- match(data$V3, month.abb) # converint month to numeric
  data$V2 <- paste("20", data$V4, "-", data$V3, "-", formatC(data$V2, width = 2, flag = "0"), sep = "") # reformatting the date
  # head(data)
  data <- data %>%
    filter(V6 == 1)
  data <- data[, -3:-4] # excluding time we dont need
  data <- data[, -5:-7] # excluding time we dont need
  data <- data[, -6:-6] # excluding time we dont need
  # head(data)
  assign(namelist[x], data)
}

# head(m1[,-31:-38])

CV <- m2[, -31:-38]
CV$group <- rep("CV", length(CV$V1))
names(CV) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
CV$fulltime <- as.POSIXct(paste(CV$date, CV$time))
head(CV)

# GF <- m4[, -31:-38]
# GF$group <- rep("GF", length(GF$V1))
# names(GF) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
# GF$fulltime <- as.POSIXct(paste(GF$date, GF$time))
# head(GF)

LB <- m1[, -31:-38]
LB$group <- rep("LB", length(LB$V1))
names(LB) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
LB$fulltime <- as.POSIXct(paste(LB$date, LB$time))
head(LB)


AP <- m3[, -31:-38]
AP$group <- rep("AP", length(AP$V1))
names(AP) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
AP$fulltime <- as.POSIXct(paste(AP$date, AP$time))
head(AP)

AL <- m4[, -31:-38]
AL$group <- rep("AL", length(AL$V1))
names(AL) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
AL$fulltime <- as.POSIXct(paste(AL$date, AL$time))
head(AL)

# Mix
GF <- m1[, -7:-30]
GF <- cbind(GF, m2[, 31:38], m3[, 31:38])
GF$group <- rep("GF", length(GF$V1))
names(GF) <- c("time_step", "date", "time", "trik_status", "data_type", "light_status", paste("t", seq(1, 24), sep = ""), "group")
GF$fulltime <- as.POSIXct(paste(GF$date, GF$time))
head(GF)


fwrite(GF, "Run05/Run05CT_GF.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(LB, "Run05/Run05CT_LB.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(AP, "Run05/Run05CT_AP.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(CV, "Run05/Run05CT_CV.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(AL, "Run05/Run05CT_AL.csv") ### wide format; gitignore because file is too large (> 100mb)


# ================== Function 2 : Plot a line chart for all time step ==================
# ========= Step 1: select the date you need ==============
# ===================Run01===================
GF <- read.csv("CT/Row_CT_data/Run01CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/Row_CT_data/Run01CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/Row_CT_data/Run01CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/Row_CT_data/Run01CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/Row_CT_data/Run01CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF <- read.csv("CT/Row_CT_data/Run02CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/Row_CT_data/Run02CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/Row_CT_data/Run02CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/Row_CT_data/Run02CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/Row_CT_data/Run02CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF <- read.csv("CT/Row_CT_data/Run03CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/Row_CT_data/Run03CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/Row_CT_data/Run03CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/Row_CT_data/Run03CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/Row_CT_data/Run03CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF <- read.csv("CT/Row_CT_data/Run04CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/Row_CT_data/Run04CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/Row_CT_data/Run04CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/Row_CT_data/Run04CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/Row_CT_data/Run04CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF <- read.csv("CT/Row_CT_data/Run05CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/Row_CT_data/Run05CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/Row_CT_data/Run05CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/Row_CT_data/Run05CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/Row_CT_data/Run05CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

GF$fulltime <- as.POSIXct(paste(GF$date, GF$time))
LB$fulltime <- as.POSIXct(paste(LB$date, LB$time))
AP$fulltime <- as.POSIXct(paste(AP$date, AP$time))
CV$fulltime <- as.POSIXct(paste(CV$date, CV$time))
AL$fulltime <- as.POSIXct(paste(AL$date, AL$time))

# data_1[data_1 == 0] <- NA

# ========================== Step 2: cut date==========================
# 利用date切割
myfunc <- function(data, x, y) {
  allm[allm$date >= x & allm$date <= y, ]
}

DATE1 <- as.Date("2023-10-12")
DATE2 <- as.Date("2023-10-13")


# 利用time_step切割
myfunc <- function(data, x, y) {
  data[data$time_step >= x & data$time_step <= y, ]
}
time_step1 <- 2
time_step2 <- 5776

# ==========================Step 3 plot 次數折線圖==========================
# ==========GF_CT==========
GF_CT <- GF %>%
  filter(data_type == "CT")
# head(GF_CT )
# GF_CT <- myfunc(GF_CT, time_step1, time_step2)

data_GF <- GF_CT %>%
  group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
  summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))


# ==========LB_CT==========
LB_CT <- LB %>%
  filter(data_type == "CT")
# head(LB_CT )
# LB_CT <- myfunc(LB_CT, time_step1, time_step2)

data_LB <- LB_CT %>%
  group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
  summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))


# ==========AP_CT==========
AP_CT <- AP %>%
  filter(data_type == "CT")
# head(AP_CT )
# AP_CT <- myfunc(AP_CT, time_step1, time_step2)

data_AP <- AP_CT %>%
  group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
  summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))



# ==========CV_CT==========
CV_CT <- CV %>%
  filter(data_type == "CT")
# head(CV_CT )
# CV_CT <- myfunc(CV_CT, time_step1, time_step2)

data_CV <- CV_CT %>%
  group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
  summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))


# ==========AL_CT==========
AL_CT <- AL %>%
  filter(data_type == "CT")
head(AL_CT)
# AL_CT <- myfunc(AL_CT, time_step1, time_step2)

data_AL <- AL_CT %>%
  group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
  summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))


# ====== Plot a multi line charts ==========
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")


# print("MAX:")
# print(max(data[,-1]))

head(data_AL)
Plot <- function(data) {
  for (i in 1:24) {
    xvar <- "fulltime"
    yvar <- name_lst[[i]]

    p <- ggplot(data = data, aes(x = .data[[xvar]], y = .data[[yvar]])) +
      geom_line(color = "steelblue") +
      theme_bw() +
      theme(
        axis.text.x = element_text(hjust = 0, vjust = 0, angle = 60),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()
      ) +
      # ylim(0, 667)
      ylim(0, max(data[, -1]))

    p <- p + annotate(
      geom = "rect", xmin = as.POSIXct("2023-12-07 19:00:00"), xmax = as.POSIXct("2023-12-08 7:00:00"),
      ymin = -Inf, ymax = +Inf, alpha = 0.4
    ) +
      annotate(
        geom = "rect", xmin = as.POSIXct("2023-12-08 19:00:00"), xmax = as.POSIXct("2023-12-09 7:00:00"),
        ymin = -Inf, ymax = +Inf, alpha = 0.4
      ) +
      annotate(
        geom = "rect", xmin = as.POSIXct("2023-12-09 19:00:00"), xmax = as.POSIXct("2023-12-10 7:00:00"),
        ymin = -Inf, ymax = +Inf, alpha = 0.4
      ) +
      annotate(
        geom = "rect", xmin = as.POSIXct("2023-12-10 19:00:00"), xmax = as.POSIXct("2023-12-11 7:00:00"),
        ymin = -Inf, ymax = +Inf, alpha = 0.4
      )

    plot_lst[[i]] <- p
  }

  # diet_name_lst <-c('t1', 't2', 't3', 't4', 't5', 't6',  't9' , 't10', 't11', 't12', 't13', 't14', 't17','t18', 't19','t20','t21','t22' ,'t25','t26','t27','t28','t29','t30' )
  # Combine all plots
  v1 <- cowplot::plot_grid(plotlist = plot_lst, nrow = 4, labels = name_lst, label_size = 8, vjust = 0.5)
  return(v1)
}



ggsave(
  "CT/Run05CT_CV.png",
  Plot(data_CV),
  width = 16,
  height = 8,
  dpi = 300
)

ggsave(
  "Run05/Run05CT_GF.png",
  Plot(data_GF),
  width = 16,
  height = 8,
  dpi = 300
)

ggsave(
  "Run05/Run05CT_AP.png",
  Plot(data_AP),
  width = 16,
  height = 8,
  dpi = 300
)


ggsave(
  "Run05/Run05CT_LB.png",
  Plot(data_LB),
  width = 16,
  height = 8,
  dpi = 300
)

ggsave(
  "Run05/Run05CT_AL.png",
  Plot(data_AL),
  width = 16,
  height = 8,
  dpi = 300
)









# ====== Plot a single line chart  ==========

data_GF <- data

p <- ggplot() +
  geom_line(data = data_GF, aes(x = time, y = t13), color = "steelblue") +
  geom_line(data = data_CV, aes(x = time, y = t13), color = "orangered3") +
  # geom_line(data=data_AP, aes(x=time, y=t13), color = "darkgreen")+
  # geom_line(data=data_LB, aes(x=time, y=t13), color = "goldenrod3")+

  theme_bw() +
  # theme(axis.text.x = element_text(hjust=0,vjust=0,angle=60)) +
  labs(x = "Time", y = "Activity") +
  ylim(0, 540) +
  scale_x_continuous(breaks = c(as.POSIXct("2023-10-12 19:00:00"), as.POSIXct("2023-10-13 19:00:00"), as.POSIXct("2023-10-14 19:00:00"), as.POSIXct("2023-10-15 19:00:00")), labels = c("Day 1", "Day 2", "Day 3", "Day 4"))

# p1 <- p + annotate(geom = "rect", xmin = as.POSIXct('2023-10-11 19:00:00'), xmax = as.POSIXct('2023-10-12 7:00:00'),
#              ymin = -Inf, ymax = +Inf, alpha = 0.4)+
p1 <- p + annotate(
  geom = "rect", xmin = as.POSIXct("2023-10-12 19:00:00"), xmax = as.POSIXct("2023-10-13 7:00:00"),
  ymin = -Inf, ymax = +Inf, alpha = 0.4
) +
  annotate(
    geom = "rect", xmin = as.POSIXct("2023-10-13 19:00:00"), xmax = as.POSIXct("2023-10-14 7:00:00"),
    ymin = -Inf, ymax = +Inf, alpha = 0.4
  ) +
  annotate(
    geom = "rect", xmin = as.POSIXct("2023-10-14 19:00:00"), xmax = as.POSIXct("2023-10-15 7:00:00"),
    ymin = -Inf, ymax = +Inf, alpha = 0.4
  ) +
  annotate(
    geom = "rect", xmin = as.POSIXct("2023-10-15 19:00:00"), xmax = as.POSIXct("2023-10-16 7:00:00"),
    ymin = -Inf, ymax = +Inf, alpha = 0.4
  )
p1

pdf(file = "CV_AX_t13.pdf", width = 6, height = 5)
p1

dev.off()








ggsave(
  "CV.png",
  p1,
  width = 6,
  height = 4,
  dpi = 300
)




# ================== Function 3 : Plot a line chart for 24 hours (zeitgeber) ==================

# ===================Run01===================
GF_1 <- read.csv("CT/Row_CT_data/Run01CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("CT/Row_CT_data/Run01CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("CT/Row_CT_data/Run01CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("CT/Row_CT_data/Run01CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("CT/Row_CT_data/Run01CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("CT/Row_CT_data/Run02CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("CT/Row_CT_data/Run02CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("CT/Row_CT_data/Run02CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("CT/Row_CT_data/Run02CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("CT/Row_CT_data/Run02CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("CT/Row_CT_data/Run03CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("CT/Row_CT_data/Run03CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("CT/Row_CT_data/Run03CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("CT/Row_CT_data/Run03CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("CT/Row_CT_data/Run03CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("CT/Row_CT_data/Run04CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("CT/Row_CT_data/Run04CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("CT/Row_CT_data/Run04CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("CT/Row_CT_data/Run04CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("CT/Row_CT_data/Run04CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("CT/Row_CT_data/Run05CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("CT/Row_CT_data/Run05CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("CT/Row_CT_data/Run05CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("CT/Row_CT_data/Run05CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("CT/Row_CT_data/Run05CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")


GF_1$fulltime <- as.POSIXct(paste(GF_1$date, GF_1$time))
LB_1$fulltime <- as.POSIXct(paste(LB_1$date, LB_1$time))
AP_1$fulltime <- as.POSIXct(paste(AP_1$date, AP_1$time))
CV_1$fulltime <- as.POSIXct(paste(CV_1$date, CV_1$time))
AL_1$fulltime <- as.POSIXct(paste(AL_1$date, AL_1$time))

GF_2$fulltime <- as.POSIXct(paste(GF_2$date, GF_2$time))
LB_2$fulltime <- as.POSIXct(paste(LB_2$date, LB_2$time))
AP_2$fulltime <- as.POSIXct(paste(AP_2$date, AP_2$time))
CV_2$fulltime <- as.POSIXct(paste(CV_2$date, CV_2$time))
AL_2$fulltime <- as.POSIXct(paste(AL_2$date, AL_2$time))

GF_3$fulltime <- as.POSIXct(paste(GF_3$date, GF_3$time))
LB_3$fulltime <- as.POSIXct(paste(LB_3$date, LB_3$time))
AP_3$fulltime <- as.POSIXct(paste(AP_3$date, AP_3$time))
CV_3$fulltime <- as.POSIXct(paste(CV_3$date, CV_3$time))
AL_3$fulltime <- as.POSIXct(paste(AL_3$date, AL_3$time))

GF_4$fulltime <- as.POSIXct(paste(GF_4$date, GF_4$time))
LB_4$fulltime <- as.POSIXct(paste(LB_4$date, LB_4$time))
AP_4$fulltime <- as.POSIXct(paste(AP_4$date, AP_4$time))
CV_4$fulltime <- as.POSIXct(paste(CV_4$date, CV_4$time))
AL_4$fulltime <- as.POSIXct(paste(AL_4$date, AL_4$time))

GF_5$fulltime <- as.POSIXct(paste(GF_5$date, GF_5$time))
LB_5$fulltime <- as.POSIXct(paste(LB_5$date, LB_5$time))
AP_5$fulltime <- as.POSIXct(paste(AP_5$date, AP_5$time))
CV_5$fulltime <- as.POSIXct(paste(CV_5$date, CV_5$time))
AL_5$fulltime <- as.POSIXct(paste(AL_5$date, AL_5$time))

# myfunc <- function(data, x, y) {
#   data[data$time_step >= x & data$time_step <= y, ]
# }
# time_step1 <- 2
# time_step2 <- 5761

# ======================= combind for each hour =======================
cal_zeitgeber <- function(data, batch_no) {
  # data_1 <- data %>%
  #   filter(data_type == "MT")
  # data_1 <- myfunc(data_1, time_step1, time_step2)
  data$time2 <- hour(data$fulltime)
  data <- data %>%
    group_by(time2) %>%
    summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))

  # data_1$hour <- c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
  data$hour <- c(18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
  data$batch <- rep(batch_no)
  return(data)
}



data_GF <- rbind(cal_zeitgeber(GF_1, "B1"), cal_zeitgeber(GF_2, "B2"), cal_zeitgeber(GF_3, "B3"), cal_zeitgeber(GF_4, "B4"), cal_zeitgeber(GF_5, "B5"))
fwrite(data_GF, "CT/zeitgeber/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- rbind(cal_zeitgeber(CV_1, "B1"), cal_zeitgeber(CV_2, "B2"), cal_zeitgeber(CV_3, "B3"), cal_zeitgeber(CV_4, "B4"), cal_zeitgeber(CV_5, "B5"))
fwrite(data_CV, "CT/zeitgeber/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(cal_zeitgeber(AP_1, "B1"), cal_zeitgeber(AP_2, "B2"), cal_zeitgeber(AP_3, "B3"), cal_zeitgeber(AP_4, "B4"), cal_zeitgeber(AP_5, "B5"))
fwrite(data_AP, "CT/zeitgeber/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(cal_zeitgeber(LB_1, "B1"), cal_zeitgeber(LB_2, "B2"), cal_zeitgeber(LB_3, "B3"), cal_zeitgeber(LB_4, "B4"), cal_zeitgeber(LB_5, "B5"))
fwrite(data_LB, "CT/zeitgeber/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(cal_zeitgeber(AL_1, "B1"), cal_zeitgeber(AL_2, "B2"), cal_zeitgeber(AL_3, "B3"), cal_zeitgeber(AL_4, "B4"), cal_zeitgeber(AL_5, "B5"))
fwrite(data_AL, "CT/zeitgeber/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)


# ================== output average zeitgeber datas (remove ghost datas in pervious files)===================
average_zeitgeber <- function(data) {
  data_1 <- data %>%
    group_by(hour) %>%
    summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
  return(data_1)
}

GF <- read.csv("CT/zeitgeber/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(GF), "CT/zeitgeber/GF.csv") ### wide format; gitignore because file is too large (> 100mb)

CV <- read.csv("CT/zeitgeber/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(CV), "CT/zeitgeber/CV.csv") ### wide format; gitignore because file is too large (> 100mb)

AP <- read.csv("CT/zeitgeber/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AP), "CT/zeitgeber/AP.csv") ### wide format; gitignore because file is too large (> 100mb)

AL <- read.csv("CT/zeitgeber/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AL), "CT/zeitgeber/AL.csv") ### wide format; gitignore because file is too large (> 100mb)

LB <- read.csv("CT/zeitgeber/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(LB), "CT/zeitgeber/LB.csv") ### wide format; gitignore because file is too large (> 100mb)

# max(data_CV)


# ================== load datas ==================
GF <- read.csv("CT/zeitgeber/GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/zeitgeber/LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/zeitgeber/AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/zeitgeber/CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/zeitgeber/AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# sat_res_nota <- read.csv("zeitgeber/Sat_significant_diff.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ================== Function 3.1: Plot a line chart for zeitgeber (five group) ==================
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")


print(max(AL[, -1]))
print(max(LB[, -1]))
print(max(CV[, -1]))
print(max(AP[, -1]))
print(max(GF[, -1]))

print(min(AL[, -1]))
print(min(LB[, -1]))
print(min(CV[, -1]))
print(min(AP[, -1]))
print(min(GF[, -1]))

# blueviolet

for (i in 1:24) {
  xvar <- "hour"
  yvar <- name_lst[[i]]

  p <- ggplot() +
    geom_line(data = CV, aes(x = hour, y = .data[[yvar]]), color = "Black") +
    geom_line(data = AL, aes(x = hour, y = .data[[yvar]]), color = "orangered3") +
    geom_line(data = GF, aes(x = hour, y = .data[[yvar]]), color = "steelblue") +
    geom_line(data = AP, aes(x = hour, y = .data[[yvar]]), color = "darkgreen") +
    geom_line(data = LB, aes(x = hour, y = .data[[yvar]]), color = "goldenrod3") +
    theme_bw() +
    # theme_light()+
    labs(x = "Zeitgeber", y = "Count") +
    ylim(4, 230) +
    scale_x_continuous(expand = c(0, 0)) +
    theme(
      axis.text.x = element_text(size = 12, color = "black"),
      axis.title.x = element_text(size = 16, color = "black"),
      axis.text.y = element_text(size = 12, color = "black"),
      axis.title.y = element_text(size = 16, color = "black")
    )

  p1 <- p + annotate(
    geom = "rect", xmin = 12, xmax = 23,
    ymin = -Inf, ymax = +Inf, alpha = 0.4
  )

  p1


  plot_lst[[i]] <- p1
}

# diet_name_lst <-c('t1', 't2', 't3', 't4', 't5', 't6',  't9' , 't10', 't11', 't12', 't13', 't14', 't17','t18', 't19','t20','t21','t22' ,'t25','t26','t27','t28','t29','t30' )
# Combine all plots
v1 <- cowplot::plot_grid(plotlist = plot_lst, nrow = 4, labels = name_lst, label_size = 8, vjust = 0.5)
v1


pdf(file = "CT/zeitgeber/zeitgeber.pdf", width = 20, height = 10)
v1
dev.off()


ggsave(
  "CT/zeitgeber/zeitgeber.png",
  v1,
  width = 16,
  height = 8,
  dpi = 300
)


# ================== Function 3.2: Plot a line chart for zeitgeber (singel group) ==================
# t7
p <- ggplot() +
  geom_line(data = CV, aes(x = hour, y = t7), color = "Black") +
  geom_line(data = AL, aes(x = hour, y = t7), color = "orangered3") +
  geom_line(data = GF, aes(x = hour, y = t7), color = "steelblue") +
  geom_line(data = AP, aes(x = hour, y = t7), color = "darkgreen") +
  geom_line(data = LB, aes(x = hour, y = t7), color = "goldenrod3") +
  theme_bw() +
  # theme_light()+
  labs(x = "Zeitgeber", y = "Activity") +
  ylim(0, 870) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 16, color = "black")
  )

p1 <- p + annotate(
  geom = "rect", xmin = 12, xmax = 23,
  ymin = -Inf, ymax = +Inf, alpha = 0.4
) +
  geom_text(
    size = 3,
    data = CV,
    mapping = aes(x = 12, y = t7[hour == "12"] + 30, label = "b")
  ) +

  geom_text(
    size = 3,
    data = AL,
    color = "orangered3",
    mapping = aes(x = 12, y = t7[hour == "12"] + 30, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = AP,
    color = "darkgreen",
    mapping = aes(x = 12, y = t7[hour == "12"] + 30, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = LB,
    color = "goldenrod3",
    mapping = aes(x = 12, y = t7[hour == "12"] + 30, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = GF,
    color = "steelblue",
    mapping = aes(x = 12, y = t7[hour == "12"] + 30, label = "a")
  )

p1


# pdf(file = "zeitgeber/t23.pdf",width = 6, height = 5)
# p1

# dev.off()

ggsave(
  "zeitgeber/t7.png",
  p1,
  width = 6,
  height = 5,
  dpi = 300
)


#
sat_res_nota
# t16
p <- ggplot() +
  geom_line(data = CV, aes(x = hour, y = t16), color = "Black") +
  geom_line(data = AL, aes(x = hour, y = t16), color = "orangered3") +
  geom_line(data = GF, aes(x = hour, y = t16), color = "steelblue") +
  geom_line(data = AP, aes(x = hour, y = t16), color = "darkgreen") +
  geom_line(data = LB, aes(x = hour, y = t16), color = "goldenrod3") +
  theme_bw() +
  # theme_light()+
  labs(x = "Zeitgeber", y = "Activity") +
  ylim(0, 870) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 16, color = "black")
  )

p1 <- p + annotate(
  geom = "rect", xmin = 12, xmax = 23,
  ymin = -Inf, ymax = +Inf, alpha = 0.4
) +
  geom_text(
    size = 3,
    data = CV,
    mapping = aes(x = 20, y = t16[hour == "20"] + 2, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = AL,
    color = "orangered3",
    mapping = aes(x = 20, y = t16[hour == "20"] + 2, label = "a")
  ) +

  geom_text(
    size = 3,
    data = AP,
    color = "darkgreen",
    mapping = aes(x = 20, y = t16[hour == "20"] + 2, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = LB,
    color = "goldenrod3",
    mapping = aes(x = 20, y = t16[hour == "20"] + 2, label = "b")
  ) +

  geom_text(
    size = 3,
    data = GF,
    color = "steelblue",
    mapping = aes(x = 20, y = t16[hour == "20"] + 2, label = "b")
  )

p1


# pdf(file = "zeitgeber/t23.pdf",width = 6, height = 5)
# p1

# dev.off()

ggsave(
  "zeitgeber/t16.png",
  p1,
  width = 6,
  height = 5,
  dpi = 300
)


sat_res_nota
# t13  hour= 3
p <- ggplot() +
  geom_line(data = CV, aes(x = hour, y = t13), color = "Black") +
  geom_line(data = AL, aes(x = hour, y = t13), color = "orangered3") +
  geom_line(data = GF, aes(x = hour, y = t13), color = "steelblue") +
  geom_line(data = AP, aes(x = hour, y = t13), color = "darkgreen") +
  geom_line(data = LB, aes(x = hour, y = t13), color = "goldenrod3") +
  theme_bw() +
  # theme_light()+
  labs(x = "Zeitgeber", y = "Activity") +
  ylim(0, 870) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 16, color = "black")
  )

p1 <- p + annotate(
  geom = "rect", xmin = 12, xmax = 23,
  ymin = -Inf, ymax = +Inf, alpha = 0.4
) +
  geom_text(
    size = 3,
    data = CV,
    mapping = aes(x = 3, y = t13[hour == "3"] + 15, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = AL,
    color = "orangered3",
    mapping = aes(x = 3, y = t13[hour == "3"] + 30, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = AP,
    color = "darkgreen",
    mapping = aes(x = 3, y = t13[hour == "3"] + 20, label = "a")
  ) +

  geom_text(
    size = 3,
    data = LB,
    color = "goldenrod3",
    mapping = aes(x = 3, y = t13[hour == "3"] + 15, label = "b")
  ) +

  geom_text(
    size = 3,
    data = GF,
    color = "steelblue",
    mapping = aes(x = 3, y = t13[hour == "3"] + 15, label = "ab")
  )

p1


# pdf(file = "zeitgeber/t23.pdf",width = 6, height = 5)
# p1

# dev.off()

ggsave(
  "zeitgeber/t13.png",
  p1,
  width = 6,
  height = 5,
  dpi = 300
)


sat_res_nota
# t8  hour= 4
p <- ggplot() +
  geom_line(data = CV, aes(x = hour, y = t8), color = "Black") +
  geom_line(data = AL, aes(x = hour, y = t8), color = "orangered3") +
  geom_line(data = GF, aes(x = hour, y = t8), color = "steelblue") +
  geom_line(data = AP, aes(x = hour, y = t8), color = "darkgreen") +
  geom_line(data = LB, aes(x = hour, y = t8), color = "goldenrod3") +
  theme_bw() +
  # theme_light()+
  labs(x = "Zeitgeber", y = "Activity") +
  ylim(0, 870) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 16, color = "black")
  )

p1 <- p + annotate(
  geom = "rect", xmin = 12, xmax = 23,
  ymin = -Inf, ymax = +Inf, alpha = 0.4
) +
  geom_text(
    size = 3,
    data = CV,
    mapping = aes(x = 4, y = t8[hour == "4"] - 15, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = AL,
    color = "orangered3",
    mapping = aes(x = 4, y = t8[hour == "4"] + 30, label = "a")
  ) +

  geom_text(
    size = 3,
    data = AP,
    color = "darkgreen",
    mapping = aes(x = 4, y = t8[hour == "4"] + 20, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = LB,
    color = "goldenrod3",
    mapping = aes(x = 4, y = t8[hour == "4"] + 15, label = "ab")
  ) +

  geom_text(
    size = 3,
    data = GF,
    color = "steelblue",
    mapping = aes(x = 4, y = t8[hour == "4"] + 15, label = "b")
  )

p1


# pdf(file = "zeitgeber/t23.pdf",width = 6, height = 5)
# p1

# dev.off()

ggsave(
  "zeitgeber/t8.png",
  p1,
  width = 6,
  height = 5,
  dpi = 300
)





p <- ggplot() +
  geom_line(data = CV, aes(x = hour, y = t24), color = "Black") +
  geom_line(data = AL, aes(x = hour, y = t24), color = "orangered3") +
  geom_line(data = GF, aes(x = hour, y = t24), color = "steelblue") +
  geom_line(data = AP, aes(x = hour, y = t24), color = "darkgreen") +
  geom_line(data = LB, aes(x = hour, y = t24), color = "goldenrod3") +
  theme_bw() +
  # theme_light()+
  labs(x = "Zeitgeber", y = "Activity") +
  ylim(0, 870) +
  scale_x_continuous(expand = c(0, 0)) +
  theme(
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 16, color = "black")
  )

p1 <- p + annotate(
  geom = "rect", xmin = 12, xmax = 23,
  ymin = -Inf, ymax = +Inf, alpha = 0.4
)


ggsave(
  "zeitgeber/t24.png",
  p1,
  width = 6,
  height = 5,
  dpi = 300
)



# ================== Function 3.3: Statistical Analysis for zeitgeber ==================
library(multcompView)

GF <- read.csv("CT/zeitgeber/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/zeitgeber/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/zeitgeber/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/zeitgeber/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/zeitgeber/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

GF$group <- rep("GF")
LB$group <- rep("LB")
AP$group <- rep("AP")
CV$group <- rep("CV")
AL$group <- rep("AL")


data_sat <- rbind(CV, AL, GF, AP, LB)
head(data_sat)



# analysis of variance
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")
sat_res <- data.frame(group = c("CV", "AL", "GF", "AP", "LB"))
rownames(sat_res) <- sat_res$group



for (h in 0:23) {
  # h = 0
  data_sat_now <- data_sat %>%
    filter(hour == h)

  for (i in 1:24) {
    sat_res_pre <- sat_res
    sat_res <- data.frame(group = c("CV", "AL", "GF", "AP", "LB"))
    rownames(sat_res) <- sat_res$group

    tube_no <- name_lst[i]
    # tube_no <- "t2"
    anova <- aov(eval(as.symbol(tube_no)) ~ group, data = data_sat_now)
    Tukey <- TukeyHSD(anova)
    cld <- multcompLetters4(anova, Tukey)
    cld <- as.data.frame.list(cld$group)
    sat_res <- merge(sat_res, cld, by = "row.names", all.x = TRUE)[, 2:3]
    colnames(sat_res) <- c("group", "Letters")
    # sat_res$tube_hour <- paste(tube_no,as.character(h),sep="_")
    sat_res$tube <- tube_no
    sat_res$hour <- h
    if (h != 0 | tube_no != "t1") {
      sat_res <- rbind(sat_res_pre, sat_res)
    }
  }
}
# sat_res
fwrite(sat_res, "CT/zeitgeber/Sat.csv") ### wide format; gitignore because file is too large (> 100mb)

sat_res_nota <- sat_res %>%
  filter(Letters != "a")
fwrite(sat_res_nota, "CT/zeitgeber/Sat_significant_diff.csv") ### wide format; gitignore because file is too large (> 100mb)


# ======================= Function 4 : customize for combind hours(average count)=======================

# ===================Run01===================
GF_1 <- read.csv("CT/Row_CT_data/Run01CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("CT/Row_CT_data/Run01CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("CT/Row_CT_data/Run01CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("CT/Row_CT_data/Run01CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("CT/Row_CT_data/Run01CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("CT/Row_CT_data/Run02CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("CT/Row_CT_data/Run02CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("CT/Row_CT_data/Run02CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("CT/Row_CT_data/Run02CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("CT/Row_CT_data/Run02CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("CT/Row_CT_data/Run03CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("CT/Row_CT_data/Run03CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("CT/Row_CT_data/Run03CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("CT/Row_CT_data/Run03CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("CT/Row_CT_data/Run03CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("CT/Row_CT_data/Run04CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("CT/Row_CT_data/Run04CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("CT/Row_CT_data/Run04CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("CT/Row_CT_data/Run04CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("CT/Row_CT_data/Run04CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("CT/Row_CT_data/Run05CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("CT/Row_CT_data/Run05CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("CT/Row_CT_data/Run05CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("CT/Row_CT_data/Run05CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("CT/Row_CT_data/Run05CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

# time:12:00:00 AM == hour:18 / time:03:00:00 PM == hour:9
CutTimefunc <- function(data, x, y) {
  data[data$time >= x & data$time <= y, ]
}

CutTimefunc <- function(data, x, y) {
  data[data$time >= y | data$time <= x, ]
}

# Bind_time <- hour:9-12 / time:15:00 - 18:00
# Bind_time <- hour:14-17 / time:20:00 - 23:00
# Bind_time <- hour:17-20 / time:23:00 - 03:00
# Bind_time <- hour:15-20 / time:21:00 - 03:00
Time1 <- "15:00:00"
Time2 <- "18:00:00"


dayfunc <- function(data, x, y) {
  data[data$time >= x & data$time <= y, ]
}

nightfunc <- function(data, x, y) {
  data[data$time > y | data$time < x, ]
}

Time1 <- "07:30:00"
Time2 <- "17:29:00"

for (x in 1:25) {
  data <- data.frame(Data[x])
  data <- data %>%
    filter(trik_status == 1)
  # data <- dayfunc(data, Time1, Time2)
  data <- nightfunc(data, Time1, Time2)
  # data <- CutTimefunc(data, Time1, Time2)
  data$fulltime <- as.POSIXct(paste(data$date, data$time))
  data <- data %>%
    summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
  data[data == 0] <- NA
  assign(namelist[x], data)
}

# GF <- rbind(bind_time(GF_1, "B1"), bind_time(GF_2, "B2"), bind_time(GF_3, "B3"), bind_time(GF_4, "B4"), bind_time(GF_5, "B5"))
# CV <- rbind(bind_time(CV_1, "B1"), bind_time(CV_2, "B2"), bind_time(CV_3, "B3"), bind_time(CV_4, "B4"), bind_time(CV_5, "B5"))
# AP <- rbind(bind_time(AP_1, "B1"), bind_time(AP_2, "B2"), bind_time(AP_3, "B3"), bind_time(AP_4, "B4"), bind_time(AP_5, "B5"))
# LB <- rbind(bind_time(LB_1, "B1"), bind_time(LB_2, "B2"), bind_time(LB_3, "B3"), bind_time(LB_4, "B4"), bind_time(LB_5, "B5"))
# AL <- rbind(bind_time(AL_1, "B1"), bind_time(AL_2, "B2"), bind_time(AL_3, "B3"), bind_time(AL_4, "B4"), bind_time(AL_5, "B5"))
GF <- rbind(GF_1, GF_2, GF_3, GF_4, GF_5)
CV <- rbind(CV_1, CV_2, CV_3, CV_4, CV_5)
AP <- rbind(AP_1, AP_2, AP_3, AP_4, AP_5)
LB <- rbind(LB_1, LB_2, LB_3, LB_4, LB_5)
AL <- rbind(AL_1, AL_2, AL_3, AL_4, AL_5)

GF$group <- rep("AX")
CV$group <- rep("CV")
AP$group <- rep("AP")
LB$group <- rep("LB")
AL$group <- rep("AP+LB")

all_data <- rbind(GF, CV, LB, AP, AL)

# fwrite(AL, "CT/customize_bind/AL.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(GF, "CT/customize_bind/GF.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(CV, "CT/customize_bind/CV.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(AP, "CT/customize_bind/AP.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(LB, "CT/customize_bind/LB.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(all_data, "CT/customize_bind/all_data.csv") ### wide format; gitignore because file is too large (> 100mb)
# transfer to satistic anaylise data formate
# data <- read.csv("CT/customize_bind/all_data.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
data <- all_data
samle_meta <- data.frame(
  ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
  level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[1, 1:24]), t(data[2, 1:24]), t(data[3, 1:24]), t(data[4, 1:24]), t(data[5, 1:24])),
    group = rep(data[1, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[6, 1:24]), t(data[7, 1:24]), t(data[8, 1:24]), t(data[9, 1:24]), t(data[10, 1:24])),
    group = rep(data[6, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[11, 1:24]), t(data[12, 1:24]), t(data[13, 1:24]), t(data[14, 1:24]), t(data[15, 1:24])),
    group = rep(data[11, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[16, 1:24]), t(data[17, 1:24]), t(data[18, 1:24]), t(data[19, 1:24]), t(data[20, 1:24])),
    group = rep(data[16, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[21, 1:24]), t(data[22, 1:24]), t(data[23, 1:24]), t(data[24, 1:24]), t(data[25, 1:24])),
    group = rep(data[21, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  )
)

fwrite(data_sat, "CT/customize_bind/nighttime_averageCT_data_sat.csv") ### wide format; gitignore because file is too large (> 100mb)




# ============== Function 4.1: Statistical Analysis for combind hours =================
# ============== Function 4.1.1: TukeyHSD analysis among groups =================
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(data.table)
library(multcomp)
library(multcompView)
data_sat <- read.csv("CT/customize_bind/daytime_averageCT_data_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
head(data_sat)

print(max(data_sat$value, na.rm = TRUE))
print(min(data_sat$value, na.rm = TRUE))

Plot <- function(data_now) {
  plot_data <- data.frame()
  ratio_list <- c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01")

  for (i in ratio_list) {
    data_1 <- data_now %>%
      filter(ratio == i)

    TUKEY <- TukeyHSD(aov(value ~ group, data_1))
    MultComp <- multcompLetters(extract_p(TUKEY$group))
    group <- names(MultComp$Letters)
    Labels <- MultComp$Letters
    df <- data.frame(group, Labels)
    df <- merge(data_1, df, by = "group")

    Max_Val <- data_1 %>%
      group_by(group) %>%
      summarise_at(
        c("value"),
        list(max = max),
        na.rm = TRUE
      ) %>%
      as.data.frame()
    Max_Val
    df <- merge(df, Max_Val, by = "group")
    plot_data <- rbind(plot_data, df)
  }
  # return(plot_data)

  level_order <- c("CV", "AX", "AP", "LB", "AP+LB")

  P <- ggplot(plot_data, aes(x = factor(group, level = level_order), y = value, fill = Labels)) +
    geom_boxplot(alpha = 0.60) +
    geom_point(aes(fill = Labels), size = 2, shape = 21, ) +
    facet_grid(~ factor(ratio, levels = c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"))) +
    geom_text(data = plot_data, aes(x = group, y = max + 8, label = Labels)) +
    theme(legend.position = "none") +
    xlab("Treatment (P:C)") +
    ylab("Average counts (count/30min)") +
    ylim(min(data_sat$value, na.rm = TRUE), max(data_sat$value, na.rm = TRUE) + 10) # !!!!

  return(P)
}

data_45 <- data_sat %>%
  filter(level == 45)
data_90 <- data_sat %>%
  filter(level == 90)
data_180 <- data_sat %>%
  filter(level == 180)
data_360 <- data_sat %>%
  filter(level == 360)


p_45 <- Plot(data_45)
p_90 <- Plot(data_90)
p_180 <- Plot(data_180)
p_360 <- Plot(data_360)



# +++++++++ save Tps plot +++++++++
pdf(file = "CT/customize_bind/daytime_averageCT_group.pdf", width = 12, height = 16)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90 +
    theme(axis.title.x = element_blank()),
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("45", "90", "180", "360"),
  align = "v",
  label_size = 16
)
dev.off()


pdf(file = "CT/customize_bind/45+90.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90,
  ncol = 1,
  labels = c("45", "90"),
  align = "v",
  label_size = 16
)
dev.off()

pdf(file = "CT/customize_bind/180+360.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("180", "360"),
  align = "v",
  label_size = 16
)
dev.off()


# ============== Function 4.1.2: TukeyHSD analysis among ratio =================
data_sat <- read.csv("CT/customize_bind/nighttime_averageCT_data_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
# data

print(max(data_sat$value, na.rm = TRUE))
print(min(data_sat$value, na.rm = TRUE))

Plot <- function(data_now) {
  plot_data <- data.frame()
  group_list <- c("CV", "AX", "AP", "LB", "AP+LB")
  # ratio_list <- c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01")

  for (i in group_list) {
    data_1 <- data_now %>%
      filter(group == i)

    TUKEY <- TukeyHSD(aov(value ~ ratio, data_1))
    MultComp <- multcompLetters(extract_p(TUKEY$ratio))
    ratio <- names(MultComp$Letters)
    Labels <- MultComp$Letters
    df <- data.frame(ratio, Labels)
    df <- merge(data_1, df, by = "ratio")


    Max_Val <- data_1 %>%
      group_by(ratio) %>%
      summarise_at(
        c("value"),
        list(max = max),
        na.rm = TRUE
      ) %>%
      as.data.frame()
    Max_Val
    df <- merge(df, Max_Val, by = "ratio")
    plot_data <- rbind(plot_data, df)
  }
  # return(plot_data)

  # level_order <- c("CV", "AX", "AP", "LB", "AP+LB")
  ratio_order <- c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01")

  P <- ggplot(plot_data, aes(x = factor(ratio, level = ratio_order), y = value, fill = Labels)) +
    geom_boxplot(alpha = 0.60) +
    geom_point(aes(fill = Labels), size = 2, shape = 21, ) +
    facet_grid(~ factor(group, levels = c("CV", "AX", "AP", "LB", "AP+LB"))) +
    geom_text(data = plot_data, aes(x = ratio, y = max + 8, label = Labels)) +
    theme(legend.position = "none") +
    xlab("Ratio (P:C)") +
    ylab("Average counts (count/30min)") +
    ylim(min(data_sat$value, na.rm = TRUE), max(data_sat$value, na.rm = TRUE) + 10) # !!!!

  return(P)
}

data_45 <- data_sat %>%
  filter(level == 45)
data_90 <- data_sat %>%
  filter(level == 90)
data_180 <- data_sat %>%
  filter(level == 180)
data_360 <- data_sat %>%
  filter(level == 360)

p_45 <- Plot(data_45)
p_90 <- Plot(data_90)
p_180 <- Plot(data_180)
p_360 <- Plot(data_360)

# +++++++++ save Tps plot +++++++++
pdf(file = "CT/customize_bind/nighttime_averageCT_ratio.pdf", width = 12, height = 16)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90 +
    theme(axis.title.x = element_blank()),
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("45", "90", "180", "360"),
  align = "v",
  label_size = 16
)
dev.off()

pdf(file = "TAG_Sta_45+90.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90,
  ncol = 1,
  labels = c("45", "90"),
  align = "v",
  label_size = 16
)
dev.off()

pdf(file = "TAG_180+360.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("180", "360"),
  align = "v",
  label_size = 16
)
dev.off()


# ==== [ Statistical Analysis]  for combined counts ====
data_sat <- read.csv("CT/customize_bind/all_data.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
head(data_sat)
# analysis of variance
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")
sat_res <- data.frame(group = c("CV", "AL", "GF", "AP", "LB"))
rownames(sat_res) <- sat_res$group


for (i in 1:24) {
  sat_res_pre <- sat_res
  sat_res <- data.frame(group = c("CV", "AL", "GF", "AP", "LB"))
  rownames(sat_res) <- sat_res$group

  tube_no <- name_lst[i]
  # tube_no <- "t2"
  anova <- aov(eval(as.symbol(tube_no)) ~ group, data = data_sat)
  Tukey <- TukeyHSD(anova)
  cld <- multcompLetters4(anova, Tukey)
  cld <- as.data.frame.list(cld$group)
  sat_res <- merge(sat_res, cld, by = "row.names", all.x = TRUE)[, 2:3]
  colnames(sat_res) <- c("group", "Letters")
  # sat_res$tube_hour <- paste(tube_no,as.character(h),sep="_")
  sat_res$tube <- tube_no
  if (tube_no != "t1") {
    sat_res <- rbind(sat_res_pre, sat_res)
  }
}
sat_res

sat_res_nota <- sat_res %>%
  filter(Letters != "a")





# ======================= Function 5 : customize for combind hours (total count (count/day)) =======================

# ===================Run01===================
GF_1 <- read.csv("CT/Row_CT_data/Run01CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("CT/Row_CT_data/Run01CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("CT/Row_CT_data/Run01CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("CT/Row_CT_data/Run01CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("CT/Row_CT_data/Run01CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("CT/Row_CT_data/Run02CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("CT/Row_CT_data/Run02CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("CT/Row_CT_data/Run02CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("CT/Row_CT_data/Run02CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("CT/Row_CT_data/Run02CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("CT/Row_CT_data/Run03CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("CT/Row_CT_data/Run03CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("CT/Row_CT_data/Run03CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("CT/Row_CT_data/Run03CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("CT/Row_CT_data/Run03CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("CT/Row_CT_data/Run04CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("CT/Row_CT_data/Run04CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("CT/Row_CT_data/Run04CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("CT/Row_CT_data/Run04CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("CT/Row_CT_data/Run04CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("CT/Row_CT_data/Run05CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("CT/Row_CT_data/Run05CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("CT/Row_CT_data/Run05CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("CT/Row_CT_data/Run05CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("CT/Row_CT_data/Run05CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

# time:12:00:00 AM == hour:18 / time:03:00:00 PM == hour:9
CutTimefunc <- function(data, x, y) {
  data[data$time >= x & data$time <= y, ]
}

CutTimefunc <- function(data, x, y) {
  data[data$time >= y | data$time <= x, ]
}

# Bind_time <- hour:9-12 / time:15:00 - 18:00
# Bind_time <- hour:14-17 / time:20:00 - 23:00
# Bind_time <- hour:17-20 / time:23:00 - 03:00
# Bind_time <- hour:15-20 / time:21:00 - 03:00
Time1 <- "15:00:00"
Time2 <- "18:00:00"


dayfunc <- function(data, x, y) {
  data[data$time >= x & data$time <= y, ]
}

nightfunc <- function(data, x, y) {
  data[data$time > y | data$time < x, ]
}

Time1 <- "07:30:00"
Time2 <- "17:29:00"

for (x in 1:25) {
  data <- data.frame(Data[x])
  data <- data %>%
    filter(trik_status == 1)
  # data <- dayfunc(data, Time1, Time2)
  data <- nightfunc(data, Time1, Time2)
  # data <- CutTimefunc(data, Time1, Time2)

  data$fulltime <- as.POSIXct(paste(data$date, data$time))
  data$time2 <- hour(data$fulltime)
  data <- data %>%
    group_by(time2) %>%
    summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE) / 4))

  data <- data %>%
    summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE)))
  data[data == 0] <- NA
  assign(namelist[x], data)
}



# GF <- rbind(bind_time(GF_1, "B1"), bind_time(GF_2, "B2"), bind_time(GF_3, "B3"), bind_time(GF_4, "B4"), bind_time(GF_5, "B5"))
# CV <- rbind(bind_time(CV_1, "B1"), bind_time(CV_2, "B2"), bind_time(CV_3, "B3"), bind_time(CV_4, "B4"), bind_time(CV_5, "B5"))
# AP <- rbind(bind_time(AP_1, "B1"), bind_time(AP_2, "B2"), bind_time(AP_3, "B3"), bind_time(AP_4, "B4"), bind_time(AP_5, "B5"))
# LB <- rbind(bind_time(LB_1, "B1"), bind_time(LB_2, "B2"), bind_time(LB_3, "B3"), bind_time(LB_4, "B4"), bind_time(LB_5, "B5"))
# AL <- rbind(bind_time(AL_1, "B1"), bind_time(AL_2, "B2"), bind_time(AL_3, "B3"), bind_time(AL_4, "B4"), bind_time(AL_5, "B5"))
GF <- rbind(GF_1, GF_2, GF_3, GF_4, GF_5)
CV <- rbind(CV_1, CV_2, CV_3, CV_4, CV_5)
AP <- rbind(AP_1, AP_2, AP_3, AP_4, AP_5)
LB <- rbind(LB_1, LB_2, LB_3, LB_4, LB_5)
AL <- rbind(AL_1, AL_2, AL_3, AL_4, AL_5)

GF$group <- rep("AX")
CV$group <- rep("CV")
AP$group <- rep("AP")
LB$group <- rep("LB")
AL$group <- rep("AP+LB")

all_data <- rbind(GF, CV, LB, AP, AL)

# fwrite(AL, "CT/customize_bind/AL.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(GF, "CT/customize_bind/GF.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(CV, "CT/customize_bind/CV.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(AP, "CT/customize_bind/AP.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(LB, "CT/customize_bind/LB.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(all_data, "CT/customize_bind/all_data.csv") ### wide format; gitignore because file is too large (> 100mb)
# transfer to satistic anaylise data formate
# data <- read.csv("CT/customize_bind/all_data.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
data <- all_data
samle_meta <- data.frame(
  ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
  level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[1, 1:24]), t(data[2, 1:24]), t(data[3, 1:24]), t(data[4, 1:24]), t(data[5, 1:24])),
    group = rep(data[1, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[6, 1:24]), t(data[7, 1:24]), t(data[8, 1:24]), t(data[9, 1:24]), t(data[10, 1:24])),
    group = rep(data[6, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[11, 1:24]), t(data[12, 1:24]), t(data[13, 1:24]), t(data[14, 1:24]), t(data[15, 1:24])),
    group = rep(data[11, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[16, 1:24]), t(data[17, 1:24]), t(data[18, 1:24]), t(data[19, 1:24]), t(data[20, 1:24])),
    group = rep(data[16, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  ),
  data.frame(
    ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[21, 1:24]), t(data[22, 1:24]), t(data[23, 1:24]), t(data[24, 1:24]), t(data[25, 1:24])),
    group = rep(data[21, "group"]), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
  )
)

fwrite(data_sat, "CT/customize_bind/nighttime_totalCT_data_sat.csv") ### wide format; gitignore because file is too large (> 100mb)

# ============== Function 5.1: Statistical Analysis for combind hours =================
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(data.table)
library(multcomp)
library(multcompView)
data_sat <- read.csv("CT/customize_bind/daytime_averageCT_data_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
head(data_sat)


print(max(data_sat$value, na.rm = TRUE))
print(min(data_sat$value, na.rm = TRUE))


Plot <- function(data_now) {
  plot_data <- data.frame()
  ratio_list <- c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01")

  for (i in ratio_list) {
    data_1 <- data_now %>%
      filter(ratio == i)

    TUKEY <- TukeyHSD(aov(value ~ group, data_1))
    MultComp <- multcompLetters(extract_p(TUKEY$group))
    group <- names(MultComp$Letters)
    Labels <- MultComp$Letters
    df <- data.frame(group, Labels)
    df <- merge(data_1, df, by = "group")


    Max_Val <- data_1 %>%
      group_by(group) %>%
      summarise_at(
        c("value"),
        list(max = max),
        na.rm = TRUE
      ) %>%
      as.data.frame()
    Max_Val
    df <- merge(df, Max_Val, by = "group")
    plot_data <- rbind(plot_data, df)
  }
  # return(plot_data)

  level_order <- c("CV", "AX", "AP", "LB", "AP+LB")

  P <- ggplot(plot_data, aes(x = factor(group, level = level_order), y = value, fill = Labels)) +
    geom_boxplot(alpha = 0.60) +
    geom_point(aes(fill = Labels), size = 2, shape = 21, ) +
    facet_grid(~ factor(ratio, levels = c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"))) +
    geom_text(data = plot_data, aes(x = group, y = max + 80, label = Labels)) +
    theme(legend.position = "none") +
    xlab("Treatment (P:C)") +
    ylab("Total counts (count/day)") +
    ylim(min(data_sat$value, na.rm = TRUE), max(data_sat$value, na.rm = TRUE) + 100) # !!!!

  return(P)
}

data_45 <- data_sat %>%
  filter(level == 45)
data_90 <- data_sat %>%
  filter(level == 90)
data_180 <- data_sat %>%
  filter(level == 180)
data_360 <- data_sat %>%
  filter(level == 360)

p_45 <- Plot(data_45)
p_90 <- Plot(data_90)
p_180 <- Plot(data_180)
p_360 <- Plot(data_360)


# +++++++++ save Tps plot +++++++++
pdf(file = "CT/customize_bind/daytime_totalCT.pdf", width = 12, height = 16)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90 +
    theme(axis.title.x = element_blank()),
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("45", "90", "180", "360"),
  align = "v",
  label_size = 16
)
dev.off()


pdf(file = "CT/customize_bind/45+90.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_45 +
    theme(axis.title.x = element_blank()),
  p_90,
  ncol = 1,
  labels = c("45", "90"),
  align = "v",
  label_size = 16
)
dev.off()

pdf(file = "CT/customize_bind/180+360.pdf", width = 10, height = 8)
cowplot::plot_grid(
  p_180 +
    theme(axis.title.x = element_blank()),
  p_360,
  ncol = 1,
  labels = c("180", "360"),
  align = "v",
  label_size = 16
)
dev.off()



# ========================Function 6 : Compute CT data for heatmap (Average CT; bind every day)==================
library(ggplot2)
library(dplyr)
library(mgcv)
# library(tidymv)
library(metR)
library(data.table)
library(fields)
# ===================Run01===================
GF_1 <- read.csv("CT/Row_CT_data/Run01CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("CT/Row_CT_data/Run01CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("CT/Row_CT_data/Run01CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("CT/Row_CT_data/Run01CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("CT/Row_CT_data/Run01CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("CT/Row_CT_data/Run02CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("CT/Row_CT_data/Run02CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("CT/Row_CT_data/Run02CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("CT/Row_CT_data/Run02CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("CT/Row_CT_data/Run02CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("CT/Row_CT_data/Run03CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("CT/Row_CT_data/Run03CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("CT/Row_CT_data/Run03CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("CT/Row_CT_data/Run03CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("CT/Row_CT_data/Run03CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("CT/Row_CT_data/Run04CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("CT/Row_CT_data/Run04CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("CT/Row_CT_data/Run04CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("CT/Row_CT_data/Run04CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("CT/Row_CT_data/Run04CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("CT/Row_CT_data/Run05CT_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("CT/Row_CT_data/Run05CT_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("CT/Row_CT_data/Run05CT_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("CT/Row_CT_data/Run05CT_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("CT/Row_CT_data/Run05CT_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")


# myfunc <- function(data, x, y) {
#   data[data$time_step >= x & data$time_step <= y, ]
# }
# time_step1 <- 2
# time_step2 <- 5776

for (x in 1:25) {
  data <- data.frame(Data[x])
  data <- data %>%
    filter(trik_status == 1)
  # data <- data %>%
  # filter(data_type == "CT")

  # data <- myfunc(data, time_step1, time_step2)
  assign(namelist[x], data)
}


head(GF_1)

data_GF <- rbind(GF_1, GF_2, GF_3, GF_4, GF_5)
fwrite(data_GF, "CT/heatmap/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- rbind(CV_1, CV_2, CV_3, CV_4, CV_5)
fwrite(data_CV, "CT/heatmap/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(AP_1, AP_2, AP_3, AP_4, AP_5)
fwrite(data_AP, "CT/heatmap/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(LB_1, LB_2, LB_3, LB_4, LB_5)
fwrite(data_LB, "CT/heatmap/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(AL_1, AL_2, AL_3, AL_4, AL_5)
fwrite(data_AL, "CT/heatmap/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)

# ========== Step 1: Compute Day and night data ==========
#  load datas
GF <- read.csv("CT/heatmap/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("CT/heatmap/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("CT/heatmap/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("CT/heatmap/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("CT/heatmap/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")


Data <- list(GF, LB, AP, CV, AL)
namelist_day <- c("GF_day", "LB_day", "AP_day", "CV_day", "AL_day")

for (x in 1:5) {
  data <- data.frame(Data[x])
  data <- data %>%
    filter(light_status == 1)
  data <- data[, 7:30]
  assign(namelist_day[x], data)
}

# metadata <- cbind(metadata, GF_day = colSums(GF_day, na.rm = TRUE)/4/5 ,
#                             LB_day = colSums(LB_day, na.rm = TRUE)/4/5 ,
#                             AP_day = colSums(AP_day, na.rm = TRUE)/4/5,
#                             CV_day = colSums(CV_day, na.rm = TRUE)/4/5,
#                             AL_day = colSums(AL_day, na.rm = TRUE)/4/5)


# metadata <- cbind(metadata, GF_day = colSums(GF_day)/60/3 ,
#                             LB_day = colSums(LB_day)/60/3 ,
#                             AP_day = colSums(AP_day)/60/3 ,
#                             CV_day = colSums(CV_day)/60/3,
#                             AL_day = colSums(AL_day)/60/3)

metadata <- cbind(metadata,
  GF_day = colMeans(GF_day, na.rm = TRUE),
  LB_day = colMeans(LB_day, na.rm = TRUE),
  AP_day = colMeans(AP_day, na.rm = TRUE),
  CV_day = colMeans(CV_day, na.rm = TRUE),
  AL_day = colMeans(AL_day, na.rm = TRUE)
) # MT CT


# ========== Sum night data ==========
namelist_night <- c("GF_night", "LB_night", "AP_night", "CV_night", "AL_night")

for (x in 1:5) {
  data <- data.frame(Data[x])
  data <- data %>%
    filter(light_status == 0)
  data <- data[, 7:30]
  assign(namelist_night[x], data)
}


# metadata <- cbind(metadata, GF_night = colSums(GF_night, na.rm = TRUE) ,
#                             LB_night = colSums(LB_night, na.rm = TRUE) ,
#                             AP_night = colSums(AP_night, na.rm = TRUE),
#                             CV_night = colSums(CV_night, na.rm = TRUE),
#                             AL_night = colSums(AL_night, na.rm = TRUE))

# metadata <- cbind(metadata, GF_night = colSums(GF_night)/60/60,
#                             LB_night = colSums(LB_night)/60/60 ,
#                             AP_night = colSums(AP_night)/60/60,
#                             CV_night = colSums(CV_night)/60/60,
#                             AL_night = colSums(AL_night)/60/60)


metadata <- cbind(metadata,
  GF_night = colMeans(GF_night, na.rm = TRUE),
  LB_night = colMeans(LB_night, na.rm = TRUE),
  AP_night = colMeans(AP_night, na.rm = TRUE),
  CV_night = colMeans(CV_night, na.rm = TRUE),
  AL_night = colMeans(AL_night, na.rm = TRUE)
)


fwrite(metadata, "CT/heatmap/CT.csv") ### wide format; gitignore because file is too large (> 100mb)
# fwrite(metadata ,"MT_metadata.csv")


# ========== Step 2a: Import day parameter and plot ==========
metadata <- read.csv("CT/heatmap/CT.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(metadata$CV_day, metadata$GF_day, metadata$AP_day, metadata$LB_day, metadata$AL_day)
name_list <- c("CV", "AX", "AP", "LB", "AP+LB")

int <- array(c(metadata$protein, metadata$suger), dim = c(24, 2)) # Define position matrix

print("MAX:")
for (i in Data) {
  print(max(i, na.rm = TRUE))
}

print("MIN:")
for (i in Data) {
  print(min(i, na.rm = TRUE))
}


# ===== save plot ( five ) =====
pdf(file = "CT/heatmap/Day_CT.pdf", width = 15, height = 9)
# png('0628.png', width = 550, height = 450)
par(oma = c(4, 4, 4, 4)) # bottom, left, top and right (oma for outside of fig)
set.panel(2, 3)
par(mar = c(1, 1, 1, 4))

for (x in 1:5) {
  s <- int
  y <- as.numeric(unlist(Data[x]))

  # fit0<- spatialProcess(s,y)
  fit0 <- Tps(s, y)

  surface(fit0,
    xlab = "", ylab = "", col = turbo(256), cex.lab = 1.5, cex.axis = 1.1, axes = FALSE,
    zlim = c(40, 99)
  ) # !!!!

  box()
  if (x %in% c(3, 4, 5)) {
    axis(1, cex.axis = 1.5)
  } # draw a x axis
  if (x %in% c(1, 4)) {
    axis(2, cex.axis = 1.5)
  } # draw a y axis

  text(x = 120, y = 320, label = name_list[x], cex = 2.5)
}

par(oma = c(0, 0, 0, 0))
# mtext("Average daytime movements (count/day)", side = 3, cex = 2.5, outer = TRUE , line = -2.5)
mtext("Average daytime counts (count/day)", side = 3, cex = 2.5, outer = TRUE, line = -2.5)
mtext(line = -2, side = 1, cex = 2, "Protein in diet (g/L)", outer = TRUE)
mtext(line = -2, side = 2, cex = 2, "Carbohydrate in diet (g/L)", outer = TRUE)
set.panel() # reset plotting device

dev.off()


# ========== Step 2b.: Import night parameter and plot ==========

metadata <- read.csv("CT/heatmap/CT.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(metadata$CV_night, metadata$GF_night, metadata$AP_night, metadata$LB_night, metadata$AL_night)
name_list <- c("CV", "AX", "AP", "LB", "AP+LB")



print("MAX:")
for (i in Data) {
  print(max(i, na.rm = TRUE))
}

print("MIN:")
for (i in Data) {
  print(min(i, na.rm = TRUE))
}


# ===== save plot ( five ) =====
pdf(file = "CT/heatmap/Night_CT.pdf", width = 15, height = 9)
# png('0628.png', width = 550, height = 450)
par(oma = c(4, 4, 4, 4)) # bottom, left, top and right (oma for outside of fig)
set.panel(2, 3)
par(mar = c(1, 1, 1, 4))

for (x in 1:5) {
  s <- int
  y <- as.numeric(unlist(Data[x]))

  fit0 <- spatialProcess(s, y)
  # fit0<- Tps(s,y)

  surface(fit0,
    xlab = "", ylab = "", col = turbo(256), cex.lab = 1.5, cex.axis = 1.1, axes = FALSE,
    zlim = c(16, 45)
  ) # !!!!

  box()
  if (x %in% c(3, 4, 5)) {
    axis(1, cex.axis = 1.5)
  } # draw a x axis
  if (x %in% c(1, 4)) {
    axis(2, cex.axis = 1.5)
  } # draw a y axis

  text(x = 120, y = 320, label = name_list[x], cex = 2.5)
}

par(oma = c(0, 0, 0, 0))
# mtext("Average nighttime movements (count/day)", side = 3, cex = 2.5, outer = TRUE , line = -2.5)
mtext("Average nighttime counts (count/day)", side = 3, cex = 2.5, outer = TRUE, line = -2.5)
mtext(line = -2, side = 1, cex = 2, "Protein in diet (g/L)", outer = TRUE)
mtext(line = -2, side = 2, cex = 2, "Carbohydrate in diet (g/L)", outer = TRUE)
set.panel() # reset plotting device


dev.off()
