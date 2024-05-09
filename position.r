#################################################################################
# Outline (please use Find function to search the codes)
# Function 1 : Output txt file to CSV files
# |__Function 1.1 : Calculated "Moving distance"
# |__Function 1.2 : Calculated "distance from diet"
#
# Function 2 : Out put the calculated position files
#
# Function 3 : Plot a line chart for 24 hours (zeitgeber)
# |__Function 3.1 : Plot a line chart for zeitgeber (five group)
# |__Function 3.2 : Statistical Analysis for zeitgeber
#
# Function 4 : customize for combind hours
# |__Function 4.1 : Statistical Analysis for combind hours
#    |__Function 4.1.1 : TukeyHSD analysis among groups
#    |__Function 4.1.2 : TukeyHSD analysis among ratio
#
# Function 5 : diet_enter_time
# |__Function 5.1 : diet_enter_time  (zeitgeber)
#
# Function 6 : Diet touch count / Total moving time
# |__Function 6.1 : Diet touch count / Total moving time  (zeitgeber)
# |__Function 6.2 : Diet touch count / Total moving time  (daytime/nighttime)
#    |__Function 6.2.1 : TukeyHSD analysis among groups
#    |__Function 6.2.2: TukeyHSD analysis among ratio
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
library(multcompView)
library(fields)
# ================== Function 1 : Output txt file to CSV files ==================
# Loading Monitor data

m1 <- read.table("Run05/Monitor1.txt")
head(m1)
m2 <- read.table("Run05/Monitor2.txt")
m3 <- read.table("Run05/Monitor3.txt")
m4 <- read.table("Run05/Monitor4.txt")

Data <- list(m1, m2, m3, m4)
namelist <- c("m1", "m2", "m3", "m4")

myfunc <- function(data, x, y) {
    data[data$V1 >= x & data$V1 <= y, ]
}
time_step1 <- 33
time_step2 <- 5777


for (x in 1:4) {
    data <- data.frame(Data[x])
    data <- data %>% filter(V10 == "Pn")
    data <- myfunc(data, time_step1, time_step2)
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

fwrite(GF, "position/Row_Pn_data/Run05Pn_GF.csv")
fwrite(LB, "position/Row_Pn_data/Run05Pn_LB.csv")
fwrite(AP, "position/Row_Pn_data/Run05Pn_AP.csv")
fwrite(CV, "position/Row_Pn_data/Run05Pn_CV.csv")
fwrite(AL, "position/Row_Pn_data/Run05Pn_AL.csv")


# ============================ Function 2 : Out put the calculated position files============================
# ============================ Function 1.1 : Calculated "Moving distance" ============================
# ===================Run02===================
GF_2 <- read.csv("position/Row_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Row_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Row_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Row_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Row_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Row_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Row_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Row_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Row_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Row_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Row_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Row_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Row_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Row_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Row_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Row_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Row_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Row_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Row_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Row_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

tubes_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")

PnCalfun <- function(data) {
    for (col in 1:24) {
        df <- data.frame()
        for (x in 2:nrow(data)) {
            p1 <- data[x - 1, 6 + col]
            p2 <- data[x, 6 + col]
            if (!is.na(p1) && !is.na(p2)) {
                dis <- data.frame(value = c(abs(p2 - p1)))
            } else {
                dis <- data.frame(value = c(NA))
            }

            df <- rbind(df, dis)
        }
        names(df) <- c(tubes_lst[col])
        if (col == 1) {
            data_1 <- df
        } else {
            data_1 <- cbind(data_1, df)
        }
    }

    data_1 <- cbind(data.frame(
        time_step = data[-1, ]$time_step,
        date = data[-1, ]$date,
        time = data[-1, ]$time,
        trik_status = data[-1, ]$trik_status,
        light_status = data[-1, ]$light_status
    ), data_1, data.frame(group = data[-1, ]$ group, fulltime = as.POSIXct(paste(data[-1, ]$date, data[-1, ]$time))))
    return(data_1)
}

head(GF_2)

fwrite(PnCalfun(GF_2), "position/Cal_Pn_data/Run02Pn_GF.csv")
fwrite(PnCalfun(LB_2), "position/Cal_Pn_data/Run02Pn_LB.csv")
fwrite(PnCalfun(AP_2), "position/Cal_Pn_data/Run02Pn_AP.csv")
fwrite(PnCalfun(CV_2), "position/Cal_Pn_data/Run02Pn_CV.csv")
fwrite(PnCalfun(AL_2), "position/Cal_Pn_data/Run02Pn_AL.csv")

fwrite(PnCalfun(GF_3), "position/Cal_Pn_data/Run03Pn_GF.csv")
fwrite(PnCalfun(LB_3), "position/Cal_Pn_data/Run03Pn_LB.csv")
fwrite(PnCalfun(AP_3), "position/Cal_Pn_data/Run03Pn_AP.csv")
fwrite(PnCalfun(CV_3), "position/Cal_Pn_data/Run03Pn_CV.csv")
fwrite(PnCalfun(AL_3), "position/Cal_Pn_data/Run03Pn_AL.csv")

fwrite(PnCalfun(GF_4), "position/Cal_Pn_data/Run04Pn_GF.csv")
fwrite(PnCalfun(LB_4), "position/Cal_Pn_data/Run04Pn_LB.csv")
fwrite(PnCalfun(AP_4), "position/Cal_Pn_data/Run04Pn_AP.csv")
fwrite(PnCalfun(CV_4), "position/Cal_Pn_data/Run04Pn_CV.csv")
fwrite(PnCalfun(AL_4), "position/Cal_Pn_data/Run04Pn_AL.csv")

fwrite(PnCalfun(GF_5), "position/Cal_Pn_data/Run05Pn_GF.csv")
fwrite(PnCalfun(LB_5), "position/Cal_Pn_data/Run05Pn_LB.csv")
fwrite(PnCalfun(AP_5), "position/Cal_Pn_data/Run05Pn_AP.csv")
fwrite(PnCalfun(CV_5), "position/Cal_Pn_data/Run05Pn_CV.csv")
fwrite(PnCalfun(AL_5), "position/Cal_Pn_data/Run05Pn_AL.csv")


# ============================ Function 1.2 : Calculated "distance from diet" ============================
Diet_disCalfun <- function(data) {
    for (col in 1:24) {
        df <- data.frame()
        for (x in 1:nrow(data)) {
            p2 <- data[x, 6 + col]
            if (!is.na(p2)) {
                dis <- data.frame(value = c(abs(15 - p2)))
            } else {
                dis <- data.frame(value = c(NA))
            }

            df <- rbind(df, dis)
        }
        names(df) <- c(tubes_lst[col])
        if (col == 1) {
            data_1 <- df
        } else {
            data_1 <- cbind(data_1, df)
        }
    }

    data_1 <- cbind(data.frame(
        time_step = data$time_step,
        date = data$date,
        time = data$time,
        trik_status = data$trik_status,
        light_status = data$light_status
    ), data_1, data.frame(group = data$ group, fulltime = as.POSIXct(paste(data$date, data$time))))
    return(data_1)
}



fwrite(Diet_disCalfun(GF_2), "position/Diet_dis_data/Run02Pn_GF.csv")
fwrite(Diet_disCalfun(LB_2), "position/Diet_dis_data/Run02Pn_LB.csv")
fwrite(Diet_disCalfun(AP_2), "position/Diet_dis_data/Run02Pn_AP.csv")
fwrite(Diet_disCalfun(CV_2), "position/Diet_dis_data/Run02Pn_CV.csv")
fwrite(Diet_disCalfun(AL_2), "position/Diet_dis_data/Run02Pn_AL.csv")

fwrite(Diet_disCalfun(GF_3), "position/Diet_dis_data/Run03Pn_GF.csv")
fwrite(Diet_disCalfun(LB_3), "position/Diet_dis_data/Run03Pn_LB.csv")
fwrite(Diet_disCalfun(AP_3), "position/Diet_dis_data/Run03Pn_AP.csv")
fwrite(Diet_disCalfun(CV_3), "position/Diet_dis_data/Run03Pn_CV.csv")
fwrite(Diet_disCalfun(AL_3), "position/Diet_dis_data/Run03Pn_AL.csv")

fwrite(Diet_disCalfun(GF_4), "position/Diet_dis_data/Run04Pn_GF.csv")
fwrite(Diet_disCalfun(LB_4), "position/Diet_dis_data/Run04Pn_LB.csv")
fwrite(Diet_disCalfun(AP_4), "position/Diet_dis_data/Run04Pn_AP.csv")
fwrite(Diet_disCalfun(CV_4), "position/Diet_dis_data/Run04Pn_CV.csv")
fwrite(Diet_disCalfun(AL_4), "position/Diet_dis_data/Run04Pn_AL.csv")

fwrite(Diet_disCalfun(GF_5), "position/Diet_dis_data/Run05Pn_GF.csv")
fwrite(Diet_disCalfun(LB_5), "position/Diet_dis_data/Run05Pn_LB.csv")
fwrite(Diet_disCalfun(AP_5), "position/Diet_dis_data/Run05Pn_AP.csv")
fwrite(Diet_disCalfun(CV_5), "position/Diet_dis_data/Run05Pn_CV.csv")
fwrite(Diet_disCalfun(AL_5), "position/Diet_dis_data/Run05Pn_AL.csv")

tubes_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")




# ================== Function 3 : Plot a line chart for 24 hours (zeitgeber) ==================
# Diet_dis_data
# Cal_Pn_data
# ===================Run02===================
GF_2 <- read.csv("position/Cal_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Cal_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Cal_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Cal_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Cal_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Cal_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Cal_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Cal_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Cal_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Cal_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Cal_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Cal_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Cal_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Cal_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Cal_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Cal_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Cal_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Cal_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Cal_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Cal_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

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



# position/diet_distance
# position/moving_distance
data_GF <- rbind(cal_zeitgeber(GF_2, "B2"), cal_zeitgeber(GF_3, "B3"), cal_zeitgeber(GF_4, "B4"), cal_zeitgeber(GF_5, "B5"))
fwrite(data_GF, "position/moving_distance/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)
# "position/zeitgeber/GF_all.csv"
data_CV <- rbind(cal_zeitgeber(CV_2, "B2"), cal_zeitgeber(CV_3, "B3"), cal_zeitgeber(CV_4, "B4"), cal_zeitgeber(CV_5, "B5"))
fwrite(data_CV, "position/moving_distance/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(cal_zeitgeber(AP_2, "B2"), cal_zeitgeber(AP_3, "B3"), cal_zeitgeber(AP_4, "B4"), cal_zeitgeber(AP_5, "B5"))
fwrite(data_AP, "position/moving_distance/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(cal_zeitgeber(LB_2, "B2"), cal_zeitgeber(LB_3, "B3"), cal_zeitgeber(LB_4, "B4"), cal_zeitgeber(LB_5, "B5"))
fwrite(data_LB, "position/moving_distance/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(cal_zeitgeber(AL_2, "B2"), cal_zeitgeber(AL_3, "B3"), cal_zeitgeber(AL_4, "B4"), cal_zeitgeber(AL_5, "B5"))
fwrite(data_AL, "position/moving_distance/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)

# ================== output average zeitgeber datas (remove ghost datas in pervious files)===================
average_zeitgeber <- function(data) {
    data_1 <- data %>%
        group_by(hour) %>%
        summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
    return(data_1)
}

GF <- read.csv("position/moving_distance/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(GF), "position/moving_distance/GF.csv") ### wide format; gitignore because file is too large (> 100mb)

CV <- read.csv("position/moving_distance/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(CV), "position/moving_distance/CV.csv") ### wide format; gitignore because file is too large (> 100mb)

AP <- read.csv("position/moving_distance/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AP), "position/moving_distance/AP.csv") ### wide format; gitignore because file is too large (> 100mb)

AL <- read.csv("position/moving_distance/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AL), "position/moving_distance/AL.csv") ### wide format; gitignore because file is too large (> 100mb)

LB <- read.csv("position/moving_distance/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(LB), "position/moving_distance/LB.csv") ### wide format; gitignore because file is too large (> 100mb)

# max(data_CV)


# ================== load datas ==================
GF <- read.csv("position/moving_distance/GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("position/moving_distance/LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("position/moving_distance/AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("position/moving_distance/CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("position/moving_distance/AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# sat_res_nota <- read.csv("zeitgeber/Sat_significant_diff.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ================== Function 3.1 : Plot a line chart for zeitgeber (five group) ==================
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")


# print("MAX:")
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
        labs(x = "Zeitgeber", y = "Moving distance") +
        # labs(x = "Zeitgeber", y = "Distance from diet") +
        ylim(0, 6) +
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


pdf(file = "position/moving_distance/zeitgeber.pdf", width = 20, height = 10)
v1
dev.off()


ggsave(
    "position/moving_distance/zeitgeber.png",
    v1,
    width = 16,
    height = 8,
    dpi = 300
)

# ================== Function 3.2 : Statistical Analysis for zeitgeber ==================
library(multcompView)

GF <- read.csv("position/zeitgeber/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("position/zeitgeber/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("position/zeitgeber/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("position/zeitgeber/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("position/zeitgeber/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

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
fwrite(sat_res, "position/zeitgeber/Sat.csv") ### wide format; gitignore because file is too large (> 100mb)

sat_res_nota <- sat_res %>%
    filter(Letters != "a")
fwrite(sat_res_nota, "position/zeitgeber/Sat_significant_diff.csv") ### wide format; gitignore because file is too large (> 100mb)





# ======================= Function 4 : customize for combind hours=======================
# ===================Run02===================
GF_2 <- read.csv("position/Cal_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Cal_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Cal_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Cal_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Cal_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Cal_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Cal_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Cal_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Cal_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Cal_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Cal_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Cal_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Cal_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Cal_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Cal_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Cal_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Cal_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Cal_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Cal_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Cal_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Data <- list(GF_2, GF_3, GF_4, GF_5, CV_2, CV_3, CV_4, CV_5, AP_2, AP_3, AP_4, AP_5, LB_2, LB_3, LB_4, LB_5, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_2", "GF_3", "GF_4", "GF_5", "CV_2", "CV_3", "CV_4", "CV_5", "AP_2", "AP_3", "AP_4", "AP_5", "LB_2", "LB_3", "LB_4", "LB_5", "AL_2", "AL_3", "AL_4", "AL_5")

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


for (x in 1:20) {
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
        summarize(across(c(t1:t24), \(x) sum(x) / 4))

    data <- data %>%
        summarize(across(c(t1:t24), \(x) sum(x)))
    data[data == 0] <- NA
    assign(namelist[x], data)
}


# GF <- rbind(bind_time(GF_1, "B1"), bind_time(GF_2, "B2"), bind_time(GF_3, "B3"), bind_time(GF_4, "B4"), bind_time(GF_5, "B5"))
# CV <- rbind(bind_time(CV_1, "B1"), bind_time(CV_2, "B2"), bind_time(CV_3, "B3"), bind_time(CV_4, "B4"), bind_time(CV_5, "B5"))
# AP <- rbind(bind_time(AP_1, "B1"), bind_time(AP_2, "B2"), bind_time(AP_3, "B3"), bind_time(AP_4, "B4"), bind_time(AP_5, "B5"))
# LB <- rbind(bind_time(LB_1, "B1"), bind_time(LB_2, "B2"), bind_time(LB_3, "B3"), bind_time(LB_4, "B4"), bind_time(LB_5, "B5"))
# AL <- rbind(bind_time(AL_1, "B1"), bind_time(AL_2, "B2"), bind_time(AL_3, "B3"), bind_time(AL_4, "B4"), bind_time(AL_5, "B5"))
GF <- rbind(GF_2, GF_3, GF_4, GF_5)
CV <- rbind(CV_2, CV_3, CV_4, CV_5)
AP <- rbind(AP_2, AP_3, AP_4, AP_5)
LB <- rbind(LB_2, LB_3, LB_4, LB_5)
AL <- rbind(AL_2, AL_3, AL_4, AL_5)

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
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 16),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 4)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[1, 1:24]), t(data[2, 1:24]), t(data[3, 1:24]), t(data[4, 1:24])),
        group = rep(data[1, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[5, 1:24]), t(data[6, 1:24]), t(data[7, 1:24]), t(data[8, 1:24])),
        group = rep(data[5, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[9, 1:24]), t(data[10, 1:24]), t(data[11, 1:24]), t(data[12, 1:24])),
        group = rep(data[9, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[13, 1:24]), t(data[14, 1:24]), t(data[15, 1:24]), t(data[16, 1:24])),
        group = rep(data[13, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[17, 1:24]), t(data[18, 1:24]), t(data[19, 1:24]), t(data[20, 1:24])),
        group = rep(data[17, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

fwrite(data_sat, "position/diet_distance/nighttime_totalPn_sat.csv") ### wide format; gitignore because file is too large (> 100mb)


# ============== Function 4.1 : Statistical Analysis for combind hours =================
# ============== Function 4.1.1 : TukeyHSD analysis among groups =================
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(data.table)
library(multcomp)
library(multcompView)
theme_set(theme_bw())
data_sat <- read.csv("position/moving_distance/nighttime_totalPn_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Total moving distance") +
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
pdf(file = "position/moving_distance/daytime_totalPn.pdf", width = 12, height = 16)
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


pdf(file = "position/customize_bind/45+90.pdf", width = 10, height = 8)
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
theme_set(theme_bw())
data_sat <- read.csv("position/moving_distance/daytime_totalPn_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        geom_text(data = plot_data, aes(x = ratio, y = max + 80, label = Labels)) +
        theme(legend.position = "none") +
        xlab("Ratio (P:C)") +
        ylab("Total moving distance") +
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
pdf(file = "position/moving_distance/daytime_totalPn_ratio.pdf", width = 12, height = 16)
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



# ==================================== Function 5 : diet_enter_time  ====================================
# ================== Function 5.1 : diet_enter_time  (zeitgeber) ==================
# ===================Run02===================
GF_2 <- read.csv("position/Row_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Row_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Row_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Row_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Row_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Row_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Row_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Row_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Row_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Row_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Row_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Row_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Row_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Row_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Row_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Row_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Row_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Row_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Row_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Row_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Diet_enter_timesCalfun <- function(Ycol) {
    df <- data.frame()
    l <- c()
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] == 15) {
            l <- append(l, 1)
        }
        for (x in 2:length(Ycol)) {
            p1 <- Ycol[x - 1]
            p2 <- Ycol[x]
            if (!is.na(p1) && !is.na(p2) && p1 != p2) {
                if (p2 == 15) {
                    l <- append(l, 1)
                }
            }
        }
        re <- length(l)
    } else {
        re <- NA
    }
    return(re)
}



cal_zeitgeber <- function(data, batch_no) {
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    data$time2 <- hour(data$fulltime)
    data <- data %>%
        group_by(time2) %>%
        summarize(across(c(t1:t24), \(x) Diet_enter_timesCalfun(x)))


    # data_1$hour <- c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    data$hour <- c(18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    data$batch <- rep(batch_no)
    return(data)
}


data_GF <- rbind(cal_zeitgeber(GF_2, "B2"), cal_zeitgeber(GF_3, "B3"), cal_zeitgeber(GF_4, "B4"), cal_zeitgeber(GF_5, "B5"))
fwrite(data_GF, "position/diet_enter_time/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- rbind(cal_zeitgeber(CV_2, "B2"), cal_zeitgeber(CV_3, "B3"), cal_zeitgeber(CV_4, "B4"), cal_zeitgeber(CV_5, "B5"))
fwrite(data_CV, "position/diet_enter_time/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(cal_zeitgeber(AP_2, "B2"), cal_zeitgeber(AP_3, "B3"), cal_zeitgeber(AP_4, "B4"), cal_zeitgeber(AP_5, "B5"))
fwrite(data_AP, "position/diet_enter_time/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(cal_zeitgeber(LB_2, "B2"), cal_zeitgeber(LB_3, "B3"), cal_zeitgeber(LB_4, "B4"), cal_zeitgeber(LB_5, "B5"))
fwrite(data_LB, "position/diet_enter_time/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(cal_zeitgeber(AL_2, "B2"), cal_zeitgeber(AL_3, "B3"), cal_zeitgeber(AL_4, "B4"), cal_zeitgeber(AL_5, "B5"))
fwrite(data_AL, "position/diet_enter_time/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)

# ================== output average zeitgeber datas (remove ghost datas in pervious files)===================
average_zeitgeber <- function(data) {
    data_1 <- data %>%
        group_by(hour) %>%
        summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
    return(data_1)
}

GF <- read.csv("position/diet_enter_time/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(GF), "position/diet_enter_time/GF.csv") ### wide format; gitignore because file is too large (> 100mb)

CV <- read.csv("position/diet_enter_time/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(CV), "position/diet_enter_time/CV.csv") ### wide format; gitignore because file is too large (> 100mb)

AP <- read.csv("position/diet_enter_time/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AP), "position/diet_enter_time/AP.csv") ### wide format; gitignore because file is too large (> 100mb)

AL <- read.csv("position/diet_enter_time/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AL), "position/diet_enter_time/AL.csv") ### wide format; gitignore because file is too large (> 100mb)

LB <- read.csv("position/diet_enter_time/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(LB), "position/diet_enter_time/LB.csv") ### wide format; gitignore because file is too large (> 100mb)

# max(data_CV)


# ================== load datas ==================
GF <- read.csv("position/diet_enter_time/GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("position/diet_enter_time/LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("position/diet_enter_time/AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("position/diet_enter_time/CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("position/diet_enter_time/AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ====繪製多折線圖 ====
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")

# print("MAX:")
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
        labs(x = "Zeitgeber", y = "Diet touch count") +
        ylim(0, 49) +
        scale_x_continuous(expand = c(0, 0)) +
        theme(
            axis.text.x = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 14, color = "black"),
            axis.text.y = element_text(size = 12, color = "black"),
            axis.title.y = element_text(size = 13, color = "black")
        )

    p1 <- p + annotate(
        geom = "rect", xmin = 12, xmax = 23.5,
        ymin = -Inf, ymax = +Inf, alpha = 0.4
    )
    p1
    plot_lst[[i]] <- p1
}

# diet_name_lst <-c('t1', 't2', 't3', 't4', 't5', 't6',  't9' , 't10', 't11', 't12', 't13', 't14', 't17','t18', 't19','t20','t21','t22' ,'t25','t26','t27','t28','t29','t30' )
# Combine all plots
v1 <- cowplot::plot_grid(plotlist = plot_lst, nrow = 4, labels = name_lst, label_size = 8, vjust = 0.5)
v1

ggsave(
    "position/diet_enter_time/zeitgeber.png",
    v1,
    width = 16,
    height = 8,
    dpi = 300
)

pdf(file = "position/diet_enter_time/zeitgeber.pdf", width = 20, height = 10)
v1
dev.off()





# ==================================== Function 6 : Diet touch count / Total moving times ====================================
# ================== Function 6.1 : Diet touch count / Total moving time  (zeitgeber) ==================
# ===================Run02===================
GF_2 <- read.csv("position/Row_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Row_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Row_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Row_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Row_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Row_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Row_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Row_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Row_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Row_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Row_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Row_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Row_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Row_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Row_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Row_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Row_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Row_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Row_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Row_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Diet_enter_timesCalfun <- function(Ycol) {
    l <- c()
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] == 15) {
            l <- append(l, 1)
        }
        for (x in 2:length(Ycol)) {
            p1 <- Ycol[x - 1]
            p2 <- Ycol[x]
            if (!is.na(p1) && !is.na(p2) && p1 != p2) {
                if (p2 == 15) {
                    l <- append(l, 1)
                }
            }
        }
        re <- length(l)
    } else {
        re <- NA
    }
    return(re)
}


Total_Move_timesCalfun <- function(Ycol) {
    l <- c()
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] == 15) {
            l <- append(l, 1)
        }
        for (x in 2:length(Ycol)) {
            p1 <- Ycol[x - 1]
            p2 <- Ycol[x]
            if (!is.na(p1) && !is.na(p2) && p1 != p2) {
                l <- append(l, 1)
            }
        }
        re <- length(l)
    } else {
        re <- NA
    }
    return(re)
}


cal_zeitgeber <- function(data, batch_no) {
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    data$time2 <- hour(data$fulltime)
    data <- data %>%
        group_by(time2) %>%
        summarize(across(c(t1:t24), \(x) (Diet_enter_timesCalfun(x) / Total_Move_timesCalfun(x)) * 100))


    # data_1$hour <- c(17,18,19,20,21,22,23,0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)
    data$hour <- c(18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    data$batch <- rep(batch_no)
    return(data)
}


data_GF <- rbind(cal_zeitgeber(GF_2, "B2"), cal_zeitgeber(GF_3, "B3"), cal_zeitgeber(GF_4, "B4"), cal_zeitgeber(GF_5, "B5"))
fwrite(data_GF, "position/diet_enter_time_percentage/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- rbind(cal_zeitgeber(CV_2, "B2"), cal_zeitgeber(CV_3, "B3"), cal_zeitgeber(CV_4, "B4"), cal_zeitgeber(CV_5, "B5"))
fwrite(data_CV, "position/diet_enter_time_percentage/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(cal_zeitgeber(AP_2, "B2"), cal_zeitgeber(AP_3, "B3"), cal_zeitgeber(AP_4, "B4"), cal_zeitgeber(AP_5, "B5"))
fwrite(data_AP, "position/diet_enter_time_percentage/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(cal_zeitgeber(LB_2, "B2"), cal_zeitgeber(LB_3, "B3"), cal_zeitgeber(LB_4, "B4"), cal_zeitgeber(LB_5, "B5"))
fwrite(data_LB, "position/diet_enter_time_percentage/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(cal_zeitgeber(AL_2, "B2"), cal_zeitgeber(AL_3, "B3"), cal_zeitgeber(AL_4, "B4"), cal_zeitgeber(AL_5, "B5"))
fwrite(data_AL, "position/diet_enter_time_percentage/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)


# ================== output average zeitgeber datas (remove ghost datas in pervious files)===================
average_zeitgeber <- function(data) {
    data_1 <- data %>%
        group_by(hour) %>%
        summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
    return(data_1)
}

GF <- read.csv("position/diet_enter_time_percentage/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(GF), "position/diet_enter_time_percentage/GF.csv") ### wide format; gitignore because file is too large (> 100mb)

CV <- read.csv("position/diet_enter_time_percentage/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(CV), "position/diet_enter_time_percentage/CV.csv") ### wide format; gitignore because file is too large (> 100mb)

AP <- read.csv("position/diet_enter_time_percentage/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AP), "position/diet_enter_time_percentage/AP.csv") ### wide format; gitignore because file is too large (> 100mb)

AL <- read.csv("position/diet_enter_time_percentage/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AL), "position/diet_enter_time_percentage/AL.csv") ### wide format; gitignore because file is too large (> 100mb)

LB <- read.csv("position/diet_enter_time_percentage/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(LB), "position/diet_enter_time_percentage/LB.csv") ### wide format; gitignore because file is too large (> 100mb)

# ================== load datas ==================
GF <- read.csv("position/diet_enter_time_percentage/GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("position/diet_enter_time_percentage/LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("position/diet_enter_time_percentage/AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("position/diet_enter_time_percentage/CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("position/diet_enter_time_percentage/AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ====繪製多折線圖 ====
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")

# print("MAX:")
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
        labs(x = "Zeitgeber", y = "Diet touch count %") +
        ylim(0, 81) +
        scale_x_continuous(expand = c(0, 0)) +
        theme(
            axis.text.x = element_text(size = 12, color = "black"),
            axis.title.x = element_text(size = 14, color = "black"),
            axis.text.y = element_text(size = 12, color = "black"),
            axis.title.y = element_text(size = 13, color = "black")
        )

    p1 <- p + annotate(
        geom = "rect", xmin = 12, xmax = 23.5,
        ymin = -Inf, ymax = +Inf, alpha = 0.4
    )
    p1
    plot_lst[[i]] <- p1
}

# diet_name_lst <-c('t1', 't2', 't3', 't4', 't5', 't6',  't9' , 't10', 't11', 't12', 't13', 't14', 't17','t18', 't19','t20','t21','t22' ,'t25','t26','t27','t28','t29','t30' )
# Combine all plots
v1 <- cowplot::plot_grid(plotlist = plot_lst, nrow = 4, labels = name_lst, label_size = 8, vjust = 0.5)
v1

ggsave(
    "position/diet_enter_time_percentage/zeitgeber.png",
    v1,
    width = 16,
    height = 8,
    dpi = 300
)

pdf(file = "position/diet_enter_time_percentage/zeitgeber.pdf", width = 20, height = 10)
v1
dev.off()


# ================== Function 6.2 : Diet touch count / Total moving time  (daytime/nighttime)==================
# ===================Run02===================
GF_2 <- read.csv("position/Row_Pn_data/Run02Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("position/Row_Pn_data/Run02Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("position/Row_Pn_data/Run02Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("position/Row_Pn_data/Run02Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("position/Row_Pn_data/Run02Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("position/Row_Pn_data/Run03Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("position/Row_Pn_data/Run03Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("position/Row_Pn_data/Run03Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("position/Row_Pn_data/Run03Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("position/Row_Pn_data/Run03Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("position/Row_Pn_data/Run04Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("position/Row_Pn_data/Run04Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("position/Row_Pn_data/Run04Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("position/Row_Pn_data/Run04Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("position/Row_Pn_data/Run04Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("position/Row_Pn_data/Run05Pn_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("position/Row_Pn_data/Run05Pn_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("position/Row_Pn_data/Run05Pn_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("position/Row_Pn_data/Run05Pn_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("position/Row_Pn_data/Run05Pn_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

Data <- list(GF_2, GF_3, GF_4, GF_5, CV_2, CV_3, CV_4, CV_5, AP_2, AP_3, AP_4, AP_5, LB_2, LB_3, LB_4, LB_5, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_2", "GF_3", "GF_4", "GF_5", "CV_2", "CV_3", "CV_4", "CV_5", "AP_2", "AP_3", "AP_4", "AP_5", "LB_2", "LB_3", "LB_4", "LB_5", "AL_2", "AL_3", "AL_4", "AL_5")

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




Diet_enter_timesCalfun <- function(Ycol) {
    l <- c()
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] == 15) {
            l <- append(l, 1)
        }
        for (x in 2:length(Ycol)) {
            p1 <- Ycol[x - 1]
            p2 <- Ycol[x]
            if (!is.na(p1) && !is.na(p2) && p1 != p2) {
                if (p2 == 15) {
                    l <- append(l, 1)
                }
            }
        }
        re <- length(l)
    } else {
        re <- NA
    }
    return(re)
}


Total_Move_timesCalfun <- function(Ycol) {
    l <- c()
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] == 15) {
            l <- append(l, 1)
        }
        for (x in 2:length(Ycol)) {
            p1 <- Ycol[x - 1]
            p2 <- Ycol[x]
            if (!is.na(p1) && !is.na(p2) && p1 != p2) {
                l <- append(l, 1)
            }
        }
        re <- length(l)
    } else {
        re <- NA
    }
    return(re)
}


for (x in 1:20) {
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
        summarize(across(c(t1:t24), \(x) (Diet_enter_timesCalfun(x) / Total_Move_timesCalfun(x)) * 100))


    data <- data %>%
        summarize(across(c(t1:t24), \(x) mean(x)))
    # data[data == 0] <- NA
    assign(namelist[x], data)
}


# GF <- rbind(bind_time(GF_1, "B1"), bind_time(GF_2, "B2"), bind_time(GF_3, "B3"), bind_time(GF_4, "B4"), bind_time(GF_5, "B5"))
# CV <- rbind(bind_time(CV_1, "B1"), bind_time(CV_2, "B2"), bind_time(CV_3, "B3"), bind_time(CV_4, "B4"), bind_time(CV_5, "B5"))
# AP <- rbind(bind_time(AP_1, "B1"), bind_time(AP_2, "B2"), bind_time(AP_3, "B3"), bind_time(AP_4, "B4"), bind_time(AP_5, "B5"))
# LB <- rbind(bind_time(LB_1, "B1"), bind_time(LB_2, "B2"), bind_time(LB_3, "B3"), bind_time(LB_4, "B4"), bind_time(LB_5, "B5"))
# AL <- rbind(bind_time(AL_1, "B1"), bind_time(AL_2, "B2"), bind_time(AL_3, "B3"), bind_time(AL_4, "B4"), bind_time(AL_5, "B5"))

GF <- rbind(GF_2, GF_3, GF_4, GF_5)
CV <- rbind(CV_2, CV_3, CV_4, CV_5)
AP <- rbind(AP_2, AP_3, AP_4, AP_5)
LB <- rbind(LB_2, LB_3, LB_4, LB_5)
AL <- rbind(AL_2, AL_3, AL_4, AL_5)

GF$group <- rep("AX")
CV$group <- rep("CV")
LB$group <- rep("LB")
AP$group <- rep("AP")
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
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 16),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 4)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[1, 1:24]), t(data[2, 1:24]), t(data[3, 1:24]), t(data[4, 1:24])),
        group = rep(data[1, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[5, 1:24]), t(data[6, 1:24]), t(data[7, 1:24]), t(data[8, 1:24])),
        group = rep(data[5, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[9, 1:24]), t(data[10, 1:24]), t(data[11, 1:24]), t(data[12, 1:24])),
        group = rep(data[9, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[13, 1:24]), t(data[14, 1:24]), t(data[15, 1:24]), t(data[16, 1:24])),
        group = rep(data[13, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = c(t(data[17, 1:24]), t(data[18, 1:24]), t(data[19, 1:24]), t(data[20, 1:24])),
        group = rep(data[17, "group"]), rep = c(rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

fwrite(data_sat, "position/diet_enter_time_percentage/nighttime_sat.csv") ### wide format; gitignore because file is too large (> 100mb)



# ============== Function 6.2.1 : TukeyHSD analysis among groups =================
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(data.table)
library(multcomp)
library(multcompView)
theme_set(theme_bw())
data_sat <- read.csv("position/diet_enter_time_percentage/nighttime_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Diet enter times (%)") +
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
pdf(file = "position/diet_enter_time_percentage/nighttime_group.pdf", width = 12, height = 16)
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


pdf(file = "position/diet_enter_time_percentage/45+90.pdf", width = 10, height = 8)
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

pdf(file = "position/diet_enter_time_percentage/180+360.pdf", width = 10, height = 8)
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

# ============== Function 6.2.2: TukeyHSD analysis among ratio =================
theme_set(theme_bw())
data_sat <- read.csv("position/diet_enter_time_percentage/nighttime_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Diet enter times (%)") +
        ylim(min(data_sat$value, na.rm = TRUE), max(data_sat$value, na.rm = TRUE) + 8) # !!!!

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
pdf(file = "position/diet_enter_time_percentage/nighttime_ratio.pdf", width = 12, height = 16)
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
