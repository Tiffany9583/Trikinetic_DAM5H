#################################################################################
# Outline (please use Find function to search the codes)
# Function 1 : Output txt file to CSV files
# Function 2 : Compute sleep data for heatmap (Average sleep time; bind every day)
#
# Function 3 : Compute sleep data for zeitgeber (bin every 30 min)
# |__Function 3.1: Plot a line chart for zeitgeber (singel group)
# |__Function 3.2: Plot a line chart for zeitgeber (five group)
# |__Function 3.3: Statistical Analysis for zeitgeber
#
# Function 4: Compute sleep data for [total sleep duration datas]
# |__Function 4.1: TukeyHSD analysis among group
# |__Function 4.2: TukeyHSD analysis among ratio
#
# Function 5: Compute sleep data for [average sleep time]
# |__Function 5.1: TukeyHSD analysis among group
# |__Function 5.2: TukeyHSD analysis among ratio
#
# Function 6: Compute sleep data for [sleep bout number per 24 h]
# |__Function 6.1: TukeyHSD analysis among group
# |__Function 6.2: TukeyHSD analysis among ratio
#
# Function 7 : customize for combind hours
# |__Function 7.1: Average sleep time
#    |__Function 7.1.1: TukeyHSD analysis among groups
#    |__Function 7.1.2: TukeyHSD analysis among ratio
# |__Function 7.2: Total sleep duration
# |__Function 7.3: Sleep bout number
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
time_step1 <- 53
time_step2 <- 5813

for (x in 1:4) {
    data <- data.frame(Data[x])
    data <- data %>% filter(V10 == "Rt")
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

fwrite(GF, "sleep/Row_Rt_data/Run05Rt_GF.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(LB, "sleep/Row_Rt_data/Run05Rt_LB.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(AP, "sleep/Row_Rt_data/Run05Rt_AP.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(CV, "sleep/Row_Rt_data/Run05Rt_CV.csv") ### wide format; gitignore because file is too large (> 100mb)
fwrite(AL, "sleep/Row_Rt_data/Run05Rt_AL.csv") ### wide format; gitignore because file is too large (> 100mb)


# ============================ Function 2 : Compute sleep data for heatmap (Average sleep time; bind every day)============================

# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

metadata <- read.csv("sleep/metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

for (x in 1:25) {
    data <- data.frame(Data[x])
    data <- data %>%
        filter(trik_status == 1)
    data <- data %>%
        filter(light_status == 0)
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    assign(namelist[x], data)
}
# head(GF_1)

# Data <- list(GF_1, LB_1, AP_1, CV_1, AL_1)
# namelist <- c("GF", "LB", "AP", "CV", "AP+LB")


RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    # if (per >= 300) {
                    #     l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    # }
                    if (per >= 1800) {
                        l <- append(l, per / 60) # sleep was defined as 30 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- mean(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}


cal_rest_time <- function(Data) {
    All_data <- data.frame(No = c(seq(24)))
    for (i in 1:5) {
        data <- data.frame(Data[i])
        for (col in 7:30) {
            Ycol <- RtCalfun(data[, col])
            if (col == 7) {
                data_now <- data.frame(Rt = c(Ycol))
            } else {
                data_now <- rbind(data_now, data.frame(Rt = c(Ycol)))
            }
        }
        All_data <- cbind(All_data, data_now)
    }
    return(All_data[2:6])
}

# output daytime files
metadata <- read.csv("sleep/metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

data_GF <- cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))
data_GF$mean <- rowMeans(data_GF, na.rm = TRUE)
names(data_GF) <- c("GF_1_dayRT", "GF_2_dayRT", "GF_3_dayRT", "GF_4_dayRT", "GF_5_dayRT", "rowMean")
fwrite(data_GF, "sleep/GF_day_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))
data_CV$mean <- rowMeans(data_CV, na.rm = TRUE)
names(data_CV) <- c("CV_1_dayRT", "CV_2_dayRT", "CV_3_dayRT", "CV_4_dayRT", "CV_5_dayRT", "rowMean")
fwrite(data_CV, "sleep/CV_day_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))
data_LB$mean <- rowMeans(data_LB, na.rm = TRUE)
names(data_LB) <- c("LB_1_dayRT", "LB_2_dayRT", "LB_3_dayRT", "LB_4_dayRT", "LB_5_dayRT", "rowMean")
fwrite(data_LB, "sleep/LB_day_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))
data_AP$mean <- rowMeans(data_AP, na.rm = TRUE)
names(data_AP) <- c("AP_1_dayRT", "AP_2_dayRT", "AP_3_dayRT", "AP_4_dayRT", "AP_5_dayRT", "rowMean")
fwrite(data_AP, "sleep/AP_day_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))
data_AL$mean <- rowMeans(data_AL, na.rm = TRUE)
names(data_AL) <- c("AL_1_dayRT", "AL_2_dayRT", "AL_3_dayRT", "AL_4_dayRT", "AL_5_dayRT", "rowMean")
fwrite(data_AL, "sleep/AL_day_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

All_data <- cbind(metadata, data_GF[6], data_LB[6], data_AP[6], data_CV[6], data_AL[6])



names(All_data) <- c("No", "protein", "sugar", "GF_day_Rt", "LB_day_Rt", "AP_day_Rt", "CV_day_Rt", "AL_day_Rt")
All_data
fwrite(All_data, "sleep/average_sleep_time/all_day_Rt_30.csv") ### wide format; gitignore because file is too large (> 100mb)


# output nighttime files
metadata <- read.csv("sleep/metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

data_GF <- cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))
data_GF$mean <- rowMeans(data_GF, na.rm = TRUE)
names(data_GF) <- c("GF_1_nightRT", "GF_2_nightRT", "GF_3_nightRT", "GF_4_nightRT", "GF_5_nightRT", "rowMean")
fwrite(data_GF, "sleep/GF_night_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))
data_CV$mean <- rowMeans(data_CV, na.rm = TRUE)
names(data_CV) <- c("CV_1_nightRT", "CV_2_nightRT", "CV_3_nightRT", "CV_4_nightRT", "CV_5_nightRT", "rowMean")
fwrite(data_CV, "sleep/CV_night_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))
data_LB$mean <- rowMeans(data_LB, na.rm = TRUE)
names(data_LB) <- c("LB_1_nightRT", "LB_2_nightRT", "LB_3_nightRT", "LB_4_nightRT", "LB_5_nightRT", "rowMean")
fwrite(data_LB, "sleep/LB_night_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))
data_AP$mean <- rowMeans(data_AP, na.rm = TRUE)
names(data_AP) <- c("AP_1_nightRT", "AP_2_nightRT", "AP_3_nightRT", "AP_4_nightRT", "AP_5_nightRT", "rowMean")
fwrite(data_AP, "sleep/AP_night_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))
data_AL$mean <- rowMeans(data_AL, na.rm = TRUE)
names(data_AL) <- c("AL_1_nightRT", "AL_2_nightRT", "AL_3_nightRT", "AL_4_nightRT", "AL_5_nightRT", "rowMean")
fwrite(data_AL, "sleep/AL_night_Rt.csv") ### wide format; gitignore because file is too large (> 100mb)

All_data <- cbind(metadata, data_GF[6], data_LB[6], data_AP[6], data_CV[6], data_AL[6])



names(All_data) <- c("No", "protein", "sugar", "GF_night_Rt", "LB_night_Rt", "AP_night_Rt", "CV_night_Rt", "AL_night_Rt")
All_data
fwrite(All_data, "sleep/average_sleep_time/all_night_Rt_30.csv") ### wide format; gitignore because file is too large (> 100mb)


# ===================== plot response surface ================
metadata <- read.csv("sleep/average_sleep_time/all_night_Rt_30.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

# Data <- list(metadata$CV_day_Rt, metadata$GF_day_Rt, metadata$AP_day_Rt, metadata$LB_day_Rt, metadata$AL_day_Rt)
Data <- list(metadata$CV_night_Rt, metadata$GF_night_Rt, metadata$AP_night_Rt, metadata$LB_night_Rt, metadata$AL_night_Rt)
name_list <- c("CV", "AX", "AP", "LB", "AP+LB")

int <- array(c(metadata$protein, metadata$sugar), dim = c(24, 2)) # Define position matrix

print("MAX:")
for (i in Data) {
    print(max(i, na.rm = TRUE))
}

print("MIN:")
for (i in Data) {
    print(min(i, na.rm = TRUE))
}

# ===== save plot ( five ) =====
# pdf(file = "sleep/average_sleep_time/Day_RT_30.pdf", width = 15, height = 9)
pdf(file = "sleep/average_sleep_time/Night_RT_30.pdf", width = 15, height = 9)
# png('0628.png', width = 550, height = 450)
par(oma = c(4, 4, 4, 4)) # bottom, left, top and right (oma for outside of fig)
set.panel(2, 3)
par(mar = c(1, 1, 1, 4))

for (x in 1:5) {
    s <- int
    y <- as.numeric(unlist(Data[x]))

    # fit0 <- spatialProcess(s, y)
    fit0 <- Tps(s, y)

    surface(fit0,
        xlab = "", ylab = "", col = turbo(256), cex.lab = 1.5, cex.axis = 1.1, axes = FALSE,
        zlim = c(52, 250)
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

# mtext("Average day time sleep (min/day)", side = 3, cex = 2.5, outer = TRUE, line = -2.5)
mtext("Average night time sleep (min/day)", side = 3, cex = 2.5, outer = TRUE, line = -2.5)

mtext(line = -2, side = 1, cex = 2, "Protein in diet (g/L)", outer = TRUE)
mtext(line = -2, side = 2, cex = 2, "Carbohydrate in diet (g/L)", outer = TRUE)
set.panel() # reset plotting device

dev.off()








# ================== Function 3 : Compute sleep data for zeitgeber (bin every 30 min)  ==================
# ================== Function 3.1: Plot a line chart for zeitgeber (singel group) ==================

GF <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")


GF$fulltime <- as.POSIXct(paste(GF$date, GF$time))
LB$fulltime <- as.POSIXct(paste(LB$date, LB$time))
AP$fulltime <- as.POSIXct(paste(AP$date, AP$time))
CV$fulltime <- as.POSIXct(paste(CV$date, CV$time))
AL$fulltime <- as.POSIXct(paste(AL$date, AL$time))


# ====== Plot a multi line charts ==========
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")


# print("MAX:")
# print(max(data[,-1]))


RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- mean(l)
            # print(l)
        }
        return(re)
    } else {
        re <- 0
        return(re)
    }
}


Plot <- function(data_o) {
    data <- data_o %>%
        group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
        summarize(across(c(t1:t24), \(x) RtCalfun(x)))

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

        plot_lst[[i]] <- p
    }

    # diet_name_lst <-c('t1', 't2', 't3', 't4', 't5', 't6',  't9' , 't10', 't11', 't12', 't13', 't14', 't17','t18', 't19','t20','t21','t22' ,'t25','t26','t27','t28','t29','t30' )
    # Combine all plots
    v1 <- cowplot::plot_grid(plotlist = plot_lst, nrow = 4, labels = name_lst, label_size = 8, vjust = 0.5)
    return(v1)
}


head(CV)
ggsave(
    "sleep/Run02Rt_CV.png",
    Plot(CV),
    width = 16,
    height = 8,
    dpi = 300
)

ggsave(
    "sleep/Row_Rt_data/Run02Rt_GF.png",
    Plot(GF),
    width = 16,
    height = 8,
    dpi = 300
)

ggsave(
    "sleep/Row_Rt_data/Run02Rt_AP.png",
    Plot(AP),
    width = 16,
    height = 8,
    dpi = 300
)


ggsave(
    "sleep/Row_Rt_data/Run02Rt_LB.png",
    Plot(LB),
    width = 16,
    height = 8,
    dpi = 300
)

ggsave(
    "sleep/Row_Rt_data/Run02Rt_AL.png",
    Plot(AL),
    width = 16,
    height = 8,
    dpi = 300
)

# ================== Function 3.2: Plot a line chart for zeitgeber (five group) ==================
# ======================= combind for each hour =======================

# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")


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



RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- mean(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_zeitgeber <- function(data, batch_no) {
    data <- data %>%
        group_by(fulltime = floor_date(fulltime, "30 mins")) %>%
        summarize(across(c(t1:t24), \(x) RtCalfun(x)))

    # data$time2 <- hour(data$fulltime)
    data$time2 <- paste(format(as.POSIXct(data$fulltime), format = "%H:%M:%S"))
    data <- data %>%
        group_by(time2) %>%
        summarize(across(c(t1:t24), \(x) sum(x, na.rm = TRUE) / 4))
    # data_1$hour <- c(17, 18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    # data_1$hour <- c(18, 19, 20, 21, 22, 23, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    data$hour <- c(
        18, 18.5, 19, 19.5, 20, 20.5, 21, 21.5, 22, 22.5, 23, 23.5, 0, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5,
        5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10, 10.5, 11, 11.5, 12, 12.5, 13, 13.5, 14, 14.5, 15, 15.5, 16, 16.5, 17, 17.5
    )

    data$batch <- rep(batch_no)
    return(data)
}


data_GF <- rbind(cal_zeitgeber(GF_1, "B1"), cal_zeitgeber(GF_2, "B2"), cal_zeitgeber(GF_3, "B3"), cal_zeitgeber(GF_4, "B4"), cal_zeitgeber(GF_5, "B5"))
fwrite(data_GF, "sleep/every30min/GF_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_CV <- rbind(cal_zeitgeber(CV_1, "B1"), cal_zeitgeber(CV_2, "B2"), cal_zeitgeber(CV_3, "B3"), cal_zeitgeber(CV_4, "B4"), cal_zeitgeber(CV_5, "B5"))
fwrite(data_CV, "sleep/every30min/CV_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AP <- rbind(cal_zeitgeber(AP_1, "B1"), cal_zeitgeber(AP_2, "B2"), cal_zeitgeber(AP_3, "B3"), cal_zeitgeber(AP_4, "B4"), cal_zeitgeber(AP_5, "B5"))
fwrite(data_AP, "sleep/every30min/AP_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_LB <- rbind(cal_zeitgeber(LB_1, "B1"), cal_zeitgeber(LB_2, "B2"), cal_zeitgeber(LB_3, "B3"), cal_zeitgeber(LB_4, "B4"), cal_zeitgeber(LB_5, "B5"))
fwrite(data_LB, "sleep/every30min/LB_all.csv") ### wide format; gitignore because file is too large (> 100mb)

data_AL <- rbind(cal_zeitgeber(AL_1, "B1"), cal_zeitgeber(AL_2, "B2"), cal_zeitgeber(AL_3, "B3"), cal_zeitgeber(AL_4, "B4"), cal_zeitgeber(AL_5, "B5"))
fwrite(data_AL, "sleep/every30min/AL_all.csv") ### wide format; gitignore because file is too large (> 100mb)


# ================== output average zeitgeber datas (remove ghost datas in pervious files)===================
average_zeitgeber <- function(data) {
    data_1 <- data %>%
        group_by(hour) %>%
        summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))
    return(data_1)
}

GF <- read.csv("sleep/every30min/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(GF), "sleep/every30min/GF.csv") ### wide format; gitignore because file is too large (> 100mb)

CV <- read.csv("sleep/every30min/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(CV), "sleep/every30min/CV.csv") ### wide format; gitignore because file is too large (> 100mb)

AP <- read.csv("sleep/every30min/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AP), "sleep/every30min/AP.csv") ### wide format; gitignore because file is too large (> 100mb)

AL <- read.csv("sleep/every30min/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(AL), "sleep/every30min/AL.csv") ### wide format; gitignore because file is too large (> 100mb)

LB <- read.csv("sleep/every30min/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
fwrite(average_zeitgeber(LB), "sleep/every30min/LB.csv") ### wide format; gitignore because file is too large (> 100mb)

# max(data_CV)


# ================== load datas ==================
GF <- read.csv("sleep/every30min/GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("sleep/every30min/LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("sleep/every30min/AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("sleep/every30min/CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("sleep/every30min/AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ====繪製多折線圖 ====
# create a list with a specific length
Sys.setlocale("LC_TIME", "English")
plot_lst <- vector("list", length = 24)
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")

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
        labs(x = "Zeitgeber", y = "Sleep (min/30min)") +
        ylim(0, 30) +
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
    "sleep/every30min/zeitgeber.png",
    v1,
    width = 16,
    height = 8,
    dpi = 300
)



# ================== Function 3.3: Statistical Analysis for zeitgeber ==================
library(multcompView)

GF <- read.csv("sleep/every30min/GF_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB <- read.csv("sleep/every30min/LB_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP <- read.csv("sleep/every30min/AP_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV <- read.csv("sleep/every30min/CV_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL <- read.csv("sleep/every30min/AL_all.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

GF$group <- rep("GF")
LB$group <- rep("LB")
AP$group <- rep("AP")
CV$group <- rep("CV")
AL$group <- rep("AP+LB")

data_sat <- rbind(CV, AL, GF, AP, LB)
head(data_sat)

# analysis of variance
name_lst <- c("t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9", "t10", "t11", "t12", "t13", "t14", "t15", "t16", "t17", "t18", "t19", "t20", "t21", "t22", "t23", "t24")
sat_res <- data.frame(group = c("CV", "AL", "GF", "AP", "LB"))
rownames(sat_res) <- sat_res$group



for (h in seq(0, 23, by = 0.5)) {
    # h = 0
    data_sat_now <- data_sat %>%
        filter(hour == h)

    for (i in 1:24) {
        sat_res_pre <- sat_res
        sat_res <- data.frame(group = c("CV", "AP+LB", "GF", "AP", "LB"))
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
# head(sat_res)

fwrite(sat_res, "sleep/every30min/Sat.csv") ### wide format; gitignore because file is too large (> 100mb)

sat_res_nota <- sat_res %>%
    filter(Letters != "a")
fwrite(sat_res_nota, "sleep/every30min/Sat_significant_diff.csv") ### wide format; gitignore because file is too large (> 100mb)



# ============================ Function 4: Compute sleep data for [total sleep duration datas] ============================
# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

metadata <- read.csv("metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

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
    data <- dayfunc(data, Time1, Time2)
    # data <- nightfunc(data, Time1, Time2)
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    assign(namelist[x], data)
}
# head(GF_1)

# Data <- list(GF_1, LB_1, AP_1, CV_1, AL_1)
# namelist <- c("GF", "LB", "AP", "CV", "AP+LB")
# RtCalfun(GF_1$t1)[1]


RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- sum(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        for (col in 7:30) {
            Ycol <- RtCalfun(data[, col])
            if (col == 7) {
                data_now <- data.frame(Rt = c(Ycol))
            } else {
                data_now <- rbind(data_now, data.frame(Rt = c(Ycol)))
            }
        }
        All_data <- rbind(All_data, data_now)
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$Rt,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$Rt,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$Rt,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$Rt,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$Rt,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

# data_day_sat / data_night_sat
fwrite(data_sat, "sleep/sleep_duration/data_day_sat.csv")

# ============== Function 4.1: TukeyHSD analysis among group =================
data_sat <- read.csv("sleep/sleep_duration/data_day_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Total sleep duration (min)") +
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

# ========= save plot =========
pdf(file = "sleep/sleep_duration/day_group.pdf", width = 12, height = 16)
# pdf(file = "sleep/sleep_duration/night_group.pdf", width = 12, height = 16)
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

# pdf(file = "sleep/sleep_duration/day_45+90.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/sleep_duration/day_180+360.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_180+360.pdf", width = 10, height = 8)
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

# ============== Function 4.2: TukeyHSD analysis among ratio =================
data_sat <- read.csv("sleep/sleep_duration/data_day_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Total sleep duration (min)") +
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
pdf(file = "sleep/sleep_duration/day_ratio.pdf", width = 12, height = 16)
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


# ============================ Function 5: Compute sleep data for [average sleep time] ============================
# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

metadata <- read.csv("sleep/metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

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
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    assign(namelist[x], data)
}
# head(GF_1)

# Data <- list(GF_1, LB_1, AP_1, CV_1, AL_1)
# namelist <- c("GF", "LB", "AP", "CV", "AP+LB")

RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 1800) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- mean(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        for (col in 7:30) {
            Ycol <- RtCalfun(data[, col])
            if (col == 7) {
                data_now <- data.frame(Rt = c(Ycol))
            } else {
                data_now <- rbind(data_now, data.frame(Rt = c(Ycol)))
            }
        }
        All_data <- rbind(All_data, data_now)
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$Rt,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$Rt,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$Rt,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$Rt,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$Rt,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

# data_day_sat / data_night_sat
fwrite(data_sat, "sleep/average_sleep_time/data_night_sat_30.csv") ### wide format; gitignore because file is too large (> 100mb)
# ================================ Function 5.1: TukeyHSD analysis among group ================================
data_sat <- read.csv("sleep/average_sleep_time/data_night_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        geom_text(data = plot_data, aes(x = group, y = max + 4, label = Labels)) +
        theme(legend.position = "none") +
        xlab("Treatment (P:C)") +
        # ylab("Average daytime sleep (min)") +
        ylab("Average nighttime sleep (min)") +
        ylim(5.84375, 128.3581) # !!!!

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

# ========= save plot =========
# pdf(file = "sleep/average_sleep_time/day_group_30.pdf", width = 12, height = 16)
pdf(file = "sleep/average_sleep_time/night_group_30.pdf", width = 12, height = 16)
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

pdf(file = "sleep/average_sleep_time/day_45+90.pdf", width = 10, height = 8)
# pdf(file = "sleep/average_sleep_time/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/average_sleep_time/day_180+360.pdf", width = 10, height = 8)
pdf(file = "sleep/average_sleep_time/night_180+360.pdf", width = 10, height = 8)
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
# ======================== Function 5.2: TukeyHSD analysis among ratio ==============================
data_sat <- read.csv("sleep/average_sleep_time/data_night_sat_30.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        # ylab("Average daytime sleep (min)") +
        ylab("Average nighttime sleep (min)") +
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
pdf(file = "sleep/average_sleep_time/night_ratio_30.pdf", width = 12, height = 16)
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

# ============================ Function 6: Compute sleep data for [sleep bout number per 24 h] ============================

# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

metadata <- read.csv("metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

Data <- list(GF_1, GF_2, GF_3, GF_4, GF_5, CV_1, CV_2, CV_3, CV_4, CV_5, AP_1, AP_2, AP_3, AP_4, AP_5, LB_1, LB_2, LB_3, LB_4, LB_5, AL_1, AL_2, AL_3, AL_4, AL_5)
namelist <- c("GF_1", "GF_2", "GF_3", "GF_4", "GF_5", "CV_1", "CV_2", "CV_3", "CV_4", "CV_5", "AP_1", "AP_2", "AP_3", "AP_4", "AP_5", "LB_1", "LB_2", "LB_3", "LB_4", "LB_5", "AL_1", "AL_2", "AL_3", "AL_4", "AL_5")

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
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    assign(namelist[x], data)
}
# head(GF_1)

# Data <- list(GF_1, LB_1, AP_1, CV_1, AL_1)
# namelist <- c("GF", "LB", "AP", "CV", "AP+LB")
# RtCalfun(GF_1$t2)[1]


RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- 1 # Fly is all sleep in this time period. So the sleep amounts == 1 !!!! Can not use in small bin
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- length(na.omit(l))
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        for (col in 7:30) {
            Ycol <- RtCalfun(data[, col]) # get sleep_bout_number
            if (col == 7) {
                data_now <- data.frame(Rt = c(Ycol))
            } else {
                data_now <- rbind(data_now, data.frame(Rt = c(Ycol)))
            }
        }
        All_data <- rbind(All_data, data_now)
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$Rt,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$Rt,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$Rt,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$Rt,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$Rt,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

# data_day_sat / data_night_sat
fwrite(data_sat, "sleep/sleep_bout_number/data_night_sat.csv")

# ======================== Function 6.1: TukeyHSD analysis among group ==============================
data_sat <- read.csv("sleep/sleep_bout_number/data_night_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        geom_text(data = plot_data, aes(x = group, y = max + 4, label = Labels)) +
        theme(legend.position = "none") +
        xlab("Treatment (P:C)") +
        ylab("Sleep bout number per day") +
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

# ========= save plot =========
# pdf(file = "sleep/sleep_bout_number/day_group.pdf", width = 12, height = 16)
pdf(file = "sleep/sleep_bout_number/night_group.pdf", width = 12, height = 16)
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

# pdf(file = "sleep/sleep_bout_number/day_45+90.pdf", width = 10, height = 8)
pdf(file = "sleep/sleep_bout_number/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/sleep_bout_number/day_180+360.pdf", width = 10, height = 8)
pdf(file = "sleep/sleep_bout_number/night_180+360.pdf", width = 10, height = 8)
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
# ======================== Function 6.2: TukeyHSD analysis among ratio ==============================
data_sat <- read.csv("sleep/sleep_bout_number/data_day_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Sleep bout number per day") +
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
pdf(file = "sleep/sleep_bout_number/day_ratio.pdf", width = 12, height = 16)
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
# ======================= Function 7 : customize for combind hours ()=======================

# ===================Run01===================
GF_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_1 <- read.csv("sleep/Row_Rt_data/Run01Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run02===================
GF_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_2 <- read.csv("sleep/Row_Rt_data/Run02Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run03===================
GF_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_3 <- read.csv("sleep/Row_Rt_data/Run03Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run04===================
GF_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_4 <- read.csv("sleep/Row_Rt_data/Run04Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

# ===================Run05===================
GF_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_GF.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
LB_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_LB.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AP_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AP.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
CV_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_CV.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
AL_5 <- read.csv("sleep/Row_Rt_data/Run05Rt_AL.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")

metadata <- read.csv("sleep/metadata.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
metadata

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
Time1 <- "20:00:00"
Time2 <- "23:00:00"

for (x in 1:25) {
    data <- data.frame(Data[x])
    data <- data %>%
        filter(trik_status == 1)
    # data <- data[1:4321, ] # only use three day data
    data <- CutTimefunc(data, Time1, Time2)
    data$fulltime <- as.POSIXct(paste(data$date, data$time))
    assign(namelist[x], data)
}

# # ======================= Function 7.1: Average sleep time =======================
RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 1800) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- mean(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        data <- data %>%
            group_by(fulltime = floor_date(fulltime, "day")) %>%
            summarize(across(c(t1:t24), \(x) RtCalfun(x)))
        # return(data)
        data <- data %>%
            summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))

        All_data <- rbind(All_data, t(data[1, ]))
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$V1,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$V1,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$V1,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$V1,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$V1,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

fwrite(data_sat, "sleep/customize_bind/average_14-17_sat.csv")

# ============== Function 7.1.1: TukeyHSD analysis among groups =================
library(ggplot2)
library(ggpubr)
library(scales)
library(dplyr)
library(data.table)
library(multcomp)
library(multcompView)
data_sat <- read.csv("sleep/customize_bind/average_14-17_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Average sleep duration (min)") +
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

# ========= save plot =========
pdf(file = "sleep/customize_bind/average_14-17_group.pdf", width = 12, height = 16)
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

# pdf(file = "sleep/sleep_duration/day_45+90.pdf", width = 10, height = 8)
pdf(file = "sleep/sleep_duration/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/sleep_duration/day_180+360.pdf", width = 10, height = 8)
pdf(file = "sleep/sleep_duration/night_180+360.pdf", width = 10, height = 8)
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



# ============== Function 7.1.2: TukeyHSD analysis among ratio =================
data_sat <- read.csv("sleep/customize_bind/average_14-17_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
pdf(file = "sleep/customize_bind/average_14-17_ratio.pdf", width = 12, height = 16)
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




# # ======================= Function 7.2: Total sleep duration =======================
RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- length(Ycol) # Fly is all sleep in this time period. So the sleep time == list length
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- sum(l)
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        data <- data %>%
            group_by(fulltime = floor_date(fulltime, "day")) %>%
            summarize(across(c(t1:t24), \(x) RtCalfun(x)))

        data <- data %>%
            summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))

        All_data <- rbind(All_data, t(data[1, ]))
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$V1,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$V1,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$V1,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$V1,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$V1,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

fwrite(data_sat, "sleep/customize_bind/duration_9-12_sat.csv")

data_sat <- read.csv("sleep/customize_bind/duration_9-12_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        ylab("Average sleep duration (min)") +
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

# ========= save plot =========
pdf(file = "sleep/customize_bind/duration_9-12.pdf", width = 12, height = 16)
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

# pdf(file = "sleep/sleep_duration/day_45+90.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/sleep_duration/day_180+360.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_180+360.pdf", width = 10, height = 8)
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


# # ======================= Function 7.3: Sleep bout number =======================
RtCalfun <- function(Ycol) {
    l <- c()
    c <- FALSE # to check fly is all sleep or all active
    if (length(na.omit(Ycol)) > 2) {
        if (Ycol[1] > 60) {
            p1 <- as.numeric((Ycol[1] - 60) / 60)
        } else {
            p1 <- 0
        }
        for (i in 2:length(Ycol)) {
            per <- as.numeric(Ycol[i - 1])
            now <- as.numeric(Ycol[i])
            if (!is.na(per)) {
                if (now < per) {
                    c <- TRUE
                    if (per >= 300) {
                        l <- append(l, per / 60) # sleep was defined as 5 or more consecutive minutes of inactivity
                    }
                }
            }
        }
        if (length(l) == 0) {
            if (c == TRUE) {
                re <- 0 # Fly is all activity in this time period.
            } else {
                re <- 1 # Fly is all sleep in this time period. So the sleep amounts == 1 !!!! Can not use in small bin
            }
        } else {
            l <- append(l, Ycol[length(Ycol)] / 60)
            l[1] <- (l[1] - p1)
            re <- length(na.omit(l))
            # print(l)
        }
        return(re)
    } else {
        re <- NA
        return(re)
    }
}

cal_rest_time <- function(Data) {
    All_data <- data.frame(Rt = c())
    for (i in 1:5) {
        data <- data.frame(Data[i])
        data <- data %>%
            group_by(fulltime = floor_date(fulltime, "day")) %>%
            summarize(across(c(t1:t24), \(x) RtCalfun(x)))

        data <- data %>%
            summarize(across(c(t1:t24), \(x) mean(x, na.rm = TRUE)))

        All_data <- rbind(All_data, t(data[1, ]))
    }
    return(All_data)
}

samle_meta <- data.frame(
    ratio = rep(c("00:01", "01:16", "01:08", "01:04", "01:02", "01:01"), 4),
    level = rep(c("45", "45", "45", "45", "45", "45", "90", "90", "90", "90", "90", "90", "180", "180", "180", "180", "180", "180", "360", "360", "360", "360", "360", "360"), 5)
)

data_sat <- rbind(
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(GF_1, GF_2, GF_3, GF_4, GF_5))$V1,
        group = rep("AX"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(CV_1, CV_2, CV_3, CV_4, CV_5))$V1,
        group = rep("CV"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(LB_1, LB_2, LB_3, LB_4, LB_5))$V1,
        group = rep("LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AP_1, AP_2, AP_3, AP_4, AP_5))$V1,
        group = rep("AP"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    ),
    data.frame(
        ratio = samle_meta$ratio, level = samle_meta$level, value = cal_rest_time(list(AL_1, AL_2, AL_3, AL_4, AL_5))$V1,
        group = rep("AP+LB"), rep = c(rep(1, 24), rep(2, 24), rep(3, 24), rep(4, 24), rep(5, 24))
    )
)

fwrite(data_sat, "sleep/customize_bind/amounts_9-12_sat.csv")

data_sat <- read.csv("sleep/customize_bind/amounts_9-12_sat.csv", header = TRUE, sep = ",", dec = ".", fileEncoding = "UTF-8-BOM")
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
        geom_text(data = plot_data, aes(x = group, y = max + 2, label = Labels)) +
        theme(legend.position = "none") +
        xlab("Treatment (P:C)") +
        ylab("Sleep bout amount") +
        ylim(min(data_sat$value, na.rm = TRUE), max(data_sat$value, na.rm = TRUE) + 5) # !!!!

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

# ========= save plot =========
pdf(file = "sleep/customize_bind/amount_9-12.pdf", width = 12, height = 16)
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

# pdf(file = "sleep/sleep_duration/day_45+90.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_45+90.pdf", width = 10, height = 8)
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

# pdf(file = "sleep/sleep_duration/day_180+360.pdf", width = 10, height = 8)
# pdf(file = "sleep/sleep_duration/night_180+360.pdf", width = 10, height = 8)
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
