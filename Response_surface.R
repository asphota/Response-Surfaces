# Last Modified Date : March 27 2020
# Contact : Asphota Wasti (wastiaa@mail.uc.edu)
# Graduate Student at University of Cincinnati
# This code is for organizing the outputs of the hydrologic model into stress response surface for Upper Arun Hydroelectric Project

# #Installing the packages
# install.packages("lubridate")
# install.packages("readr")
# install.packages("dplyr")
# install.packages("reshape2")
# install.packages("stringr")
# install.packages("ggplot2")
# install.packages("scales")
# install.packages("xlsx")
# install.packages("mvtnorm")


#Loading required libraries
library(lubridate)
library(readr)
library(dplyr)
library(reshape2)
library(stringr)
library(ggplot2)
library(scales)
library(xlsx)


out_dir <- "D:\\Upper_Arun_project\\C_climate_response_surface\\outputs\\"
GCM_dir <- "D:\\Upper_Arun_project\\C_climate_response_surface\\gcms\\"
setwd(in_dir)
out_dir <- "D:\\Upper_Arun_project\\C_climate_response_surface\\outputs\\centred_around_2036\\"
######################################################################################################
#STEP 2: LOAD THE PERTURBED DATA FOR PLOTTING THE RESPONSE SURFACE

#rm(list=ls())
setwd(out_dir)
Annual_Streamflow_all <- read.csv("Annual_Streamflow.csv")
# Monsoon_Streamflow_all <-  read.csv("Monsoon_Streamflow.csv")
# Non_Monsoon_Streamflow_all <-  read.csv("Non_Monsoon_Streamflow.csv")
# Annual_energy_all <- read.csv("Annual_energy.csv")
# Sediment_tonnes_all <- read.csv("Sediment_tonnes.csv")
# NPV_all <- read.csv("Net_Present_Value.csv")

#Arranging the data in the format required in the Surface Plot
P_change_num <- c(0.6,0.7, 0.8,0.9,  1, 1.1, 1.2, 1.3, 1.4)
T_change_num <- unique(Annual_Streamflow_all$Temperature)

# Setting up the trigger for response surface
Annual_Streamflow_trigger <- Annual_Streamflow_all[29,3]
# Monsoon_Streamflow_trigger <- Monsoon_Streamflow_all[29,3]
# Non_Monsoon_Streamflow_trigger <- Non_Monsoon_Streamflow_all[29,3]
# Annual_energy_trigger <- Annual_energy_all[29,3]
# Sediment_tonnes_trigger <- Sediment_tonnes_all[29,3]
# NPV_trigger <- 0


Annual_Streamflow_matrix <- matrix(data = Annual_Streamflow_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
colnames(Annual_Streamflow_matrix) <- c("T1","T2","T3","T4","T5","T6", "T7")
# 
# Annual_energy_matrix <- matrix(data = Annual_energy_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
# colnames(Annual_energy_matrix) <- c("T1","T2","T3","T4","T5","T6", "T7")
# 
# Monsoon_Streamflow_matrix <- matrix(data = Monsoon_Streamflow_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
# colnames(Monsoon_Streamflow_matrix) <- c("T1","T2","T3","T4","T5","T6", "T7")
# 
# Non_Monsoon_Streamflow_matrix <- matrix(data = Non_Monsoon_Streamflow_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
# colnames(Non_Monsoon_Streamflow_matrix) <- c("T1","T2","T3","T4","T5","T6", "T7")
# 
# Sediment_tonnes_matrix <- matrix(data = Sediment_tonnes_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
# colnames(Sediment_tonnes_matrix ) <- c("T1","T2","T3","T4","T5","T6", "T7")
# 
# NPV_matrix <- matrix(data = NPV_all[,3], nrow = 9, ncol = 7, byrow = TRUE)
# colnames(NPV_matrix) <- c("T1","T2","T3","T4","T5","T6", "T7")


######################################################################################################
######################################################################################################
#STEP 3: LOAD GCM SHIFTS FOR EACH IPCC SCENARIO, ORGANIZE THEM, AND PLOT THEM
setwd(GCM_dir)

#GCMs <- "Uwagaun_CMIP5_Hist(1971-2000)_RCP(2036-2065).xlsx"
GCMs <- "Uwagaun_CMIP5_Hist(1971-2000)_RCP(2020-2050).xlsx"
sheets <- c("pr_historical","pr_rcp26", "pr_rcp45","pr_rcp60","pr_rcp85","tas_historical","tas_rcp26", "tas_rcp45", "tas_rcp60", "tas_rcp85")

GCM_list <- list()
for(i in 1:length(sheets)) {
  print(sheets[i])
  GCM_list[[i]] <- read.xlsx(GCMs, sheetName = sheets[i])
}


P_hist <- GCM_list[[1]] #Units are mm
P_hist[14] <- rowMeans(P_hist[,2:13])
P_RCP26 <- GCM_list[[2]]
P_RCP26[14] <- rowMeans(P_RCP26[,2:13])
P_RCP45 <- GCM_list[[3]]
P_RCP45[14] <- rowMeans(P_RCP45[,2:13])
P_RCP60 <- GCM_list[[4]]
P_RCP60[14] <- rowMeans(P_RCP60[,2:13])
P_RCP85 <- GCM_list[[5]]
P_RCP85[14] <- rowMeans(P_RCP85[,2:13])

DP_RCP26 <- array(NA,dim(P_RCP26)[1])
for (i in 1:dim(P_RCP26)[1]) {
  index <- which(as.vector(P_hist[,1]) == as.vector(P_RCP26[i,1]))
  DP_RCP26[i] <- (P_RCP26[i,14]) / P_hist[index,14]
}
DP_RCP45 <- array(NA,dim(P_RCP45)[1])
for (i in 1:dim(P_RCP45)[1]) {
  index <- which(as.vector(P_hist[,1]) == as.vector(P_RCP45[i,1]))
  DP_RCP45[i] <- (P_RCP45[i,14]) / P_hist[index,14]
}
DP_RCP60 <- array(NA,dim(P_RCP60)[1])
for (i in 1:dim(P_RCP60)[1]) {
  index <- which(as.vector(P_hist[,1]) == as.vector(P_RCP60[i,1]))
  DP_RCP60[i] <- (P_RCP60[i,14]) / P_hist[index,14]
}
DP_RCP85 <- array(NA,dim(P_RCP85)[1])
for (i in 1:dim(P_RCP85)[1]) {
  index <- which(as.vector(P_hist[,1]) == as.vector(P_RCP85[i,1]))
  DP_RCP85[i] <- (P_RCP85[i,14]) / P_hist[index,14]
}

T_hist <- GCM_list[[6]] #Units are mm
T_hist[14] <- rowMeans(T_hist[,2:13])
T_RCP26 <- GCM_list[[7]]
T_RCP26[14] <- rowMeans(T_RCP26[,2:13])
T_RCP45 <- GCM_list[[8]]
T_RCP45[14] <- rowMeans(T_RCP45[,2:13])
T_RCP60 <- GCM_list[[9]]
T_RCP60[14] <- rowMeans(T_RCP60[,2:13])
T_RCP85 <- GCM_list[[10]]
T_RCP85[14] <- rowMeans(T_RCP85[,2:13])

DT_RCP26 <- array(NA,dim(T_RCP26)[1])
for (i in 1:dim(T_RCP26)[1]) {
  index <- which(as.vector(T_hist[,1]) == as.vector(T_RCP26[i,1]))
  DT_RCP26[i] <- T_RCP26[i,14]
}
DT_RCP45 <- array(NA,dim(T_RCP45)[1])
for (i in 1:dim(T_RCP45)[1]) {
  index <- which(as.vector(T_hist[,1]) == as.vector(T_RCP45[i,1]))
  DT_RCP45[i] <- T_RCP45[i,14]
}
DT_RCP60 <- array(NA,dim(T_RCP60)[1])
for (i in 1:dim(T_RCP60)[1]) {
  index <- which(as.vector(T_hist[,1]) == as.vector(T_RCP60[i,1]))
  DT_RCP60[i] <- T_RCP60[i,14]
}
DT_RCP85 <- array(NA,dim(T_RCP85)[1])
for (i in 1:dim(T_RCP85)[1]) {
  index <- which(as.vector(T_hist[,1]) == as.vector(T_RCP85[i,1]))
  DT_RCP85[i] <- T_RCP85[i,14]
}

######################################################################################################
#ORGANIZE THE GCMS (just RCP 4.5 and RCP 8.5) FOR PLOTTING

#FIT PDF TO GCMS (just RCP 4.5 and RCP 8.5)
#put both RCPs for all GCMs into a single dataframe
RCP45_dataframe <- data.frame(RCP=rep("RCP45",times=length(DT_RCP45)),GCM=T_RCP45[,1],DT=DT_RCP45,DP=DP_RCP45)
RCP85_dataframe <- data.frame(RCP=rep("RCP85",times=length(DT_RCP85)),GCM=T_RCP85[,1],DT=DT_RCP85,DP=DP_RCP85)
GCMs_dataframe <- rbind(RCP45_dataframe,RCP85_dataframe)

#calibrate the delta P's for the axis used in Climate Response Surface
DP_RCP26 <- DP_RCP26 + 1
DP_RCP45 <- DP_RCP45 + 1
DP_RCP60 <- DP_RCP60 + 1
DP_RCP85 <- DP_RCP85 + 1

######################################################################################################
######################################################################################################

out_dir_plot <-   paste0(out_dir,"Plots\\")


#STEP 4: PLOT CLIMATE RESPONSE SURFACES

##################
# Average Annual Streamflow
##################
clim_resp_surf_data <- Annual_Streamflow_matrix
Trigger <- Annual_Streamflow_trigger #For Base_Design
units <- 1 #For m3/s

#FOR CASES IN WHICH THE TRIGGER LEVEL IS WITHIN THE RANGE OF ZZLIM
zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
zzlim

Trigger_level = Trigger/units 
Trigger_level

a <- 8
b <- 8
mylevel <- c(seq(zzlim[1],Trigger_level,length.out=a),seq(Trigger_level,zzlim[2],length.out=b)[-1])
mycolors <- c(colorRampPalette(c("red","white"))(a)[-a],colorRampPalette(c("white","blue"))(b)[-1]) #if bigger numbers are better

plot_title <- paste0("Average Streamflow (MCM) ")
plot_tile_save <- paste0(out_dir_plot,"Average_Annual_Streamflow_stress_response_surface.png")

#windows(8,7)
png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
               levels=mylevel,col=mycolors, 
               plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
                 points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
                 points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
                 points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
                 axis(1);axis(2)},
               main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
text(.9,.3, paste0(round(Trigger_level, 1)))

dev.off()
# 

# 
# ##########################
# # Average Annual Energy
# ##########################
# 
# clim_resp_surf_data <- Annual_energy_matrix
# Trigger <-  Annual_energy_trigger #For Base_Design
# units <- 1 # GWhr
# 
# #FOR CASES IN WHICH THE TRIGGER LEVEL IS WITHIN THE RANGE OF ZZLIM
# zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
# zzlim
# 
# Trigger_level = Trigger/units
# Trigger_level
# 
# a <- 8
# b <- 8
# mylevel <- c(seq(zzlim[1],Trigger_level,length.out=a),seq(Trigger_level,zzlim[2],length.out=b)[-1])
# mycolors <- c(colorRampPalette(c("red","white"))(a)[-a],colorRampPalette(c("white","blue"))(b)[-1]) #if bigger numbers are better
# 
# plot_title <- paste0("Annual Energy (GWhr) ")
# plot_tile_save <- paste0(out_dir_plot,"Average_Annual_Energy_stress_response_surface.png")
# 
# #windows(8,7)
# png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
# filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
#                levels=mylevel,col=mycolors, 
#                plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
#                  points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
#                  points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
#                  points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
#                  axis(1);axis(2)},
#                main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
# text(.9,.3, paste0(round(Trigger_level, 1)))
# 
# dev.off()
# 
# 
# # 
# # 
# 
# ############################
# # Average Monsoon Streamflow
# ############################
# 
# clim_resp_surf_data <- Monsoon_Streamflow_matrix
# Trigger <- Monsoon_Streamflow_trigger #For Base_Design
# units <- 1 #For MCM
# 
# #FOR CASES IN WHICH THE TRIGGER LEVEL IS WITHIN THE RANGE OF ZZLIM
# zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
# zzlim
# 
# Trigger_level = Trigger/units #Reliability indicates amount of KWh to be added to the
# Trigger_level
# 
# a <- 8
# b <- 8
# mylevel <- c(seq(zzlim[1],Trigger_level,length.out=a),seq(Trigger_level,zzlim[2],length.out=b)[-1])
# mycolors <- c(colorRampPalette(c("red","white"))(a)[-a],colorRampPalette(c("white","blue"))(b)[-1]) #if bigger numbers are better
# 
# plot_title <- paste0(" Wet season streamflow (MCM) ")
# plot_tile_save <- paste0(out_dir_plot,"Monsoon_Streamflow_stress_response_surface.png")
# 
# #windows(8,7)
# png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
# filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
#                levels=mylevel,col=mycolors,
#                #plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
#                #          points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
#                #         points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
#                #        points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
#                #       axis(1);axis(2)},
#                main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
# text(.9,.3, paste0(round(Trigger_level, 1)))
# 
# dev.off()
# 
# #################################
# # Average Non Monsoon Streamflow
# #################################
# 
# clim_resp_surf_data <- Non_Monsoon_Streamflow_matrix
# Trigger <- Non_Monsoon_Streamflow_trigger #For Base_Design
# units <- 1 #For m3/s
# 
# #FOR CASES IN WHICH THE TRIGGER LEVEL IS WITHIN THE RANGE OF ZZLIM
# zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
# zzlim
# 
# Trigger_level = Trigger/units #Reliability indicates amount of KWh to be added to the
# Trigger_level
# 
# a <- 8
# b <- 8
# mylevel <- c(seq(zzlim[1],Trigger_level,length.out=a),seq(Trigger_level,zzlim[2],length.out=b)[-1])
# mycolors <- c(colorRampPalette(c("red","white"))(a)[-a],colorRampPalette(c("white","blue"))(b)[-1]) #if bigger numbers are better
# 
# plot_title <- paste0("Dry season streamflow (MCM) ")
# plot_tile_save <- paste0(out_dir_plot,"Non_Monsoon_Streamflow_stress_response_surface.png")
# 
# #windows(8,7)
# png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
# filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
#                levels=mylevel,col=mycolors,
#                #plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
#                #          points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
#                #         points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
#                #        points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
#                #       axis(1);axis(2)},
#                main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
# text(.9,.3, paste0(round(Trigger_level, 1)))
# 
# dev.off()
# 
# 
# ##########################
# # Average Annual Sediment Load
# ##########################
# 
# clim_resp_surf_data <- Sediment_tonnes_matrix
# Trigger <-  Sediment_tonnes_trigger #For Base_Design
# units <- 10^6 # Tonnes
# 
# #FOR CASES IN WHICH THE TRIGGER LEVEL IS WITHIN THE RANGE OF ZZLIM
# zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
# zzlim
# 
# Trigger_level = Trigger/units
# Trigger_level
# 
# a <- 8
# b <- 8
# mylevel <- c(seq(zzlim[1],Trigger_level,length.out=a),seq(Trigger_level,zzlim[2],length.out=b)[-1])
# #mycolors <- c(colorRampPalette(c("red","white"))(a)[-a],colorRampPalette(c("white","blue"))(b)[-1]) #if bigger numbers are better
# mycolors <- c(colorRampPalette(c("blue","white"))(a)[-a],colorRampPalette(c("white","red"))(b)[-1]) #if smaller numbers are better
# 
# plot_title <- paste0("Annual Sediment Load (Million Tons) ")
# plot_tile_save <- paste0(out_dir_plot,"Average_Annual_Sediment_load.png")
# 
# #windows(8,7)
# png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
# filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
#                levels=mylevel,col=mycolors, 
#                plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
#                  points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
#                  points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
#                  points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
#                  axis(1);axis(2)},
#                main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
# text(.9,.3, paste0(round(Trigger_level, 2)))
# 
# dev.off()
# 
# 
# 
# #Average Net Present Value
# ##########################
# 
# clim_resp_surf_data <- NPV_matrix
# Trigger <-  NPV_trigger #For Base_Design
# units <- 10^6 # Millions
# 
# # For cases of all above or below threshold
# zzlim <- c(min(clim_resp_surf_data/units),max(clim_resp_surf_data/units))
# Trigger_level <- 0/units
# a <- 16
# mylevel <- c(seq(zzlim[1],zzlim[2],length.out=a))
# #mycolors <- c(colorRampPalette(c("red","pink"))(a)[-a]) #if all worse than threshold and bigger numbers are better
# #mycolors <- c(colorRampPalette(c("pink","red"))(a)[-a]) #if all worse than threshold and smaller numbers are better
# #NPV
# mycolors <- c(colorRampPalette(c("light blue","blue"))(a)[-a]) #if all better than threshold and bigger numbers are better
# 
# #Trigger_level = Trigger/units
# #Trigger_level
# 
# plot_title <- paste0("Net Present Value (Million $) ")
# plot_tile_save <- paste0(out_dir_plot,"Average_Net_Present_Value_stress_response_surface.png")
# 
# #windows(8,7)
# png(plot_tile_save, width = 6, height = 4.65, units = "in", res = 400, pointsize = 12)
# filled.contour(x=P_change_num,y=T_change_num,z=clim_resp_surf_data/units,
#                levels=mylevel,col=mycolors, 
#                plot.axes={points(x=DP_RCP26,y=DT_RCP26,pch=16,col="green");
#                  points(x=DP_RCP45,y=DT_RCP45,pch=16,col="cyan");
#                  points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow");
#                  points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta");
#                  axis(1);axis(2)},
#                main=plot_title,xlab="Precipitation (Fraction of Historic/Baseline)",ylab="Change in Temperature (C)")
# text(.9,.3, paste0(round(NPV_all[29,3]/10^6,0)), cex = 1)
# dev.off()
# 
# 
# 
# # Future precipitation and temperature in the basin.
# plot_title <- paste0("CMIP5 Future Projections: 2020-2050")
# plot_tile_save <- paste0(out_dir_plot,"Centred in 2036.png")
# 
# 
# png(plot_tile_save, width = 5, height = 4.65, units = "in", res = 400, pointsize = 12)
#                plot(x= DP_RCP26,y=DT_RCP26,pch=16,col="green",
#                     main=plot_title,xlab="Precipitation (Fraction of historical)",ylab="Increase in Temperature (C)", 
#                     xlim=c(.6,1.4), ylim=c(0,6), panel.first=grid())
#                 
#                 points(x=DP_RCP60,y=DT_RCP60,pch=16,col="yellow")
#                 points(x=DP_RCP85,y=DT_RCP85,pch=16,col="magenta")
#                 points(x =1, y = 0, pch = 15, col="black")
#                 #text(1,.3, paste0("Historical Observation"), cex = 1)
# 
# dev.off()
