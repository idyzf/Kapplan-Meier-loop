library(WriteXLS)
library(survival)
library(survminer)
library(dplyr)
library(openxlsx)
library(ggplot2)


rm(list=ls())

rawdata <- read.csv("code count none background none sample_content low.cv.geo.mean .csv", sep=";")
rownames(rawdata)<- rawdata$X
rawdata<- rawdata[,-1:-4]
rawdata<- as.data.frame(t(rawdata))
rawdata<- rawdata[,-1:-12]
clinical<- read.csv("clinical_data_OS_replicatas.csv", sep=";")
clinical<- subset(clinical, clinical$REDCAP.ID %in% rownames(rawdata))
status <-as.data.frame(clinical$Vital.Status)
rownames(status) <- clinical$REDCAP.ID
time<- as.data.frame(clinical$OS)
rownames(time)<- clinical$REDCAP.ID
features<- merge(status, time, by=0)
rownames(features) <- features$Row.names
#med<- median(data.gene$`rawdata[, i]`)

i=1
for (i in 1:ncol(rawdata)){
  gene<- as.data.frame(rawdata[i])
  rownames(gene)<- rownames(rawdata)
  data.gene<- merge(gene, features, by=0)
  data.gene$status <- factor(data.gene$"clinical$Vital.Status", 
                               levels = c("Alive", "Deceased"), 
                               labels = c("1", "2"))
  med<- median(data.gene[,2])
  data.gene$median<- data.gene[,2]
  data.gene$median[which(data.gene$median > med)] <- "high"
  data.gene$median[which(data.gene$median != "high")] <- "low"
  #hist(data.gene[,2])
  #hist(data.gene$`clinical$OS`)
  data.gene$status <-as.numeric(as.character(data.gene$status))
  data.gene$medianbi <- factor(data.gene$median)
  surv_object <- Surv(time = data.gene$`clinical$OS`, 
                      event = data.gene$status)
  surv_object 
  data_esp <- data.gene$`clinical$OS`
  fit1 <- survfit(surv_object ~ median, data = data.gene)
  summary(fit1)
  
  
  file_name = paste("kapplan-meier", colnames(rawdata[i]), ".tiff", sep="")
  tiff(file_name)
  print(ggsurvplot (fit1 , pval = TRUE, title = colnames(rawdata[i])))
  dev.off()
  
  data.gene <- NULL
  print(colnames(rawdata[i]))
  i=i+1
}
