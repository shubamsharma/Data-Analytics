setwd("D:/11_github/Data-Analytics/11_knorex_solution")
untar("raw-bid-win.tar")

# library used
library(jsonlite)
library("dplyr")


do_processing <- function(file){
  json_data <- stream_in(gzfile(file))
  json_data[which(is.na(json_data))] <- "NA"
  
  output <- matrix(ncol=10, nrow=nrow(json_data))
  
  for(i in c(1:nrow(json_data))) {
    vector <- c(		  
      ifelse(is.null(json_data[i,]$auctionId),"NA",json_data[i,]$auctionId),
      ifelse(is.null(json_data[i,]$biddingMainAccount),"NA",json_data[i,]$biddingMainAccount),
      ifelse(is.null(json_data[i,]$bidResponseCreativeName),"NA",json_data[i,]$bidResponseCreativeName),
      ifelse(is.null(json_data[i,]$biddingSubAccount),"NA",json_data[i,]$biddingSubAccount),
      ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$userAgent),"NA",fromJSON(json_data[i,]$bidRequestString)$userAgent),
      ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$url),"NA",fromJSON(json_data[i,]$bidRequestString)$url),
      ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$device$geo$country),"NA",fromJSON(json_data[i,]$bidRequestString)$device$geo$country),
      ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$exchange),"NA",fromJSON(json_data[i,]$bidRequestString)$exchange),
      ifelse(is.null(json_data[i,]$winPrice),"NA",json_data[i,]$winPrice),
      ifelse(is.null(json_data[i,]$timestamp),"NA",json_data[i,]$timestamp)
    )
    
    print(paste(vector,collapse = " ")) 
    print("*****************")
    print(length(vector))
    print("*****************")
    output[i,] <- vector
    
  }
  
  return(data.frame(output,stringsAsFactors = FALSE))
  
}

#Declare data frame
header <- c("auctionId","campaignId","creativeId","adgroupId","userAgent","site","geo","exchange","price","time")
final_data_frame <- data.frame(matrix(ncol = length(header), nrow = 0))

# getting the file list in the folder
for(file in (list.files(pattern = "*.gz", recursive = TRUE))){
  paste("File Name is", file, sep = " ")
  final_data_frame <- rbind(final_data_frame,do_processing(file))
}

colnames(final_data_frame) <- header

str(final_data_frame)

final_data_frame$price = as.numeric(gsub("USD/1M", "", final_data_frame$price))
final_data_frame = rbind(final_data_frame,final_data_frame[1:20,])
final <- final_data_frame %>%
  select(auctionId,creativeId,adgroupId,geo,time,price) %>%
  group_by(auctionId,creativeId,adgroupId,geo,time) %>%
  summarise(sum_price = sum(price), min_price = min(price), max_price = max(price),total_enteries = n())

final


file <- "D:/11_github/Data-Analytics/11_knorex_solution/raw-bid-win/2017/01/11/00/00/0DuPDhwm1IUapAQhN48h.gz"




json_data <- stream_in(gzfile(file))
json_data[which(is.na(json_data))] <- "NA"

output <- matrix(ncol=10, nrow=nrow(json_data))

head(json_data)

for(i in c(1:nrow(json_data))) {
  vector <- c(		  
    ifelse(is.null(json_data[i,]$auctionId),"NA",json_data[i,]$auctionId),
    ifelse(is.null(json_data[i,]$biddingMainAccount),"NA",json_data[i,]$biddingMainAccount),
    ifelse(is.null(json_data[i,]$bidResponseCreativeName),"NA",json_data[i,]$bidResponseCreativeName),
    ifelse(is.null(json_data[i,]$biddingSubAccount),"NA",json_data[i,]$biddingSubAccount),
    ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$userAgent),"NA",fromJSON(json_data[i,]$bidRequestString)$userAgent),
    ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$url),"NA",fromJSON(json_data[i,]$bidRequestString)$url),
    ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$device$geo$country),"NA",fromJSON(json_data[i,]$bidRequestString)$device$geo$country),
    ifelse(is.null(fromJSON(json_data[i,]$bidRequestString)$exchange),"NA",fromJSON(json_data[i,]$bidRequestString)$exchange),
    ifelse(is.null(json_data[i,]$winPrice),"NA",json_data[i,]$winPrice),
    ifelse(is.null(json_data[i,]$timestamp),"NA",json_data[i,]$timestamp)
  )
  
  print(paste(vector,collapse = " ")) 
  print("*****************")
  print(length(vector))
  print("*****************")
  output[i,] <- vector
  
}

output

data.frame(output,stringsAsFactors = FALSE)
