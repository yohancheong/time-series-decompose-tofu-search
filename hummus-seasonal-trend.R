
library(forecast)

filename <- './raw/tofuhummus.csv'

con  <- file(filename, open = "r")
linecount <- 0
stringdata <- ""
while (length(oneLine <- readLines(con, n = 1, warn = FALSE)) > 0) {
  linecount <- linecount + 1
  
  if (linecount < 3) {
    filename <- paste0(filename,oneLine)     
  }
  
  # get headers at line 5
  if (linecount == 5) rowheaders = strsplit(oneLine, ",")[[1]]
  
  # skip firt 5 lines
  if (linecount > 5) {
    # break when there is no more main data
    if (gsub(pattern=",", x=oneLine, replacement="") == "") break
    
    stringdata <- paste0(stringdata,oneLine,"\n")
  }
}
close(con)

newData <- read.table(textConnection(stringdata), sep=",", header=FALSE, stringsAsFactors = FALSE)
names(newData) <- rowheaders

newData$StartDate <- as.Date(sapply(strsplit(as.character(newData[,1]), " - "), `[`, 1))
newData$EndDate <- as.Date(sapply(strsplit(as.character(newData[,1]), " - "), `[`, 2))
newData$year <- sapply(strsplit(as.character(newData$StartDate), "-"), `[`, 1)
newData<- newData[c("StartDate", "EndDate", "tofu", "hummus", "year")]

data <- newData[newData$year < 2015,]

hummustimeseries<-ts(data$hummus, frequency=52, start=c(2004,1,4))
plot(decompose(hummustimeseries))
