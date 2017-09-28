# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TUNG-SHENG, CHEN
# DATE:   2017/09/27
# Package: jsonlite, xts
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# install.packages(c("jsonlite", "xts"))
library(jsonlite)
library(xts)
# =====================================================================================================
# Parameter:
#   stockNo: String, Stock code of TWSE, default: 6510 (CHPT)
#   from:    Vector, c(year, month), default: now
#   to:      Vector, c(year, month), default: now
# Examples:
#   ## return '6510', this month 
#   getGreTai()
#   
#   ## return '6510', from 2016/01 ~ now
#   getGreTai("6510", 2016)
#   
#   ## return '6510', from 2015/01 ~ 2016/12
#   getGreTai("6510", 2015, 2016)
#
#   ## return '6510', from 2015/06 ~ 2017/03
#   getGreTai("6510", c(2105, 6), c(2017, 3))
# =====================================================================================================
getGreTai <- function(stkno = "6510", 
                    from = c(as.integer(format(Sys.Date(),"%Y")), as.integer(format(Sys.Date(),"%m"))),
                    to = c(as.integer(format(Sys.Date(),"%Y")), as.integer(format(Sys.Date(),"%m")))) {
    # =================================================================================================
    # Inside Function: Change Date to yyyy-mm-dd
    # =================================================================================================
    CNV_DATE <- function(x){
        TMP <- strsplit(x, split = "/")
        paste(as.integer(TMP[[1]][1])+1911, TMP[[1]][2], TMP[[1]][3], sep = "-")
    }
    # =================================================================================================
    url <- "http://www.tpex.org.tw/web/stock/aftertrading/daily_trading_info/st43_result.php?l=zh-tw"
    # =================================================================================================
    # Using this function, you must library(jsonlite)、library(xts)
    # =================================================================================================
    packages <- gsub("package:", replacement = "", search())
    if (!("jsonlite" %in% packages)){
        print("Error: The 'jsonlite' package has not been loaded.")
        return(NULL)
    } else if (!("xts" %in% packages)){
        print("Error: The 'xts' package has not been loaded.")
        return(NULL)
    }
    # =================================================================================================
    # parameter check & parse
    # =================================================================================================
    if (from[1] > to[1]){
        print("The starting year is greater than the deadline.")
        return(NULL)
    } else if (from[1] < 1994 | to[1] < 1994){
        print("This information has been available since January 1994.")
        return(NULL)
    }
    nowYear <- as.integer(format(Sys.Date(),"%Y")) 
    nowMonth <- as.integer(format(Sys.Date(),"%m"))
    MM <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
    queryDate <- NULL
    historyStock <- NULL
    startYM <- from
    endYM <- to
    if (length(startYM) == 1) startYM <- c(startYM, 1)
    if (length(endYM) == 1){
        ifelse (endYM[1] == nowYear, 
                endYM <- c(endYM, nowMonth), endYM <- c(endYM, 12))
    }
    startYM[1] <- startYM[1]-1911
    endYM[1] <- endYM[1]-1911
    # Check
    if (startYM[2] < 1 | startYM[2] > 12 | endYM[2] < 1 | endYM[2] > 12){
        print("Month must be between 1 ~ 12.")
        return(NULL) 
    } else if (startYM[1] > endYM[1]) {
      print("Start year is greater than the End year.")
      return(NULL) 
    } else if ((startYM[1] == endYM[1]) & (startYM[2] > endYM[2])) {
      print("Start month is greater than the End month.")
      return(NULL) 
    }
    msg <- paste0("Stock Code=", stkno,
                  ", from(", (startYM[1]+1911), ", ", startYM[2], ")",
                  " to(", (endYM[1]+1911), ", ", endYM[2], ")")
    # =======================================================================================
    if (startYM[1] == endYM[1]) {
        queryDate <- paste(startYM[1], MM[startYM[2]:endYM[2]], sep = "/")
    } else if ((endYM[1] - startYM[1]) == 1) {
        queryDate <- c(paste(startYM[1], MM[startYM[2]:12], sep = "/"), 
                       paste(endYM[1], MM[1:endYM[2]], sep = "/"))
    } else {
        tmpY <- c((startYM[1]+1):(endYM[1]-1))
        queryDate <- paste(startYM[1], MM[startYM[2]:12], sep = "/")
        for (tY in tmpY){
            queryDate <- c(queryDate, paste(tY, MM, sep = "/"))
        }
        queryDate <- c(queryDate, paste(endYM[1], MM[1:endYM[2]], sep = "/"))
    }
    # ========================================================================================
    # to GreTai get History Stock
    # ========================================================================================
    for (qyDate in queryDate){
        ttime <- as.character(as.integer(as.POSIXct(Sys.time()))*100)
        gretaiUrl <- paste0(url, "&d=", qyDate, "&stkno=", stkno, "&_=", ttime)
        jsonData <- fromJSON(gretaiUrl, flatten = TRUE)
        if (length(jsonData$aaData) != 0){
            tmpStock <- data.frame(jsonData$aaData[, 1], 
                                   jsonData$aaData[, 4:7], 
                                   jsonData$aaData[, 3],
                                   jsonData$aaData[, 2],
                                   stringsAsFactors = FALSE) 
            historyStock <- rbind(historyStock, tmpStock)
        }
        
    }
    # ========================================================================================
    # Convert Data Format: 
    # If the string has a comma, it can not be converted to a value.
    # gsub(',', replacement = '', x) --> remove comma
    #
    # Ex. as.numeric("196,857,432") --> Warning message: NAs introduced by coercion 
    #     as.numeric("196857432") --> 196857432 
    # ========================================================================================
    if (!is.null(historyStock)){
        # colnames(historyStock) <- c("Date", "Open", "High", "Low", "Close", "Number", "Value")
        colnames(historyStock) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Number")
        historyStock$Date <- sapply(historyStock$Date, CNV_DATE)
        historyStock$Open <- as.numeric(gsub(',', replacement = '', historyStock$Open))
        historyStock$High <- as.numeric(gsub(',', replacement = '', historyStock$High))
        historyStock$Low <- as.numeric(gsub(',', replacement = '', historyStock$Low))
        historyStock$Close <- as.numeric(gsub(',', replacement = '', historyStock$Close))
        historyStock$Volume <- as.numeric(gsub(',', replacement = '', historyStock$Volume))*1000
        historyStock$Number <- as.numeric(gsub(',', replacement = '', historyStock$Number))*1000
        # data frame to xts
        historyStock <- xts(historyStock[, -1], order.by = as.Date(historyStock[, 1]))
        
        print(paste0(msg, ", Stock information, download complete. rows=", nrow(historyStock)))
    } else{
        print(paste0(msg, ", No Data found."))
    }
    # ========================================================================================
    return(historyStock)
}

# ============================================================================================
# TEST
# ============================================================================================
library(quantmod)
tw6510 <- getGreTai("6510", c(2017, 1), c(2017, 9))
chartSeries(tw6510, theme = chartTheme("white", up.col = "red", dn.col = "green"), 
            name = "CHPT 6510", show.grid = TRUE) 
