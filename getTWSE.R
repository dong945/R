# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: TUNG-SHENG, CHEN
# DATE:   2017/09/26
# Package: jsonlite, xts
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# install.packages(c("jsonlite", "xts"))
library(jsonlite)
library(xts)
# =====================================================================================================
# Parameter:
#   stockNo: String, Stock code of TWSE, default: 2330 (TSMC)
#   from:    Vector, c(year, month), default: now
#   to:      Vector, c(year, month), default: now
# Examples:
#   ## return '2330', this month 
#   getTWSE()
#   
#   ## return '2330', from 2016/01 ~ now
#   getTWSE("2330", 2016)
#   
#   ## return '2330', from 2015/01 ~ 2016/12
#   getTWSE("2330", 2015, 2016)
#
#   ## return '2330', from 2015/06 ~ 2017/03
#   getTWSE("2330", c(2105, 6), c(2017, 3))
# =====================================================================================================
getTWSE <- function(stockNo = "2330", 
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
    response <- "json"
    url <- "http://www.tse.com.tw/exchangeReport/STOCK_DAY?"
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
        ifelse (endYM[1] == nowYear, endYM <- c(endYM, nowMonth), endYM <- c(endYM, 12))
    }
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
    msg <- paste0("Stock Code=", stockNo,
                  ", from(", startYM[1], ", ", startYM[2], ")",
                  " to(", endYM[1], ", ", endYM[2], ")")
    # =======================================================================================
    if (startYM[1] == endYM[1]) {
        queryDate <- paste0(startYM[1], MM[startYM[2]:endYM[2]], "01")
    } else if ((endYM[1] - startYM[1]) == 1) {
        queryDate <- c(paste0(startYM[1], MM[startYM[2]:12], "01"), 
                       paste0(endYM[1], MM[1:endYM[2]], "01"))
    } else {
        tmpY <- c((startYM[1]+1):(endYM[1]-1))
        queryDate <- paste0(startYM[1], MM[startYM[2]:12], "01")
        for (tY in tmpY){
            queryDate <- c(QueryDate, paste0(tY, MM, "01"))
        }
        queryDate <- c(QueryDate, paste0(endYM[1], MM[1:endYM[2]], "01"))
    }
    # ========================================================================================
    # to TWSE get History Stock
    # ========================================================================================
    for (qyDate in queryDate){
        ttime <- as.character(as.integer(as.POSIXct(Sys.time()))*100)
        twseUrl <- paste0(url, "response=", response, "&date=", qyDate, "&stockNo=", stockNo,
                          "&_=", ttime)
        jsonData <- fromJSON(twseUrl, flatten = TRUE)
        if (jsonData$stat == "OK"){
            tmpStock <- data.frame(jsonData$data[, 1], 
                                       jsonData$data[, 4:7], 
                                       jsonData$data[, 2:3],
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
        colnames(historyStock) <- c("Date", "Open", "High", "Low", "Close", "Volume", "Value")
        historyStock$Date <- sapply(historyStock$Date, CNV_DATE)
        historyStock$Open <- as.numeric(gsub(',', replacement = '', historyStock$Open))
        historyStock$High <- as.numeric(gsub(',', replacement = '', historyStock$High))
        historyStock$Low <- as.numeric(gsub(',', replacement = '', historyStock$Low))
        historyStock$Close <- as.numeric(gsub(',', replacement = '', historyStock$Close))
        historyStock$Volume <- as.numeric(gsub(',', replacement = '', historyStock$Volume))
        historyStock$Value <- as.numeric(gsub(',', replacement = '', historyStock$Value))
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
tw3008 <- getTWSE("3008", c(2017, 1), c(2017, 9))

chartSeries(tw3008, theme = chartTheme("white", up.col = "red", dn.col = "green"), 
            name = "LARGAN 3008", show.grid = TRUE) 
