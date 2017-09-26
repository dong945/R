# R
getTWSE.R為抓取證交所的股票資料函數.

修改記錄:
為了符合quantmod套件的chartSeries函數輸入，資料集除為OHLC格式外，還要有一個Column叫"Volume"，才能正確的畫出圖形。
