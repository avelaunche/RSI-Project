import pandas as pd

spy = pd.read_csv("spy_daily_2020.csv")

ticker_list = list(set(spy["symbol"])) 

dict = {}

amd_rows = spy[spy["symbol"] == "AMD"]

for x in ticker_list[0:5]:
    print(x)
    