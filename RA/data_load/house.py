import requests
import api

from sqlalchemy import create_engine
engine = create_engine("sqlite:///house.db")


import api

loc_all = api.loc_code('all') #477개
ym_06_24 = api.create_ym_list(2006,2024) #48

api.save_all(engine, "comm", "commercial_raw", loc_all, ym_06_24)
# apt_trade_raw
# commercial_raw

api.save_all()