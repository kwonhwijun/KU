
import json
import xmltodict
import sqlite3

import os
from dotenv import load_dotenv


load_dotenv("key.env")

house_decode= os.getenv('api_key_house_decode')
house_encode = os.getenv('api_key_house_encode')

print("API Key 1:", house_decode)
print("API Key 2:", house_encode)




class APIHandler :
    def __init__(self, api_key):
        self.api_key = api_key
        self.db_path = db_path

    def fetch_data(self, url, lawd_cd, deal_ymd):
        params = {
            'serviceKey' : self.api_key,
            'LAWD_CD' : lawd_cd,
            'DEAL_YMD' : deal_ymd
        }
        requests.get(url,params= params)
