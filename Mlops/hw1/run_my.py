import sqlite3
import pandas as pd
import numpy as np
import database as db

db.change_dir()


db_name = "steel.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute(""" SELECT name FROM sqlite_master WHERE type='table' """)
print(c.fetchall())

c.execute(""" SELECT count(*) FROM train """)
print(c.fetchall())
c.execute(""" SELECT count(*) FROM test """)
print(c.fetchall())
c.execute(""" SELECT count(*) FROM predict """)
print(c.fetchall())

# 100개의 온라인 추론
# 1. test 테이블에서 랜덤하게 데이터 100개 추출
#idx = 1
#c.execute(""" SELECT * FROM test ORDER BY RANDOM() LIMIT 2""")

import call_func as cf
import inference as infer
ind = np.random.randint(0,583, size=100) # 100개의 랜덤한 행 인덱스 출력
for i in ind :
    dat = cf.call_x_value(str(i)) # test table에서 i번째 행의 데이터 불러오기
    pred = infer.inference(dat)[0]
    dat["pred"] = pred
    dat.to_sql("predict", conn, if_exists= "append", index = False)

print("After Appending 100 online inference")
c.execute(""" SELECT count(*) FROM predict """)
print(c.fetchall())
db.print_table("steel.db", "predict", nrow = 700)
#print_table(db_path = db_name, table_name='predict')