import sqlite3
import pandas as pd
import numpy as np

db_name = "/data/steel.db"
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
conn.close()



import call_func as cf
ind = str(np.random.randint(0,583, size=1)[0])
dat = cf.call_x_value(ind)


import inference as infer
infer.inference(dat)[0]