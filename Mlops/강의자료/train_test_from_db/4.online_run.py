import sqlite3
import pandas as pd
import numpy as np
import inference as inf
import sys

rid = sys.argv[1]
print("{}-th row need to be predicted".format(rid))

db_name = "/data/steel.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute("SELECT * FROM test where rowid = {}".format(rid))
cols = [col[0] for col in c.description]
dat = pd.DataFrame(data=c.fetchall(),columns=cols)
conn.close()

pred = inf.inference(dat)

print(pred)
