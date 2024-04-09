import sqlite3
import pandas as pd
import numpy as np
import inference as inf
import sys
import database as db

db.change_dir()

rid = sys.argv[1]
print(rid)
print("{}-th row need to be predicted".format(rid))

db_name = "steel.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute("SELECT * FROM test where rowid = {}".format(rid))
cols = [col[0] for col in c.description]
dat = pd.DataFrame(data=c.fetchall(),columns=cols)
conn.close()

pred = inf.inference(dat)

print(pred)
