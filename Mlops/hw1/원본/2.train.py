import sqlite3
import pandas as pd
import numpy as np
import pickle as pkl


db_name = "steel.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute("SELECT * FROM train")
cols = [col[0] for col in c.description]
dat = pd.DataFrame(data=c.fetchall(),columns=cols)
conn.close()

