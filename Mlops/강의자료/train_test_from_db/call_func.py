import sqlite3
import pandas as pd

def call_x_value(ind):
    db_name = "/data/steel.db"
    conn = sqlite3.connect(db_name)
    c = conn.cursor()
    c.execute(""" SELECT * FROM test WHERE rowid={}""".format(ind))
    cols = [column[0] for column in c.description]
    dat = pd.DataFrame.from_records(data=c.fetchall(), columns=cols)
    conn.close()
    return dat
