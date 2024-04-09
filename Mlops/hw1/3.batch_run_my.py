import sqlite3
import pandas as pd
import numpy as np
import inference as inf
import database as db

db.change_dir()

## load test dataset
dat = db.sql2df(db_path="steel.db", table_name = 'test')

## make prediction for test set
print("\n\n make prediction...")
pred = inf.inference(dat)
dat["pred"] = pred


## insert prediction into predict table 
db.df2sql(df_name = dat, db_path = "steel.db", table_name = "predict") # Insert 하는 방식으로 수정해야 함

## print predict table
db.print_table(db_path="steel.db",table_name = "predict", nrow = 10)

