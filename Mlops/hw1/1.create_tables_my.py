import database
from database import change_dir, data2sql, create_table, print_table
import os

change_dir()
db_name = 'steel.db'
train_path = "data/train.csv"
test_path = "data/test.csv"

# create train/test table into DB
data2sql(db_path = db_name , data_path = train_path, table_name = "train")
data2sql(db_path = db_name, data_path = test_path, table_name = "test")

# create predict table
col_name ={"predict" : "TEXT" }
create_table(db_path = db_name, table_name = "predict", col_dict = col_name)

# print table
print_table(db_path = db_name, table_name='train')
print_table(db_path = db_name, table_name='test')
print_table(db_path = db_name, table_name='predict')