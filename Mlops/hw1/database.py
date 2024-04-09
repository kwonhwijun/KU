import sqlite3
import pandas as pd
import os



def change_dir() :
    # 
    current_file_path = os.path.abspath(__file__) # 현재 파일의 절대 경로
    current_dir_path = os.path.dirname(current_file_path) # 파일이 위치한 디렉토리의 경로
    os.chdir(current_dir_path)

#def connect2db(db_path):
#    conn = sqlite3.connect(db_path)
#    return conn.cursor

def data2sql(db_path, data_path, table_name) :
    conn = sqlite3.connect(db_path)
    c = conn.cursor()
    df = pd.read_csv(data_path)
    df.to_sql(table_name, conn, if_exists= "replace", index = False)
    print(f"Data {data_path} has been saved to Table {table_name} ")
    conn.close()


def df2sql(df_name, db_path, table_name):
    conn = sqlite3.connect(db_path)
    c = conn.cursor()
    df_name.to_sql(table_name, conn, if_exists = 'replace', index = False)
    conn.commit()
    conn.close()

def create_table(db_path, table_name, col_dict):
    conn = sqlite3.connect(db_path)
    c = conn.cursor()
    col_sql = ", ".join([f"{name} {dtype}" for name, dtype in col_dict.items()])
    c.execute(f"DROP TABLE IF EXISTS {table_name}")
    c.execute(f"""CREATE TABLE {table_name}({col_sql})""")
    conn.commit()
    print(f"Table {table_name} is created")
    conn.close()


def print_table(db_path, table_name, nrow = 10) :
    conn = sqlite3.connect(db_path)
    c = conn.cursor()
    c.execute(f"SELECT * FROM {table_name} limit {nrow}")
    items = c.fetchall()
    print(f"\n\n {table_name} table...")
    for item in items :
        print(item)
    conn.close()


def sql2df(db_path, table_name):
    conn = sqlite3.connect(db_path)
    c = conn.cursor()
    c.execute(f"SELECT * FROM {table_name}")
    cols = [col[0] for col in c.description]
    data = pd.DataFrame(data = c.fetchall(), columns = cols)
    print(f"Table {table_name} is loaded")
    conn.close()

    return data