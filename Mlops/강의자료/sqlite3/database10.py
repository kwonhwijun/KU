import sqlite3

## AND / OR

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("SELECT rowid, * FROM test_table WHERE last_name LIKE 'Br%' ")
# c.execute("SELECT rowid, * FROM test_table WHERE last_name LIKE 'Br%' AND rowid = 3 ")
# c.execute("SELECT rowid, * FROM test_table WHERE last_name LIKE 'Br%' OR rowid = 3 ")

items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()