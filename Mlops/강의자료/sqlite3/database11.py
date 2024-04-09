import sqlite3


## LIMITING RESULTS

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("SELECT rowid, * FROM test_table LIMIT 2")
# c.execute("SELECT rowid, * FROM test_table ORDER BY rowid DESC LIMIT 3 ")

items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()