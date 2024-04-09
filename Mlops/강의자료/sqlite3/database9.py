import sqlite3


## UPDATE RESULTS ORDER BY

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

# c.execute("SELECT rowid, * FROM test_table ORDER BY rowid DESC")
c.execute("SELECT rowid, * FROM test_table ORDER BY last_name ASC")

items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()