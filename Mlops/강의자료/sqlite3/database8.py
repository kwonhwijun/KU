import sqlite3

## DELETE RECORDS

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("DELETE FROM test_table WHERE rowid = 6")
conn.commit()


c.execute("SELECT rowid, * FROM test_table")
items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()