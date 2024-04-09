import sqlite3


## DROP TABLE

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("DROP TABLE test_table")
c.commit()



c.execute("SELECT rowid, * FROM test_table")

items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()
