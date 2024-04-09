import sqlite3

## WHERE CLAUSE

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

# c.execute("SELECT rowid, * FROM test_table")
c.execute("SELECT * FROM test_table WHERE last_name = 'Elder'")
# c.execute("SELECT * FROM test_table WHERE last_name LIKE 'Br%'")
# c.execute("SELECT * FROM test_table WHERE email LIKE '%korea.ac.kr'")

items = c.fetchall()

for item in items:
    print(item)

## commit our command
conn.commit()

## close our connection
conn.close()