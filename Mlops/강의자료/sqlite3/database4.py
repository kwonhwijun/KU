import sqlite3

## QUERY AND FETCHALL

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("SELECT * FROM test_table")

print(c.fetchone())
# print(c.fetchmany(3))
# print(c.fetchall())


# print("Command executed successfully...")
## commit our command
conn.commit()

## close our connection
conn.close()