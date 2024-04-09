import sqlite3

## UPDATE RECORDS

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("""UPDATE test_table SET first_name = 'Bob'
             WHERE last_name = 'Elder'
""")
# c.execute("""UPDATE test_table SET first_name = 'John'
#              WHERE rowid = 1
# """)
# c.execute("""UPDATE test_table SET first_name = 'Marty'
#              WHERE last_name = 'Brown'
# """)
# c.execute("""UPDATE test_table SET first_name = 'Mary'
#              WHERE last_name = 'Brown'
# """)
# c.execute("""UPDATE test_table SET first_name = 'Wes'
#              WHERE rowid = 4
# """)



conn.commit()

c.execute("SELECT rowid,* FROM test_table")

items = c.fetchall()

for item in items:
    print(item)

## close our connection
conn.close()