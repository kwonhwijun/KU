import sqlite3

## CREATE TABLE ##

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute("""CREATE TABLE test_table(
        first_name text, 
        last_name text,
        email text
        )""")

# Datatypes:
# NULL
# INTEGER
# REAL
# TEXT
# BLOB

## commit our command
conn.commit()

## close our connection
conn.close()