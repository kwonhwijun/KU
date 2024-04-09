import sqlite3

## INSERT ONE RECORD INTO TABLE

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()
c.execute(""" INSERT INTO test_table VALUES ('John','Elder','john@korea.ac.kr') """)
# c.execute(""" INSERT INTO test_table VALUES ('Tim','Smith','tim@korea.ac.kr') """)
# c.execute(""" INSERT INTO test_table VALUES ('Mary','Brown','mary@korea.ac.kr') """)


print("Command executed sucessfully...")

## commit our command
conn.commit()

## close our connection
conn.close()