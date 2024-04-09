import sqlite3

## INSERT MULTIPLE RECORDS INTO TABLE

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

many_customers = [
                    ('Wes', 'Brown', 'wes@brown.com'),
                    ('Steph', 'Kuewa', 'steph@kuewa.com'),
                    ('Dan', 'Pas', 'dan@pas.com')
                ]

c.executemany("INSERT INTO test_table VALUES (?,?,?)", many_customers)


print("Command executed successfully...")
 
## commit our command
conn.commit()

## close our connection
conn.close()