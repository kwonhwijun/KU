import sqlite3

## FORMAT RESULTS

db_name = "test.db"
conn = sqlite3.connect(db_name)
c = conn.cursor()

c.execute("SELECT * FROM test_table")

items = c.fetchall()

print("NAME " + "\t\tEMAIL")
print("-----" + "\t\t--------")
for item in items:
    print(item[0] + " " + item[1] + "\t" + item[2])

## commit our command
conn.commit()

## close our connection
conn.close()