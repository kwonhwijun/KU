def db2excel(engine, table_name, file_name):     
    import pandas as pd
    df = pd.read_sql(f'SELECT * FROM {table_name}', con=engine)
    df.to_excel(f'{file_name}.xlsx', index=False)


# db2excel(engine_06_10, "apt_trades", "apt_06_10")

