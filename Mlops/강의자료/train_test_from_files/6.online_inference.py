import pandas as pd
import numpy as np
import pickle as pkl
from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier

## load preprocessing and model
trans = pkl.load(open("trans.pkl","rb"))
model = pkl.load(open("model.pkl","rb"))

## define function to get new data
def select_row(id):
    a=id-1
    dat = pd.read_csv("test.csv")
    row = dat[a:id]
    return row

## preprocessing for new data
new_data = select_row(1)
x_cols = ['V'+str(i) for i in range(1,28)]
x = trans.transform(new_data[x_cols])

## prediction for new data
pred = model.predict(x)
pred

