import pandas as pd
import numpy as np
from sklearn.ensemble import RandomForestClassifier
import pickle as pkl

## load preprocessing and model
trans = pkl.load(open("trans.pkl","rb"))
model = pkl.load(open("model.pkl","rb"))

## load new dataset
new_data = pd.read_csv("test.csv")
x_cols = ['V'+str(i) for i in range(1,28)]

## prediction for new dataset
x = trans.transform(new_data[x_cols])
pred = model.predict(x)

