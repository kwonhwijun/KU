from sklearn.preprocessing import StandardScaler
from sklearn.ensemble import RandomForestClassifier
import pickle as pkl

def inference(trans, model, dat):
    
    ## x variables preprocessing 
    x_cols = ['V'+str(i) for i in range(1,28)]
    test_x = trans.transform(dat[x_cols])

    ## make prediction for testset
    pred = model.predict(test_x)
    return pred, x_cols
