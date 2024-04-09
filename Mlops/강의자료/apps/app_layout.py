import dash
from dash import html, dcc, dash_table
from dash.dependencies import Input, Output

import pandas as pd
import numpy as np

import layout_components as lc



def app_layout(items):
    
    ## parameter parsing
    button_id = items[0]
    dropdown_id = items[1]
    graph_id = items[2]
    df = items[3]
    
    menu = df.columns.tolist()
    
    ## layout
    layout = html.Div(children = [
        
        html.Div(children=[ 
            
            lc.dropdown_menu(idname=dropdown_id, menu=menu),
            lc.showgraph(idname=graph_id),
            lc.button(idname=button_id, text='Click'),
            
        ],style={'display':'flex','flex-direction':'column'}),
        
        
        html.Div(children=[
            
            lc.showtable(idname='table1', dataframe=df, max_rows=20)
            
        ],style={'display':'flex','flex-direction':'column'}),        
        
        
    ]
    ,style={ 'display':'flex','flex-direction':'row'})
    
    return layout
