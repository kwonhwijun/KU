import dash
from dash import html, dcc, dash_table
from dash.dependencies import Input, Output

import pandas as pd
import numpy as np

import layout_components as lc
import datetime

import mylib as my

def app_layout(items):
    
    button_id = items[0]
    time_id = items[1]
    predict_id = items[2]
    graph_id = items[3]
    table_id = items[4]
    current_time_id = items[5]
    counter_id = items[6]
    
    ## layout
    layout = html.Div(children = [
                        
                ## 최근 호출시점+예측값
                html.Div(children=[
                    
                    html.Div(children=[
                        html.H4(children="현재시각"),
                        html.H1(id=current_time_id),
                    ], style={'flex':'1'}),                
                    
                    
                    html.Div(children=[
                        html.H4(children="최근예측시점"),
                        html.H1(id=time_id),
                    ], style={'flex':'1'}),

                    html.Div(children=[
                        html.H4(children="최근예측값"),
                        html.H1(id=predict_id),
                    ], style={'flex':'1'}),
                    
                    lc.button(idname=button_id, text="예측 (inference)"),

                ],style={'display':'flex','flex-direction':'row'}),

                
                ## 누적 yhat 그래프
                html.H4(children="예측값 Trned"),
                lc.showgraph(idname=graph_id),

                ## 누적 X 테이블 
                html.H4(children="입력값"),
                lc.showtable(idname=table_id),
                
                dcc.Interval(id=counter_id,
                             interval=2*1000, # in milliseconds
                             n_intervals=0
                            ),

    ])
    
    return layout

