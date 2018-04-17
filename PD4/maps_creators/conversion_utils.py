# -*- coding: utf-8 -*-
"""
Created on Mon May 29 07:49:34 2017

@author: slowik
"""

import sqlite3
import pandas as pd
import json

def _create_circle(self, x, y, r, **kwargs):
    return self.create_oval(x-r, y-r, x+r, y+r, **kwargs)
    
class converterMap:
    x1 = 20.788000
    y1 = 52.160088
    x2 = 21.227348
    y2 = 52.330103
    
    def __init__(self, width, height):
        self.width = width
        self.height = height
    
    def isOnMap(self, lon, lat):
        if(self.x1 < lon < self.x2 and self.y1 < lat < self.y2):
            return True
        else:
            return False
    
    def getPosition(self, lon, lat):
        x = (lon - self.x1) / (self.x2 - self.x1) * self.width
        y = self.height - (lat - self.y1) / (self.y2 - self.y1) * self.height
        return (x,y)

# SLOW; not recommended
def get_dataframe_from_db(db_name, table_name, json_column_names,
                          start_req_ind, end_req_ind):
    with sqlite3.connect(db_name) as con:
        cur = con.cursor()
        cur.execute("SELECT * FROM " + table_name)
        all_rows = cur.fetchall();

        df = pd.DataFrame(columns=json_column_names)

        for (cnt, results_string) in all_rows:
            print(cnt)
            if cnt < start_req_ind or cnt % 2 == 0:
                continue

            if cnt > end_req_ind:
                break                
            
            results_json = json.loads(results_string)
            json_array = results_json["result"]

            for json_item in json_array:
                df.loc[len(df)] = json_item

    return df

