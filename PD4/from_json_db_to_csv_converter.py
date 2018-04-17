# -*- coding: utf-8 -*-
"""
Created on Wed May 31 13:20:18 2017

@author: slowik
"""

import json
import csv
import sqlite3
import sys

def from_json_to_iterable(column_names, json_object):
    try:
        return [json_object[column_name] for column_name in column_names]
    except:
        return None
    
def from_json_array_to_iterable(column_names, json_array):
    return filter(lambda x: x != None,
                  (from_json_to_iterable(column_names, json_object)
                      for json_object in json_array))
                    
def convert_json_db_to_csv_file(db_name, csv_file_name, column_names):
    with sqlite3.connect(db_name) as db_con, open(csv_file_name, 'w', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(column_names)
        
        for db_row in db_con.execute("SELECT * from data"):
            csv_writer.writerows(from_json_array_to_iterable(column_names, json.loads(db_row[1])["result"]))


if len(sys.argv) != 3:
    print("arguments:")
    print("db path")
    print("target csv file path")
else:
    convert_json_db_to_csv_file(sys.argv[1], sys.argv[2], ["Lat", "Lon", "Time", "Lines", "Brigade"])