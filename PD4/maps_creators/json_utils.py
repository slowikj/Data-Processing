# -*- coding: utf-8 -*-
"""
Created on Mon May 29 08:18:03 2017

@author: slowik
"""

import json
import requests

def get_json_array_from_db_row(db_row):
    return json.loads(db_row[1])["result"]
    

def get_json_array(request_url):
    return json.loads(str(requests.get(request_url).text))["result"]
