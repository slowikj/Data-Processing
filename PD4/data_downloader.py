# -*- coding: utf-8 -*-
"""
Created on Sun May 21 23:10:12 2017

@author: sebassud
"""

import requests
import json
import sqlite3
import time

conn1 = sqlite3.connect("autobusyPoniedzialek")
conn1.execute("""
CREATE Table data(
    id INTEGER PRIMARY KEY,
    response TEXT
)
""");

conn2 = sqlite3.connect("tramwajePoniedzialek")
conn2.execute("""
CREATE Table data(
    id INTEGER PRIMARY KEY,
    response TEXT
)
""");

conn3 = sqlite3.connect("tramwaje2Poniedzialek")
conn3.execute("""
CREATE Table data(
    id INTEGER PRIMARY KEY,
    response TEXT
)
""");
#conn.execute("INSERT INTO data(id, response) VALUES (?,?)", (1,"costam"))
#conn.execute("INSERT INTO data(id, response) VALUES (?,?)", (2,"costam2"))
#for row in conn.execute("SELECT * FROM data"):
#    print(row[0])
count = 1
while (True):
    # type=1 autobusy type=2 tramwaje
    try:
        r = requests.get('https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id= f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey=a94779ec-47e0-4545-baa1-62206517940e&type=1')
        conn1.execute("INSERT INTO data(id, response) VALUES (?,?)", (count, str(r.text)))
        r = requests.get('https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id= f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey=a94779ec-47e0-4545-baa1-62206517940e&type=2')
        conn2.execute("INSERT INTO data(id, response) VALUES (?,?)", (count, str(r.text)))
        r = requests.get('https://api.um.warszawa.pl/api/action/wsstore_get/?id=c7238cfe-8b1f-4c38-bb4a-de386db7e776&apikey=a94779ec-47e0-4545-baa1-62206517940e');
        conn3.execute("INSERT INTO data(id, response) VALUES (?,?)", (count, str(r.text)))
        print(count)
        count += 1
        time.sleep(30) # co ile sekund
        conn1.commit();
        conn2.commit();
    except:
        print("error")
        
