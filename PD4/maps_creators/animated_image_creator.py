# -*- coding: utf-8 -*-
"""
Created on Mon May 29 08:00:19 2017

@author: slowik
"""

import drawing_utils as du
import conversion_utils as cu
import json_utils as ju
import time
import sys
import sqlite3

def create_animated_image_from_db(bus_con, tram_con,
                                  background_image_path,
                                  target_file_name,
                                  image_width, image_height):
    (root, canvas, photo) = du.get_canvas_drawing_data(background_image_path,
                                            image_width, image_height, target_file_name)
    cm = cu.converterMap(image_width, image_height)

    bus_iter = iter(bus_con.cursor().execute("SELECT * FROM " + "data").fetchall())
    tram_iter = iter(tram_con.cursor().execute("SELECT * FROM " + "data").fetchall())
    
    while True:
        try:
            bus_row = next(bus_iter)
            tram_row = next(tram_iter)
            
            bus_json_array = ju.get_json_array_from_db_row(bus_row)
            tram_json_array = ju.get_json_array_from_db_row(tram_row)
            
            canvas.create_image(image_width / 2, image_height / 2, image=photo)
            du.set_points_on_canvas(cm, bus_json_array, canvas, root, "blue")
            du.set_points_on_canvas(cm, tram_json_array, canvas, root, "red")
            
            (hours, minutes, seconds) = get_time(int(bus_row[0]) * 30)
            txt = "{:02}:{:02}:{:02}".format(hours, minutes, seconds);            
            canvas.create_text(70,30, text=txt, font="Times 25 bold")

            time.sleep(0.01)
            
            root.update()
            canvas.delete("all")            
        except Exception:
            break
    
    root.mainloop()        
    
def get_time(full_seconds):
    hours = int(full_seconds / (60 * 60))
    minutes = int(full_seconds / 60) % 60
    seconds = (full_seconds % 60)
    
    return (hours, minutes, seconds)

if len(sys.argv) != 4:
    print("arguments:")
    print("buses db path")
    print("trams db path")
    print("background image path")
else:
    bus_db_name, tram_db_name, background_image_path = sys.argv[1:]
    print(bus_db_name)
    print(tram_db_name)
    print(background_image_path)
    create_animated_image_from_db(sqlite3.connect(bus_db_name),
                                  sqlite3.connect(tram_db_name),
                                  background_image_path,
                                  "photo",
                                  1280, 810)