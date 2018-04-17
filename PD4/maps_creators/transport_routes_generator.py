# -*- coding: utf-8 -*-
"""
Created on Mon May 29 08:28:29 2017

@author: slowik
"""

from static_image_creator import *
import sys
import sqlite3

if len(sys.argv) != 4:
    print("arguments:")
    print("db path")
    print("background image path"); print("suffix name")
else:
    db_path, background_image_path, suffix_name = sys.argv[1:]
    print(db_path)
    print(background_image_path)
    print(suffix_name)    
    
    lines_routes_dict = create_static_line_images_from_db(sqlite3.connect(db_path),
                                                          background_image_path,
                                                          1280, 810)
     
    print(len(lines_routes_dict))                                                     
    for line_name, image_tuple in lines_routes_dict.items():
        try:
            print(line_name + " START")
            f_name = "./line_routes/" + line_name + "_" + suffix_name + ".png"
            image_tuple[1].save(f_name)
            print(line_name + " DONE")
        except:
            print("an error during saving " + f_name)
