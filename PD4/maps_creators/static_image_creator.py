# -*- coding: utf-8 -*-
"""
Created on Mon May 29 07:55:07 2017

@author: slowik
"""

import conversion_utils as cu
import drawing_utils as du
import json_utils as ju

def create_static_image_from_data_frame(target_file_name, data_frame,
                                        background_image_path,
                                        image_width, image_height):
    (draw, image) = du.get_static_image_drawing_data(background_image_path)
    cm = cu.converterMap(image_width, image_height)
    du.set_points_on_static_image(cm, data_frame, draw, image, "blue", target_file_name)
    
def create_static_line_images_from_db(db_con,
                                      background_image_path,
                                      image_width, image_height):
    line_images = {}
    cm = cu.converterMap(image_width, image_height)
    
    all_rows = db_con.execute("SELECT * FROM data").fetchall()
    for row in all_rows:
        json_array = ju.get_json_array_from_db_row(row)
        
        for json_item in json_array:
            try:
                line = json_item["Lines"].strip()
                longitude = float(json_item["Lon"])
                latitude = float(json_item["Lat"])
                
                if line not in line_images:
                    line_images[line] = du.get_static_image_drawing_data(background_image_path)
                
                du.set_point_on_static_image(cm, longitude, latitude, line_images[line][0], "blue", 3)
            except Exception as e:
                pass
            
    return line_images
