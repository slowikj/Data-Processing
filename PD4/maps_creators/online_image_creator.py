# -*- coding: utf-8 -*-
"""
Created on Mon May 29 08:22:01 2017

@author: slowik
"""

import drawing_utils as du
import conversion_utils as cu
import json_utils as ju
import time
import sys

def create_online_image(bus_request_url, tram_request_url,
                        target_file_name,
                        background_image_path,
                        image_width, image_height):
    (root, canvas, photo) = du.get_canvas_drawing_data(background_image_path,
                                            image_width, image_height, target_file_name)
    cm = cu.converterMap(image_width, image_height)
    
    while True:
        try:
            bus_json_array = ju.get_json_array(bus_request_url)
            tram_json_array = ju.get_json_array(tram_request_url)
            
            canvas.create_image(image_width / 2, image_height / 2, image=photo)
            du.set_points_on_canvas(cm, bus_json_array, canvas, root, "blue")
            du.set_points_on_canvas(cm, tram_json_array, canvas, root, "red")
            
            time.sleep(10)
            
            root.update()
            canvas.delete("all")            
        except Exception:
            break
        
    root.mainloop()
    
bus_request_url = 'https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id= f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey=a94779ec-47e0-4545-baa1-62206517940e&type=1'
tram_request_url = 'https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id= f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey=a94779ec-47e0-4545-baa1-62206517940e&type=2'

if len(sys.argv) != 2:
    print("arguments:")
    print("background image path")
else:
    background_image_path = sys.argv[1]
    print(background_image_path)
    create_online_image(bus_request_url, tram_request_url,
                        "photo",
                        background_image_path,
                        1280, 810)

