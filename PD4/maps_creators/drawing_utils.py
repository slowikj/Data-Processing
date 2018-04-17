# -*- coding: utf-8 -*-
"""
Created on Mon May 29 08:03:32 2017

@author: slowik
"""

import PIL.Image
import PIL.ImageDraw
from tkinter import *
import conversion_utils as cu

def get_canvas_drawing_data(background_image_path, image_width, image_height, target_file_name):
    root = Tk()
    canvas_width = get_canvas_width(image_width)
    canvas_height = get_canvas_height(image_height)
    root.geometry(str(canvas_width) + "x" + str(canvas_height))
    
    Canvas.create_circle = cu._create_circle
    canvas = Canvas(root,width=canvas_width,height=canvas_height)
    canvas.pack()
    
    temp=PIL.Image.open(background_image_path)
    temp = temp.save(target_file_name + ".ppm","ppm")
    photo = PhotoImage(file = target_file_name + ".ppm")
    
    return (root, canvas, photo)
    

def get_canvas_width(image_width):
    return image_width + 10

def get_canvas_height(image_height):
    return image_height + 10

def set_points_on_canvas(cm, json_array, canvas, root, fill_color):
    for row in json_array:
        try:
            longitude = float(row["Lon"])
            latitude = float(row["Lat"])
        
            if cm.isOnMap(longitude, latitude):
                p = cm.getPosition(longitude, latitude)
                canvas.create_circle(p[0], p[1], 2, fill=fill_color, width=1)
        except Exception:
            pass
        
    root.update()
    
def set_points_on_static_image(cm, data_frame, draw, image, point_color, new_file_name):
    point_size = 3
    for row in data_frame.iterrows():
        row_series = row[1]
        longitude = float(row_series["Lon"])
        latitude = float(row_series["Lat"])
        
        set_point_on_static_image(cm, longitude, latitude, draw, point_color, point_size)
        
    image.save(new_file_name)
    
def set_point_on_static_image(cm, longitude, latitude, draw, point_color, point_size):
    if cm.isOnMap(longitude, latitude):
        p = cm.getPosition(longitude, latitude)
            
        draw.ellipse([p[0] - point_size,
                      p[1] - point_size,
                      p[0] + point_size,
                      p[1] + point_size], fill=point_color)
    

def get_static_image_drawing_data(background_image_path):
    image = PIL.Image.open(background_image_path)
    draw = PIL.ImageDraw.Draw(image)
    
    return (draw, image)
    