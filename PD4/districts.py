# -*- coding: utf-8 -*-
"""
Created on Sat May 27 22:02:55 2017

@author: RAPTOR
"""

import matplotlib.path as mplPath
from PIL import Image, ImageDraw
import numpy as np

def allPoints(lat, lon):
    return True

def myCentrum(lat, lon):
    bbPath = mplPath.Path(np.array([[52.247130, 20.970801],
                     [52.213549, 20.989464],
                     [52.219056, 21.053315],
                     [52.247934, 21.018419],
                     [52.256370, 20.981776]]))
    return bbPath.contains_point((lat, lon))

def notMyCentrum(lat, lon):
    bbPath = mplPath.Path(np.array([[52.247130, 20.970801],
                     [52.213549, 20.989464],
                     [52.219056, 21.053315],
                     [52.247934, 21.018419],
                     [52.256370, 20.981776]]))
    return not bbPath.contains_point((lat, lon))

def wola(lat, lon):
    bbPath = mplPath.Path(np.array([[52.227485, 21.000983],
                     [52.209885, 20.925311],
                     [52.248103, 20.934312],
                     [52.259209, 20.978093]]))
    return bbPath.contains_point((lat, lon))

def mokotow(lat, lon):
    bbPath = mplPath.Path(np.array([[52.219320, 21.074916],
                     [52.210696, 20.989257],
                     [52.170707, 20.987884],
                     [52.170391, 21.070453],
                     [52.192600, 21.103584]]))
    return bbPath.contains_point((lat, lon))

def bialoleka(lat, lon):
    bbPath = mplPath.Path(np.array([[52.355270, 20.914341],
                     [52.290014, 20.987586],
                     [52.311218, 21.083544],
                     [52.368693, 21.073245]]))
    return bbPath.contains_point((lat, lon))

def ursynow(lat, lon):
    bbPath = mplPath.Path(np.array([[52.171869, 20.987956],
                     [52.173659, 21.047351],
                     [52.106865, 21.103141],
                     [52.102964, 20.996711]]))
    return bbPath.contains_point((lat, lon))

class converterMap:
    x1 = 20.788000
    y1 = 52.160088
    x2 = 21.227348
    y2 = 52.330103
    length = 1280
    width = 810
    
    def isOnMap(self, lon, lat):
        if(self.x1 < lon < self.x2 and self.y1 < lat < self.y2):
            return True
        else:
            return False
    
    def getPosition(self, lon, lat):
        x = (lon - self.x1) / (self.x2 - self.x1) * self.length
        y = self.width - (lat - self.y1) / (self.y2 - self.y1) * self.width
        return (x,y)


image = Image.open('mapa_warszawy.jpg')
draw = ImageDraw.Draw(image)
cm = converterMap()
p1 = cm.getPosition(20.970801, 52.247130)
p2 = cm.getPosition(20.989464, 52.213549)
p3 = cm.getPosition(21.053315, 52.219056)
p4 = cm.getPosition(21.018419, 52.247934)
p5 = cm.getPosition(20.981776, 52.256370)
draw.line((p1[0], p1[1], p2[0], p2[1]), fill=128, width=5)
draw.line((p2[0], p2[1], p3[0], p3[1]), fill=128, width=5)
draw.line((p3[0], p3[1], p4[0], p4[1]), fill=128, width=5)
draw.line((p4[0], p4[1], p5[0], p5[1]), fill=128, width=5)
draw.line((p5[0], p5[1], p1[0], p1[1]), fill=128, width=5)
#image.save('myCentrum.bmp')