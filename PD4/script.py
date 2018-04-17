# -*- coding: utf-8 -*-
"""
Created on Thu May 25 22:46:57 2017

@author: sebassud
"""

import requests
import json
import sqlite3
import time
from math import radians, cos, sin, asin, sqrt
from datetime import datetime
from datetime import timedelta
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches
import matplotlib.path as mplPath
import numpy as np
import pandas as pd
from districts import *

def haversine(lon1, lat1, lon2, lat2):
    """
    Calculate the great circle distance between two points 
    on the earth (specified in decimal degrees)
    """
    # convert decimal degrees to radians 
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    # haversine formula 
    dlon = lon2 - lon1 
    dlat = lat2 - lat1 
    a = sin(dlat/2)**2 + cos(lat1) * cos(lat2) * sin(dlon/2)**2
    c = 2 * asin(sqrt(a)) 
    km = 6367 * c
    return km

def quickDistance(lon1, lat1, lon2, lat2):
    R = 6371  # radius of the earth in km
    lon1, lat1, lon2, lat2 = map(radians, [lon1, lat1, lon2, lat2])
    x = (lon2 - lon1) * cos( 0.5*(lat2+lat1) )
    y = lat2 - lat1
    d = R * sqrt( x*x + y*y )
    return d

def differenceStringDate(date1, date2):
    """
    return hours
    """
    diffDate = datetime.strptime(date1 , '%Y-%m-%d %H:%M:%S') - datetime.strptime(date2 , '%Y-%m-%d %H:%M:%S')
    
    if(diffDate.seconds == 0):
        return 0
    else:
        return diffDate.seconds / 3600

def minAvgMax(prevList, currList, fun):
    vlist = []
    for currItem in currList:
        if(fun(float(currItem['Lat']), float(currItem['Lon'])) == False or currItem['Lines']==""):
            continue
        prevItem = None
        for prev in prevList:
            if(prev['Lines'] == currItem['Lines'] and prev['Brigade'] == currItem['Brigade']):
                prevItem = prev
                break
        if(prevItem is not None):
            diffTime = differenceStringDate(currItem['Time'], prevItem['Time'])
            if(diffTime != 0):
                dist = haversine(currItem['Lon'], currItem['Lat'], prevItem['Lon'], prevItem['Lat'])
                if(dist / diffTime < 100):
                    vlist.append(dist / diffTime)
    if(len(vlist) == 0):
        return (0, 0, 0)
    return (min(vlist), sum(vlist) / len(vlist), max(vlist))

def calculateNumberVehicles(conn, fun):
    scores = []
    for row in conn.execute("SELECT * FROM data"):
        data = json.loads(row[1])
        currList = data['result']
        try:
            scores.append(sum(1 for item in currList if(fun(float(item['Lat']), float(item['Lon']))) ))
        except:
            scores.append(-1)
    return scores

def calculateScoresMinAvgMax(conn, fun):
    first = True
    scores = []
    times = -1
    for row in conn.execute("SELECT * FROM data"):
        data = json.loads(row[1])
        if(first):
            first = False
            prevList = data['result']
        else:
            times += 1
            if(times % 5 != 0):
                continue
            try:
                currList = data['result']
                scores.append(minAvgMax(prevList, currList, fun))
                prevList = currList
            except:
                scores.append((-1, -1, -1))
            
    return scores

class LineAvgMax:
    def __init__(self, v, line):
        self.count = 1
        self.sum = v
        self.maxV = v
        self.line = line
        
    def addV(self, v):
        self.count += 1
        self.sum += v
        if(v > self.maxV):
            self.maxV = v
        
    def calAvg(self):
        return self.sum / self.count
    
def lineAvgMax(prevList, currList, fun, dictionary):
    for currItem in currList:
        if(fun(float(currItem['Lat']), float(currItem['Lon'])) == False):
            continue
        prevItem = None
        for prev in prevList:
            if(prev['Lines'] == currItem['Lines'] and prev['Brigade'] == currItem['Brigade']):
                prevItem = prev
                break
        if(prevItem is not None):
            diffTime = differenceStringDate(currItem['Time'], prevItem['Time'])
            if(diffTime != 0):
                dist = haversine(currItem['Lon'], currItem['Lat'], prevItem['Lon'], prevItem['Lat'])
                if(dist / diffTime < 100):
                    if(currItem['Lines'] in dictionary):
                        dictionary[currItem['Lines']].addV(dist / diffTime)
                    else:
                        dictionary[currItem['Lines']] = LineAvgMax(dist / diffTime, currItem['Lines'])
    
def calculateLineAvgMax(conn, fun):
    first = True
    dictionary = dict()
    times = 0
    for row in conn.execute("SELECT * FROM data"):
        data = json.loads(row[1])
        if(first):
            first = False
            prevList = data['result']
        else:
            times += 1
            if(times % 2 != 0):
                continue
            currList = data['result']
            try:
                lineAvgMax(prevList, currList, fun, dictionary)
                prevList = currList
            except:
                2
            
    return dictionary
            
def calculateLines(conn, conn2, fun, fun2):
    d1 = calculateLineAvgMax(conn, fun)
    d2 = calculateLineAvgMax(conn2,fun2)
    values = set().union(d1.values(), d2.values())
    lines = []
    [lines.append(el.line) for el in values]
    ds1 = {'Avg' :  pd.Series([el.calAvg() for el in values], lines), 
    'Max' :  pd.Series([el.maxV for el in values], lines)}
    df1 = pd.DataFrame(ds1)
    
    print(df1.sort_values(by='Avg', ascending=False))
    
    
    return 0

def calculateAvg(conn, conn2, startDate, interval, fun, fun2, lred, lblue, file):
    scores = calculateScoresMinAvgMax(conn, fun)
    
    listAvg1 = []
    [listAvg1.append(i[1]) for i in scores]
    listTime = []
    [listTime.append(startDate + timedelta(0,interval*i)) for i in range(len(listAvg1))]
    
    scores = calculateScoresMinAvgMax(conn2, fun2)
    listAvg2 = []
    [listAvg2.append(i[1]) for i in scores]
    
    clearScores(listAvg1, listAvg2, listTime)
    
    plt.clf()
    plt.plot(listTime, listAvg1, 'r', listTime, listAvg2, 'b')
    red_patch = mpatches.Patch(color='red', label=lred)
    blue_patch = mpatches.Patch(color='blue', label=lblue)
    plt.legend(handles=[red_patch, blue_patch])
    plt.ylabel('Avg v')
    #plt.ylim(ymin=0)
    #plt.show()
    fig = plt.gcf()
    fig.set_size_inches(18.5, 10.5)
    fig.savefig(file, dpi=120)
    return 0

def calculateNumber(conn, conn2, startDate, interval, fun, fun2, lred, lblue, file):
    scores1 = calculateNumberVehicles(conn, fun)
    
    listTime = []
    [listTime.append(startDate + timedelta(0,interval*i)) for i in range(len(scores1))]
        
    scores2 = calculateNumberVehicles(conn2, fun2)
    clearScores(scores1, scores2, listTime)
    
    plt.clf()
    plt.plot(listTime, scores1, 'r', listTime, scores2, 'b')
    red_patch = mpatches.Patch(color='red', label=lred)
    blue_patch = mpatches.Patch(color='blue', label=lblue)
    plt.legend(handles=[red_patch, blue_patch])
    plt.ylabel('Ilość')
    plt.ylim(ymin=0)
    #plt.show()
    fig = plt.gcf()
    fig.set_size_inches(18.5, 10.5)
    fig.savefig(file, dpi=120)
    return 0
    
def clearScores(scores1, scores2, listTime):
    if(len(scores1) != len(scores2)):
        while(len(scores2) != len(scores1)):
            scores2.pop()
        
    for i in range(len(scores1)):
        if(i >= len(scores1)):
            break
        if(scores1[i] < 0 or scores2[i] < 0):
            del scores1[i]
            del scores2[i]
            del listTime[i]
            i -= 1
                
def calculateAvg4(conn, startDate, interval, fun, fun2, fun3, fun4, lred, lblue, lyellow, lgreen, file):
    scores = calculateScoresMinAvgMax(conn, fun)
    
    listAvg1 = []
    [listAvg1.append(i[1]) for i in scores]
    listTime = []
    [listTime.append(startDate + timedelta(0,interval*i)) for i in range(len(listAvg1))]
    
    scores = calculateScoresMinAvgMax(conn, fun2)
    listAvg2 = []
    [listAvg2.append(i[1]) for i in scores]
    
    scores = calculateScoresMinAvgMax(conn, fun3)
    listAvg3 = []
    [listAvg3.append(i[1]) for i in scores]
    
    scores = calculateScoresMinAvgMax(conn, fun4)
    listAvg4 = []
    [listAvg4.append(i[1]) for i in scores]
    
    clearScores4(listAvg1, listAvg2, listAvg3, listAvg4, listTime)
    plt.clf()
    plt.plot(listTime, listAvg1, 'r', listTime, listAvg2, 'b', listTime, listAvg3, 'y', listTime, listAvg4, 'g')
    red_patch = mpatches.Patch(color='red', label=lred)
    blue_patch = mpatches.Patch(color='blue', label=lblue)
    yellow_patch = mpatches.Patch(color='yellow', label=lyellow)
    green_patch = mpatches.Patch(color='green', label=lgreen)
    plt.legend(handles=[red_patch, blue_patch, yellow_patch, green_patch])
    plt.ylabel('Avg v')
    plt.ylim(ymin=0)
    #plt.show()
    fig = plt.gcf()
    fig.set_size_inches(18.5, 10.5)
    fig.savefig(file, dpi=120)
    return 0

def calculateNumber4(conn, startDate, interval, fun, fun2, fun3, fun4, lred, lblue, lyellow, lgreen, file):
    scores1 = calculateNumberVehicles(conn, fun)
    
    listTime = []
    [listTime.append(startDate + timedelta(0,interval*i)) for i in range(len(scores1))]
        
    scores2 = calculateNumberVehicles(conn, fun2)
    scores3 = calculateNumberVehicles(conn, fun3)
    scores4 = calculateNumberVehicles(conn, fun4)
    
    clearScores4(scores1, scores2, scores3, scores4, listTime)
    plt.clf()
    plt.plot(listTime, scores1, 'r', listTime, scores2, 'b', listTime, scores3, 'y', listTime, scores4, 'g')
    red_patch = mpatches.Patch(color='red', label=lred)
    blue_patch = mpatches.Patch(color='blue', label=lblue)
    yellow_patch = mpatches.Patch(color='yellow', label=lyellow)
    green_patch = mpatches.Patch(color='green', label=lgreen)
    plt.legend(handles=[red_patch, blue_patch, yellow_patch, green_patch])
    plt.ylabel('Ilość')
    plt.ylim(ymin=0)
    #plt.show()
    fig = plt.gcf()
    fig.set_size_inches(18.5, 10.5)
    fig.savefig(file, dpi=120)
    return 0

def clearScores4(scores1, scores2, scores3, scores4, listTime):
    if(len(scores1) == len(scores2) == len(scores3) == len(scores4)):
        for i in range(len(scores1)):
            if(i >= len(scores1)):
                break
            if(scores1[i] < 0 or scores2[i] < 0):
                del scores1[i]
                del scores2[i]
                del scores3[i]
                del scores4[i]
                del listTime[i]
                i -= 1
                
def createTable():
    conn = sqlite3.connect("autobusySobota")
    conn2 = sqlite3.connect("tramwajeSobota")
    calculateLines(conn,conn2, allPoints, allPoints)








