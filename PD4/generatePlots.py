# -*- coding: utf-8 -*-
"""
Created on Sun May 28 19:30:48 2017

@author: RAPTOR
"""

from script import *

def generatePartOne():
    conn = sqlite3.connect("autobusySobota")
    conn2 = sqlite3.connect("tramwajeSobota")
    
    date = datetime(2017, 5, 27)
    calculateAvg(conn, conn2, date, 154, allPoints, allPoints, "Autobusy", "Tramwaje", "plots/2_1_1a")
    calculateAvg(conn, conn2, date, 154, myCentrum, myCentrum, "Autobusy", "Tramwaje", "plots/2_1_1b")
    calculateAvg(conn, conn2, date, 154, notMyCentrum, notMyCentrum, "Autobusy", "Tramwaje", "plots/2_1_1c")

    calculateAvg(conn, conn2, date, 154, wola, wola, "Autobusy", "Tramwaje", "plots/2_1_2a")
    calculateAvg(conn, conn2, date, 154, mokotow, mokotow, "Autobusy", "Tramwaje", "plots/2_1_2b")
    calculateAvg(conn, conn2, date, 154, ursynow, ursynow, "Autobusy", "Tramwaje", "plots/2_1_2c")
    calculateAvg(conn, conn2, date, 154, bialoleka, bialoleka, "Autobusy", "Tramwaje", "plots/2_1_2d")
    calculateAvg4(conn, date, 154, wola, mokotow, ursynow, bialoleka, "wola", "mokotow", "ursynow", "bialoleka", "plots/2_1_2e")
    calculateAvg4(conn2, date, 154, wola, mokotow, ursynow, bialoleka, "wola", "mokotow", "ursynow", "bialoleka", "plots/2_1_2f")
    
    
    #calculateNumber(conn, conn2, date, 31, allPoints, allPoints, "Autobusy", "Tramwaje", "plots/2_2_1a")
    #calculateNumber(conn, conn2, date, 31, myCentrum, myCentrum, "Autobusy", "Tramwaje", "plots/2_2_1b")
    #calculateNumber(conn, conn2, date, 31, notMyCentrum, notMyCentrum, "Autobusy", "Tramwaje", "plots/2_2_1c")
    
    #calculateNumber(conn, conn2, date, 31, wola, wola, "Autobusy", "Tramwaje", "plots/2_2_2a")
    #calculateNumber(conn, conn2, date, 31, mokotow, mokotow, "Autobusy", "Tramwaje", "plots/2_2_2b")
    #calculateNumber(conn, conn2, date, 31, ursynow, ursynow, "Autobusy", "Tramwaje", "plots/2_2_2c")
    #calculateNumber(conn, conn2, date, 31, bialoleka, bialoleka, "Autobusy", "Tramwaje", "plots/2_2_2d")
    #calculateNumber4(conn, date, 31, wola, mokotow, ursynow, bialoleka, "wola", "mokotow", "ursynow", "bialoleka", "plots/2_2_2e")
    #calculateNumber4(conn2, date, 31, wola, mokotow, ursynow, bialoleka, "wola", "mokotow", "ursynow", "bialoleka", "plots/2_2_2f")
    
    
def generatePartTwo():
    date = datetime(2017, 5, 29)
    conn = sqlite3.connect("autobusyPoniedzialek")
    conn2 = sqlite3.connect("tramwajePoniedzialek")
    conn3 = sqlite3.connect("autobusySobota")
    conn4 = sqlite3.connect("tramwajeSobota")
    
    calculateAvg(conn, conn2, date, 154, allPoints, allPoints, "Autobusy", "Tramwaje", "plots/3_1a")
    calculateAvg(conn, conn2, date, 154, myCentrum, myCentrum, "Autobusy", "Tramwaje", "plots/3_1b")
    calculateAvg(conn, conn2, date, 154, notMyCentrum, notMyCentrum, "Autobusy", "Tramwaje", "plots/3_1c")
    
    calculateAvg(conn, conn3, date, 154, allPoints, allPoints,  "Poniedziałek", "Sobota", "plots/3_2_1a")
    calculateAvg(conn, conn3, date, 154, myCentrum, myCentrum,  "Poniedziałek", "Sobota", "plots/3_2_1b")
    calculateAvg(conn, conn3, date, 154, notMyCentrum, notMyCentrum,  "Poniedziałek", "Sobota", "plots/3_2_1c")
    
    calculateAvg(conn2, conn4, date, 154, allPoints, allPoints,  "Poniedziałek", "Sobota", "plots/3_2_2a")
    calculateAvg(conn2, conn4, date, 154, myCentrum, myCentrum,  "Poniedziałek", "Sobota", "plots/3_2_2b")
    calculateAvg(conn2, conn4, date, 154, notMyCentrum, notMyCentrum,  "Poniedziałek", "Sobota", "plots/3_2_2c")
    
    
generatePartOne()