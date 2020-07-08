#!/bin/python
# -*- coding: utf-8 -*-

# Procedure
# Surf to https://openweathermap.org/city
# Fill in your CITY
# e.g. Antwerp Belgium
# Check url
# https://openweathermap.org/city/2803138
# you will the city code at the end
# create an account on this website
# create an api key (free)
# LANG included thanks to krive001 on discord
import requests

CITY = "4174402"
API_KEY = "612dc5e09c077d4ab77c62f8157ed2b3"
UNITS = "Metric"
UNIT_KEY = "C"
LANG = "en"

REQ = requests.get("http://api.openweathermap.org/data/2.5/weather?id={}&lang={}&appid={}&units={}".format(CITY, LANG,  API_KEY, UNITS))

try:
    if REQ.status_code == 200:
        CURRENT = REQ.json()["weather"][0]["description"].capitalize()
        TEMP = int(float(REQ.json()["main"]["temp"]))
        ICON = REQ.json()["weather"][0]["main"].capitalize()

        if ICON == "RAIN": print(" {}, {} °{}".format(CURRENT, TEMP, UNIT_KEY))
        else if ICON == "CLOUDS": println(" {}, {} °{}".format(CURRENT, TEMP, UNIT_KEY))
        else if ICON == "CLEAR": println(" {}, {} °{}".format(CURRENT, TEMP, UNIT_KEY))
        else if ICON == "THUNDERSTORM": println(" {}, {} °{}".format(CURRENT, TEMP, UNIT_KEY))
        else: println("{} {}, {} °{}".format(ICON, CURRENT, TEMP, UNIT_KEY)) 
    else:
        print("Error: BAD HTTP STATUS CODE " + str(REQ.status_code))
except (ValueError, IOError):
    print("Error: Unable to print the data")
