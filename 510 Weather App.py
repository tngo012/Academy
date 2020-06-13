"""
File: NGO_WEEK10_DSC510.py
Name: Tai Ngo
Date: 11/17/2019
Course: DSC 510 - Introduction to Programming
Desc: This program interacts with a webservice to obtain data.
      This program prompts the user for their city or zip code and requests weather forecast data from OpenWeatherMap.
      This program displays the weather information in a readable format to the user.
      This program allows the user to run multiple times to get weather information for different locations.
      This program will check to see if the user entered a valid zip code as well as check the URL connection.
"""
# use Requests library to request data from the webservice
import requests

# welcome statement
print('Welcome to the Weather Program')

# check whether zip code is valid
try:
    # prompt user zip code
    user_input = input('Enter your zip code: ')
    print('Your zip code is valid.')

    # check if the connection is successful
    try:
        # loop allows users to get weather info from other zip codes
        while user_input:

            # create the main function
            def main():

                # obtain the data from OpenWeatherMap API by inputting a zip code
                r = requests.get('http://api.openweathermap.org/data/2.5/weather?zip='+user_input+',us&appid=5c27a4a79691f7135bf3b66997607162')
                data = r.json()
                # check to see if URL is valid
                if r:
                    print('The connection to the webservice is successful.')
                else:
                    print('Something went wrong.')

                # assign the json data to individual variables and convert from Kelvin to Fahrenheit
                get_info = data['main']
                get_tempk = get_info['temp']
                get_temp = int((get_tempk - 273.13) * 9 / 5 + 32)
                get_pressure = get_info['pressure']
                get_humidity = get_info['humidity']
                get_mintempk = get_info['temp_min']
                get_mintemp = int((get_mintempk - 273.13) * 9 / 5 + 32)
                get_maxtempk = get_info['temp_max']
                get_maxtemp = int((get_maxtempk - 273.13) * 9 / 5 + 32)

                # display the weather info
                print('The current temperature is: {}F.'.format(get_temp))
                print('Temperature ranges from {}F to {}F. '.format(get_mintemp, get_maxtemp))
                print('The current pressure is: {} hpa.'.format(get_pressure))
                print('The current humidity is: {} %.'.format(get_humidity))
                return
            # call the main function
            main()

            # allow the user to continue or stop
            user_input2 = input('Enter 1 to enter another zip code\nEnter 0 to end\n')
            # stop the loop when 0 is entered
            if user_input2 == '0':
                print('Exiting Program!')
                break
            # continue loop when 1 is entered
            elif user_input2 == '1':
                # prompt user for the zip code
                user_input = input('Enter your zip code: ')
            # wrong input when type something other than 1 or 0
            else:
                print('Invalid Input!')
                print('Exiting Program!')
                break

    except KeyError:
        print('The connection is not valid.')

except KeyError:
    print('Your zip code is invalid.')


