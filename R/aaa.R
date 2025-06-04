# Stores list of flights from CEDA after list_flights() is called once

the = new.env(parent = emptyenv())

the$flightList = NULL
the$listOfFlights = list()
the$access_token = NULL
the$access_token_expires = NULL