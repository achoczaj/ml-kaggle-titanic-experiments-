# Lifeboat Total Rated Capacity: 1,178 persons
# 

# 
# 
# 2 Parlor Suites each with a 50 foot private promenade and 67 other First Class Staterooms & Suites
# 
# People on board: 2228
# 
# 337 First Class
# 285 Second Class
# 721 Third Class
# 885 Crew
# 
# Survived: 705
# Perished: 1523
# 
# RATIO of SURVIVORS
#               Women & Children	Men	  Total
# First Class	  94%	              31%	  60%
# Second Class	81%	              10%	  44%
# Third Class	  47%	              14%	  25%
# Crew	        87%	              22%	  24%
# 
# 
# First Class: 416
# Titanic’s first class accommodation was capable of housing up to 735 passengers.  The style of accommodation on offer ranged from comfortable single-berth cabins to magnificent multi-room parlour suites. 
# The first class cabins and suites were located over five decks, generally amidships, where the ship’s motion would be least felt.  First class accommodation also held 350 cheaper standard cabins with single beds, used for accompanying servants and staff.

# Second Class: 162
# Third Class: 262 plus 40 open berthing areas
# 84 two-berth cabins for third class passengers
# most cabins in third class had bunk beds for between 4 and 6 people
# 
# Total capacity: 3547 passengers and crew
# 
# Boat Deck Cabins
# A - T - X - Y
# 
# A Deck Cabins
# A1 - A36
# 
# B Deck Cabins
# B1 - B107
# 
# C Deck Cabins
# C1 - C148
# 
# D Deck Cabins
# D1 - D136
# 
# E Deck Cabins
# E1 - E167
# 
# F Deck Cabins
# F1 - F204
# 
# impute missing values - proc 
# 
# 
# 
#   
# 
# MORE FACTS
# First Class (parlor suite) £870/$4,350 
# children and dogs travelled at half fare in First Class.
# First Class (berth) £30/$150 
# Second Class £12/$60
#
# Third Class £3 to £8/$40 

# 
# 1. impute missing Deck: ~ Pclass, TicketDigits, Fare, FarePerCabin, TicketCodeSTD, Ticket, Name
# 
# 2. impute missing Cabin: ~ Pclass, TicketDigits, Fare, FarePerCabin, TicketCodeSTD, Ticket, Name, Deck
# ? Cabin1stNumb 
#   
#   
# [1] "PassengerId"           "Survived"              "Pclass"               
# [4] "Name"                  "Sex"                   "Age"                  
# [7] "SibSp"                 "Parch"                 "Ticket"               
# [10] "Fare"                  "Cabin"                 "Embarked"             
# [13] "IsTrainSet"            "Surname"               "Cabin1stNumb"         
# [16] "Cabin2ndNumb"          "Cabin3rdNumb"          "Cabin4thNumb"         
# [19] "NumbOfCabinsPerTicket" "FarePerCabin"          "Deck"                 
# [22] "TicketDigits"          "TicketCode"            "TicketCodeSTD"