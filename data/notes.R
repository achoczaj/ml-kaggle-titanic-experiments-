# ML Data Flow


# Data Exploration

# Data Visualization

# Preprocessing Data 

# - Casting Data
# - Renaming Columns
# - Feature Engineering

#    use feature engineering to create new features
# Titles
# 
# First up the Name column is currently not being used, but we can at least extract the title from the name. There are quite a few titles going around, but I want to reduce them all to Mrs, Miss, Mr and Master.  To do this we’ll need a function that searches for substrings. Thankfully the library ‘strings’ has just what we need.
# 
# import strings
# def substrings_in_string(big_string, substrings):
#   for substring in substrings:
#   if string.find(big_string, substring) != -1:
#   return substring
# print big_string
# return np.nan
# Using this iteratively I was able to get a full list of titles.
# 
# title_list=['Mrs', 'Mr', 'Master', 'Miss', 'Major', 'Rev',
#             'Dr', 'Ms', 'Mlle','Col', 'Capt', 'Mme', 'Countess',
#             'Don', 'Jonkheer']
# Now that I have them, I recombine them to the four categories.
# 
# df['Title']=df['Name'].map(lambda x: substrings_in_string(x, title_list))
# 
# #replacing all titles with mr, mrs, miss, master
# def replace_titles(x):
#   title=x['Title']
# if title in ['Don', 'Major', 'Capt', 'Jonkheer', 'Rev', 'Col']:
#   return 'Mr'
# elif title in ['Countess', 'Mme']:
#   return 'Mrs'
# elif title in ['Mlle', 'Ms']:
#   return 'Miss'
# elif title =='Dr':
#   if x['Sex']=='Male':
#   return 'Mr'
# else:
#   return 'Mrs'
# else:
#   return title
# df['Title']=df.apply(replace_titles, axis=1)
# You may be interested to know that ‘Jonkheer’ is a male honorific for Dutch nobility. Also interesting is that I was tempted to just send ‘Dr’ -> ‘Mr’, but decided to check first, and there was indeed a female doctor aboard! It seems 1912 was further ahead of its time than Doctor Who!
#   
#   Curious, I looked her up: her name was Dr. Alice Leader, and she and her husband were physicians in New York city.
# 
# 
# 
# But I digress. On to the decks.
# Deck
# 
# This is going be very similar, we have a ‘Cabin’ column not doing much, only 1st class passengers have cabins, the rest are ‘Unknown’. A cabin number looks like ‘C123’. The letter refers to the deck, and so we’re going to extract these just like the titles.
# 
# #Turning cabin number into Deck
# cabin_list = ['A', 'B', 'C', 'D', 'E', 'F', 'T', 'G', 'Unknown']
# df['Deck']=df['Cabin'].map(lambda x: substrings_in_string(x, cabin_list))
# Family Size
# One thing you can do to create new features is linear combinations of features. In a model like linear regression this should be unnecessary, but for a decision tree may find it hard to model such relationships. Reading on the forums at Kaggle, some people have considered the size of a person’s family, the sum of their ‘SibSp’ and ‘Parch’ attributes. Perhaps people traveling alone did better? Or on the other hand perhaps if you had a family, you might have risked your life looking for them, or even giving up a space up to them in a lifeboat. Let’s throw that into the mix.
# 
# #Creating new family_size column
# df['Family_Size']=df['SibSp']+df['Parch']
# Age*Class
# This is an interaction term, since age and class are both numbers we can just multiply them.
# 
# df['Age*Class']=df['Age']*df['Pclass']
# Fare per Person
# 
# Here we divide the fare by the number of family members traveling together, I’m not exactly sure what this represents, but it’s easy enough to add in.
# 
# df['Fare_Per_Person']=df['Fare']/(df['Family_Size']+1)
# Get The Code
# I have incorporated these new features in a new data cleaning cleantitanic2.py script, which you can download here.