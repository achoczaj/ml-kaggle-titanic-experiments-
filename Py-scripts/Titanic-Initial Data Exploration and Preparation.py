# -*- coding: utf-8 -*-
"""
Created on Tue Aug  1 17:22:01 2017

@author: Arek
"""

#%matplotlib inline

import numpy as np
import pandas as pd
import os

# Set working directory
os.chdir('C:\\Labs_ML\\Kaggle\\C01\\data') 

# Read the data
titanic_train = pd.read_csv("train.csv")

# Check dimensions   
titanic_train.shape              

titanic_train.dtypes

# Check the first 5 rows
print(titanic_train.head(5))  


print( titanic_train.describe() )

categorical = titanic_train.dtypes[titanic_train.dtypes == "object"].index
print(categorical)

titanic_train[categorical].describe()


