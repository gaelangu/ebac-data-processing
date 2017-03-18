
# coding: utf-8

# ## Logistic Regression - with Data Cleaning and Categorical Variables
# 
# Provided traning and test data in messy format, lots of cleanup required

# In[1]:

import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
from patsy import dmatrices
from sklearn.linear_model import LogisticRegression
from sklearn.cross_validation import train_test_split
from sklearn import metrics
from sklearn.cross_validation import cross_val_score
get_ipython().magic('matplotlib inline')


# Reading CSV with special params
# 1. Treating '?' as NA , as lots of places ? is present, with spaces appended also
# 2. Skipping initial space of all columns and values

# In[2]:

train = pd.read_csv("salary-train.csv", sep=',',na_values=['?',' ?', '? '],skipinitialspace=True)
test = pd.read_csv("salary-test.csv", sep=',',na_values=['?',' ?', '? '],skipinitialspace=True)


# In[3]:

print(train.shape)
print(test.shape)
train = train.dropna()
test = test.dropna()
print(train.shape)
print(test.shape)


# So the size of train before cleaning was 32,561 and after cleaning becomes 30,162 with 14 variables
# and the size of test before cleaning was 16,281 and after cleaning becomes 15,060 with 14 variables

# In[4]:


train.describe()


# In[5]:

train.head(5)


# As Observed, the class variable of test are appended with '.', so, need to remove them first to make train and test class similar

# In[6]:

test['class'] = test['class'].replace('<=50K.','<=50K')
test['class'] = test['class'].replace('>50K.','>50K')
test.head(5)


# In[7]:

print(train.columns.tolist())


# Defining a common method, for doing all preprocessing like, 
# 1. One hard coding or dummy encoding for all categorical variables
# 2. replacing the existing columns with encoded columns
# 3. returning the features and class variables

# In[8]:



def preprocessing(data):
    from sklearn import preprocessing
    X=data[data.columns.difference(['class'])]
    y=data['class']
    #print(preprocessing.LabelEncoder())
    label_encoder = preprocessing.LabelEncoder()
    encoded_workclass = label_encoder.fit_transform(data["workclass"])
    encoded_education = label_encoder.fit_transform(data["education"])
    encoded_marital = label_encoder.fit_transform(data["marital"])
    encoded_occupation = label_encoder.fit_transform(data["occupation"])
    encoded_relationship = label_encoder.fit_transform(data["relationship"])
    encoded_race = label_encoder.fit_transform(data["race"])
    encoded_sex = label_encoder.fit_transform(data["sex"])
    encoded_native_country = label_encoder.fit_transform(data["native-country"])

    X = X.drop('workclass', axis=1)
    X['workclass'] = encoded_workclass

    X = X.drop('education', axis=1)
    X['education'] = encoded_education

    X = X.drop('marital', axis=1)
    X['marital'] = encoded_marital

    X = X.drop('occupation', axis=1)
    X['occupation'] = encoded_occupation

    X = X.drop('relationship', axis=1)
    X['relationship'] = encoded_relationship

    X = X.drop('race', axis=1)
    X['race'] = encoded_race

    X = X.drop('sex', axis=1)
    X['sex'] = encoded_sex

    X = X.drop('native-country', axis=1)
    X['native-country'] = encoded_native_country
    
    return(X,y)
    


# In[9]:


# Initialize label encoder
X_train, y_train = preprocessing(train)
X_test, y_test = preprocessing(test)


# In[10]:


print(X_train.head(3))
y_train.head(3)


# In[11]:

X_test.head(3)


# Initializing the Logistic Regression and fitting the training data

# In[12]:


model = LogisticRegression()
model = model.fit(X_train, y_train)


# In[13]:

model.score(X_train,y_train)


# In[14]:

y_predicted = model.predict(X_test)
y_predicted


# In[15]:

# generate evaluation metrics
print(y_test[1])
print(y_predicted[:5])
metrics.accuracy_score(y_test, y_predicted)


# In[16]:

metrics.confusion_matrix(y_test, y_predicted)


# In[17]:


print(metrics.classification_report(y_test, y_predicted))


# So Accuracy score is **78.3%**

# ### The case could be because of test and train split before cleaning , so lets try to combine clean and then split

# In[18]:

data_combined = pd.concat([train,test])
data_combined.shape


# In[19]:

X,y = preprocessing(data_combined)


# In[20]:

X_train2, X_test2, y_train2, y_test2 = train_test_split(X, y, test_size=0.3, random_state=0)
model2 = LogisticRegression()
model2.fit(X_train2, y_train2)


# In[21]:

predicted_y2 = model2.predict(X_test2)


# In[22]:

print(metrics.accuracy_score(y_test2, predicted_y2))


# In[23]:

print(metrics.confusion_matrix(y_test2, predicted_y2))
print(metrics.classification_report(y_test2, predicted_y2))


# there is slighly increase in precision and recall, and very littel increase in accuracy also to **78.8%**

# In[24]:

# evaluate the model using 10-fold cross-validation
scores = cross_val_score(LogisticRegression(), X, y, scoring='accuracy', cv=10)
print(scores)
print(scores.mean())


# Nice, as it still performs accuracy with **78.9%**
