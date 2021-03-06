# -*- coding: utf-8 -*-
"""images_classification.ipynb

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1BWVysPbSBZVirQFUu8TOoQnQyPFAXsih

Using PCA in Image classification
"""

from keras.datasets import fashion_mnist
from xgboost import XGBClassifier
from sklearn.decomposition import PCA
from sklearn.metrics import accuracy_score, confusion_matrix
import time

(X_train, y_train), (X_test, y_test) = fashion_mnist.load_data()

# Reshape the data to create the features columns as pixel values in every image
d1, d2, d3 = X_train.shape
X_train = X_train.reshape(d1, d2*d3)
d1, d2, d3 = X_test.shape
X_test = X_test.reshape(d1, d2*d3)

# Classification model with pixels as features
start_time = time.time()
grad_boost_model = XGBClassifier()
hist = grad_boost_model.fit(X_train,y_train)
pred = grad_boost_model.predict(X_test)
print("--- %s seconds ---" % (time.time() - start_time))

# Model evaluation in test data using Confusion matrix
confusion_matrix(y_test, pred)

# Full model accuracy
accuracy_score(y_test, pred)

start_time = time.time()
# Carry out Principal components on the feature dataset and examine classification with a smaller feature set
# Define a PCA object with some 50 features as reduced dimension set
princ_comp_model = PCA(n_components=50)
princ_comp_model.fit(X_train)
# Obtain a smaller dimension dataset with 50 columns (each for a component)
X_train2 = princ_comp_model.transform(X_train)
print(sum(princ_comp_model.explained_variance_ratio_))
# Define a XGBClassifier to build a model on reduced dimension X_train2
grad_boost_model2 = XGBClassifier()
hist2 = grad_boost_model2.fit(X_train2,y_train)
# Transform test dataset to reduced dimension dataset(50 components)
X_test2 = princ_comp_model.transform(X_test)
pred2 = grad_boost_model2.predict(X_test2)
print("--- %s seconds ---" % (time.time() - start_time))

# Model evaluation using Confusion Matrix
confusion_matrix(y_test, pred2)

accuracy_score(y_test, pred2)