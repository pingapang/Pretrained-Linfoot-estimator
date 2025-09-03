import sys
import tensorflow.keras
import pandas as pd
import tensorflow as tf
import numpy as np
import os
import keras
from tensorflow import data as tf_data
from keras import layers
import platform


model = keras.saving.load_model("combined_model_2.keras")

data = pd.read_csv('input.csv')
nsamples = data.shape[0]

prefix = ['mean', 'var', "skewness", 'fnn', 'pearson']

image = data.iloc[:, (56):(56+2500)]
stats = data.filter(regex='|'.join(f'^{x}' for x in prefix)).dropna(how='all')

image = image.to_numpy()
image = image.reshape(nsamples, 50, 50, 1)
stats = stats.to_numpy()
stats = stats.reshape(nsamples, 56)

predictions = model.predict([image, stats])
print(predictions)

