import texture_toolbox as tt
import numpy as np
from pprint import pprint
import time

np.random.seed(33)
array = np.random.randint(20,25, size=(10,12))
array[-1,-2] = array[-1,-1]
# array[-2:, -2:] = array[-1,-1]
print(array)

array = np.arange(1, 10*12+1).reshape(10,12)

t1 = time.time()
glcm = tt.Matrix.GLCM(array)
t2 = time.time()
glrlm = tt.Matrix.GLRLM(array)
t3 = time.time()
glszm = tt.Matrix.GLSZM(array)
t4 = time.time()
ngtdm = tt.Matrix.NGTDM(array)
t5 = time.time()

print("GLCM: ", t2-t1)
print("GLRLM: ", t3-t2)
print("GLSZM: ", t4-t3)
print("NGTDM: ", t5-t4)

print("-------------")
print(array)