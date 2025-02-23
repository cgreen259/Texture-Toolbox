import texture_toolbox as tt
import numpy as np
from pprint import pprint

array = np.random.randint(5, size=(3,3))

print(array)

glcm = tt.features.all_features(array)
pprint(vars(glcm))