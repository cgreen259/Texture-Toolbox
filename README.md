# Texture-Toolbox
This projects allows the user to input a 2D or 3D image, in the form of a numpy array and calculate 43, commonly used, texture features of the image.
Quantisation methods are also avaiable to imporve feature stability in the form of Fixed Bin Width (FBW) and Fixed Bin Number (FBN). 

This project uses numpy, scipy, f2py and fortran in feature calculation. Fortran was used in this project due to its ability to work with numpy and its speed in array based calculations. Due to the nature of the feature matrix calculations indiviual pixels need to be looped over for calculations, which python/numpy arrays struggle to do effieciently, so this part of the project was passed over to fortran for significant speed improvement.

Requires fortran compiler
