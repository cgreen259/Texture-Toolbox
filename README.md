# Texture-Toolbox
This projects allows the user to input a 2D or 3D image, in the form of a numpy array and calculate 43, commonly used, texture features of the image.
Quantisation methods are also avaiable to imporve feature stability in the form of Fixed Bin Width (FBW) and Fixed Bin Number (FBN). 

This project uses numpy, scipy, f2py and fortran in feature calculation. Fortran was used in this project due to its ability to work with numpy and its speed in array based calculations. Due to the nature of the feature matrix calculations indiviual pixels need to be looped over for calculations, which python/numpy arrays struggle to do effieciently, so this part of the project was passed over to fortran for significant speed improvement.

Requires fortran compiler

Available features
----------------------------------------------------------------------------------------------------------------------------------------------------------------

Global:
  Variance, 
  Skewness, 
  Kurtosis

GLCM:
  * Energy, 
  * Contrast, 
  * Entropy, 
  * Homogeneity, 
  * Correlation, 
  * SumAverage, 
  * Variance, 
  * Dissimilarity, 
  * AutoCorrelation
  
GLRLM:
  * SRE   (Short Run length Emphasis), 
* LRE   (Long Run length Emphasis), 
  GLN   (Grey Level Non-uniformity), 
  RLN   (Run Length Non-uniformity), 
  RP    (Run Percentage), 
  LGRE  (Low Grey level Run Emphasis), 
  HGRE  (High Grey level Run Emphasis), 
  SRLGE (Short Run Low Grey level Emphasis), 
  SRHGE (Short Run High Grey level Emphasis), 
  LRLGE (Long Run Low Grey level Emphasis), 
  LRHGE (Long Run High Grey level Emphasis), 
  GLV   (Grey Level Variance), 
  RLV   (Run Length Variance)

GLSZM:
  SZE   (Small Zone Emphasis), 
  LZE   (Large Zone Emphasis), 
  GLN   (Grey Level Non-uniformity), 
  ZSN   (Zone Size Non-uniformity), 
  ZP    (Zone Percentage), 
  LGZE  (Low Grey level Zone Emphasis), 
  HGZE  (High Grey level Zone Emphasis), 
  SZLGE (Small Zone Low Grey Emphasis), 
  SZHGE (Small Zone High Grey Emphasis), 
  LZLGE (Large Zome Low Grey Emphasis), 
  LZHGE (Large Zone High Grey Emphasis), 
  GLV   (Grey Level Variance), 
  ZSV   (Zone SIze Variance)
  
NGTDM:
  Coarseness, 
  Contrast, 
  Busyness, 
  Complexity, 
  Strength


