from GLCM import glcm
from GLRLM import glrlm
from GLSZM import glszm
from scipy.stats import skew, kurtosis

import numpy as np
from ngtdm import ngtdm2d, ngtdm3d
import math

class Matrix:
    ###############################################################################
    def crop_matrix(matrix):
        if matrix.ndim == 2:
            xs, ys = np.where(matrix != 0)
            return matrix[:max(xs)+1, :max(ys)+1]
        
        elif matrix.ndim == 1:
            xs = np.where(matrix !=0)
            return matrix[:max(xs)+1]
    ###############################################################################
    def process_array(array):
        array_ = array.copy()
                
        i_max = int(np.nanmax(array_))
        i_min = int(np.nanmin(array_))
        
        if np.sum(np.isnan(array_)) != 0 :
            array_[np.isnan(array_)] = i_min - 1
            
        return array_, i_min, i_max
        
    ###############################################################################
    def GLCM(image, normalise=True):
        """Calculates GLCM of 2D or 3D matrix """
        
        image_, i_min, i_max = Matrix.process_array(image)
            
        glcm_ = np.zeros((i_max+1, i_max+1))
        if image_.ndim == 2:
            glcm_ = glcm2d(image_, glcm_, i_max, i_min, image.shape[0], image.shape[1])
        elif image_.ndim == 3:
            glcm_ = glcm3d(image_, glcm_, i_max, i_min, image.shape[0], image.shape[1], image.shape[2])
        output = glcm_.copy()
        
        if normalise == True:
            output = output / np.sum(output)
        return output
    ###############################################################################
    def GLRLM(array_, normalise=True, weights=True):
        """Calculates GLRLM of 2D or 3D array """
        
        array_, minvalue, maxvalue =  Matrix.process_array(array_)
       
        if weights == True:
            s1 = 1.0
            s2 = math.sqrt(2.0)
            s3 = math.sqrt(3.0)
        else:
            s1 = 1.0
            s2 = 1.0
            s3 = 1.0
        
        if array_.ndim == 2:
            angles = np.array([[1, 0],
                               [0, 1],
                               [1, -1],
                               [1, 1]])
            
            s = np.array([s1, s1, s2, s2])
            
            x,y = array_.shape
            glrlm_sum = np.zeros((maxvalue+1, x*y+1))
            for idx in range(angles.shape[0]):
                glrlm_ = np.zeros((maxvalue+1, x*y+1))
                glrlm_ = glrlm2d(array_, glrlm_, x, y, maxvalue, minvalue, angles[idx,:], s[idx])
                glrlm_sum += glrlm_
            
        elif array_.ndim == 3:
            
            angles = np.array([[0,1,0],#0 matches [100]
                               [1,0,0],#1 matches [010]
                               [-1,1,0],#2 matches [110]
                               [1,1,0],#3 matches [-110]
                               [0,0,1],#4 matches [001]
                               [0,-1,1],# matches [101]
                               [0,1,1],#6 matches [-101]
                               [-1,0,1],#7 matches [011]
                               [1,0,1],#8 matches [0-11]
                               [-1, 1, 1],#9 matches [111]
                               [1, 1, 1],#10 matches [-111]
                               [-1, -1, 1],#11 matches [-11-1]
                               [1, -1, 1]])#12 matches [11-1]
            
    
            s = np.array([s1, s1, s2, s2, s1, s2, s2, s2,s2, s3, s3, s3, s3]) 
            
            x, y, z = array_.shape
            glrlm_sum = np.zeros((maxvalue+1, x*y*z+1))
            for idx in range(13):
                if idx in[0,1,2,3,4,5,6,7,8,9,10,11,12]:
                    glrlm_ = np.zeros((maxvalue+1, x*y*z+1))
                    glrlm_ = glrlm3d(array_, glrlm_, x, y, z, maxvalue, minvalue, angles[idx,:], s[idx])
                    glrlm_sum += glrlm_
    
    
        glrlm_sum = Matrix.crop_matrix(glrlm_sum)
        
        if normalise == True:
            glrlm_sum = glrlm_sum / np.sum(glrlm_sum)
        return glrlm_sum
    ###############################################################################
    def GLSZM(array_, normalise=True):
        """Calculates GLSZM of 2D or 3D array """
        
        array_, minvalue, maxvalue =  Matrix.process_array(array_)
        
        
        if array_.ndim == 2:
            x,y = array_.shape
            glszm_ = np.zeros((maxvalue+1, x*y+1))
            glszm_ = glszm2d(array_, glszm_, x, y, maxvalue, minvalue)
            
        elif array_.ndim == 3:
            x, y, z = array_.shape
            glszm_ = np.zeros((maxvalue+1, x*y*z+1))
            glszm_ = glszm3d(array_, glszm_, x, y, z, maxvalue, minvalue)
    
        glszm_ = Matrix.crop_matrix(glszm_)
        
        if normalise == True:
            glszm_ = glszm_ / np.sum(glszm_)
        return glszm_
    ###############################################################################
    def NGTDM(array_):   
        """Calculates NGTDM of 2D or 3D array """
        
        u_, c_ = np.unique(array_, return_counts=True)
        # if np.sum(np.isnan(u_)) != 0 :
        #     counts = counts[:-1]
        u_min = int(np.nanmin(u_))
        u_max = int(np.nanmax(u_)+1)
        u_min = min(0, u_min)
        counts = np.zeros((u_max-u_min))
        
        count_index = 0
        for unique in range(u_min, u_max):
            if unique in u_:
                counts[unique] = c_[count_index]
                count_index += 1
                
        
        array_, i_min, maxvalue = Matrix.process_array(array_)
        ngtdm_array = np.zeros(maxvalue+1).astype(float)
        
        if array_.ndim == 2:
            x, y = array_.shape
            ngtdm_ = ngtdm2d(array_.astype(float), ngtdm_array, x, y, maxvalue)
        elif array_.ndim == 3:
            x, y, z = array_.shape
            ngtdm_ = ngtdm3d(array_.astype(float), ngtdm_array, x, y, z, maxvalue)
            
        return ngtdm_, counts


###############################################################################
"""Texture feature functions """
###############################################################################
class features:

    global_names = ['Variance', 'Skewness', 'Kurtosis']
    glcm_names = ['Energy', 'Contrast', 'Entropy', 'Homogeneity', 'Correlation', 'Sum Average', 'Variance', 'Dissimilarity', 'Auto Correlation']
    glrlm_names = ['SRE', 'LRE', 'GLN', 'RLN', 'RP', 'LGRE', 'HGRE', 'SRLGE', 'SRHGE', 'LRLGE', 'LRHGE', 'GLV', 'RLV']
    glszm_names = ['SZE', 'LZE', 'GLN', 'ZSN', 'ZP', 'LGZE', 'HGZE', 'SZLGE', 'SZHGE', 'LZLGE', 'LZHGE', 'GLV', 'ZSV']
    ngtdm_names = ['Coarseness', 'Contrast', 'Busyness', 'Complexity','Strength']
          
    
    def Global(image):
        """
        Variance
        Skewness
        Kurtosis
        """
        array_flat = image.flatten()
        array_flat = array_flat[~np.isnan(array_flat)]
    
        f_ = np.zeros(3)
        f_[0] = np.var(array_flat)
        f_[1] = skew(array_flat)
        f_[2] = kurtosis(array_flat)
        
        return f_
    
    ###############################################################################
    def GLCM(glcm):       
        """GLCM Features: (9)
           0 Energy
           1 Contrast
           2 Entropy
           3 Homogeneity
           4 Correlation
           5 SumAverage
           6 Variance
           7 Dissimilarity
           8 AutoCorrelation
        """
        
        glcm_ = glcm.copy()
        f_ = np.zeros((9))
        
        glcm_[np.isnan(glcm_)] = 0
        
        glcm_ = glcm_ / np.sum(glcm_)
        
        glcm_height, glcm_width = glcm_.shape
        
        row_num, col_num = np.mgrid[0:glcm_height, 0:glcm_width] #i, j
        
        row_num = row_num +1
        col_num = col_num +1
        
        ij_diff = row_num-col_num
        row_g = row_num * glcm_
        col_g = col_num * glcm_
        
        indVect = np.arange(1,glcm_height+1)
        jndVect = np.arange(1, glcm_width+1)
        
        ui = np.sum(indVect * np.sum(glcm_, 1))
        uj = np.sum(jndVect * np.sum(glcm_, 0))
        
        row_ui = row_num - ui
        col_uj = col_num - uj
        
        """Energy """
        f_[0] = np.nansum(glcm_*glcm_)
        
        """Contrast """
        f_[1] = np.nansum(glcm_ * (ij_diff)*(ij_diff))
        
        """Entropy """
        E_ = glcm_.copy()
        E_[E_ == 0.0] = np.nan
        E_ = E_ * np.log2(E_)
        f_[2] = -1 * np.nansum(E_)
        
        """Homogeneity """
        f_[3] = np.nansum(glcm_ / (1 + (abs(ij_diff)))) 
        
        """Correlation """
        """See last feature, relies on variance """
        
        """Sum Average """
        coeff = (0.5 / (glcm_height * glcm_width))
        f_[5] =  coeff * np.nansum((row_g) + (col_g))
        
        """Variance """
    
        f_[6] = coeff * np.sum((row_ui)*(row_ui)*glcm_ + (col_uj)*(col_uj)*glcm_)
        
        """Dissimilarity """
        f_[7] = np.nansum(glcm_ * abs(ij_diff))
        
        """AutoCorrelation """
        f_[8] = np.nansum(row_g*col_num)
        
        """Correlation """
        f_[4] = np.sum(((row_ui) * (col_uj)) * glcm_) / (f_[6]+1E-10) / (glcm_height*glcm_width)
        
        return f_
    
    
    ###############################################################################
    def GLRLM(glrlm):
        """GLRLM Features: (13)
           0 SRE   (Short Run length Emphasis)
           1 LRE   (Long Run length Emphasis)
           2 GLN   (Grey Level Non-uniformity)
           3 RLN   (Run Length Non-uniformity)
           4 RP    (Run Percentage)
           5 LGRE  (Low Grey level Run Emphasis)
           6 HGRE  (High Grey level Run Emphasis)
           7 SRLGE (Short Run Low Grey level Emphasis)
           8 SRHGE (Short Run High Grey level Emphasis)
           9 LRLGE (Long Run Low Grey level Emphasis)
          10 LRHGE (Long Run High Grey level Emphasis)
          11 GLV   (Grey Level Variance)
          12 RLV   (Run Length Variance)
        """
        
        glrlm_ = glrlm.copy()
        glrlm_ = glrlm_ / np.sum(glrlm_)
        
        f_ = np.zeros((13))
        
    
        cvect = np.arange(1, glrlm_.shape[1]+1, dtype='float')
        rvect = np.arange(1, glrlm_.shape[0]+1, dtype='float')
        
        row_num, col_num = np.mgrid[0:glrlm_.shape[0], 0:glrlm_.shape[1]].astype(float) + 1
        
        row_squared = row_num * row_num
        col_squared = col_num * col_num
        cvect_squared = cvect * cvect
        rvect_squared = rvect * rvect
        
        glrlm_shape = glrlm_.shape[0] * glrlm_.shape[1]
        
        pg = np.sum(glrlm_, 1)
        pr = np.sum(glrlm_, 0)
        
        ug = np.sum((pg * rvect) / (glrlm_shape))
        ur = np.sum((pr * cvect) / (glrlm_shape))
        
        
        """SRE """
        f_[0] = np.sum(pr * (1/(cvect_squared)))
        
        """LRE """
        f_[1] = np.sum(pr * cvect_squared)
        
        """GLN """
        f_[2] = np.sum(pg*pg)
        
        """RLN """
        f_[3] = np.sum(pr*pr)
        
        """ RP """
        f_[4] = np.sum(pg / np.sum((pr*cvect)))
        
        """LGRE """
        f_[5] = np.sum(pg * (1/rvect_squared))
        
        """HGRE """
        f_[6] = np.sum(pg * rvect_squared)
        
        """SRLGE """
        rsi = 1 / row_squared
        csi = 1 / col_squared
        g_rsi = glrlm_ * rsi
        g_csi = glrlm_ * csi
        
        # f_[7] = np.sum(glrlm_*(1/row_squared)*(csi))
        f_[7] = np.sum(g_rsi * csi)
        
        """SRHGE """
        f_[8] = np.sum(g_csi * row_squared)
        
        """LRLGE """
        f_[9] = np.sum(g_rsi * col_squared)
        
        """LGHGE """
        f_[10] = np.sum(glrlm_*(row_squared)*(col_squared))
        
        """GLV """
        temp_ = (glrlm_*row_num)-ug
        f_[11] = np.sum(temp_ * temp_) / glrlm_shape
    
        
        """RLV """
        temp_ = (glrlm_*col_num)-ur
        f_[12] = np.sum(temp_ * temp_) / glrlm_shape
    
        
        return f_
    ###############################################################################
    def GLSZM(glszm):
        """GLSZM Features: (13)
           0 SZE   (Small Zone Emphasis)
           1 LZE   (Large Zone Emphasis)
           2 GLN   (Grey Level Non-uniformity)
           3 ZSN   (Zone Size Non-uniformity)
           4 ZP    (Zone Percentage)
           5 LGZE  (Low Grey level Zone Emphasis)
           6 HGZE  (High Grey level Zone Emphasis)
           7 SZLGE (Small Zone Low Grey Emphasis)
           8 SZHGE (Small Zone High Grey Emphasis)
           9 LZLGE (Large Zome Low Grey Emphasis)
          10 LZHGE (Large Zone High Grey Emphasis)
          11 GLV   (Grey Level Variance)
          12 ZSV   (Zone SIze Variance)
        """
        
        glszm_ = glszm.copy()
        glszm_ = glszm_ / np.sum(glszm_)
        
        f_ = np.zeros((13))
        
    
        cvect = np.arange(1, glszm_.shape[1]+1, dtype='float')
        rvect = np.arange(1, glszm_.shape[0]+1, dtype='float')
        
        row_num, col_num = np.mgrid[0:glszm_.shape[0], 0:glszm_.shape[1]].astype(float) + 1
        
        row_squared = row_num * row_num
        col_squared = col_num * col_num
        cvect_squared = cvect * cvect
        rvect_squared = rvect * rvect
        
        glszm_shape = glszm_.shape[0] * glszm_.shape[1]
        
        pg = np.sum(glszm_, 1)
        pr = np.sum(glszm_, 0)
        
        ug = np.sum((pg * rvect) / (glszm_shape))
        ur = np.sum((pr * cvect) / (glszm_shape))
        
        
        """SRE """
        f_[0] = np.sum(pr * (1/(cvect_squared)))
        
        """LRE """
        f_[1] = np.sum(pr * cvect_squared)
        
        """GLN """
        f_[2] = np.sum(pg*pg)
        
        """RLN """
        f_[3] = np.sum(pr*pr)
        
        """ RP """
        f_[4] = np.sum(pg / np.sum((pr*cvect)))
        
        """LGRE """
    
        f_[5] = np.sum(pg * (1/rvect_squared))
        
        """HGRE """
        f_[6] = np.sum(pg * rvect_squared)
        
        """SRLGE """
        rsi = np.reciprocal(row_squared)
        csi = np.reciprocal(col_squared)
        g_rsi = glszm_ * rsi
        g_csi = glszm_ * csi
        
        f_[7] = np.sum(g_rsi * csi)
        
        """SRHGE """
        f_[8] = np.sum(g_csi *row_squared)
        
        """LRLGE """
        f_[9] = np.sum(g_rsi*(col_squared))
        
        """LGHGE """
        f_[10] = np.sum(glszm_*(row_squared)*(col_squared))
        
        """GLV """
        temp_ = (glszm_*row_num)-ug
        f_[11] = np.sum(temp_ * temp_) / glszm_shape
    
        
        """RLV """
        temp_ = (glszm_*col_num)-ur
        f_[12] = np.sum(temp_ * temp_) / glszm_shape
    
        
        return f_
    ###############################################################################
    def NGTDM(ngtdm, counts):
        """NGTDM Features: (5)
           0 Coarseness
           1 Contrast
           2 Busyness
           3 Complexity
           4 Strength    
        """
        
        ngtdm_ = np.squeeze(ngtdm.copy())
        counts_ = counts.copy()
        
        f_ = np.zeros((5))
        
        ntotal = np.sum(counts)
        counts_ = np.squeeze(counts_ / ntotal)
        
        NG = len(np.where(ngtdm_ != 0)[0])
        
        pvalid = np.where(counts > 0)[0]
    
        
        row_num = np.arange(0, ngtdm.shape[0])
        
        countval_pval = np.array([counts_[x_] for x_ in pvalid])
        countval_ij = np.array([countval_pval + x_ for x_ in countval_pval])
        pval_minus_pval = np.array([pvalid - x_ for x_ in pvalid])
            
        """Coarseness """
        try:
            ngtdm_[ngtdm_==0] = np.nan
        except:
            print('Error calculating NGTDM coarseness')
            pass    
        f_[0] = 1 / np.nansum((counts_ * ngtdm_))
        
        """Contrast """
        temp_ = np.array([y_ - row_num for y_ in row_num])
        f_[1] = np.sum(np.array([x_ * counts_ for x_ in counts_]) * (temp_ * temp_))
        f_[1] = f_[1] * np.nansum(ngtdm_) / ((NG*(NG-1)*ntotal) + 1E-10)
        
        """Busyness """
        temp_ = (pvalid+1) * countval_pval
        temp2_ = np.array([temp_ - x_ for x_ in temp_])
        f_[2] = np.sum(np.abs(temp2_))
        f_[2] = np.nansum(counts_ * ngtdm_) / (f_[2] + 1E-10)
        
        """Complexity """
        temp_ = np.abs(pval_minus_pval) / (ntotal * countval_ij)
        ngtdm_val = np.array([ngtdm_[y_] for y_ in pvalid])
        temp2_ = countval_pval * ngtdm_val
        temp3_ = np.array([temp2_ + z_ for z_ in temp2_])
        f_[3] = np.nansum(temp_ * temp3_)
        
        """Strength """
        temp2_ = np.array([countval_pval + y_ for y_ in countval_pval])    
        valid_ = pval_minus_pval * pval_minus_pval
        f_[4] = np.nansum(temp2_ * valid_) / np.nansum(ngtdm_)
    
        return f_
    
    ###############################################################################
    def all_features(image):
        """Calculates gloabl, GLCM, GLRLM, GLSZM and NGTDM features """
        glcm_ = Matrix.GLCM(image)
        glrlm_ = Matrix.GLRLM(image)
        glszm_ = Matrix.GLSZM(image)
        ngtdm_, counts = Matrix.NGTDM(image)
        
        class Feature_class:
            pass
        
        fc = Feature_class()
        fc.first_order = features.Global(image)
        fc.glcm_features = features.GLCM(glcm_)
        fc.glrlm_features = features.GLRLM(glrlm_)
        fc.glszm_features = features.GLSZM(glszm_)
        fc.ngtdm_features = features.NGTDM(ngtdm_, counts)
        
        return fc
        
        
###############################################################################
"""Image rebinning """
###############################################################################
class Quantise:
    
    def rebin(image, levels):
        im2 = image.copy()
        im2 = np.round(im2)
        
        level_max = np.max(im2)
        level_min = np.min(im2)
        level_range = level_max - level_min
        level_range = level_range+1
        
        level_list = np.linspace(1, levels, levels)
        bin_list = level_list / levels
        bin_list = (bin_list*level_range) + level_min -1
        bin_list = np.round(bin_list)
        max_bin = np.max(bin_list)
        
        i=0
        for bins in bin_list:
            i = i+1
            im2[im2 <= bins] = i + max_bin
        
        im2 = im2 - max_bin

        return im2

    ###############################################################################
    def FBW(image_, bw=25):
        import numpy as np
        
        if np.sum(np.isnan(image_)) == 0:
            
            if np.min(image_ < -1000):
                print('[Warning] Minimum value in image less than -1000' )
                
            im_ = np.zeros_like(image_)
            g_levels = np.arange(-1000, int(np.max(image_)+1), bw)
             
            for i_ ,level in enumerate(g_levels):
                im_[(image_ >= level)*(image_ < level+bw)] = i_+1
       
        else:
            print('Array contains nans')
            image_[np.isnan(image_)] = -2000000.0
            im_ = image_.copy()
            g_levels = np.arange(-1000, int(np.nanmax(image_)+1), bw)
            
            for i_, level in enumerate(g_levels):
                im_[(image_ > -2000000)*(image_ >= level)*(image_ < level+bw)] = i_+1
            
            im_[im_==-2000000] = np.nan

        return im_



    ###############################################################################
    def FBWmin(image_, bw=25):
        if np.sum(np.isnan(image_)) == 0:
            
            im_ = np.zeros_like(image_)
            #g_levels = np.arange(-np.min(image_), int(np.max(image_)+1), bw)
            g_levels = np.arange(int(np.min(image_)), int(np.max(image_)+1), bw)
             
            for i_ ,level in enumerate(g_levels):
                im_[(image_ >= level)*(image_ < level+bw)] = i_+1
       
        else:
            print('Array contains nans')
            g_levels = np.arange(np.nanmin(image_), int(np.nanmax(image_)+1), bw)
            image_[np.isnan(image_)] = -2000000.0
            im_ = image_.copy()

            
            for i_, level in enumerate(g_levels):
                im_[(image_ > -2000000)*(image_ >= level)*(image_ < level+bw)] = i_+1
            
            im_[im_==-2000000] = np.nan

        return im_

    ###############################################################################
    def PETFBW(image_, bw=25):
        if np.sum(np.isnan(image_)) == 0:
            
            im_ = np.zeros_like(image_)
            g_levels = np.arange(0, int(np.max(image_)+1), bw)
             
            for i_ ,level in enumerate(g_levels):
                im_[(image_ >= level)*(image_ < level+bw)] = i_+1
       
        else:
            print('Array contains nans')
            im_ = image_.copy()
            im_[np.isnan(im_)] = -10.
            
            g_levels = np.arange(0, int(np.nanmax(im_)+1), bw)
            
            for i_, level in enumerate(g_levels):
                im_[(image_ > 0)*(image_ >= level)*(image_ < level+bw)] = i_+1
            
            im_[im_==-10.] = np.nan

        return im_

    ###############################################################################
    def PETFBWnan(image_, bw=25):
        import numpy as np
        print('Array contains nans')
        im_ = image_.copy()
        
        im_[np.isnan(im_)] = -10.
        g_levels = np.arange(0, int(np.nanmax(im_)+1), bw)
        
        for i_, level in enumerate(g_levels):
            im_[(image_ > 0)*(image_ >= level)*(image_ < level+bw)] = i_+1
        
        im_[im_==-10.] = np.nan
        return im_




































