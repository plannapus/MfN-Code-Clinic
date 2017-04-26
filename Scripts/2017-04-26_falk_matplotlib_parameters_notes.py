"""
in order of execution priority:
(i)   matplotlibrc file
(ii)  custom styles
(iii) manual RC parameter configuration

"""

#######################

### (i) matplotlibrc
# find the default local matplotlibrc file:
# (root)# find / -name "*matplotlibrc*" 2> /dev/null
# for example here: /usr/lib/python3.6/site-packages/matplotlib/mpl-data/matplotlibrc
# copy it here:
# ~/.config/matplotlib/matplotlibrc
#
# open it in text editor and manipulate the default configuration.

### (ii) Styles!!
# custom styles can go to ~/.config/matplotlib/stylelib
# originals in /usr/lib/python3.6/site-packages/matplotlib/mpl-data/stylelib
# in python:
#     print(MPP.style.available)
#     MPP.style.use('seaborn-paper')

### (iii) Manual rc configuration
# in python:
#     MP.rcParams['font.size'] = 10
## or:
#     MPP.rc('font',**{'family': 'Iwona', 'size': 10})
