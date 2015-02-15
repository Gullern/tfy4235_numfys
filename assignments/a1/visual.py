# Reads and plots data from:
file_name = 'gauss.dat';

import numpy
from numpy import linspace
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True)

def main():
    fid = open(file_name, 'r');

    lines = fid.readlines();
    length = len(lines);
    data = [float(line.strip()) for line in lines];

    bins = 20;
    plt.figure(figsize=(5,5));
    plt.hist(data, bins = bins);
    #plt.axis([0, length, 0, iterations])
    #plt.xlabel('$x$');
    #plt.ylabel('Count time');
    plt.savefig('gauss.png');

main();

