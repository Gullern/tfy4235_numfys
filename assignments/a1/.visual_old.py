#!/usr/bin/env python

####### Imports ##############
import numpy as np
from numpy import linspace

import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

from scipy.integrate import quad
##############################

#
# Fig label class
#
class figInfo:
    def __init__(self, title, xlabel, ylabel):
        self.title = title;
        self.xlabel = xlabel;
        self.ylabel = ylabel;


figures = {};
figures['gauss'] = figInfo('Guass', 'x', 'y');
figures['diffusion'] = figInfo('Potential energy', 'x', 'y');
figures['biased_diffusion'] = figInfo('Potential energy', 'x', 'y');
figures['potential_energy'] = figInfo('Potential energy', 'x', 'y');



#
# Graph data: index=x, data=y
#
def graph(file_name):
    (length, width, data) = read_matrix(file_name);
    plt.figure();
    x = linspace(0, length - 1, length);
    y = data;
    plt.plot(x, y);
    print('Generating plot: ' + file_name + '.png');
    save_figure(file_name);

#
# Reversed graph of data: index=y, data=x
#
def graph_r(file_name):
    (length, width, data) = read_matrix(file_name);
    plt.figure();
    y = linspace(0, length - 1, length);
    x = data;
    plt.plot(x, y);
    print('Generating reversed plot: ' + file_name + '.png');
    save_figure(file_name);

#
# Histogram of data
#
# Optional arguments:
#   - n_bins: number of bins (std=100)
#   - normalize: toggle normalization of data (std=false)
#
def hist(file_name, n_bins = 100, normalize = False):
    (length, width, data) = read_flat(file_name);
    print('bins: ' + str(n_bins) + ', normalize: ' + str(normalize));

    hist, bins = np.histogram(data, bins = n_bins, normed = normalize);
    plt.figure();
    #plt.hist(data, bins = bins);
    plt.plot(np.linspace(0, 1, n_bins), hist, 'ro');

    x = linspace(0, 1, 1001);
    f = lambda x: np.exp(-x * 10) / (4.17E-21 * (1 - np.exp(-10)));
    area = quad(f, 0.0, 1)[0];
    func = lambda x: f(x) / area;
    plt.plot(x, func(x), 'r');
    print('Generating histogram: ' + file_name + '.png');
    save_figure(file_name);

#
# Adds correct labels and saves figure. 
#
def save_figure(file_name):
    plt.title(figures[file_name].title);
    plt.xlabel(figures[file_name].xlabel);
    plt.ylabel(figures[file_name].ylabel);
    plt.savefig(paths['PATH_FIG'] + file_name + '.png');

#
# Read data as matrix
#
def read_matrix(file_name):
    print('Reading file: ' + str(file_name) + '.dat');
    print('Interpret: matrix');
    fid = open(paths['PATH_OUTPUT'] + file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0].split());
    data = [[float(element.strip()) for element in line.split()] for line in lines];
    print('lines: ' + str(length) + ', columns: ' + str(width));
    return (length, width, data);

#
# Read data flattened
#
def read_flat(file_name):
    print('Reading file: ' + str(file_name) + '.dat');
    print('Interpret: flattened');
    fid = open(paths['PATH_OUTPUT'] + file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0].split());
    data = [float(element.strip()) for line in lines for element in line.split()];
    print('lines: ' + str(length) + ', columns: ' + str(width));
    return (length, width, data);

import term_con
paths = term_con.get_path_structure();
term_con.terminal_call(locals());

