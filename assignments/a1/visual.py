#!/usr/bin/env python

############# Imports ###################
import numpy as np
from numpy import linspace

import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

from scipy.integrate import quad


############# Global defs ###############

#
# Fig label class
#
class figInfo:
    def __init__(self, title, xlabel, ylabel, legend):
        self.title = title;
        self.xlabel = xlabel;
        self.ylabel = ylabel;
        self.legend = legend;

# Dictionary of figure label information
figures = {};
figures['gauss'] = figInfo('Gaussian random numbers', 'Random number', 'Density', ['Generated', 'Theoretical distribution']);
figures['diffusion'] = figInfo('Potential energy', 'x', 'y', []);
figures['biased_diffusion'] = figInfo('Potential energy', 'x', 'y', []);
figures['potential_energy'] = figInfo('Potential energy', 'Energy U [eV]', 'Density', ['Simulation', 'Theoretical']);

# Dictionary of constants from logfile
constants = {};

############# Plot functions ############

#
# Plot checking if random numbers are Gaussian distributed. 
#
def gauss_check(n_bins = 40, normalize = True):
    # -- Load files
    file_name = 'gauss';
    (length, width, data) = read_flat(file_name);

    # -- Generate histogram
    xmax = max(np.max(data), - np.min(data));
    xmin = - xmax;
    x = np.linspace(xmin, xmax, 10000);
    (hist, bins) = np.histogram(data, bins = n_bins, normed = normalize);
    center = (bins[:-1] + bins[1:]) / 2.0;

    # -- Compare with theoretical distribution
    f = lambda x: np.exp(-x ** 2 / 2.0) / ((2 * np.pi) ** (1 / 2.0));

    # -- Produce plot
    plt.figure();
    plt.plot(center, hist, 'ro');
    plt.plot(x, f(x), 'b');
    print('Generating histogram: ' + file_name + '.png');
    save_figure(file_name);


def trajectories():
    return;

#
# Plots the potential energy as a histogram. 
#
def potential_energy(n_bins = 100, normalize = True):
    # -- Load files
    file_name = 'potential_energy';
    (length, width, data) = read_flat(file_name);
    read_logfile();
    print('bins: ' + str(n_bins) + ', normalize: ' + str(normalize));

    # -- Convert to dimensionful units
    data = [number * constants['DeltaU'] for number in data];

    # -- Generate histogram
    xmin = np.min(data);
    xmax = np.max(data);
    x = np.linspace(xmin, xmax, 10000);
    (hist, bins) = np.histogram(data, bins = n_bins, normed = normalize);
    center = (bins[:-1] + bins[1:]) / 2;

    # -- Compare with theoretical distribution
    f = lambda x: np.exp(-x / constants['kB_T']) / (constants['kB_T'] * (1 - np.exp(- constants['DeltaU'] / constants['kB_T'])));
    #area = quad(f, 0.0, 1)[0];
    #print(area);

    # -- Produce plot
    plt.figure();
    plt.plot(center, hist, 'ro');
    plt.plot(x, f(x), 'b');
    print('Generating histogram: ' + file_name + '.png');
    save_figure(file_name);


############# Generic functions #########

#
# Adds correct labels and saves figure. 
#
def save_figure(file_name):
    plt.title(figures[file_name].title);
    plt.xlabel(figures[file_name].xlabel);
    plt.ylabel(figures[file_name].ylabel);
    plt.legend(figures[file_name].legend);
    plt.savefig(paths['PATH_FIG'] + file_name + '.png');

#
# Read data as matrix
#
def read_matrix(file_name):
    print('Reading file: ' + file_name + '.dat');
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
    print('Reading file: ' + file_name + '.dat');
    print('Interpret: flattened');
    fid = open(paths['PATH_OUTPUT'] + file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0].split());
    data = [float(element.strip()) for line in lines for element in line.split()];
    print('lines: ' + str(length) + ', columns: ' + str(width));
    return (length, width, data);

#
# Load constants from logfile
#
def read_logfile():
    file_name = 'logfile.log';
    print('Reading logfile: ' + file_name);
    fid = open(paths['PATH_OUTPUT'] + file_name, 'r');

    while (fid.readline().strip() != 'Constants:'):
        pass;

    for line in fid.readlines():
        if (line.startswith('#')):
            continue;
        line = line.split('=');
        if (len(line) != 2):
            continue;
        constants[line[0].strip()] = float(line[1].strip());


############# Main ######################
import term_con
paths = term_con.get_path_structure();
term_con.terminal_call(locals());

