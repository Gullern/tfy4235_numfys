#!/usr/bin/env python

import numpy as np
from numpy import linspace
import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

from scipy.integrate import quad

def graph(file_name):
    (length, width, data) = read_matrix(file_name);
    plt.figure();
    x = linspace(0, length - 1, length);
    y = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def graph_r(file_name):
    (length, width, data) = read_matrix(file_name);
    plt.figure();
    y = linspace(0, length - 1, length);
    x = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def hist(file_name):
    (length, width, data) = read_flat(file_name);
    n_bins = 100;

    hist, bins = np.histogram(data, bins = n_bins, normed=True);
    plt.figure();
    #plt.hist(data, bins = bins);
    plt.plot(np.linspace(0, 1, n_bins), hist, 'ro');

    x = linspace(0, 1, 1001);
    f = lambda x: np.exp(-x * 10) / (4.17E-21 * (1 - np.exp(-10)));
    area = quad(f, 0.0, 1)[0];
    func = lambda x: f(x) / area;
    print(area);
    print(quad(func, 0.0, 1));
    plt.plot(x, func(x), 'r');
    #plt.axis([0, length, 0, iterations]);
    #plt.xlabel('$x$');
    #plt.ylabel('Count time');
    plt.savefig(file_name + '.png');

def read_matrix(file_name):
    fid = open(file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0].split());
    data = [[float(element.strip()) for element in line.split()] for line in lines];
    print('lines: ' + str(length) + ', columns: ' + str(width));
    return (length, width, data);

def read_flat(file_name):
    fid = open(file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0].split());
    data = [float(element.strip()) for line in lines for element in line.split()];
    print('lines: ' + str(length) + ', columns: ' + str(width));
    print('flattened');
    return (length, width, data);

import term_con
paths = term_con.get_path_structure();
term_con.terminal_call(locals());

