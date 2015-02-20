#!/usr/bin/env python

import numpy
from numpy import linspace
import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

def graph(file_name):
    (length, width, data) = read_data(file_name);
    plt.figure();
    x = linspace(0, length - 1, length);
    y = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def graph_r(file_name):
    (length, width, data) = read_data(file_name);
    plt.figure();
    y = linspace(0, length - 1, length);
    x = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def hist(file_name):
    (length, width, data) = read_data(file_name);
    bins = 20;
    plt.figure();
    plt.hist(data, bins = bins);
    #plt.axis([0, length, 0, iterations]);
    #plt.xlabel('$x$');
    #plt.ylabel('Count time');
    plt.savefig(file_name + '.png');

def read_data(file_name):
    fid = open(file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    width = len(lines[0]);
    data = [[float(element.strip()) for element in line.split()] for line in lines]
    print('lines: ' + str(length) + ', columns: ' + str(width));
    return (length, width, data);

import term_con
term_con.terminal_call(locals());

