#!/usr/bin/env python

# Reads and plots data from:
file_name = 'diffusion';

import numpy
from numpy import linspace
import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

def graph():
    (length, data) = read_data();
    plt.figure();
    x = linspace(0, length - 1, length);
    y = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def graph_r():
    (length, data) = read_data();
    plt.figure();
    y = linspace(0, length - 1, length);
    x = data;
    plt.plot(x, y);
    plt.savefig(file_name + '.png');

def hist():
    (length, data) = read_data();
    bins = 20;
    plt.figure();
    plt.hist(data, bins = bins);
    #plt.axis([0, length, 0, iterations]);
    #plt.xlabel('$x$');
    #plt.ylabel('Count time');
    plt.savefig(file_name + '.png');

def read_data():
    fid = open(file_name + '.dat', 'r');
    lines = fid.readlines();
    length = len(lines);
    data = [float(line.strip()) for line in lines];
    print('lines: ' + str(length));
    return (length, data);

def main():
    graph_r();

import sys
print sys.argv;

print locals()['graph'];

main();

