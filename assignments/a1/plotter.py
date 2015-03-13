#!/usr/bin/env python

############# Imports ###################
from matplotlib import pyplot as plt

############# Plotter ###################

class Plotter:
    """
    Provides functions for autmating figure
    plotting. 
    """
    def __init__(self, paths):
        self.paths = paths;
        self.figures = {}

    class FigInfo:
        """ Figure information class. """
        def __init__(self, title, xlabel, ylabel, legend):
            self.title = title;
            self.xlabel = xlabel;
            self.ylabel = ylabel;
            self.legend = legend;

    def add_figinfo(self, name, title, xaxis, yaxis, legend):
        """ Add figure information to dictionary. """
        figInfo = self.FigInfo(title, xaxis, yaxis, legend);
        self.figures[name] = figInfo;

    def save_figure(self, file_name):
        """ Add correct label and save figure. """
        plt.title(self.figures[file_name].title);
        plt.xlabel(self.figures[file_name].xlabel);
        plt.ylabel(self.figures[file_name].ylabel);
        plt.legend(self.figures[file_name].legend);
        plt.savefig(self.paths['PATH_FIG'] + file_name + '.png');

    def read_matrix(self, file_name):
        """ Read data as matrix. """
        print('Reading file: ' + file_name + '.dat');
        print('Interpret: matrix');
        fid = open(self.paths['PATH_OUTPUT'] + file_name + '.dat', 'r');
        lines = fid.readlines();
        length = len(lines);
        width = len(lines[0].split());
        data = [[float(element.strip()) for element in line.split()] for line in lines];
        print('lines: ' + str(length) + ', columns: ' + str(width));
        return (length, width, data);

    def read_flat(self, file_name):
        """ Read data flattened. """
        print('Reading file: ' + file_name + '.dat');
        print('Interpret: flattened');
        fid = open(self.paths['PATH_OUTPUT'] + file_name + '.dat', 'r');
        lines = fid.readlines();
        length = len(lines);
        width = len(lines[0].split());
        data = [float(element.strip()) for line in lines for element in line.split()];
        print('lines: ' + str(length) + ', columns: ' + str(width));
        return (length, width, data);

    def read_logfile(self):
        """ Load logfile. """
        # Include all logfile info later
        constants = {};
        file_name = 'logfile.log';
        print('Reading logfile: ' + file_name);
        fid = open(self.paths['PATH_OUTPUT'] + file_name, 'r');

        while (fid.readline().strip() != 'Constants:'):
            pass;

        for line in fid.readlines():
            if (line.startswith('#')):
                continue;
            line = line.split('=');
            if (len(line) != 2):
                continue;
            constants[line[0].strip()] = float(line[1].strip());
        return constants;

