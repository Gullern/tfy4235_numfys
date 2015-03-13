#!/usr/bin/env python

############# Imports ###################

# Scipy and numpy
import numpy as np
from scipy.integrate import quad

# Matplotlib
import matplotlib
matplotlib.use('Agg');
import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True);

# Modules
import term_con
from plotter import Plotter


############# Global ####################

# Initiate plotter
plotter = Plotter(term_con.get_path_structure());

# Dictionary of constants from logfile
constants = plotter.read_logfile();

# Figure label information
plotter.add_figinfo('gauss', 'Gaussian random numbers', 'Random number', 'Density', ['Generated', 'Theoretical distribution']);
plotter.add_figinfo('diffusion', 'Potential energy', 'x', 'y', []);
plotter.add_figinfo('biased_diffusion', 'Potential energy', 'x', 'y', []);
plotter.add_figinfo('potential_energy', 'Potential energy', 'Energy U [eV]', 'Density', ['Simulation', 'Theoretical']);


############# Plot functions ############

def gauss_check(n_bins = 40, normalize = True):
    """ Plot random numbers compared to Gaussian distribution. """
    # Load files
    file_name = 'gauss';
    (length, width, data) = plotter.read_flat(file_name);

    # Generate histogram
    xmax = max(np.max(data), - np.min(data));
    xmin = - xmax;
    x = np.linspace(xmin, xmax, 10000);
    (hist, bins) = np.histogram(data, bins = n_bins, normed = normalize);
    center = (bins[:-1] + bins[1:]) / 2.0;

    # Compare with theoretical distribution
    f = lambda x: np.exp(-x ** 2 / 2.0) / ((2 * np.pi) ** (1 / 2.0));

    # Produce plot
    plt.figure();
    plt.plot(center, hist, 'ro');
    plt.plot(x, f(x), 'b');
    print('Generating histogram: ' + file_name + '.png');
    plotter.save_figure(file_name);


def trajectories():
    return;

def potential_energy(n_bins = 100, normalize = True):
    """ Plot the potential energy as a histogram. """
    # Load files
    file_name = 'potential_energy';
    (length, width, data) = plotter.read_flat(file_name);
    plotter.read_logfile();
    print('bins: ' + str(n_bins) + ', normalize: ' + str(normalize));

    # Convert to dimensionful units
    data = [number * constants['DeltaU'] for number in data];

    # Generate histogram
    xmin = np.min(data);
    xmax = np.max(data);
    x = np.linspace(xmin, xmax, 10000);
    (hist, bins) = np.histogram(data, bins = n_bins, normed = normalize);
    center = (bins[:-1] + bins[1:]) / 2;

    # Compare with theoretical distribution
    f = lambda x: np.exp(-x / constants['kB_T']) / (constants['kB_T'] * (1 - np.exp(- constants['DeltaU'] / constants['kB_T'])));
    #area = quad(f, 0.0, 1)[0];
    #print(area);

    # Produce plot
    plt.figure();
    plt.plot(center, hist, 'ro');
    plt.plot(x, f(x), 'b');
    print('Generating histogram: ' + file_name + '.png');
    plotter.save_figure(file_name);


############# Main ######################
term_con.terminal_call(locals());

