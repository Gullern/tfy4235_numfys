file_name = 'mutations.dat';

from numpy import linspace

import matplotlib
matplotlib.use('Agg')

import matplotlib.pyplot as plt
from matplotlib import rc
rc('text', usetex=True)


def main():
	fid = open(file_name, 'r');
	header = fid.readline().split(',');
	length = int(header[0].split('=')[1].strip());
	iterations = int(header[1].split('=')[1].strip());

	print(length, iterations);

	lines = fid.readlines();
	mutations = [int(line.strip()) for line in lines];

	x = mutations;
	y = linspace(1, iterations, iterations);

	plt.figure(figsize=(5,5));

	simple_plot = False;
	if (simple_plot):
		plt.scatter(x, y, s=2);
	else:
		max_skip = 1.0 * length / 10;
		for i in range(iterations - 1):
			if (abs(x[i] - x[i + 1]) < max_skip):
				plt.plot(x[i:i + 2], y[i:i + 2], 'k');

	plt.axis([0, length, 0, iterations])
	plt.xlabel('$x$');
	plt.ylabel('Count time');
	plt.savefig('mutations.png');


main();

