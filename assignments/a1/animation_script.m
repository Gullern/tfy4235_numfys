% Animated histogram of particle data
% using MATLAB

% Asumes data is loaded into: particles

[len, number] = size(particles);
figure(1);
clf;
x = -1.2 : 0.1 : 1.2;
axis([x(1), x(end), 0, 30]);
hold all;
for i = 1 : len
    disp(i);
    cla(gca);
    hist(particles(i, :), x);
    drawnow;
    pause(0.001);
end
