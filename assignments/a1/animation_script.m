% Animated histogram of particle data
% using MATLAB

% Asumes data is loaded into: particles

[len, number] = size(particles);
figure(1);
clf;
axis([-0.05, 0.05, 0, 1000]);
hold all;
for i = 1 : len
    cla(gca);
    hist(particles(i, :), -0.05:0.004:0.05);
    drawnow;
    pause(0.001);
end
