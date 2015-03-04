#!/usr/bin/env python

path_structure_file_name = ".structure";

def terminal_call(functions):
    import sys
    from collections import deque
    arguments = deque(sys.argv[1:]);
    sys_args = [];
    while(len(arguments) > 0 and len(arguments[0]) > 0 and arguments[0][0] == '-'):
        sys_args.append(arguments.popleft()[1:]);
    while (len(arguments) != 0):
        func = arguments.popleft();
        func_args = []
        while(len(arguments) > 0 and len(arguments[0]) > 0 and arguments[0][0] == '-'):
            func_args.append(arguments.popleft()[1:]);
        try:
            functions[func](*func_args);
        except KeyError:
            print('TermCon Error: Cannot find function named: ' + func);
            exit();

def get_path_structure():
    paths = {};
    fid = open(path_structure_file_name, 'r');
    lines = fid.readlines();
    for line in lines:
        if (line.strip() == ''):
            continue;
        data = line.split('=');
        if (len(data) != 2):
            print('Parse error: invalid file syntax on\n' + str(data));
            exit();
        paths[data[0].strip()] = data[1].strip();
    return paths;

