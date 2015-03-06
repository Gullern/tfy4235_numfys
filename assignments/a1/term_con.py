#!/usr/bin/env python

path_structure_file_name = ".structure";


#
# Raises error if string is not a bool
#
def bool_check(string):
    if (string not in ("True", "False")):
        raise ValueError;
    return string == "True";


#
# Removes surrounding quatation signs
# 
def extract_string(string):
    if (string[0] == '"' and string[-1] == '"'):
        return string[1:-1];i
    return string;


#
# Processing argument string into correct type
#
def process_argument(argument_string):
    for convert in [int, float, bool_check]:
        try:
            arg = convert(argument_string);
            return arg;
        except ValueError:
            pass;

    # If not int, float or bool, assume string
    return extract_string(argument_string);


#
# Handles calls to python script from terminal
#
def terminal_call(functions):
    import sys
    from collections import deque
    arguments = deque(sys.argv[1:]);
    sys_args = [];
    while(len(arguments) > 0 and len(arguments[0]) > 0 and arguments[0][0] == '-'):
        sys_args.append(process_argument(arguments.popleft()[1:]));
    while (len(arguments) != 0):
        func = arguments.popleft();
        func_args = [];
        while(len(arguments) > 0 and len(arguments[0]) > 0 and arguments[0][0] == '-'):
            func_args.append(process_argument(arguments.popleft()[1:]));
        try:
            functions[func](*func_args);
        except KeyError:
            print('TermCon fatal error: Cannot find function named: ' + func);
            exit();


#
# Returns a dictionary containing local pahts
#
def get_path_structure():
    paths = {};
    fid = open(path_structure_file_name, 'r');
    lines = fid.readlines();
    for line in lines:
        if (line.strip() == ''):
            continue;
        data = line.split('=');
        if (len(data) != 2):
            print('Parsing fatal error: invalid file syntax on\n' + str(data));
            exit();
        paths[data[0].strip()] = extract_string(data[1].strip());
    return paths;

