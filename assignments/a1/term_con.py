#!/usr/bin/env python

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
            print('Error! Cannot find function named:' + func);

