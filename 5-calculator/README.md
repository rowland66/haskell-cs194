# Homework #5 - Calculator

This solution goes beyond the homework assignment to provide a command line calculator using the Haskeline library and the State monad to store variable values. The calculator can be used to solve mathmatical expressions by simply entering the expression at the command line
```
% (4+2)*3
18
%
```

Commands are prefixed with a ':', and the calculator supports 4 command:
```
s - set the value of a variable e.g 'x=5'
d - display all variables that have been set
u - unset a variable
q - quit
```
