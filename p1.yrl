Nonterminals expr statement function params.

Terminals 'integer' 'var' '+' '-' '*' '/' '(' ')' '=' ',' 'and' 'or' '=='.

Rootsymbol statement.

Left 100 '+'.
Left 100 '-'.
Left 100 'or'.
Left 200 '*'.
Left 200 '/'.
Left 200 'and'.
Unary 600 '-'.
Unary 600 '+'.

statement -> statement ',' statement : {sequence, '$1', '$3'}.
statement -> expr : '$1'.
statement -> function : '$1'.

expr -> expr '+' expr : {add, '$1', '$3'}.
expr -> expr '-' expr : {subtract, '$1', '$3'}.
expr -> expr '*' expr : {multiply, '$1', '$3'}.
expr -> expr '/' expr : {divide, '$1', '$3'}.
expr -> expr 'and' expr : {land, '$1', '$3'}.
expr -> expr 'or' expr : {lor, '$1', '$3'}.
expr -> expr '==' expr : {eq, '$1', '$3'}.
expr -> '(' expr ')' : '$2'.
expr -> '-' expr : {negative, '$2'}.
expr -> '+' expr : {positive, '$2'}.
expr -> 'integer' : '$1'.
expr -> 'var' '(' params ')' : {funccall, '$1', '$3'}.
expr -> 'var' '(' ')' : {funccall, '$1', nil}
expr -> 'var' : {funccall, '$1', nil}.

function -> 'var' '(' params ')' '=' expr : {funcdef, '$1', '$3', '$6'}.
function -> 'var' '(' ')' '=' expr : {funcdef, '$1', nil, '$5'}.

params -> 'var' ',' params : {param, '$1', '$3'}.
params -> 'var' : {param, '$1', nil}.