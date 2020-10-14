```
program ::= extern_list decl_list
          | decl_list

extern_list ::= extern extern_list
              | extern
              
extern ::= "extern" fun_type IDENT "(" params ")" ";"

decl_list ::= decl decl_list
            | decl

decl ::= var_decl
       | fun_decl

var_decl ::= var_type IDENT ";"

fun_type ::= "void"
            | var_type

var_type ::= "int" 
           | "float" 
           | "bool"

fun_decl ::= fun_type IDENT "(" params ")" block

params ::= param_list
         | "void" 
         | epsilon

param_list ::= param "," param_list
             | param

param ::= var_type IDENT
block ::= "{" local_decls stmt_list "}"

local_decls ::= local_decl local_decls
              | epsilon

local_decl ::= var_type IDENT ";"

stmt_list ::= stmt stmt_list
            | epsilon

stmt ::= expr_stmt
       | block
       | if_stmt
       | while_stmt
       | return_stmt

expr_stmt ::= expr ";"
            | ";"

while_stmt ::= "while" "(" expr ")" stmt

if_stmt ::= "if" "(" expr ")" block else_stmt

else_stmt ::= "else" block
            | epsilon

return_stmt ::= "return" ";"
              | "return" expr ";"

expr ::= IDENT "=" expr
       | disj

disj  ::= conj _disj 
_disj ::= "||" conj _disj
        | epsilon  

conj  ::= equal "&&" _conj
_conj ::= "&&" equal _conj
        | epsilon

equal  ::= order _equal
_equal ::= "==" order _equal
         | "!=" order _equal
         | epsilon

order  ::= term _order
_order ::= "<=" term _order 
         | "<" term _order 
         | ">=" term _order 
         | ">" term _order
         | epsilon

term  ::= factor _term
_term ::= "+" factor _term 
        | "-" factor _term
        | epsilon

factor  ::= literal _factor
_factor ::= "*" literal _factor 
          | "/" literal _factor 
          | "%" literal _factor
          | epsilon

literal ::= "-" literal 
          | "!" literal
          | "(" expr ")"
          | IDENT | IDENT "(" args ")"
          | INT_LIT | FLOAT_LIT | BOOL_LIT

args ::= arg_list
       | epsilon

arg_list ::= expr "," arg_list
           | expr
```
