```
<program> ::= <externs> <decls>
            | <decls>

<externs> ::= <extern> <externs>
            | <extern>

<extern> ::= "extern" <fun_sign> ";"

<fun_sign> ::= <fun_type> IDENT "(" <params> ")"

<decls> ::= <decl> <decls>
          | <decl>

<decl> ::= <var_decl>
         | <fun_decl>

<var_decl> ::= <var_type> IDENT ";"

<fun_decl> ::= <fun_sign> <block_stmt>

<var_type> ::= "int" 
             | "float" 
             | "bool"

<fun_type> ::= "void"
             | <var_type>

<params> ::= <param_list>
           | "void" 
           | ""

<param_list> ::= <param> "," <param_list>
               | <param>

<param> ::= <var_type> IDENT

<local_decls> ::= <var_decl> <local_decls>
                | ""
              
<stmt_list> ::= <stmt> <stmt_list>
              | ""

<stmt> ::= <block_stmt>
         | <if_stmt>
         | <while_stmt>
         | <return_stmt>
         | <expr_stmt>

<block_stmt> ::= "{" <local_decls> <stmt_list> "}"

<while_stmt> ::= "while" "(" <expr> ")" <stmt>

<if_stmt> ::= "if" "(" <expr> ")" <block_stmt> <else_stmt>

<else_stmt> ::= "else" <block_stmt>
              | ""

<return_stmt> ::= "return" ";"
                | "return" <expr> ";"

<expr_stmt> ::= <expr> ";"
              | ";"

<args> ::= <arg_list>
         | ""

<arg_list> ::= <expr> "," <arg_list>
             | <expr>

<expr> ::= IDENT "=" <expr>
         | <disj>

<disj>  ::= <conj> <_disj>
<_disj> ::= "||" <conj> <_disj>
          | ""  

<conj>  ::= <equal> "&&" <_conj>
<_conj> ::= "&&" <equal> <_conj>
          | ""

<equal>  ::= <order> <_equal>
<_equal> ::= "==" <order <_equal>
           | "!=" <order> <_equal>
           | ""

<order>  ::= <term> <_order>
<_order> ::= "<=" <term> <_order>
           | "<" <term> <_order>
           | ">=" <term> <_order>
           | ">" <term> <_order>
           | ""

<term>  ::= <factor> <_term>
<_term> ::= "+" <factor> <_term> 
          | "-" <factor> <_term>
          | ""

<factor>  ::= <literal> <_factor>
<_factor> ::= "*" <literal> <_factor> 
            | "/" <literal> <_factor> 
            | "%" <literal> <_factor>
            | ""

<literal> ::= "-" <literal> 
            | "!" <literal>
            | "(" <expr> ")"
            | IDENT 
            | IDENT "(" <args> ")"
            | INT_LIT 
            | FLOAT_LIT 
            | BOOL_LIT
```
