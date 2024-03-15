grammar Toy;

file
    :   (
            funcDeclarations+=funcDecl |
            funcDefinitions+=funcDef
        )+
    ;

funcDecl
    :   'extern' 'def' ident=Ident '(' params=paramList ')' (':' returnType=Ident)? ';'
    ;

funcDef
    :   'def' ident=Ident '(' params=paramList ')' (':' returnType=Ident)? body=block
    ;

paramList
    :   (params+=param (',' params+=param)* ','?)?
    ;

param
    :   ident=Ident ':' type=Ident      # paramDecl
    |   '...'                           # paramVarArg
    ;

block
    :   '{' stmts+=statement* '}'
    ;

statement
    :   expr=expression ';'                                             # stmtExpr
    |   'return' expr=expression? ';'                                   # stmtReturn
    |   'let' ident=Ident ':' type=Ident '=' expr=expression ';'        # stmtLet
    |   stmt=ifStatement                                                # stmtIf
    |   'while' condition=expression body=block                         # stmtWhile
    ;

ifStatement
    :   'if' condition=expression ifBranch=block ('else' (elseIf=ifStatement|elseBranch=block))?
    ;

expression
    :   literal=IntLiteral                                                  # exprIntLiteral
    |   literal=FloatLiteral                                                # exprFloatLiteral
    |   literal=BoolLiteral                                                 # exprBoolLiteral
    |   literal=CharLiteral                                                 # exprCharLiteral
    |   literal=StringLiteral                                               # exprStringLiteral
    |   ident=Ident                                                         # exprIdentifier
    |   '(' expr=expression ')'                                             # exprParen
    |   ident=Ident '(' args+=expression (',' args+=expression)* ','? ')'   # exprCall
    |   op=('+'|'-') expr=expression                                        # exprPosNeg
    |   '!' expr=expression                                                 # exprNot
    |   lhs=expression op=('+'|'-') rhs=expression                          # exprAddSub
    |   lhs=expression op=('*'|'/'|'%') rhs=expression                      # exprMulDivMod
    |   lhs=expression op=('<'|'>'|'<='|'>='|'=='|'!=') rhs=expression      # exprComparison
    |   lhs=expression op=('&&'|'||') rhs=expression                        # exprLogical
    ;

Ident
    :   [a-zA-Z_][a-zA-Z_0-9]*
    ;

FloatLiteral
    :   [0-9]+ '.' [0-9]+
    ;

IntLiteral
    :   [0-9]+
    ;

BoolLiteral
    : 'true' | 'false'
    ;

CharLiteral
    : '\'' (EscapeSequence | ~['\\])* '\''
    ;

StringLiteral
    : '"' (EscapeSequence | ~["\\])* '"'
    ;

EscapeSequence
    : '\\' [btnfr"\\]
    ;

Whitespace: [ \t\r\n]+ -> skip;

Comment
    : '/*' (Comment | .)*? '*/' -> skip
    ;

LineComment
    : '//' (~[\r\n])* -> skip
    ;
