from ply.lex import lex
from ply.yacc import yacc

t_PLUS           = r'\+'
t_MINUS          = r'-'
t_DIVIDE         = r'/'
t_TIMES          = r"\*"
t_LPAREN         = r'\('
t_RPAREN         = r'\)'
t_LBRACEBRACKET  = r'{'   #Brace Brackets
t_RBRACEBRACKET  = r"}"
t_COMMA          = r","
t_LBOXBRACKETS   = r'\['  #Box Brackets
t_RBOXBRACKETS   = r'\]'
t_SEMICOLON      = r'\;'
t_COLON          = r'\:'
t_DOT            = r'\.'        
t_QUOTATION      = r'\''         
t_DOUBLEQUOTATION= r'\"'  #double quotes
t_MOD            = r'\%'        
t_LESS           = r'<'   #Less than
t_GREATER        = r'>'   #Greater than
t_EQUAL          = r"=="
t_ASSIGNMENT     = r"="
t_NOTEQUAL       = r"!="
t_LNOT           = r"!"  #logical not
t_OR             = r"\|\|"
t_AND            = r"&&"
t_LESSEQUAL      = r"<="  #less than or equal
t_GREATEREQUAL   = r">="  #Greater than or equal
t_LINECOMMENT    = r'\/\/'#line comment
t_OCOMMENT       = r'\/\*'#open comment
t_CCOMMENT       = r'\*\/'#close comment
t_INCREMENT      = r'\+\+'
t_DECREMENT      = r"\-\-"

reserved = {
    'abstract'  : 'ABSTRACT_KEYWORD',
    'assert'    : 'ASSERT_KEYWORD',
    'boolean'   : 'BOOLEAN_KEYWORD',
    'break'     : 'BREAK_KEYWORD',
    'byte'      : 'BYTE_KEYWORD',
    'case'      : 'CASE_KEYWORD',
    'char'      : 'CHAR_KEYWORD',
    'catch'     : 'CATCH_KEYWORD',
    'class'     : 'CLASS_KEYWORD',
    'const'     : 'CONST_KEYWORD',
    'continue'  : 'CONTINUE_KEYWORD',
    'default'   : 'DEFAULT_KEYWORD',
    'do'        : 'DO_KEYWORD',
    'double'    : 'DOUBLE_KEYWORD',
    'else'      : 'ELSE_KEYWORD',
    'enume'     : 'ENUME_KEYWORD',
    'extends'   : 'EXTENDS_KEYWORD',
    'finally'   : 'FINALLY_KEYWORD',
    'false'     : 'FALSE_KEYWORD',
    'float'     : 'FLOAT_KEYWORD',
    'final'     : 'FINAL_KEYWORD',
    'for'       : 'FOR_KEYWORD',
    'goto'      : 'GOTO_KEYWORD',
    'int'       : 'INT_KEYWORD',  
    'interface' : 'INTERFACE_KEYWORD',
    'import'    : 'IMPORT_KEYWORD',
    'if'        : 'IF_KEYWORD',
    'instanceof': 'INSTANCEOF_KEYWORD',
    'implements': 'IMPLEMENTS_KEYWORD',
    'long'      : 'LONG_KEYWORD',
    'native'    : 'NATIVE_KEYWORD',
    'new'       : 'NEW_KEYWORD',
    'null'      : 'NULL_KEYWORD',
    'package'   : 'PACKAGE_KEYWORD',
    'public'    : 'PUBLIC_KEYWORD',
    'private'   : 'PRIVATE_KEYWORD',
    'protected' : 'PROTECTED_KEYWORD',
    'return'    : 'RETURN_KEYWORD',
    'short'     : 'SHORT_KEYWORD',
    'static'    : 'STATIC_KEYWORD',
    'strictfp'  : 'STRICTFP_KEYWORD',
    'super'     : 'SUPER_KEYWORD',
    'switch'    : 'SWITCH_KEYWORD',
    'synchronized':'SYNCHRONIZED_KEYWORD',
    'this'      : 'THIS_KEYWORD',
    'throw'     : 'THROW_KEYWORD',
    'throws'    : 'THROWS_KEYWORD',
    'transient' : 'TRANSIENT_KEYWORD',
    'true'      : 'TRUE_KEYWORD',
    'try'       : 'TRY_KEYWORD',
    'void'      : 'VOID_KEYWORD',    
    'volatile'  : 'VOLATILE_KEYWORD',
    'while'     : 'WHILE_KEYWORD',
}

tokens = ['NUMBER','LPAREN','RPAREN','LBRACEBRACKET',
'RBRACEBRACKET','COMMA','LBOXBRACKETS','RBOXBRACKETS',
'PLUS','MINUS','TIMES','DIVIDE','SEMICOLON','LESS',
'GREATER','QUOTATION','MOD','DOUBLEQUOTATION','EQUAL',
'NOTEQUAL','AND','OR','COLON','DOT','LESSEQUAL',
'ASSIGNMENT' ,'GREATEREQUAL','LINECOMMENT','OCOMMENT',
'INCREMENT','DECREMENT','LNOT','CCOMMENT',
'ID'] + list(reserved.values())

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)    
    return t

def t_ID(t):
     r'[a-zA-Z_][a-zA-Z_0-9]*'
     t.type = reserved.get(t.value,'ID')    
     return t  
 
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
 
t_ignore  = ' \t'
 
def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

precedence = (
('right', 'ASSIGNMENT'),
('left', 'OR'),
('left', 'AND'),
('left', 'NOTEQUAL', 'EQUAL'),
('left', 'GREATER', 'LESS', 'GREATEREQUAL', 'LESSEQUAL'),
('left', 'PLUS', 'MINUS', 'PLUS'),
('left', 'TIMES', 'DIVIDE', 'MOD'),
('right','NEW_KEYWORD', 'LNOT','MINUS'),
('left', 'DOT')
)

def p_program(p):
    '''program : classdeclaration'''

def p_classdeclaration(p):
    '''classdeclaration : CLASS_KEYWORD ID LBRACEBRACKET inside_metode RBRACEBRACKET'''

def p_inside_metode(p):
    '''inside_metode : list inside_metode 
                     | list'''

def p_list(p):
    '''list : fielddeclaration 
            | methoddeclaration'''

def p_fielddeclaration(p):
    '''fielddeclaration : declarators ID'''

def p_methoddeclaration(p):
    '''methoddeclaration : declarators ID LPAREN parameterlist RPAREN LBRACEBRACKET listt RBRACEBRACKET
                         | declarators ID LPAREN RPAREN LBRACEBRACKET listt RBRACEBRACKET'''

def p_inside_metodee(p):
    '''inside_metodee : statement inside_metodee
                      | empty'''

def p_listt(p):
    '''listt : inside_metodee  RETURN_KEYWORD expression SEMICOLON 
             | inside_metodee'''

def p_declarators(p):
    '''declarators : PUBLIC_KEYWORD STATIC_KEYWORD type
                   | PRIVATE_KEYWORD STATIC_KEYWORD type
                   | PUBLIC_KEYWORD type
                   | PRIVATE_KEYWORD type
                   | STATIC_KEYWORD type
                   | type'''

def p_type(p):
    '''type : primtype 
            | arrtype'''

def p_primtype(p):
    '''primtype : INT_KEYWORD 
                | BOOLEAN_KEYWORD 
                | VOID_KEYWORD'''

def p_arrtype(p):
    '''arrtype : INT_KEYWORD  LBOXBRACKETS RBOXBRACKETS
               | ID  LBOXBRACKETS RBOXBRACKETS'''

def p_parameterlist(p):
    '''parameterlist : type ID inside_metodeee
                     | type ID '''

def p_inside_metodeee(p):
    '''inside_metodeee : COMMA type ID inside_metodeee
                       | COMMA type ID '''

def p_argumentlist(p):
    '''argumentlist : expression inside_metodeeee
                    | expression '''

def p_inside_metodeeee(p):
    '''inside_metodeeee : COMMA expression inside_metodeeee
                        | COMMA expression '''

def p_reference(p):
    '''reference : THIS_KEYWORD inside_metod
                 | ID inside_metod
                 | THIS_KEYWORD
                 | ID'''

def p_inside_metod(p):
    '''inside_metod : DOT ID inside_metod
                    | DOT ID'''

def p_statement(p):
    '''statement : LBRACEBRACKET inside_metodd RBRACEBRACKET 
                 | type ID ASSIGNMENT expression SEMICOLON
                 | reference ASSIGNMENT expression SEMICOLON
                 | reference LBOXBRACKETS  expression RBOXBRACKETS ASSIGNMENT expression SEMICOLON
                 | reference LPAREN RPAREN SEMICOLON
                 | reference LPAREN argumentlist RPAREN SEMICOLON
                 | IF_KEYWORD LPAREN expression RPAREN statement
                 | IF_KEYWORD LPAREN expression RPAREN statement ELSE_KEYWORD statement 
                 | WHILE_KEYWORD LPAREN expression RPAREN statement'''

def p_inside_metodd(p):
    '''inside_metodd : statement inside_metodd
                     | empty'''

def p_expression(p):
    '''expression : reference LBOXBRACKETS expression RBOXBRACKETS  
                  | reference LPAREN RPAREN
                  | reference LPAREN argumentlist RPAREN 
                  | MINUS expression 
                  | LNOT expression 
                  | binop 
                  | factor
                  | ID
                  | LPAREN expression RPAREN  
                  | TRUE_KEYWORD
                  | FALSE_KEYWORD  
                  | NEW_KEYWORD ID LPAREN RPAREN 
                  | INT_KEYWORD LBOXBRACKETS expression RBOXBRACKETS
                  | ID LBOXBRACKETS expression RBOXBRACKETS '''

# def p_term(p):
#     '''term : LBOXBRACKETS expression RBOXBRACKETS term
#             | LBOXBRACKETS expression RBOXBRACKETS'''

def p_factor(p):
    '''factor : NUMBER '''

def p_binop(p):
    '''binop : expression PLUS expression
             | expression MINUS expression
             | expression TIMES expression
             | expression DIVIDE expression
             | expression AND expression
             | expression OR expression
             | expression LESS expression
             | expression LESSEQUAL expression
             | expression GREATER expression
             | expression GREATEREQUAL expression
             | expression EQUAL expression
             | expression NOTEQUAL expression'''

def p_empty(p):
    'empty : '
    pass
 
def p_error(p):
    print(f'Syntax error at {p.value!r}') 

lexer = lex()
parser = yacc(debug=True)

data = '''public class Main {
  public static void main(String[] args) {
    int x = 5;
    System.out.println(x > 3 & x < 10); 
  }
}'''

ast = parser.parse(data,debug = True)
