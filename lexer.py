import ply.lex as lex
import sys

# Lista de tokens
tokens = [
    # Palavras reservadas primeiro para garantir precedência
    'PROGRAM',
    'VAR',
    'BEGIN',
    'END',
    'FUNCTION',
    'PROCEDURE',
    'IF',
    'THEN',
    'ELSE',
    'WHILE',
    'DO',
    'AND',
    'OR',
    'FOR',
    'TO',
    'DOWNTO',
    'WRITELN',
    'READLN',
    'INTEGER',
    'BOOLEAN',
    'STRING',
    'TRUE',
    'FALSE',
    'DIV',
    'MOD',
    'ARRAY',
    'OF',
    # Outros tokens
    'ID',
    'NUMBER',
    'STRING_LITERAL',
    'PLUS',
    'MINUS',
    'TIMES',
    'DIVIDE',
    'EQUAL',
    'NE',
    'LT',
    'LE',
    'GT',
    'GE',
    'ASSIGN',
    'LPAREN',
    'RPAREN',
    'LBRACKET',
    'RBRACKET',
    'COLON',
    'SEMICOLON',
    'COMMA',
    'DOT',
    'DOTDOT'
]

# Expressões regulares para palavras reservadas (com precedência sobre ID)
def t_PROGRAM(t):
    r'program'
    return t

def t_VAR(t):
    r'var'
    return t

def t_BEGIN(t):
    r'begin'
    return t

def t_END(t):
    r'end'
    return t

def t_FUNCTION(t):
    r'function'
    return t

def t_PROCEDURE(t):
    r'procedure'
    return t

def t_IF(t):
    r'if'
    return t

def t_THEN(t):
    r'then'
    return t

def t_ELSE(t):
    r'else'
    return t

def t_WHILE(t):
    r'while'
    return t

def t_DO(t):
    r'do'
    return t

def t_AND(t):
    r'and'
    return t

def t_OR(t):
    r'or'
    return t

def t_FOR(t):
    r'for'
    return t

def t_TO(t):
    r'to'
    return t

def t_DOWNTO(t):
    r'downto'
    return t

def t_WRITELN(t):
    r'writeln'
    return t

def t_READLN(t):
    r'readln'
    return t

def t_INTEGER(t):
    r'integer'
    return t

def t_BOOLEAN(t):
    r'boolean'
    return t

def t_STRING(t):
    r'string'
    return t

def t_TRUE(t):
    r'true'
    return t

def t_FALSE(t):
    r'false'
    return t

def t_DIV(t):
    r'div'
    return t

def t_MOD(t):
    r'mod'
    return t

def t_ARRAY(t):
    r'array'
    return t

def t_OF(t):
    r'of'
    return t

# Regra para identificadores - deve ter precedência menor que as palavras reservadas
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

# Regras para outros tokens
def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)  # Converte para inteiro
    return t

def t_STRING_LITERAL(t):
    r'\'([^\\\']|\\.)*\''
    t.value = t.value[1:-1]  # Remove as aspas simples
    return t

# Tokens simples
t_PLUS = r'\+'
t_MINUS = r'-'
t_TIMES = r'\*'
t_DIVIDE = r'/'
t_EQUAL = r'='
t_NE = r'<>'
t_LT = r'<'
t_LE = r'<='
t_GT = r'>'
t_GE = r'>='
t_ASSIGN = r':='
t_LPAREN = r'\('
t_RPAREN = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_COLON = r':'
t_SEMICOLON = r';'
t_COMMA = r','
t_DOT = r'\.'
t_DOTDOT = r'\.\.'

# Ignorar espaços e tabulações
t_ignore = ' \t'

# Ignorar comentários de uma linha e multilinhas
def t_COMMENT(t):
    r'\{[^}]*\}|\(\*[^*]*\*\)'
    pass

# Contar linhas
def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

# Erro
def t_error(t):
    print(f"Caractere ilegal '{t.value[0]}', linha {t.lineno}")
    t.lexer.skip(1)

# Construir o lexer
lexer = lex.lex()

if __name__ == "__main__":
    ficheiro_test = sys.argv[1]
    with open(ficheiro_test, 'r') as f:
        data = f.read()
    lexer.input(data)
    for tok in lexer:
        print(tok)