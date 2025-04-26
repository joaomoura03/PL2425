import ply.lex as lex
import sys

# Palavras-chave reservadas
reserved = {
    'program': 'PROGRAM',
    'var': 'VAR',
    'begin': 'BEGIN',
    'end': 'END',
    'function': 'FUNCTION',
    'procedure': 'PROCEDURE',
    'if': 'IF',
    'then': 'THEN',
    'else': 'ELSE',
    'while': 'WHILE',
    'do': 'DO',
    'and': 'AND',
    'or': 'OR',
    'for': 'FOR',
    'to': 'TO',
    'downto': 'DOWNTO',
    'writeln': 'WRITELN',
    'readln': 'READLN',
    'integer': 'INTEGER',
    'boolean': 'BOOLEAN',
    'string': 'STRING',
    'true': 'TRUE',
    'false': 'FALSE',
    'div': 'DIV',
    'mod': 'MOD',
    'array': 'ARRAY',
    'of': 'OF'
}

# Lista de tokens
tokens = [
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
] + list(reserved.values())

# Regras para tokens simples
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

# Regras para tokens mais complexos
def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value.lower(), 'ID')  # Verifica se é uma palavra-chave
    return t

def t_NUMBER(t):
    r'\d+'
    t.value = int(t.value)  # Converte para inteiro
    return t

def t_STRING_LITERAL(t):
    r'\'([^\\\']|\\.)*\''
    t.value = t.value[1:-1]  # Remove as aspas simples
    return t

# Ignorar espaços e tabulações
t_ignore = ' \t\n'

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