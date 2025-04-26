import ply.yacc as yacc
import sys

from lexer import tokens

# Lista para armazenar o código VM gerado
vm_code = []

# Tabela de símbolos para mapear variáveis para endereços
symbol_table = {}
next_address = 0  # Próximo endereço disponível na memória
label_counter = 0  # Contador para gerar rótulos únicos

# Função para adicionar instruções ao código VM
def emit(instruction):
    vm_code.append(instruction)

# Função para gerar rótulos únicos
def new_label(prefix):
    global label_counter
    label = f"{prefix}_{label_counter}"
    label_counter += 1
    return label

# Regras da gramática

# Programa principal
def p_program(p):
    """program : PROGRAM ID SEMICOLON block DOT"""
    p[0] = ('program', p[2], p[4])
    emit("STOP")  # Finaliza o programa

# Bloco principal
def p_block(p):
    """block : declarations BEGIN statements END"""
    p[0] = ('block', p[3])

# Declarações de variáveis
def p_declarations(p):
    """declarations : VAR var_declaration_list
                    | empty"""
    p[0] = p[2] if len(p) == 3 else []

def p_var_declaration_list(p):
    """var_declaration_list : var_declaration SEMICOLON var_declaration_list
                            | var_declaration SEMICOLON"""
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]

def p_var_declaration(p):
    """var_declaration : id_list COLON type"""
    global next_address
    p[0] = ('var', p[1], p[3])

    for var in p[1]:
        if var not in symbol_table:
            if isinstance(p[3], dict) and p[3].get('type') == 'array':
                size = p[3]['upper'] - p[3]['lower'] + 1
                symbol_table[var] = {
                    'address': next_address,
                    'type': 'array',
                    'lower': p[3]['lower'],
                    'upper': p[3]['upper'],
                    'element_type': p[3]['element_type']
                }
                emit(f"PUSHN {size}")  # Reserva espaço para o array
                next_address += size
            else:
                symbol_table[var] = next_address
                emit("PUSHN 1")  # Variável simples
                next_address += 1

def p_id_list(p):
    """id_list : ID
               | ID COMMA id_list"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

def p_array_type(p):
    """array_type : ARRAY LBRACKET NUMBER DOTDOT NUMBER RBRACKET OF type"""
    p[0] = {
        'type': 'array',
        'lower': p[3],
        'upper': p[5],
        'element_type': p[8]
    }

def p_type(p):
    """type : INTEGER
            | BOOLEAN
            | STRING
            | array_type"""
    p[0] = p[1]

# Lista de statements
def p_statements(p):
    """statements : statement SEMICOLON statements
                  | statement"""
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        p[0] = [p[1]]

# Statement individual
def p_statement(p):
    """statement : assignment
                 | writeln
                 | readln
                 | if_statement
                 | while_statement
                 | for_statement
                 | compound_statement
                 | empty"""
    p[0] = p[1]

# Bloco de código composto (begin/end)
def p_compound_statement(p):
    """compound_statement : BEGIN statements END"""
    p[0] = ('compound', p[2])

# Atribuição
def p_assignment(p):
    """assignment : variable ASSIGN expression"""
    p[0] = ('assignment', p[1], p[3])
    
    # Verificar se é uma atribuição a um elemento de array
    if isinstance(p[1], tuple) and p[1][0] == 'array_element':
        var_name = p[1][1]
        # O valor está no topo da pilha
        # O endereço do elemento do array já está calculado na pilha
        emit("STOREN")
    else:
        # Atribuição a variável simples
        var_name = p[1]
        if var_name not in symbol_table:
            raise SyntaxError(f"Variável '{var_name}' não declarada")
        emit(f"STOREG {symbol_table[var_name]}")

# Variável (simples ou elemento de array)
def p_variable(p):
    """variable : ID
                | ID LBRACKET expression RBRACKET"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        # Elemento de array
        var_name = p[1]
        if var_name not in symbol_table:
            raise SyntaxError(f"Array '{var_name}' não declarado")
        
        entry = symbol_table[var_name]
        if not isinstance(entry, dict) or entry.get('type') != 'array':
            raise SyntaxError(f"'{var_name}' não é um array")
        
        base_address = entry['address']
        lower_bound = entry['lower']
        
        # Calcula o endereço do elemento: base + (índice - lower_bound)
        # A expressão do índice já está na pilha
        emit(f"PUSHI {lower_bound}")
        emit("SUB")  # índice - lower_bound
        emit(f"PUSHI {base_address}")
        emit("ADD")  # base + (índice - lower_bound)
        
        p[0] = ('array_element', var_name)

# Comando writeln
def p_writeln(p):
    """writeln : WRITELN LPAREN expression_list RPAREN"""
    p[0] = ('writeln', p[3])
    
    # Processar cada expressão na lista e escrever
    for _ in range(len(p[3])):
        emit("WRITES")
    
    emit("WRITELN")

def p_expression_list(p):
    """expression_list : expression
                       | expression COMMA expression_list"""
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = [p[1]] + p[3]

# Comando readln
def p_readln(p):
    """readln : READLN LPAREN variable RPAREN"""
    p[0] = ('readln', p[3])
    
    if isinstance(p[3], tuple) and p[3][0] == 'array_element':
        # Ler para um elemento de array - o endereço já está na pilha
        emit("READ")     # Lê um valor da entrada
        emit("STOREN")   # Armazena no endereço calculado
    else:
        # Ler para uma variável simples
        var_name = p[3]
        if var_name not in symbol_table:
            raise SyntaxError(f"Variável '{var_name}' não declarada")
        emit("READ")     # Lê um valor da entrada
        emit(f"STOREG {symbol_table[var_name]}")  # Armazena o valor na variável

# Comando if
def p_if_statement(p):
    """if_statement : IF expression THEN statement
                    | IF expression THEN statement ELSE statement"""
    if len(p) == 5:
        end_label = new_label("end_if")
        
        # Após a avaliação da expressão, decide se salta
        emit(f"JZ {end_label}")  # Se a expressão for falsa, salta para o fim
        
        # Aqui estaria o código do THEN (já processado)
        
        emit(f"{end_label}:")  # Marca o fim do if
        
        p[0] = ('if', p[2], p[4])
    else:
        else_label = new_label("else")
        end_label = new_label("end_if")
        
        # Avalia expressão e decide
        emit(f"JZ {else_label}")  # Se a expressão for falsa, salta para o else
        
        # Aqui estaria o código do THEN (já processado)
        
        emit(f"JUMP {end_label}")  # Após executar o then, salta o else
        emit(f"{else_label}:")     # Marca o início do else
        
        # Aqui estaria o código do ELSE (já processado)
        
        emit(f"{end_label}:")      # Marca o fim do if-else
        
        p[0] = ('if', p[2], p[4], p[6])

# Comando while
def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    start_label = new_label("while_start")
    end_label = new_label("while_end")
    
    emit(f"{start_label}:")  # Início do loop
    
    # A expressão de condição já foi processada em p[2]
    
    emit(f"JZ {end_label}")  # Se a condição for falsa, salta para o fim
    
    # O corpo do loop já foi processado em p[4]
    
    emit(f"JUMP {start_label}")  # Volta ao início para testar a condição
    emit(f"{end_label}:")        # Marca o fim do loop
    
    p[0] = ('while', p[2], p[4])

# Comando for
def p_for_statement(p):
    """for_statement : FOR ID ASSIGN expression TO expression DO statement"""
    loop_var = p[2]
    
    if loop_var not in symbol_table:
        raise SyntaxError(f"Variável '{loop_var}' não declarada")
    
    start_label = new_label("for_start")
    end_label = new_label("for_end")
    
    # Inicializa o contador com a expressão inicial (já na pilha)
    emit(f"STOREG {symbol_table[loop_var]}")
    
    # Guarda o valor limite em uma variável temporária
    global next_address
    limit_addr = next_address
    next_address += 1
    emit(f"STOREG {limit_addr}")
    
    # Início do loop
    emit(f"{start_label}:")
    
    # Teste: se contador > limite, sai do loop
    emit(f"PUSHG {symbol_table[loop_var]}")  # Empilha o contador
    emit(f"PUSHG {limit_addr}")              # Empilha o limite
    emit("SUP")                              # Testa contador > limite
    emit(f"JZ {end_label}")                  # Se contador > limite, sai do loop
    
    # Corpo do loop (statement) - já processado em p[8]
    
    # Incrementa o contador
    emit(f"PUSHG {symbol_table[loop_var]}")  # Empilha o contador
    emit("PUSHI 1")                          # Empilha o incremento
    emit("ADD")                              # Soma contador + 1
    emit(f"STOREG {symbol_table[loop_var]}") # Armazena o resultado no contador
    
    # Volta ao início
    emit(f"JUMP {start_label}")
    
    # Fim do loop
    emit(f"{end_label}:")
    
    p[0] = ('for', p[2], p[4], p[6], p[8])

# Expressões
def p_expression(p):
    """expression : simple_expression
                  | simple_expression EQUAL simple_expression
                  | simple_expression NE simple_expression
                  | simple_expression LT simple_expression
                  | simple_expression LE simple_expression
                  | simple_expression GT simple_expression
                  | simple_expression GE simple_expression"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])
        if p[2] == '=':
            emit("EQUAL")
        elif p[2] == '<>':
            emit("EQUAL")
            emit("NOT")
        elif p[2] == '<':
            emit("INF")
        elif p[2] == '<=':
            emit("INFEQ")
        elif p[2] == '>':
            emit("SUP")
        elif p[2] == '>=':
            emit("SUPEQ")

def p_simple_expression(p):
    """simple_expression : term
                         | simple_expression PLUS term
                         | simple_expression MINUS term
                         | simple_expression OR term"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])
        if p[2] == '+':
            emit("ADD")
        elif p[2] == '-':
            emit("SUB")
        elif p[2] == 'or':
            emit("OR")

def p_term(p):
    """term : factor
            | term TIMES factor
            | term DIVIDE factor
            | term DIV factor
            | term MOD factor
            | term AND factor"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])
        if p[2] == '*':
            emit("MUL")
        elif p[2] == '/':
            emit("DIV")
        elif p[2] == 'div':
            emit("DIV")
        elif p[2] == 'mod':
            emit("MOD")
        elif p[2] == 'and':
            emit("AND")

def p_factor(p):
    """factor : variable
              | NUMBER
              | STRING_LITERAL
              | TRUE
              | FALSE
              | LPAREN expression RPAREN"""
    if len(p) == 2:
        if isinstance(p[1], int):
            # Número
            emit(f"PUSHI {p[1]}")
        elif isinstance(p.slice[1].value, str) and p.slice[1].type == "STRING_LITERAL":
            # String literal
            emit(f'PUSHS "{p[1]}"')
        elif p.slice[1].type == "TRUE":
            emit("PUSHI 1")
        elif p.slice[1].type == "FALSE":
            emit("PUSHI 0")
        elif isinstance(p[1], tuple) and p[1][0] == 'array_element':
            # Elemento de array - endereço já está na pilha
            emit("LOADN")
        elif isinstance(p[1], str):
            # Variável simples
            if p[1] not in symbol_table:
                raise SyntaxError(f"Variável '{p[1]}' não declarada")
            emit(f"PUSHG {symbol_table[p[1]]}")
    elif len(p) == 4:
        # Expressão entre parênteses - já processada em p[2]
        p[0] = p[2]

# Regras auxiliares
def p_empty(p):
    """empty :"""
    p[0] = None

# Erro de sintaxe
def p_error(p):
    if p:
        print(f"Erro de sintaxe em '{p.value}', linha {p.lineno}")
    else:
        print("Erro de sintaxe no final do arquivo")

# Construir o parser
parser = yacc.yacc()

if __name__ == "__main__":
    ficheiro_test = sys.argv[1]
    with open(ficheiro_test, 'r') as f:
        data = f.read()
    result = parser.parse(data)
    print("Parsing finalizado. Código VM gerado:")
    for line in vm_code:
        print(line)