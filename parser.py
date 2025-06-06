import ply.yacc as yacc
import sys

from lexer import tokens

# código VM
vm_code = []

# Tabela de símbolos para mapear variáveis para endereços
symbol_table = {}
next_address = 0  # Próximo endereço disponível na memória
label_counter = 0  # Contador para gerar rótulos únicos

# Tabela de procedimentos para armazenar informações sobre procedures
procedure_table = {}

# Função para adicionar instruções ao código VM
def emit(instruction):
    vm_code.append(instruction)

# Função para gerar rótulos únicos
def new_label(prefix):
    global label_counter
    label = f"{prefix}{label_counter}"
    label_counter += 1
    return label


# Função para determinar o tipo de uma expressão
def get_expression_type(expr):
    if isinstance(expr, int):
        return 'integer'
    elif isinstance(expr, float):
        return 'real'
    elif isinstance(expr, str):
        if expr in symbol_table:
            if isinstance(symbol_table[expr], dict):
                return symbol_table[expr]['type']
            else:
                return 'integer'
        else:
            return 'string'
    elif isinstance(expr, tuple):
        if expr[0] == 'array_element':
            var_name = expr[1]
            if var_name in symbol_table:
                return symbol_table[var_name]['element_type']
        elif expr[0] == 'binop':
            op = expr[1]
            if op in ['=', '<>', '<', '<=', '>', '>=', 'and', 'or']:
                return 'boolean'
            elif op in ['/', '*', '+', '-'] and (get_expression_type(expr[2]) == 'real' or get_expression_type(expr[3]) == 'real'):
                return 'real'
            else:
                return get_expression_type(expr[2])
    return 'unknown'


# Função para processar expressões e gerar código na ordem correta
def process_expression(expr):
    if isinstance(expr, int):
        emit(f"PUSHI {expr}")
    elif isinstance(expr, float):
        emit(f"PUSHF {expr}")
    elif isinstance(expr, str):
        if expr in symbol_table:
            var_info = symbol_table[expr]
            var_addr = var_info['address'] if isinstance(var_info, dict) else var_info
            emit(f"PUSHG {var_addr}")
        else:
            emit(f'PUSHS "{expr}"')
    elif isinstance(expr, tuple):
        if expr[0] == 'array_element':
            var_name = expr[1]
            index_expr = expr[2]
            
            # Processar a expressão do índice
            process_expression(index_expr)
            
            # Calcular o endereço do elemento
            entry = symbol_table[var_name]
            base_address = entry['address']
            lower_bound = entry['lower']
            
            emit(f"PUSHI {lower_bound}")
            emit("SUB")  # índice - lower_bound
            emit(f"PUSHI {base_address}")
            emit("ADD")  # base + (índice - lower_bound)
            emit("LOADN")  # Carrega o valor do endereço calculado
            
        elif expr[0] == 'binop':
            op = expr[1]
            left = expr[2]
            right = expr[3]
            
            # Processar operandos
            process_expression(left)
            process_expression(right)
            
            # Gerar código para operação
            left_type = get_expression_type(left)
            right_type = get_expression_type(right)
            using_real = (left_type == 'real' or right_type == 'real')
            
            if op == '+':
                emit("FADD" if using_real else "ADD")
            elif op == '-':
                emit("FSUB" if using_real else "SUB")
            elif op == '*':
                emit("FMUL" if using_real else "MUL")
            elif op == '/':
                emit("FDIV" if using_real else "DIV")
            elif op == 'div':
                emit("DIV")
            elif op == 'mod':
                emit("MOD")
            elif op == '=':
                emit("EQUAL")
            elif op == '<>':
                emit("EQUAL")
                emit("NOT")
            elif op == '<':
                emit("FINF" if using_real else "INF")
            elif op == '<=':
                emit("FINFEQ" if using_real else "INFEQ")
            elif op == '>':
                emit("FSUP" if using_real else "SUP")
            elif op == '>=':
                emit("FSUPEQ" if using_real else "SUPEQ")
            elif op == 'and':
                emit("AND")
            elif op == 'or':
                emit("OR")


# Função para processar statements na ordem correta
def process_statement(stmt):
    if stmt is None:
        return
        
    if isinstance(stmt, tuple):
        if stmt[0] == 'assignment':
            var = stmt[1]
            expr = stmt[2]
            
            # Processar a expressão primeiro
            process_expression(expr)
            
            # Armazenar o resultado
            if isinstance(var, tuple) and var[0] == 'array_element':
                var_name = var[1]
                index_expr = var[2]
                
                # Calcular endereço do elemento do array
                process_expression(index_expr)
                entry = symbol_table[var_name]
                base_address = entry['address']
                lower_bound = entry['lower']
                
                emit(f"PUSHI {lower_bound}")
                emit("SUB")
                emit(f"PUSHI {base_address}")
                emit("ADD")
                emit("STOREN")
            else:
                if var not in symbol_table:
                    raise SyntaxError(f"Variável '{var}' não declarada")
                var_info = symbol_table[var]
                addr = var_info['address'] if isinstance(var_info, dict) else var_info
                emit(f"STOREG {addr}")
                
        elif stmt[0] == 'writeln':
            expr_list = stmt[1]
            for expr in expr_list:
                process_expression(expr)
                expr_type = get_expression_type(expr)
                
                if expr_type == 'integer' or expr_type == 'boolean':
                    emit("WRITEI")
                elif expr_type == 'real':
                    emit("WRITEF")
                elif expr_type == 'string':
                    emit("WRITES")
                else:
                    emit("WRITEI")
            emit("WRITELN")
            
        elif stmt[0] == 'readln':
            var = stmt[1]
            emit("READ")
            
            if isinstance(var, tuple) and var[0] == 'array_element':
                emit("ATOI")
                var_name = var[1]
                index_expr = var[2]
                
                # Calcular endereço do elemento
                process_expression(index_expr)
                entry = symbol_table[var_name]
                base_address = entry['address']
                lower_bound = entry['lower']
                
                emit(f"PUSHI {lower_bound}")
                emit("SUB")
                emit(f"PUSHI {base_address}")
                emit("ADD")
                emit("STOREN")
            else:
                if var not in symbol_table:
                    raise SyntaxError(f"Variável '{var}' não declarada")
                
                var_info = symbol_table[var]
                var_addr = var_info['address'] if isinstance(var_info, dict) else var_info
                var_type = var_info['type'] if isinstance(var_info, dict) else 'integer'
                
                if var_type == 'integer':
                    emit("ATOI")
                elif var_type == 'real':
                    emit("ATOF")
                elif var_type == 'boolean':
                    emit("ATOI")
                
                emit(f"STOREG {var_addr}")
                
        elif stmt[0] == 'procedure_call':
            proc_name = stmt[1]
            if proc_name not in procedure_table:
                raise SyntaxError(f"Procedimento '{proc_name}' não declarado")
            
            proc_info = procedure_table[proc_name]
            proc_label = proc_info['label']
            
            # Gerar chamada para o procedimento
            emit(f"PUSHA {proc_label}")
            emit("CALL")
                
        elif stmt[0] == 'if':
            condition = stmt[1]
            then_stmt = stmt[2]
            else_stmt = stmt[3] if len(stmt) > 3 else None
            
            # Processar condição
            process_expression(condition)
            
            if else_stmt:
                else_label = new_label("else")
                end_label = new_label("endif")
                
                emit(f"JZ {else_label}")
                process_statement(then_stmt)
                emit(f"JUMP {end_label}")
                emit(f"{else_label}:")
                process_statement(else_stmt)
                emit(f"{end_label}:")
            else:
                end_label = new_label("endif")
                emit(f"JZ {end_label}")
                process_statement(then_stmt)
                emit(f"{end_label}:")
                
        elif stmt[0] == 'while':
            condition = stmt[1]
            body = stmt[2]
            
            start_label = new_label("while")
            end_label = new_label("endwhile")
            
            emit(f"{start_label}:")
            process_expression(condition)
            emit(f"JZ {end_label}")
            process_statement(body)
            emit(f"JUMP {start_label}")
            emit(f"{end_label}:")
            
        elif stmt[0] == 'for':
            loop_var = stmt[1]
            start_expr = stmt[2]
            end_expr = stmt[3]
            body = stmt[4]
            direction = stmt[5] if len(stmt) > 5 else 'to'  # padrão é 'to'
        
            if loop_var not in symbol_table:
                raise SyntaxError(f"Variável '{loop_var}' não declarada")
        
            var_info = symbol_table[loop_var]
            var_addr = var_info['address'] if isinstance(var_info, dict) else var_info
        
            # Gerar rótulos
            start_label = new_label("for")
            end_label = new_label("endfor")
        
            # Processar valor inicial e armazenar na variável de controle
            process_expression(start_expr)
            emit(f"STOREG {var_addr}")
        
            # Processar valor final e armazenar em endereço temporário
            global next_address
            limit_addr = next_address
            next_address += 1
            emit("PUSHN 1")  # Reservar espaço para o limite
        
            process_expression(end_expr)
            emit(f"STOREG {limit_addr}")
        
            # Início do loop
            emit(f"{start_label}:")
        
            # Verificar condição baseada na direção
            emit(f"PUSHG {var_addr}")    # valor da variável de controle
            emit(f"PUSHG {limit_addr}")  # valor limite
        
            if direction == 'to':
                emit("INFEQ")
                emit(f"JZ {end_label}")
            else:  # downto
                emit("SUPEQ")
                emit(f"JZ {end_label}")
        
            # Executar corpo do loop
            process_statement(body)
        
            # Incrementar ou decrementar variável de controle
            emit(f"PUSHG {var_addr}")
            if direction == 'to':
                emit("PUSHI 1")
                emit("ADD")
            else:
                emit("PUSHI 1")
                emit("SUB")
            emit(f"STOREG {var_addr}")
        
            emit(f"JUMP {start_label}")
            emit(f"{end_label}:")
            
        elif stmt[0] == 'compound':
            stmt_list = stmt[1]
            for s in stmt_list:
                process_statement(s)
    elif isinstance(stmt, list):
        for s in stmt:
            process_statement(s)


# Programa principal
def p_program(p):
    """program : PROGRAM ID SEMICOLON block DOT"""
    p[0] = ('program', p[2], p[4])


# Bloco principal
def p_block(p):
    """block : declarations procedures BEGIN statements END"""
    p[0] = ('block', p[4])
    for stmt in p[4]:
        process_statement(stmt)
    emit("STOP")


# Declarações de procedimentos
def p_procedures(p):
    """procedures : procedure_declaration procedures
                  | empty"""
    if len(p) == 3:
        p[0] = [p[1]] + p[2]
    else:
        p[0] = []


def p_procedure_declaration(p):
    """procedure_declaration : PROCEDURE ID SEMICOLON procedure_block SEMICOLON"""
    proc_name = p[2]
    proc_label = new_label(f"proc{proc_name}")
    
    # Adicionar procedimento à tabela de procedimentos
    procedure_table[proc_name] = {
        'label': proc_label,
        'body': p[4]
    }
    
    # Gerar um JUMP para pular o código do procedimento durante a execução principal
    jump_label = new_label("skipproc")
    emit(f"JUMP {jump_label}")
    
    # Gerar o rótulo e o código do procedimento
    emit(f"{proc_label}:")
    process_statement(p[4])
    emit("RETURN")
    
    # Rótulo para continuar após o procedimento
    emit(f"{jump_label}:")
    
    p[0] = ('procedure', proc_name, p[4])


def p_procedure_block(p):
    """procedure_block : declarations BEGIN statements END"""
    p[0] = ('compound', p[3])


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
                emit(f"PUSHN {size}")
                next_address += size
            else:
                symbol_table[var] = {
                    'address': next_address,
                    'type': p[3]
                }
                emit("PUSHN 1")
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
            | REAL
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
                 | procedure_call
                 | empty"""
    p[0] = p[1]


# Chamada de procedimento como statement separado
def p_procedure_call(p):
    """procedure_call : ID"""
    # Durante o parsing, assumir que é uma chamada de procedimento
    # A verificação será feita durante o processamento
    p[0] = ('procedure_call', p[1])


# Bloco de código composto (begin/end)
def p_compound_statement(p):
    """compound_statement : BEGIN statements END"""
    p[0] = ('compound', p[2])


# Atribuição
def p_assignment(p):
    """assignment : variable ASSIGN expression"""
    p[0] = ('assignment', p[1], p[3])


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
        
        p[0] = ('array_element', var_name, p[3])


# Comando writeln
def p_writeln(p):
    """writeln : WRITELN LPAREN expression_list RPAREN"""
    p[0] = ('writeln', p[3])


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


# Comando if
def p_if_statement(p):
    """if_statement : IF expression THEN statement
                    | IF expression THEN statement ELSE statement"""
    if len(p) == 5:
        p[0] = ('if', p[2], p[4])
    else:
        p[0] = ('if', p[2], p[4], p[6])


# Comando while
def p_while_statement(p):
    """while_statement : WHILE expression DO statement"""
    p[0] = ('while', p[2], p[4])


# Comando for
def p_for_statement(p):
    """for_statement : FOR ID ASSIGN expression TO expression DO statement
                     | FOR ID ASSIGN expression DOWNTO expression DO statement"""
    if p[5] == 'to':
        p[0] = ('for', p[2], p[4], p[6], p[8], 'to')
    else:
        p[0] = ('for', p[2], p[4], p[6], p[8], 'downto')


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


def p_simple_expression(p):
    """simple_expression : term
                         | simple_expression PLUS term
                         | simple_expression MINUS term
                         | simple_expression OR term"""
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = ('binop', p[2], p[1], p[3])


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


def p_factor(p):
    """factor : variable
              | NUMBER
              | STRING_LITERAL
              | TRUE
              | FALSE
              | LPAREN expression RPAREN"""
    if len(p) == 2:
        if isinstance(p[1], int):
            p[0] = p[1]
        elif isinstance(p.slice[1].value, str) and p.slice[1].type == "STRING_LITERAL":
            p[0] = p[1]
        elif p.slice[1].type == "TRUE":
            p[0] = 1
        elif p.slice[1].type == "FALSE":
            p[0] = 0
        elif isinstance(p[1], tuple) and p[1][0] == 'array_element':
            p[0] = p[1]
        elif isinstance(p[1], str):
            p[0] = p[1]
    elif len(p) == 4:
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
    print("Parsing finalizado\nCódigo VM gerado")
    with open("cod_vm.txt", "w") as out_file:
        for line in vm_code:
            out_file.write(line + "\n")