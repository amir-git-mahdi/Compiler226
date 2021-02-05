import os
from Lexer import *
from Parser import *

global_symbol_table = SymbolTable()
global_symbol_table.set("NULL", Number(0))
global_symbol_table.set("FALSE", Number(0))
global_symbol_table.set("TRUE", Number(1))


def run(fn, text):
    # Generate tokens
    lexer = Lexer(fn, text)
    tokens, error = lexer.make_tokens()
    if error: return None, error

    # Generate AST
    parser = Parser(tokens)
    ast = parser.parse()
    if ast.error: return None, ast.error

    # Run program
    interpreter = Interpreter()
    context = Context('<program>')
    context.symbol_table = global_symbol_table
    result = interpreter.visit(ast.node, context)

    return result.value, result.error


if __name__ == '__main__':

    temp = ""
    lines = ""
    with open('./__int__/input.txt') as fr:
        
        lines = fr.read().splitlines()
        str_list = list(filter(None, lines))
    for i in lines:
        #text = input('basic > ')
        if("Printl" in i):
            temp = i.split(':')[1]
            with open('./output.txt', 'a') as fwl:
                fwl.write(temp + os.linesep)
           
        elif("Printv" in i):
            temp = i.split(':')[1]
            result, error = run('<stdin>', temp)
            with open('./output.txt', 'a') as fwv:
                fwv.write(str(result) + os.linesep)
                
        else:
            result, error = run('<stdin>', i)
            if error:
                with open('./output.txt', 'a') as fwe:
                    fwe.write(str(error))	
       # elif result:
        #    print(result)
