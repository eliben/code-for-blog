import ast
import functools
import inspect

def astjit(func):
    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        source = inspect.getsource(func)
        tree = ast.parse(source)
        print(ast.dump(tree, indent=4))
        return func(*args, **kwargs)
    return wrapper
