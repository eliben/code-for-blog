import numpy as np

def translate_einsum(subscript):
    """Translates an explicit np.einsum subscript into Python code.
    """
    # split by ->
    ins_s, out = map(str.strip, subscript.split('->'))
    inputs = list(map(str.strip, ins_s.split(',')))

    lines = []
    indent = 0
    def emit_line(line=''):
        prefix = ' ' * indent
        lines.append(prefix + line)
    
    # We have len(inputs) input arrays to the emitted function; their names
    # will be '__a', '__b', '__c', etc.
    input_names = ['__'+chr(ord('a') + i) for i in range(len(inputs))]

    emit_line(f"def calc({', '.join(input_names)}):")
    indent += 4

    # Input dimension sizes: for each input label in the subscript, create
    # a variable that holds the size of that dimension. For example, if we
    # have 'ij' for the first input, we'll create:
    #  i_size = a.shape[0]
    #  j_size = a.shape[1]
    #
    # If such a variable already exists (because the same label appears in
    # multiple inputs), we'll assert it's the same and reuse it.
    dim_sizes = set()

    for input, input_name in zip(inputs, input_names):
        for i, label in enumerate(input):
            dim_size = f"{label}_size"
            if dim_size in dim_sizes:
                emit_line(f"assert {dim_size} == {input_name}.shape[{i}]")
            else:
                emit_line(f"{dim_size} = {input_name}.shape[{i}]")
                dim_sizes.add(dim_size)

    # Output array: create an array of the correct shape to hold the result.
    emit_line()
    emit_line(f"out = np.zeros(({', '.join(f'{label}_size' for label in out)}))")

    emit_line()
    saved_indent = indent
    # Loop over all indices in the output array
    for label in out:
        emit_line(f"for {label} in range({label}_size):")
        indent += 4

    # Create the indexing expressions for each input array and the output array.
    out_access = f'out[{', '.join(label for label in out)}]'
    in_accesses = [
        f'{input_name}[{', '.join(label for label in input)}]'
        for input, input_name in zip(inputs, input_names)
    ]

    # Determine if there are input dimensions that were not used in the output.
    # If so, we'll need to sum over them.
    if unused_dims := {label for label in ''.join(inputs)} - set(out):
        for label in unused_dims:
            emit_line(f"for {label} in range({label}_size):")
            indent += 4

        emit_line(f'{out_access} += {' * '.join(in_accesses)}')
    else:
        # If all input dimensions are used in the output, we can just assign
        # the product directly.
        emit_line(f'{out_access} = {' * '.join(in_accesses)}')

    indent = saved_indent
    emit_line('return out')
    return '\n'.join(lines)


if __name__ == "__main__":
    A = np.arange(6).reshape(2, 3)
    B = np.arange(12).reshape(3, 4)+1

    func_code = translate_einsum('ij,jk->ik')
    print(func_code)

    namespace = {'np': np}
    exec(func_code, namespace)
    print(namespace['calc'](A, B))
