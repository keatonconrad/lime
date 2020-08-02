from llvmlite import ir
import random
import string


class Program():
    def __init__(self, value):
        self.value = value

    def eval(self):
        for val in self.value:
            val.eval()


class Integer():
    def __init__(self, builder, module, value):
        self.builder = builder
        self.module = module
        self.value = value
        self.type = ir.IntType(32)

    def eval(self):
        i = ir.Constant(self.type, int(self.value))
        return i


class Double():
    def __init__(self, builder, module, value):
        self.builder = builder
        self.module = module
        self.value = value
        self.type = ir.DoubleType

    def eval(self):
        i = ir.Constant(self.type, float(self.value))
        return i


class BinaryOp():
    def __init__(self, builder, module, left, right):
        self.builder = builder
        self.module = module
        self.left = left
        self.right = right


class Sum(BinaryOp):
    def eval(self):
        if self.left.type == ir.DoubleType and self.right.type == ir.DoubleType:
            i = self.builder.fadd(self.left.eval(), self.right.eval())
            return i
        elif str(self.left.type).startswith('i') and str(self.right.type).startswith('i'):
            i = self.builder.add(self.left.eval(), self.right.eval())
            return i


class Sub(BinaryOp):
    def eval(self):
        i = self.builder.sub(self.left.eval(), self.right.eval())
        return i


class Div(BinaryOp):
    def eval(self):
        i = self.builder.udiv(self.left.eval(), self.right.eval())
        return i


class Mul(BinaryOp):
    def eval(self):
        i = self.builder.mul(self.left.eval(), self.right.eval())
        return i


class Mod(BinaryOp):
    def eval(self):
        i = self.builder.urem(self.left.eval(), self.right.eval())
        return i


class Print():
    def __init__(self, builder, module, printf, value):
        self.builder = builder
        self.module = module
        self.printf = printf
        self.value = value

    @staticmethod
    def get_random_string(length):
        letters = string.ascii_lowercase
        result_str = ''.join(random.choice(letters) for i in range(length))
        return result_str

    def eval(self):
        value = self.value.eval()

        # Declare argument list
        voidptr_ty = ir.IntType(8).as_pointer()
        fmt = "%i \n\0"
        c_fmt = ir.Constant(ir.ArrayType(ir.IntType(8), len(fmt)),
                            bytearray(fmt.encode("utf8")))
        global_fmt = ir.GlobalVariable(self.module, c_fmt.type, name=self.get_random_string(128))
        global_fmt.linkage = 'internal'
        global_fmt.global_constant = True
        global_fmt.initializer = c_fmt
        fmt_arg = self.builder.bitcast(global_fmt, voidptr_ty)

        # Call Print Function
        self.builder.call(self.printf, [fmt_arg, value])


class Variable():
    def __init__(self, builder, module, name):
        self.builder = builder
        self.module = module
        self.name = name

    def eval(self):
        variable = ir.GlobalVariable(self.module, ir.IntType(8), str(self.name))
        return variable

    def to_string(self):
        return str(self.name)
