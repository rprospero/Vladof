#Base class for nodes in OpenCL syntax tree

import pyopencl as cl
import numpy
import numpy.linalg as la
import random
import string


ctx = cl.create_some_context()
queue = cl.CommandQueue(ctx)

mf = cl.mem_flags

class Node:
    text = ""
    params = {}
    def __add__(self,x):
        return self.binary(x,"+")
    def __sub__(self,x):
        return self.binary(x,"-")
    def __mul__(self,x):
        return self.binary(x,"*")
    def __mod__(self,x):
        return self.binary(x,"%")
    def __truediv__(self,x):
        return self.binary(x,"/")
    def binary(self,x,op):
        if not isinstance(x,Node):
            x = Data(x)
        result = Node()
        result.text = "(" + self.text + op + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result


class Data(Node):
    def __init__(self,data,name=None):
        if name is None:
            name = ''.join(random.choice(string.ascii_lowercase) for x in range(10))
        if type(data) is numpy.ndarray:
            buffer = cl.Buffer(ctx, mf.READ_ONLY | mf.COPY_HOST_PTR,
                               hostbuf=data)
            self.params = {name: buffer}
            self.text = name + "[gid]"
        else:
            self.params = {}
            self.text = str(data)
