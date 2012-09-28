#Base class for nodes in OpenCL syntax tree

import pyopencl as cl
import numpy
import random
import string
import math


ctx = cl.create_some_context()
queue = cl.CommandQueue(ctx)

mf = cl.mem_flags

def sin(x):
    if isinstance(x,Node):
        return x.sin()
    else:
        print(type(x))
        return math.sin(x)
def cos(x):
    if isinstance(x,Node):
        return x.cos()
    else:
        print(type(x))
        return math.cos(x)
def tan(x):
    if isinstance(x,Node):
        return x.tan()
    else:
        print(type(x))
        return math.tan(x)

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
    def __pow__(self,x):
        if not isinstance(x,Node):
            x = Data(x)
        result = Node()
        result.text = "pow("+self.text+","+x.text+")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result

    def sin(self):
        return self.uniary("sin")
    def cos(self):
        return self.uniary("cos")
    def tan(self):
        return self.uniary("tan")
        

    def uniary(self,op):
        result = Node()
        result.text = op + "(" + self.text + ")"
        result.params = self.params.copy()
        return result

    def binary(self,x,op):
        if not isinstance(x,Node):
            x = Data(x)
        result = Node()
        result.text = "(" + self.text + op + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def compile(self,name=None):
        return Kernel(self.text,self.params,name)


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

class Kernel:
    params = {}
    def __init__(self,source,params,name=None):
        self.params=params
        if name is None:
            name = ''.join(random.choice(string.ascii_lowercase) for x in range(10))       
        self.name = name
        outname = ''.join(random.choice(string.ascii_lowercase) for x in range(10))  
        outname = "c"

        size = 0
#        for k,v in params:
#            size = max(size,v.length)
        size = 50000
        self.size = size

        self.output = cl.Buffer(ctx,mf.WRITE_ONLY,size*4)
        
        plist = ["__global const float *" + k
                 for k in params.keys()]

        text = "__kernel void "
        text += name + "("
        
        #Creates proper comma delimited list
        text += ", ".join(plist)
        text += ", __global float *" + outname + ")\n"
        text += "{\n"+"int gid=get_global_id(0);\n"
        text += outname + "[gid]=" +source+";\n}"

        self.text = text
        self.prog = cl.Program(ctx,text).build()
    def run(self):
        kern = self.prog.__getattr__(self.name)
        plist = [queue,(self.size,),None]
        plist.extend([self.params[k] for k in self.params.keys()])
        plist.append(self.output)
        kern(*plist)
        output = numpy.empty((self.size,),dtype=numpy.float32)
        cl.enqueue_copy(queue,output,self.output)
        return output

if __name__ == "__main__":
    a = numpy.random.rand(50000).astype(numpy.float32)
    b = numpy.random.rand(50000).astype(numpy.float32)
    a_cl = Data(a,"a")
    b_cl = Data(b,"b")
    c_cl = a_cl+b_cl
    p = c_cl.compile("sum")
    c = p.run()
    temp = c[c==(a+b)]
    print(temp.shape)

