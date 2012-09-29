#Base class for nodes in OpenCL syntax tree

import pyopencl as cl
import numpy
import random
import string
import math


ctx = cl.create_some_context()
queue = cl.CommandQueue(ctx)

mf = cl.mem_flags

def overload(x,term):
    if isinstance(x,Node):
        return x.__getattribute__(term)()
    else:
        print(type(x))
        return math.__getattribute__(term)(x)

def acos(x):
    return overload(x,"acos")
def acosh(x):
    return overload(x,"acosh")
def asin(x):
    return overload(x,"asin")
def asinh(x):
    return overload(x,"asinh")
def atan(x):
    return overload(x,"atan")
def atan2(x):
    return overload(x,"atan2")
def atanh(x):
    return overload(x,"atanh")
def ceil(x):
    return overload(x,"ceil")
def copysign(x):
    return overload(x,"copysign")
def cos(x):
    return overload(x,"cos")
def cosh(x):
    return overload(x,"cosh")
def degrees(x):
    return overload(x,"degrees")
def e(x):
    return overload(x,"e")
def erf(x):
    return overload(x,"erf")
def erfc(x):
    return overload(x,"erfc")
def exp(x):
    return overload(x,"exp")
def expm1(x):
    return overload(x,"expm1")
def fabs(x):
    return overload(x,"fabs")
def factorial(x):
    return overload(x,"factorial")
def floor(x):
    return overload(x,"floor")
def fmod(x):
    return overload(x,"fmod")
def frexp(x):
    return overload(x,"frexp")
def fsum(x):
    return overload(x,"fsum")
def gamma(x):
    return overload(x,"gamma")
def hypot(x):
    return overload(x,"hypot")
def isfinite(x):
    return overload(x,"isfinite")
def isinf(x):
    return overload(x,"isinf")
def isnan(x):
    return overload(x,"isnan")
def ldexp(x):
    return overload(x,"ldexp")
def lgamma(x):
    return overload(x,"lgamma")
def log(x):
    return overload(x,"log")
def log10(x):
    return overload(x,"log10")
def log1p(x):
    return overload(x,"log1p")
def modf(x):
    return overload(x,"modf")
def pi(x):
    return overload(x,"pi")
def pow(x):
    return overload(x,"pow")
def radians(x):
    return overload(x,"radians")
def sin(x):
    return overload(x,"sin")
def sinh(x):
    return overload(x,"sinh")
def sqrt(x):
    return overload(x,"sqrt")
def tan(x):
    return overload(x,"tan")
def tanh(x):
    return overload(x,"tanh")
def trunc(x):
    return overload(x,"trunc")

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

