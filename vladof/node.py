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

def overload2(x,y,term):
    if isinstance(x,Node):
        return x.__getattribute__(term)(y)
    else:
        print(type(x))
        return math.__getattribute__(term)(x,y)

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
def atan2(x,y):
    return overload2(x,y,"atan2")
def atanh(x):
    return overload(x,"atanh")
def ceil(x):
    return overload(x,"ceil")
def copysign(x,y):
    return overload2(x,y,"copysign")
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
def fmod(x,y):
    return overload2(x,y,"fmod")
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
    def acos(self):
        return self.uniary("acos")
    def acosh(self):
        return self.uniary("acosh")
    def asin(self):
        return self.uniary("asin")
    def asinh(self):
        return self.uniary("asinh")
    def atan(self):
        return self.uniary("atan")
    def atan2(self,x):
        result = Node()
        result.text = "atan2(" + self.text + "," + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def atanh(self):
        return self.uniary("atanh")
    def ceil(self):
        return self.uniary("ceil")
    def copysign(self,x):
        result = Node()
        result.text = "copysign(" + self.text + "," + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def cos(self):
        return self.uniary("cos")
    def cosh(self):
        return self.uniary("cosh")
    def degrees(self):
        return self.uniary("degrees")
    def erf(self):
        result = Node()
        result.text = "erf(" + self.text + "," + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def erfc(self):
        result = Node()
        result.text = "erfc(" + self.text + "," + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def exp(self):
        return self.uniary("exp")
    def expm1(self):
        return self.uniary("expm1")
    def fabs(self):
        return self.uniary("fabs")
    def factorial(self):
        return self.uniary("factorial")
    def floor(self):
        return self.uniary("floor")
    def fmod(self,x):
        result = Node()
        result.text = "fmod(" + self.text + "," + x.text + ")"
        result.params = self.params.copy()
        result.params.update(x.params)
        return result
    def gamma(self):
        return self.uniary("gamma")
    def hypot(self):
        return self.uniary("hypot")
    def isfinite(self):
        return self.uniary("isfinite")
    def isinf(self):
        return self.uniary("isinf")
    def isnan(self):
        return self.uniary("isnan")
    def ldexp(self):
        return self.uniary("ldexp")
    def lgamma(self):
        return self.uniary("lgamma")
    def log(self):
        return self.uniary("log")
    def log10(self):
        return self.uniary("log10")
    def log1p(self):
        return self.uniary("log1p")
    def modf(self):
        return self.uniary("modf")
    def pow(self):
        return self.uniary("pow")
    def radians(self):
        return self.uniary("radians")
    def sin(self):
        return self.uniary("sin")
    def sinh(self):
        return self.uniary("sinh")
    def sqrt(self):
        return self.uniary("sqrt")
    def tan(self):
        return self.uniary("tan")
    def tanh(self):
        return self.uniary("tanh")
    def trunc(self):
        return self.uniary("trunc")

        

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
