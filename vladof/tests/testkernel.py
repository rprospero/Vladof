import unittest
import numpy
import node

def test_function(x,y):
    return (x**y+y/x)*(x-y*2)

class TestNode(unittest.TestCase):

    def setUp(self):
        x = numpy.random.rand(50000).astype(numpy.float32)
        y = numpy.random.rand(50000).astype(numpy.float32)

        self.x = x
        self.y = y

        self.clx = node.Data(x,"x")
        self.cly = node.Data(y,"y")

    def test_sum(self):
        clz = self.clx+self.cly
        z = clz.compile("sum").run()
        self.assertTrue(numpy.all(z==self.x+self.y))

    def test_weird(self):
        clz = test_function(self.clx,self.cly)
        z = clz.compile("sum").run()
        t = test_function(self.x,self.y)
        max = numpy.max(numpy.abs((z-t)/t))
        index = numpy.abs((z-t)/t)==max
        #Since the GPU does 32-bit math while the processor uses 80,
        #I'm only checking for 21 bits of precision in the final result.
        #This assumes that the GPU loses some bits.
        self.assertTrue(numpy.mean(((z-t)/t)**2)**0.5<5e-7)

if __name__ == '__main__':
    unittest.main()
