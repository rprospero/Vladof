import unittest
import numpy
import node
from numpy.testing import assert_array_equal,assert_almost_equal
#from scipy import erf,erfc
#from scipy.special import gamma

def problem(a,b):
    """Locates the specific elements in an array where the two values aren't equal"""
    return numpy.argmax(numpy.abs(a-b))

class NodeTest(unittest.TestCase):

    def setUp(self):
        self.a = numpy.random.rand(50000).astype(numpy.float32)
        self.b = numpy.random.rand(50000).astype(numpy.float32)
        self.a_cl = node.Data(self.a,"a")
        self.b_cl = node.Data(self.b,"b")

#Basic Math

    def test_add(self):
        c_cl = (self.a_cl + self.b_cl).compile("add").run()
        assert_array_equal(c_cl,self.a+self.b)

    def test_sub(self):
        c_cl = (self.a_cl - self.b_cl).compile("sub").run()
        assert_array_equal(c_cl,self.a-self.b)

    def test_mul(self):
        c_cl = (self.a_cl * self.b_cl).compile("sub").run()
        assert_array_equal(c_cl,self.a*self.b)

    def test_div(self):
        c_cl = (self.a_cl / self.b_cl).compile("sub").run()
        index = problem(c_cl,self.a/self.b)
        #Since division can blow up so horribly, we're only going 
        #to look at relative difference, as opposed to absolute difference
        assert_almost_equal(c_cl/(self.a/self.b)-1,numpy.zeros(50000),
                            decimal=6)

    def test_exponent(self):
        c_cl = (self.a_cl ** self.b_cl).compile("sub").run()
        assert_almost_equal(c_cl,self.a**self.b,decimal=6)

#Beyond Algebra

    def test_acos(self):
        c_cl = node.acos(self.a_cl).compile("acos_test").run()
        assert_almost_equal(c_cl,numpy.arccos(self.a))

    def test_acosh(self):
        a_cl = node.Data(self.a*2,"a")
        c_cl = node.acosh(a_cl).compile("acosh_test").run()
        assert_almost_equal(c_cl,numpy.arccosh(self.a * 2),verbose=True)

    def test_asin(self):
        c_cl = node.asin(self.a_cl).compile("asin_test").run()
        assert_almost_equal(c_cl,numpy.arcsin(self.a))

    def test_asinh(self):
        c_cl = node.asinh(self.a_cl).compile("asinh_test").run()
        assert_almost_equal(c_cl,numpy.arcsinh(self.a))

    def test_atan(self):
        c_cl = node.atan(self.a_cl).compile("atan_test").run()
        assert_almost_equal(c_cl,numpy.arctan(self.a))

#    def test_atan2(self):
#        c_cl = node.atan2(self.a_cl).compile("atan2_test").run()
#        assert_almost_equal(c_cl,numpy.atan2(self.a))

    def test_atanh(self):
        c_cl = node.atanh(self.a_cl).compile("atanh_test").run()
        assert_almost_equal(c_cl,numpy.arctanh(self.a),decimal=5)

    def test_ceil(self):
        c_cl = node.ceil(self.a_cl).compile("ceil_test").run()
        assert_almost_equal(c_cl,numpy.ceil(self.a))

#    def test_copysign(self):
#        c_cl = node.copysign(self.a_cl).compile("copysign_test").run()
#        assert_almost_equal(c_cl,numpy.copysign(self.a))

    def test_cos(self):
        c_cl = node.cos(self.a_cl).compile("cos_test").run()
        assert_almost_equal(c_cl,numpy.cos(self.a))

    def test_cosh(self):
        c_cl = node.cosh(self.a_cl).compile("cosh_test").run()
        assert_almost_equal(c_cl,numpy.cosh(self.a))

    def test_degrees(self):
        c_cl = node.degrees(self.a_cl).compile("degrees_test").run()
        assert_almost_equal(c_cl,numpy.degrees(self.a),decimal=5)

#    def test_erf(self):
#        c_cl = node.erf(self.a_cl).compile("erf_test").run()
#        assert_almost_equal(c_cl,erf(self.a))

#    def test_erfc(self):
#        c_cl = node.erfc(self.a_cl).compile("erfc_test").run()
#        assert_almost_equal(c_cl,erfc(self.a))

    def test_exp(self):
        c_cl = node.exp(self.a_cl).compile("exp_test").run()
        assert_almost_equal(c_cl,numpy.exp(self.a),decimal=5)

    def test_expm1(self):
        c_cl = node.expm1(self.a_cl).compile("expm1_test").run()
        assert_almost_equal(c_cl,numpy.expm1(self.a))

    def test_fabs(self):
        c_cl = node.fabs(self.a_cl).compile("fabs_test").run()
        assert_almost_equal(c_cl,numpy.fabs(self.a))

#    def test_factorial(self):
#        c_cl = node.factorial(self.a_cl).compile("factorial_test").run()
#        assert_almost_equal(c_cl,numpy.factorial(self.a))

    def test_floor(self):
        c_cl = node.floor(self.a_cl).compile("floor_test").run()
        assert_almost_equal(c_cl,numpy.floor(self.a))

#    def test_fmod(self):
#        c_cl = node.fmod(self.a_cl).compile("fmod_test").run()
#        assert_almost_equal(c_cl,numpy.fmod(self.a))

    def test_frexp(self):
        c_cl = node.frexp(self.a_cl,self.b_cl).compile("frexp_test").run()
        assert_almost_equal(c_cl,numpy.frexp(self.a,self.b))

    def test_fsum(self):
        c_cl = node.fsum(self.a_cl,self.b_cl).compile("fsum_test").run()
        assert_almost_equal(c_cl,numpy.fsum(self.a,self.b))

#    def test_gamma(self):
#        c_cl = node.gamma(self.a_cl).compile("gamma_test").run()
#        assert_almost_equal(c_cl,gamma(self.a))

#    def test_hypot(self):
#        c_cl = node.hypot(self.a_cl).compile("hypot_test").run()
#        assert_almost_equal(c_cl,numpy.hypot(self.a))

    def test_isfinite(self):
        c_cl = node.isfinite(self.a_cl).compile("isfinite_test").run()
        assert_almost_equal(c_cl,numpy.isfinite(self.a))

    def test_isinf(self):
        c_cl = node.isinf(self.a_cl).compile("isinf_test").run()
        assert_almost_equal(c_cl,numpy.isinf(self.a))

    def test_isnan(self):
        c_cl = node.isnan(self.a_cl).compile("isnan_test").run()
        assert_almost_equal(c_cl,numpy.isnan(self.a))

#    def test_ldexp(self):
#        c_cl = node.ldexp(self.a_cl).compile("ldexp_test").run()
#        assert_almost_equal(c_cl,numpy.ldexp(self.a))

#    def test_lgamma(self):
#        c_cl = node.lgamma(self.a_cl).compile("lgamma_test").run()
#        assert_almost_equal(c_cl,numpy.lgamma(self.a))

    def test_log(self):
        c_cl = node.log(self.a_cl).compile("log_test").run()
        assert_almost_equal(c_cl,numpy.log(self.a),decimal=5)

    def test_log10(self):
        c_cl = node.log10(self.a_cl).compile("log10_test").run()
        assert_almost_equal(c_cl,numpy.log10(self.a),decimal=5)

    def test_log1p(self):
        c_cl = node.log1p(self.a_cl).compile("log1p_test").run()
        assert_almost_equal(c_cl,numpy.log1p(self.a))

#    def test_modf(self):
#        c_cl = node.modf(self.a_cl).compile("modf_test").run()
#        assert_almost_equal(c_cl,numpy.modf(self.a))

#    def test_pow(self):
#        c_cl = node.pow(self.a_cl).compile("pow_test").run()
#        assert_almost_equal(c_cl,numpy.pow(self.a))

    def test_radians(self):
        c_cl = node.radians(self.a_cl).compile("radians_test").run()
        assert_almost_equal(c_cl,numpy.radians(self.a))

    def test_sin(self):
        c_cl = node.sin(self.a_cl).compile("sin_test").run()
        assert_almost_equal(c_cl,numpy.sin(self.a))

    def test_sinh(self):
        c_cl = node.sinh(self.a_cl).compile("sinh_test").run()
        assert_almost_equal(c_cl,numpy.sinh(self.a),decimal=5)

    def test_sqrt(self):
        c_cl = node.sqrt(self.a_cl).compile("sqrt_test").run()
        assert_almost_equal(c_cl,numpy.sqrt(self.a))

    def test_tan(self):
        c_cl = node.tan(self.a_cl).compile("tan_test").run()
        assert_almost_equal(c_cl,numpy.tan(self.a),decimal=5)

    def test_tanh(self):
        c_cl = node.tanh(self.a_cl).compile("tanh_test").run()
        assert_almost_equal(c_cl,numpy.tanh(self.a))

    def test_trunc(self):
        c_cl = node.trunc(self.a_cl).compile("trunc_test").run()
        assert_almost_equal(c_cl,numpy.trunc(self.a))





if __name__ == '__main__':
    unittest.main()
        
