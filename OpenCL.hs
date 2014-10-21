module OpenCL (OpenCL(platform,dev,context,queue),getContext,makeKernel,Kernel(kernel),makeParameter,toParameter,Parameter(kern,ptr,vecsize),outParam,inParam) where

import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.Marshal.Array( newArray, peekArray )
import GHC.Ptr (Ptr)

data OpenCL = OpenCL {platform :: CLPlatformID,
                      dev :: CLDeviceID,
                      context :: CLContext,
                      queue :: CLCommandQueue}

data Kernel = Kernel {cl :: OpenCL,
                      kernel :: CLKernel}

data Parameter a = Parameter {kern :: Kernel,
                              ptr :: GHC.Ptr.Ptr a,
                              vecsize :: Int}

getContext :: IO OpenCL
getContext = do
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
  q <- clCreateCommandQueue context dev []
  return $ OpenCL platform dev context q

makeKernel cl name src = do
  program <- clCreateProgramWithSource (context cl) src
  clBuildProgram program [dev cl] ""
  k <- clCreateKernel program name
  return $ Kernel cl k

inParam = [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR]
outParam = [CL_MEM_WRITE_ONLY]

makeParameter param k ptr size index = do
  buffer <- clCreateBuffer (context.cl$k) param (size, castPtr ptr) 
  clSetKernelArgSto (kernel k) index buffer
  return $ Parameter k buffer size

toParameter param k original index = do
  let elemsize = sizeOf $ head original
      vecsize = elemsize * length original
  input <- newArray original
  makeParameter param k input vecsize 0
  return $ Parameter k input vecsize
