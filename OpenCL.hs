module OpenCL (OpenCL(platform,dev,context,queue),getContext,makeKernel,Kernel(kernel),makeParameter,toParameter,Parameter(kern,ptr,vecsize)) where

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

makeParameter k ptr size index = clCreateBuffer (context.cl$k) [CL_MEM_READ_ONLY, CL_MEM_COPY_HOST_PTR] (size, castPtr ptr) >>= clSetKernelArgSto (kernel k) index

toParameter k original index = do
  let elemsize = sizeOf $ head original
      vecsize = elemsize * length original
  input <- newArray original
  makeParameter k input vecsize 0
  return $ Parameter k input vecsize
