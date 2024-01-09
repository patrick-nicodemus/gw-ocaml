su = scaling_unbalanced
sc = scaling_unbalanced_single_core
mc = scaling_unbalanced_multicore
opencl = scaling_unbalanced_opencl
cuda = scaling_unbalanced_cuda

all: single_core/$(sc).fut multicore/$(mc).fut opencl/$(opencl).fut cuda/$(cuda).fut

single_core/$(sc).fut: $(su).fut
	cp $(su).fut $(sc).fut
	futhark-bindgen run $(sc).fut single_core/$(sc).ml
	mv $(sc).* single_core

multicore/$(mc).fut: $(su).fut
	cp $(su).fut $(mc).fut
	futhark-bindgen run --backend multicore $(mc).fut multicore/$(mc).ml
	mv $(mc).* multicore

opencl/$(opencl).fut: $(su).fut
	cp $(su).fut $(opencl).fut
	futhark-bindgen run --backend opencl $(opencl).fut opencl/$(opencl).ml
	mv $(opencl).* opencl

cuda/$(cuda).fut: $(su).fut
	cp $(su).fut $(cuda).fut
	futhark-bindgen run --backend cuda $(cuda).fut cuda/$(cuda).ml
	mv $(cuda).* cuda
