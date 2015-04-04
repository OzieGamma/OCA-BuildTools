# High Level Assembly (HLA)
In order to accomodate for different CPUs and architecture the assembly generation is divided in two steps:
- High Level Assembly (HLA) generation: a generic risk-like assembly that provides as many instructions as possible to the user.
- Target Assembly generation: HLA is mapped to CPU-specific assembly. One HLA instruction can map to several instructions on the target CPU.

**NB**: HLA only supports 32-bit registers, it **might** work with other sizes but there is not guarantee.

## Registers
HLA-compatible CPUs, should use a minimum of 16 Registers.
  
  - `zero` is a special register that always contains the 0 constant.
  - `ra` is used to store the return address of a subroutine call.
  - `ea` is used to store the exception return address.
  - `sp` is used to store the stack pointer.
  - `t0-t65535` are used for temporary values, they should be **caller saved**. Return values of a procedure are stored starting from t0.
  - `s0-s65535` are used for saved values, they should be **callee saved**. Arguments of a procedure should be passed starting from s0.
 
 In order to maintain compatibility, any program written in assembly (ie not compiled from a higher level language), should use **only** the following registers:
  
  - `zero`
  - `ra`
  - `ea`
  - `sp`
  - `t0-t4`
  - `s0-s4`

## Immediate values.
Immediate values can be specified in the following ways:

- As a positive integer literal (dec/hex)
- As a negative integer literal (dec/hex)
- As a label. Depending on the instruction this will be resolved to a value at final assembly into a relative or absolution value.
 
Unsigned or signed immediate instructions will be generated based on what the immediate values is.
 
**NB**: Using large literals might not be supported.
 
## Assembly instructions and macros
[ApiDocs](apidocs/oca-asmlib-hlainstr.html)

## Assembly process
source (text) <-> HLA -> Resolved HLA (No labels) -> Target Representation <-> Binary
<->: with data loss
->: without data loss