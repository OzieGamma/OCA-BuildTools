.def VGA_BASE 0x8000

boot:
	set t0, 0x41
	set t1, 0
	addi t2, t1, 300
	
loop:
	bge t1, t2, end
	
	stw t0, t1(VGA_BASE)
	
	addi t1, t1, 1
	br loop
	
end:
	br end
	