#NO_APP
.text
	.align 1
.globl _setjump
_setjump:
	.word 0x0
	movl    4(ap),r0
	movq    r2,(r0)+
	movq    r4,(r0)+
	movq    r6,(r0)+
	movq    r8,(r0)+
	movq    r10,(r0)+
	movl    fp,(r0)+
	movo    4(fp),(r0)+
	movq    20(fp),(r0)
	clrl    r0
	ret
	ret
	.align 1
.globl _longjump
_longjump:
	.word 0x0
	movl    4(ap),r0
	movq    (r0)+,r2
	movq    (r0)+,r4
	movq    (r0)+,r6
	movq    (r0)+,r8
	movq    (r0)+,r10
	movl    (r0)+,r1
	movo    (r0)+,4(r1)
	movq    (r0),20(r1)
	movl    8(ap),r0
	movl    r1,fp
	ret
	ret
