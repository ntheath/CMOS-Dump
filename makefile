cdump:  cdump.obj
        link cdump.obj;

cdump.obj:  cdump.asm
        masm cdump.asm;
clean:
        del *.obj
        del *.exe

