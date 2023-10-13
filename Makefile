debug:
	zig build-exe ziglox.zig -target x86_64-windows 

release:
	zig build-exe ziglox.zig -O ReleaseSmall --strip -target x86_64-windows 

.PHONY: run
run: debug
	./ziglox.exe

.PHONY: test
test:
	zig test list.zig 