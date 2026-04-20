program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, i, c: int8;
	elm, l, r, x: int32;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	a := tlist<int32>.create;
	for i := 0 to n-1 do begin
		read(elm);
		a.add(elm);
		a.exchange(i, random(i+1));
	end;
	readln;
	a.sort;

	l := 0;
	r := 1 shl 30;
	while r-l > 1 do begin
		x := (l+r) div 2;

		c := 0;
		for i := 0 to n-1 do
			if a[i] >= x then inc(c);

		if c >= x then
			l := x
		else
			r := x;
	end;

	writeln(l);
	a.free;
end.
