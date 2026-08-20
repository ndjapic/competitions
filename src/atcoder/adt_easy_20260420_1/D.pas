program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
var
	n, i, x: int8;
	elm: int32;
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

	x := n;
	while (x > 0) and (a[n-x] < x) do dec(x);

	writeln(x);
	a.free;
end.
