program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort
uses
	generics.collections,
	generics.defaults, math;
var
	n, i, elm, cx, cy: int32;
	x, y: int64;
	a, b: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, x, y);


	a := tlist<int32>.create;
	for i := 1 to n do begin
		read(elm);
		a.add(elm);
		a.exchange(i-1, random(i));
	end;
	readln;
	a.sort;

	i := n-1;
	while (i >= 0) and (x >= 0) do begin
		dec(x, a[i]);
		dec(i);
	end;
	cx := n-1-i;
	a.free;

	b := tlist<int32>.create;
	for i := 1 to n do begin
		read(elm);
		b.add(elm);
		b.exchange(i-1, random(i));
	end;
	readln;
	b.sort;

	i := n-1;
	while (i >= 0) and (y >= 0) do begin
		dec(y, b[i]);
		dec(i);
	end;
	cy := n-1-i;
	b.free;


	writeln(min(cx, cy));
end.
