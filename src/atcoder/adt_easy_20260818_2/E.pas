program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #default #sort #binary #search
uses
	generics.collections, generics.defaults;
var
	n, q, i, j, x, l, r: int32;
	a: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n, q);

	a := tlist<int32>.create;
	for i := 1 to n do begin
		read(x);
		a.add(x);
		a.exchange(i-1, random(i));
	end;
	readln;
	a.sort;

	for j := 1 to q do begin
		readln(x);
		l := -1;
		r := n;

		while r-l > 1 do begin
			i := (r+l) div 2;
			if a[i] < x then
				l := i
			else
				r := i;
		end;

		writeln(n-r);
	end;
	a.free;
end.
