program _B;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections,
	generics.defaults;
const
	NN = 100;
var
	n, i, j, k, a, x: int8;
	b: array [1 .. NN] of tlist<int8>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);
	randomize;

	readln(n);

	for a := 1 to n do b[a] := tlist<int8>.create;

	for i := 1 to n do begin
		read(k);
		for j := 1 to k do begin
			read(a);
			b[a].add(i);
			x := b[a].count;
			b[a].exchange(x-1, random(x));
		end;
		readln;
	end;

	for a := 1 to n do begin
		x := b[a].count;
		write(x);
		b[a].sort;
		for j := 0 to x-1 do write(' ', b[a][j]);
		writeln;
		b[a].free;
	end;
end.
