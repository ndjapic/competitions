# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	nn = 200 * 1000;
var
	n, m, c, i, j, x: int32;
	sales: int64;
	a, b: tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m, c);

	a := tlist<int32>.create;
	for i := 1 to n do begin
		read(x);
		a.add(x);
	end;
	readln;
	a.sort;

	b := tlist<int32>.create;
	for j := 1 to m do begin
		read(x);
		b.add(x);
	end;
	readln;
	b.sort;

	sales := 0;
	i := 0;
	j := 0;
	while (i < n) and (j < m) do
		if a[i] >= b[j] then begin
			inc(sales, c);
			inc(i);
			inc(j);
		end else
			inc(i);

	writeln(sales);

	a.Free;
	b.Free;
end.

```
