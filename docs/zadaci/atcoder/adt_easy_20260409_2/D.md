# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	generics.collections, generics.defaults;
const
	nn = 100 * 1000;
var
	n, m, i, j, a, b, d: int32;
	adj: array [1 .. nn] of tlist<int32>;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);

	for a := 1 to n do adj[a] := tlist<int32>.create;

	for i := 1 to m do begin
		readln(a, b);
		adj[a].add(b);
		adj[b].add(a);
	end;

	for a := 1 to n do begin
		d := adj[a].count;
		write(d);

		if d > 0 then begin
			write(' ');
			adj[a].sort;
			for j := 0 to d-2 do write(adj[a][j], ' ');
			write(adj[a][d-1]);
		end;
		writeln;
	end;

	for a := 1 to n do adj[a].free;
end.

```
