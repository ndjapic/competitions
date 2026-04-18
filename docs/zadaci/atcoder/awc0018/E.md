# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 16;
	mm = 120;
	inf = 1 shl 30;
var
	n, u, v: int8;
	d: int64;
	x, y, p: array [1 .. nn] of int32;
	fuel: array [1 .. nn, 1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for v := 1 to n do begin
		readln(x[v], y[v]);
		for u := 1 to v-1 do begin
			fuel[u, v] := sqr(x[u] - x[v]) + sqr(y[u] - y[v]);
			fuel[v, u] := fuel[u, v];
		end;
		fuel[v, v] := 0;
	end;

	p[1] := 1;

	for v := 2 to n do begin
		d := fuel[1, v];
		u := v;
		for i := v downto 1 do begin
			if fu
		end;
	end;
end.

```
