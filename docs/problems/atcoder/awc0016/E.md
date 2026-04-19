# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 12;
	mm = 66;
	inf = 1 shl 30;
var
	n, m, u, v, s, t, j: int8;
	d: int32;
	p, dist: array [1 .. nn] of int32;
	w: array [-mm .. mm] of int32;
	adj: array [1 .. nn] of int8;
	sib, tar: array [-mm .. mm] of int8;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure arrow(u, v, j: int8);
begin
	tar[j] := v;
	sib[j] := adj[u];
	adj[u] := j;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	for v := 1 to n do begin
		read(p[v]);
		adj[v] := 0;
	end;
	readln;
	readln(s, t);

	for j := 1 to m do begin
		readln(u, v, w[j]);
		arrow(u, v, j);
		arrow(v, u, -j);
	end;
end.

```
