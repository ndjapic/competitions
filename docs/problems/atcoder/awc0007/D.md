# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 501;
type
	tab = array [1 .. nn, 1 .. nn] of int8;
var
	n, a, b, r, c, ans: int32;
	tak, aok: tab;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure process(k: int8; var t: tab);
var
	i, r1, c1, r2, c2: int32;
begin
	for r := 1 to n+1 do
		for c := 1 to n+1 do
			t[r, c] := 0;

	for i := 1 to k do begin
		readln(r1, c1, r2, c2);
		inc(t[r1, c1]);
		dec(t[r1, c2+1]);
		dec(t[r2+1, c1]);
		inc(t[r2+1, c2+1]);
	end;

	for r := 1 to n+1 do
		for c := 1 to n do
			inc(t[r, c+1], t[r, c]);

	for r := 1 to n do
		for c := 1 to n+1 do
			inc(t[r+1, c], t[r, c]);
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, a, b);

	process(a, tak);
	process(b, aok);

	ans := 0;
	for r := 0 to n do
		for c := 0 to n do
			if (tak[r, c] > 0) and (aok[r, c] > 0) then inc(ans);

	writeln(ans);
end.

```
