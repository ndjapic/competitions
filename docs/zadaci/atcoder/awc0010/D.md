# Задатак: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i, k: int32;
	turns: int64;
	h, h2: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compare(l, r: int32): int32;
begin
	compare := r - l;
end;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (
			compare(h[i], h[j]) <= 0
		) then begin
			h2[k] := h[i];
			inc(i);
		end else begin
			h2[k] := h[j];
			inc(j);
		end;
	for k := l to r-1 do h[k] := h2[k];
end;

procedure msort(l, r: int32);
var
	m: int32;
begin
	if r-l > 1 then begin
		m := (l+r) div 2;
		msort(l, m);
		msort(m, r);
		merge(l, m, r);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(h[i]);
	readln;
	msort(1, n+1);

	turns := k;
	for i := k+1 to n do inc(turns, h[i]);
	writeln(turns);
end.

```
