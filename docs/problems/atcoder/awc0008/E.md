# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
var
	n, i: int32;
	ans: int64;
	a, b: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	for k := l to r-1 do b[k] := a[k];
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (b[i] <= b[j]) then begin
			a[k] := b[i];
			inc(i);
		end else begin
			a[k] := b[j];
			inc(ans, j-k);
			inc(j);
		end;
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

	readln(n);

	for i := 1 to n do read(a[i]);
	readln;

	ans := 0;
	msort(1, n+1);

	writeln(ans);
end.

```
