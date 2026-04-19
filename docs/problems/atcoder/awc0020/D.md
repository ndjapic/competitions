# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
var
	n, l, i, o, taka: int32;
	x, r, p, p2: array [1 .. nn] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compare(a, b: int32): int32;
begin
	compare := x[a] - r[a] - x[b] + r[b];
end;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (
			compare(p[i], p[j]) <= 0
		) then begin
			p2[k] := p[i];
			inc(i);
		end else begin
			p2[k] := p[j];
			inc(j);
		end;
	for k := l to r-1 do p[k] := p2[k];
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

	readln(n, l);

	for i := 1 to n do begin
		readln(x[i], r[i]);
		p[i] := i;
	end;

	msort(1, n+1);

	taka := 0;
	for o := 1 to n do begin
		i := p[o];
		if x[i] - r[i] <= taka then
			taka := max(taka, x[i] + r[i]);
	end;

	if taka >= l then
		writeln('Yes')
	else
		writeln('No');
end.

```
