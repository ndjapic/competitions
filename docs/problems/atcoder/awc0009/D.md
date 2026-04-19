# Problem: D.pas

```pascal
program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	mm = 100 * 1000 + 1;
	inf = int64(1) shl 60;
type
	period = record
		l, r: int64;
	end;
var
	m, i: int32;
	n, l, r: int64;
	rainy, rainy2: array [1 .. mm] of period;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function compare(l, r: period): int64;
begin
	compare := l.l - r.l;
end;

procedure merge(l, m, r: int32);
var
	i, j, k: int32;
begin
	i := l;
	j := m;
	for k := l to r-1 do
		if (j = r) or (i < m) and (
			compare(rainy[i], rainy[j]) < 0
		) then begin
			rainy2[k] := rainy[i];
			inc(i);
		end else begin
			rainy2[k] := rainy[j];
			inc(j);
		end;
	for k := l to r-1 do rainy[k] := rainy2[k];
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

	readln(n, m);

	for i := 1 to m do readln(rainy[i].l, rainy[i].r);
	rainy[m+1].l := inf;
	rainy[m+1].r := inf;
	msort(1, m+2);

	i := 1;
	r := 0;
	while (i <= m+1) and (n > 0) do begin
		l := rainy[i].l;
		if l > r then dec(n, l-r-1);
		r := max(r, rainy[i].r);
		inc(i);
	end;

	writeln(l-1+n);
end.

```
