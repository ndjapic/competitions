# Задатак: C.pas

```pascal
program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	maxn = 300 * 1000;
	maxt = 1024 * 1024;
var
	n, q, i, j: int32;
	k, x: int8;
	a: array [1 .. maxn] of int32;
	b, a2: array [1 .. 5] of int32;
	st: array [1 .. maxt] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure combine(v: int32);
begin
	st[v] := min(st[2*v], st[2*v+1]);
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		combine(v);
	end else
		st[v] := a[l];
end;

procedure update(v, l, r, i: int32);
var
	m: int32;
begin
	if (i < l) or (r < i) then
	else if (i <= l) and (r <= i) then begin
		st[v] := a[i];
	end else {if l < r then} begin
		m := (l+r) div 2;
		update(2*v, l, m, i);
		update(2*v+1, m+1, r, i);
		combine(v);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(a[i]); readln;
	build(1, 1, n);

	for j := 1 to q do begin
		readln(k);

		for x := 1 to k do begin
			read(i);
			b[x] := i;
			a2[x] := a[i];
			a[i] := high(int32);
			update(1, 1, n, i);
		end;
		readln;

		writeln(st[1]);

		for x := 1 to k do begin
			i := b[x];
			a[i] := a2[x];
			update(1, 1, n, i);
		end;
	end;
end.

```
