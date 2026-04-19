# Problem: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, k, i, ans: int32;
	h: array [1 .. nn] of int32;
	st, qt: array [1 .. tt] of record
		mx, mn: int32;
	end;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		st[v].mx := max(st[2*v].mx, st[2*v+1].mx);
		st[v].mn := min(st[2*v].mn, st[2*v+1].mn);
	end else begin
		st[v].mx := h[l];
		st[v].mn := h[l];
	end;
end;

procedure query(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then begin
		qt[v].mx := low(int32);
		qt[v].mn := high(int32);
	end else if (l <= vl) and (vr <= r) then begin
		qt[v].mx := st[v].mx;
		qt[v].mn := st[v].mn;
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query(2*v, vl, m, l, r);
		query(2*v+1, m+1, vr, l, r);
		qt[v].mx := max(qt[2*v].mx, qt[2*v+1].mx);
		qt[v].mn := min(qt[2*v].mn, qt[2*v+1].mn);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do read(h[i]);
	readln;
	build(1, 1, n);

	ans := 0;
	for i := k to n do begin
		query(1, 1, n, i-k+1, i);
		ans := max(ans, qt[1].mx - qt[1].mn);
	end;
	writeln(ans);
end.

```
