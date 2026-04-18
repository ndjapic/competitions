# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100 * 1000;
	tt = 256 * 1024;
var
	n, q, i, j, k: int32;
	p, c, l, r, seen, last, prev, ans: array [1 .. nn] of int32;
	st, qt: array [1 .. tt] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure stcombine(v: int32);
begin
	st[v] := st[2*v] + st[2*v+1];
end;

procedure qtcombine(v: int32);
begin
	qt[v] := qt[2*v] + qt[2*v+1];
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		stcombine(v);
	end else
		st[v] := 0;
end;

procedure update(v, vl, vr, k: int32);
var
	m: int32;
begin
	if (k < vl) or (vr < k) then
	else if (k <= vl) and (vr <= k) then begin
		st[v] := c[k];
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, k);
		update(2*v+1, m+1, vr, k);
		stcombine(v);
	end;
end;

procedure query(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		qt[v] := 0
	else if (l <= vl) and (vr <= r) then
		qt[v] := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query(2*v, vl, m, l, r);
		query(2*v+1, m+1, vr, l, r);
		qtcombine(v);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do begin
		read(p[i]);
		last[i] := 0;
	end;
	readln;

	for j := 1 to q do begin
		readln(l[j], r[j]);
		prev[j] := last[r[j]];
		last[r[j]] := j;
	end;

	build(1, 1, n);

	for k := 1 to n do seen[k] := 0;

	for i := 1 to n do begin
		k := p[i];

		if seen[k] > 0 then begin
			c[seen[k]] := 0;
			update(1, 1, n, seen[k]);
		end;

		seen[k] := i;
		c[i] := 1;
		update(1, 1, n, i);

		j := last[i];
		while j > 0 do begin
			query(1, 1, n, l[j], i);
			ans[j] := qt[1];
			j := prev[j];
		end;
	end;

	for j := 1 to q do writeln(ans[j]);
end.

```
