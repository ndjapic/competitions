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
	n, q, i, k, l, r, d: int32;
	tp: int8;
	c: array [1 .. nn] of int32;
	st, zt, qt: array [1 .. tt] of int64;

procedure stcombine(v: int32);
begin
	st[v] := st[2*v] + st[2*v+1];
end;

procedure qtcombine(v: int32);
begin
	qt[v] := qt[2*v] + qt[2*v+1];
end;

procedure push(v, ld, rd: int32);
begin
	inc(st[2*v], zt[v] * ld);
	inc(st[2*v+1], zt[v] * rd);
	inc(zt[2*v], zt[v]);
	inc(zt[2*v+1], zt[v]);
	zt[v] := 0;
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	zt[v] := 0;
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		stcombine(v);
	end else
		st[v] := c[l];
end;

procedure update(v, vl, vr, l, r, d: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		inc(st[v], int64(vr-vl+1) * d);
		inc(zt[v], d);
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		push(v, m-vl+1, vr-m);
		update(2*v, vl, m, l, r, d);
		update(2*v+1, m+1, vr, l, r, d);
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
		push(v, m-vl+1, vr-m);
		query(2*v, vl, m, l, r);
		query(2*v+1, m+1, vr, l, r);
		qtcombine(v);
	end;
end;

begin
	readln(n, q);
	for i := 1 to n do read(c[i]); readln;
	build(1, 1, n);

	{update(1, 1, n, 2, n-1, 10);

	for i := 1 to n do begin
		query(1, 1, n, i, i);
		write(' ', qt[1]);
	end;
	writeln;}

	for k := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				read{ln}(l, r, d);
				update(1, 1, n, l, r, d);
			end;

			2: begin
				read{ln}(l, r);
				query(1, 1, n, l, r);
				writeln(qt[1]);
			end;
		
		end;
	end;
end.

```
