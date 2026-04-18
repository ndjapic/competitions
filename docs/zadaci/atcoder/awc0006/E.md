# Задатак: E.pas

```pascal
program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, q, i, j, l, r, x, v: int32;
	tp: int8;
	s: array [1 .. nn] of int32;
	st: array [1 .. tt] of int64;

procedure combine(v: int32); inline;
begin
	st[v] := st[2*v] + st[2*v+1];
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
		st[v] := s[l];
end;

procedure update(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		st[v] := s[l];
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, l, r);
		update(2*v+1, m+1, vr, l, r);
		combine(v);
	end;
end;

function query(v, vl, vr, l, r: int32): int64;
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		query := 0
	else if (l <= vl) and (vr <= r) then
		query := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query :=
			query(2*v, vl, m, l, r) +
			query(2*v+1, m+1, vr, l, r);
	end;
end;

begin
	readln(n, q);

	for i := 1 to n do read(s[i]);
	readln;
	build(1, 1, n);

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(l, r);
				writeln(query(1, 1, n, l, r));
			end;

			2: begin
				readln(x, v);
				s[x] := v;
				update(1, 1, n, x, x);
			end;

		end;
	end;
end.

```
