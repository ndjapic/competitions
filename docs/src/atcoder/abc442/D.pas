program D_;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, q, i, j, l, r, a0, a1: int32;
	tp: int8;
	a: array [1 .. nn] of int32;
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
		st[v] := a[l];
end;

procedure update(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		st[v] := a[l];
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, l, r);
		update(2*v+1, m+1, vr, l, r);
		combine(v);
	end;
end;

function query(v, vl, vr, l, r: int32): int32;
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

	for i := 1 to n do read(a[i]);
	readln;
	build(1, 1, n);

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(i);
				a0 := a[i];
				a1 := a[i+1];
				a[i] := a1;
				a[i+1] := a0;
				update(1, 1, n, i, i);
				update(1, 1, n, i+1, i+1);
			end;

			2: begin
				readln(l, r);
				writeln(query(1, 1, n, l, r));
			end;

		end;
	end;
end.
