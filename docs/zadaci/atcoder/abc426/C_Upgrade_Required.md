# Задатак: C_Upgrade_Required.pas

```pascal
program C_Upgrade_Required;
const
	tt = 2048 * 1024;
var
	n, q, i, x, y, x1, ans: int32;
	st, lz: array [1 .. tt] of int32;

procedure combine(v: int32);
begin
	st[v] := st[2*v] + st[2*v+1];
end;

procedure push(v: int32);
begin
	inc(st[2*v], lz[v]);
	inc(st[2*v+1], lz[v]);
	inc(lz[2*v], lz[v]);
	inc(lz[2*v+1], lz[v]);
	lz[v] := 0;
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	lz[v] := 0;
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		combine(v);
	end else
		st[v] := 1;
end;

procedure update(v, vl, vr, l, r, d: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		inc(st[v], d);
		inc(lz[v], d);
	end else {if vl < vr then} begin
		push(v);
		m := (vl+vr) div 2;
		update(2*v, vl, m, l, r, d);
		update(2*v+1, m+1, vr, l, r, d);
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
		push(v);
		m := (vl+vr) div 2;
		query :=
			query(2*v, vl, m, l, r) +
			query(2*v+1, m+1, vr, l, r)
		;
	end;
end;

begin
	readln(n, q);

	build(1, 1, n);

	x1 := 1;
	for i := 1 to q do begin
		readln(x, y);
		if x < x1 then
			writeln(0)
		else begin
			ans := query(1, 1, n, x1, x);
			x1 := x+1;
			writeln(ans);
			update(1, 1, n, y, y, ans);
		end;
	end;
end.

```
