# Problem: C_Cyclic_Merging.pas

```pascal
program C_Cyclic_Merging;
uses
	math;
const
	nn = 200 * 1000;
	tt = 512 * 1024;
	NIL_INDEX = 0;
var
	notc, tci, n, i, k: int32;
	ans: int64;
	a, l, r, cost: array [1 .. nn] of int32;
	st: array [1 .. tt] of int32;

procedure combine(v: int32);
begin
	if cost[st[2*v]] < cost[st[2*v+1]] then
		st[v] := st[2*v]
	else
		st[v] := st[2*v+1];
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
		st[v] := l;
end;

procedure update(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		st[v] := l;
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, l, r);
		update(2*v+1, m+1, vr, l, r);
		combine(v);
	end;
end;

function query(v, vl, vr, l, r: int32): int32;
var
	m, lq, rq: int32;
begin
	if (r < vl) or (vr < l) then
		query := NIL_INDEX
	else if (l <= vl) and (vr <= r) then begin
		if a[st[v]] = -1 then
			query := NIL_INDEX
		else
			query := st[v];
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		lq := query(2*v, vl, m, l, r);
		rq := query(2*v+1, m+1, vr, l, r);
		if (rq = NIL_INDEX) or (lq <> NIL_INDEX) and (cost[lq] < cost[rq]) then
			query := lq
		else
			query := rq;
	end;
end;

begin
	readln(notc);
	for tci := 1 to notc do begin
		readln(n);

		for i := 1 to n do begin
			read(a[i]);
			l[i] := i-1;
			r[i] := i+1;
		end;

		l[1] := n;
		r[n] := 1;

		for i := 1 to n do cost[i] := max(a[i], a[r[i]]);
		build(1, 1, n);

		ans := 0;
		for k := 2 to n do begin
			i := query(1, 1, n, 1, n);
			a[i] := cost[i];
			inc(ans, cost[i]);
			{a[r[i]] := -1;
			cost[r[i]] := -1;}
			update(1, 1, n, r[i], r[i]);
			r[i] := r[r[i]];
			l[r[i]] := i;
			cost[i] := max(a[i], a[r[i]]);
			update(1, 1, n, i, i);
			cost[l[i]] := max(a[i], a[l[i]]);
			update(1, 1, n, l[i], l[i]);
		end;

		writeln(ans);

	end;
end.

```
