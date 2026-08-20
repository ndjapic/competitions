program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #segtree #lazy #inversions
uses
	math;
const
	NN = 200 * 1000;
	TT = 512 * 1024;
var
	n, i, x: int32;
	k, ans: int64;
	p, pos: array [1 .. NN] of int32;
	st: array [1 .. TT] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure combine(v: int32);
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
		st[v] := 1;
end;

procedure update(v, vl, vr, l, r: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (r <= vl) and (vr <= l) then begin
		st[v] := 0;
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
		result := 0
	else if (l <= vl) and (vr <= r) then
		result := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		result :=
			query(2*v, vl, m, l, r) +
			query(2*v+1, m+1, vr, l, r);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	for i := 1 to n do begin
		read(p[i]);
		pos[p[i]] := i;
	end;
	readln;

	build(1, 1, n);

	ans := -k;
	for x := 1 to n do begin
		update(1, 1, n, pos[x], pos[x]);
		inc(ans, query(1, 1, n, 1, pos[x]));
	end;

	writeln(max(ans, 0));
end.
