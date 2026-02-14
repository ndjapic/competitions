program _E;
{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 100 * 1000;
	tt = 256 * 1024;
	neutral = low(int32);
var
	n, q, i, l, r: int32;
	a: array [1 .. nn] of int32;
	st: array [1 .. tt] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function combine(x, y: int32): int32; inline;
begin
	combine := max(x, y);
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		st[v] := combine(st[2*v], st[2*v+1]);
	end else
		st[v] := a[l];
end;

function query(v, vl, vr, l, r: int32): int32;
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		query := neutral
	else if (l <= vl) and (vr <= r) then
		query := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query := combine(
			query(2*v, vl, m, l, r),
			query(2*v+1, m+1, vr, l, r)
		);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(a[i]);
	readln;

	build(1, 1, n);

	for i := 1 to q do begin
		readln(l, r);
		writeln(query(1, 1, n, l, r));
	end;
end.
