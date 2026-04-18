program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, k, i: int32;
	a: array [1 .. nn] of int32;
	dp: array [1 .. nn] of int64;
	st: array [1 .. tt] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure update(v, vl, vr, i: int32);
var
	m: int32;
begin
	if (i < vl) or (vr < i) then
	else if (i <= vl) and (vr <= i) then begin
		st[v] := dp[i];
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, i);
		update(2*v+1, m+1, vr, i);
		st[v] := max(st[2*v], st[2*v+1]);
	end;
end;

function query(v, vl, vr, l, r: int32): int64;
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		query := low(int64)
	else if (l <= vl) and (vr <= r) then
		query := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query := max(
			query(2*v, vl, m, l, r),
			query(2*v+1, m+1, vr, l, r)
		);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, k);

	read(a[1]);
	dp[1] := a[1];
	update(1, 1, n, 1);

	for i := 2 to n do begin
		read(a[i]);
		dp[i] := query(1, 1, n, max(1, i-k), i-1) + a[i];
		update(1, 1, n, i);
	end;
	readln;

	writeln(dp[n]);
end.
