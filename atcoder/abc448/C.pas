program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	maxn = 300 * 1000;
	maxt = 1024 * 1024;
var
	n, q, i, j, mn: int32;
	k, x: int8;
	a: array [1 .. maxn] of int32;
	b: array [0 .. 6] of int32;
	st: array [1 .. maxt] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		st[v] := min(st[2*v], st[2*v+1]);
	end else
		st[v] := a[l];
end;

function query(v, vl, vr, l, r: int32): int32;
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		query := high(int32)
	else if (l <= vl) and (vr <= r) then
		query := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		query := min(
			query(2*v, vl, m, l, r),
			query(2*v+1, m+1, vr, l, r)
		);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(a[i]); readln;
	build(1, 1, n);
	b[0] := 0;

	for j := 1 to q do begin
		read{ln}(k);
		mn := high(int32);

		for x := 1 to k+1 do begin
			if x <= k then
				read(b[x])
			else
				b[x] := n+1;
			mn := min(mn, query(1, 1, n, b[x-1]+1, b[x]-1));
		end;
		{readln;}

		writeln(mn);
	end;
end.
