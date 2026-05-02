program _D;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 22;
	ee = 4;
	bigmod = uint64(1) shl 30;
type
	tbigint = array [0 .. ee] of uint64;
var
	n, i, e, ans: int8;
	p, q: array [1 .. nn] of int8;
	a, b: tbigint;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

function bigeq(a, b: tbigint): boolean;
var
	e: int8;
begin
	e := 0;
	while (e <= ee) and (a[e] = b[e]) do inc(e);
	result := e > ee;
end;

function bigmul(a: tbigint; b: int8): tbigint;
var
	e: int8;
begin
	result[0] := 0;
	for e := 0 to ee do begin
		inc(result[e], a[e] * b);
		if e < ee then begin
			result[e+1] := result[e] div bigmod;
			result[e] := result[e] mod bigmod;
		end;
	end;
end;

procedure dfs(i, d: int8; a, b: tbigint);
begin
	if d < ans then begin
		if i < n then begin
			inc(i);
			dfs(i, d, a, b);
			dfs(i, d+1, bigmul(a, p[i]), bigmul(b, q[i]));
		end else if (d >= 2) and bigeq(a, b) then
			ans := d;
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n);
	for i := 1 to n do readln(p[i], q[i]);

	a[0] := 1;
	b[0] := 1;
	for e := 1 to ee do begin
		a[e] := 0;
		b[e] := 0;
	end;

	ans := n+1;
	dfs(0, 0, a, b);
	if ans > n then ans := -1;
	writeln(ans);
end.
