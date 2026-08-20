program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #segtree #point #update #global #query
uses
	math;
const
	NN = 200 * 1000;
	TT = 512 * 1024;
var
	n, q, i, k, v: int32;
	c: char;
	a, b: array [1 .. NN] of int32;
	st: array [1 .. TT] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure update(v, vl, vr, i: int32);
var
	m: int32;
begin
	if (i < vl) or (vr < i) then
	else if (i <= vl) and (vr <= i) then begin
		st[v] := min(a[i], b[i]);
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, i);
		update(2*v+1, m+1, vr, i);
		st[v] := st[2*v] + st[2*v+1];
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);
	for i := 1 to n do read(a[i]); readln;
	for i := 1 to n do read(b[i]); readln;
	for i := 1 to n do update(1, 1, n, i);

	for k := 1 to q do begin
		readln(c, i, v);
		case c of
			'A': a[i] := v;
			'B': b[i] := v;
		end;
		update(1, 1, n, i);
		writeln(st[1]);
	end;
end.
