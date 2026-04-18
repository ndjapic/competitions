program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
uses
	math;
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, m, i, j, v, l, r, ans: int32;
	w, c: array [1 .. nn] of int32;
	st: array [1 .. tt] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure combine(v: int32);
begin
	st[v] := max(st[2*v], st[2*v+1]);
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
		st[v] := c[l];
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, m);
	for i := 1 to n do read(w[i]); readln;
	for j := 1 to m do read(c[j]); readln;
	build(1, 1, m);

	ans := 0;
	for i := 1 to n do
		if st[1] >= w[i] then begin
			v := 1;
			l := 1;
			r := m;
			while l < r do begin
				j := (l+r) div 2;
				inc(v, v);
				if st[v] < w[i] then begin
					inc(v);
					l := j+1;
				end else
					r := j;
			end;

			inc(ans);
			st[v] := 0;
			while v > 1 do begin
				v := v div 2;
				combine(v);
			end;
		end;

	writeln(ans);
end.
