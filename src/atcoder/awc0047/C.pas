program _C;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
const
	nn = 200 * 1000;
	tt = 512 * 1024;
var
	n, q, i, k, l, r, x: int32;
	tp: int8;
	a: array [1 .. nn] of int64;
	st: array [1 .. tt] of int64;
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
		st[v] := 0;
end;

procedure update(v, vl, vr, l, r: int32; x: int64);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		inc(st[v], x);
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		update(2*v, vl, m, l, r, x);
		update(2*v+1, m+1, vr, l, r, x);
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
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(a[i]); readln;

	build(1, 0, n);

	for k := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(l, r, x);
				update(1, 0, n, l-1, l-1, x);
				update(1, 0, n, r, r, -x);
			end;

			2: begin
				readln(i);
				writeln(a[i] + query(1, 0, n, 0, i-1));
			end;
		
		end;
	end;

end.
