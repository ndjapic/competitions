program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #lazy #segtree #point #segment #update #segment #query
const
	NN = 200 * 1000;
	TT = 512 * 1024;
var
	n, q, i, j, l, r: int32;
	tp: int8;
	d: int64;
	st, lz: array [1 .. TT] of int64;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure push(v, l, m, r: int32);
begin
	inc(st[2*v], lz[v] * (m - l + 1));
	inc(st[2*v+1], lz[v] * (r - m));
	inc(lz[2*v], lz[v]);
	inc(lz[2*v+1], lz[v]);
	lz[v] := 0;
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
	end;
	st[v] := 0;
	lz[v] := 0;
end;

procedure update(v, vl, vr, l, r: int32; d: int64);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		inc(st[v], d * (vr - vl + 1));
		inc(lz[v], d);
	end else {if vl < vr then} begin
		m := (vl+vr) div 2;
		push(v, vl, m, vr);
		update(2*v, vl, m, l, r, d);
		update(2*v+1, m+1, vr, l, r, d);
		st[v] := st[2*v] + st[2*v+1];
	end;
end;

function query(v, vl, vr, l, r: int32): int64;
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
		query := 0
	else if (l <= vl) and (vr <= r) then
		query := st[v]
	else {if vl < vr then} begin
		m := (vl+vr) div 2;
		push(v, vl, m, vr);
		query :=
			query(2*v, vl, m, l, r) +
			query(2*v+1, m+1, vr, l, r);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);
	build(1, 1, n);

	for j := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(l, r);
				update(1, 1, n, l, r, 1);
			end;

			2: begin
				readln(i);
				d := query(1, 1, n, i, i);
				update(1, 1, n, i, i, -d);
			end;

			3: begin
				readln(l, r);
				d := query(1, 1, n, l, r);
				writeln(d);
			end;

		end;
	end;
end.
