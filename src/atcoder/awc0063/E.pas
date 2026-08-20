program _E;
{$MODE DELPHI}{$OPTIMIZATION LEVEL3,ON}
// #segtree #lazy
uses
	math;
const
	NN = 200 * 1000;
	TT = 512 * 1024;
type
	tnode = record
		lc, rc, nb: int32;
	end;
var
	n, q, i, k, l, r, x: int32;
	tp: int8;
	c: array [1 .. NN] of int32;
	st: array [1 .. TT] of tnode;
	lz: array [1 .. TT] of int32;
	InputBuf, OutputBuf: array [1 .. 65536] of Char;

procedure combine(v: int32);
begin
	st[v].lc := st[2*v].lc;
	st[v].rc := st[2*v+1].rc;
	st[v].nb := st[2*v].nb + st[2*v+1].nb;
	if st[2*v].rc = st[2*v+1].lc then dec(st[v].nb)
end;

procedure unipaint(v, x: int32);
begin
	st[v].lc := x;
	st[v].rc := x;
	st[v].nb := 1;
end;

procedure push(v: int32);
begin
	if lz[v] > 0 then begin
		unipaint(2*v, lz[v]);
		unipaint(2*v+1, lz[v]);
		lz[2*v] := lz[v];
		lz[2*v+1] := lz[v];
		lz[v] := 0;
	end;
end;

procedure build(v, l, r: int32);
var
	m: int32;
begin
	lz[v] := 0;
	if l < r then begin
		m := (l+r) div 2;
		build(2*v, l, m);
		build(2*v+1, m+1, r);
		combine(v);
	end else
		unipaint(v, c[l]);
end;

procedure update(v, vl, vr, l, r, x: int32);
var
	m: int32;
begin
	if (r < vl) or (vr < l) then
	else if (l <= vl) and (vr <= r) then begin
		unipaint(v, x);
		lz[v] := x;
	end else {if vl < vr then} begin
		push(v);
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
		result := 0
	else if (l <= vl) and (vr <= r) then
		result := st[v].nb
	else {if vl < vr then} begin
		push(v);
		m := (vl+vr) div 2;
		result :=
			query(2*v, vl, m, l, r) +
			query(2*v+1, m+1, vr, l, r);
		if (l <= m) and (m+1 <= r) and (st[2*v].rc = st[2*v+1].lc) then
			dec(result);
	end;
end;

begin
	SetTextBuf(Input, InputBuf);
	SetTextBuf(Output, OutputBuf);

	readln(n, q);

	for i := 1 to n do read(c[i]); readln;

	build(1, 1, n);

	for k := 1 to q do begin
		read(tp);
		case tp of

			1: begin
				readln(l, r, x);
				update(1, 1, n, l, r, x);
			end;

			2: begin
				readln(l, r);
				writeln(query(1, 1, n, l, r));
			end;
		
		end;
	end;
end.
